using System;
using System.Collections.Generic;
using System.IO;
using ESME.Scenarios;
using HRC.Aspects;

namespace ESME.Simulator
{
    public class SimulationLog : IDisposable
    {
        public const ulong Magic = 0xa57d8ee659dc45ec;
        public TimeSpan TimeStepSize { get; private set; }
        public DateTime StartTime { get; private set; }
        public DateTime EndTime { get; private set; }
        public string CreatingUser { get; private set; }
        public string CreatingComputer { get; private set; }
        public int TimeStepCount { get { return _timeStepOffsets.Count; } }
        public ScenarioIdentityRecord ScenarioIdentityRecord { get; set; }
        [Initialize] public List<PlatformIdentityRecord> PlatformIdentityRecords { get; set; }
        [Initialize] public List<SpeciesIdentityRecord> SpeciesIdentityRecords { get; set; }
        // Add any further metadata here

        readonly List<long> _timeStepOffsets = new List<long>();
        BinaryWriter _writer;
        BinaryReader _reader;
        int _curStepIndex;
        TimeSpan _curTimeStep;

        SimulationLog() {}

        public static SimulationLog Create(string fileName, TimeSpan timeStepSize, Scenario scenario)
        {
            if (timeStepSize.TotalMilliseconds <= 0) throw new ArgumentOutOfRangeException("timeStepSize", "The time step size must be greater than zero");
            var result = new SimulationLog
            {
                _writer = new BinaryWriter(new FileStream(fileName, FileMode.Create, FileAccess.Write, FileShare.None)),
                StartTime = DateTime.Now,
                CreatingUser = System.Environment.UserName,
                CreatingComputer = System.Environment.MachineName,
                TimeStepSize = timeStepSize,
                ScenarioIdentityRecord = new ScenarioIdentityRecord(scenario),
            };
            foreach (var platform in scenario.Platforms) result.PlatformIdentityRecords.Add(new PlatformIdentityRecord(platform));
            foreach (var species in scenario.ScenarioSpecies) result.SpeciesIdentityRecords.Add(new SpeciesIdentityRecord(species));
            return result;
        }

        public static SimulationLog Open(string fileName)
        {
            var result = new SimulationLog
            {
                _reader = new BinaryReader(new FileStream(fileName, FileMode.Open, FileAccess.Read, FileShare.Read)),
            };
            result.ReadTrailer();
            return result;
        }

        void WriteTrailer()
        {
            if (_writer == null) throw new IOException("The simulation log file has not been opened for writing");
            var trailerOffset = _writer.Seek(0, SeekOrigin.End);
            _writer.Write(TimeStepSize.Ticks);
            _writer.Write(StartTime.Ticks);
            _writer.Write(DateTime.Now.Ticks);
            _writer.Write(CreatingUser);
            _writer.Write(CreatingComputer);

            ScenarioIdentityRecord.Write(_writer);

            _writer.Write(PlatformIdentityRecords.Count);
            foreach (var record in PlatformIdentityRecords) record.Write(_writer);

            _writer.Write(SpeciesIdentityRecords.Count);
            foreach (var record in SpeciesIdentityRecords) record.Write(_writer);

            _writer.Write(_timeStepOffsets.Count);
            foreach (var offset in _timeStepOffsets) _writer.Write(offset);

            _writer.Write(trailerOffset);
            _writer.Write(Magic);
        }

        void ReadTrailer()
        {
            if (_reader == null) throw new IOException("The simulation log file has not been opened for reading");
            _reader.BaseStream.Seek(-16, SeekOrigin.End);
            var trailerOffset = _reader.ReadInt64();
            if (Magic != _reader.ReadUInt64()) throw new FileFormatException("Error reading simulation log file, magic number not seen at expected location");
            _reader.BaseStream.Seek(trailerOffset, SeekOrigin.Begin);
            TimeStepSize = new TimeSpan(_reader.ReadInt64());
            StartTime = new DateTime(_reader.ReadInt64());
            EndTime = new DateTime(_reader.ReadInt64());
            CreatingUser = _reader.ReadString();
            CreatingComputer = _reader.ReadString();

            ScenarioIdentityRecord = ScenarioIdentityRecord.Read(_reader);

            var count = _reader.ReadInt32();
            for (var i = 0; i < count; i++) PlatformIdentityRecords.Add(PlatformIdentityRecord.Read(_reader));

            count = _reader.ReadInt32();
            for (var i = 0; i < count; i++) SpeciesIdentityRecords.Add(SpeciesIdentityRecord.Read(_reader));

            count = _reader.ReadInt32();
            for (var i = 0; i < count; i++) _timeStepOffsets.Add(_reader.ReadInt64());
        }

        public SimulationTimeStepRecord this[int timeStepIndex]
        {
            get
            {
                if (_reader == null) throw new IOException("The simulation log file has not been opened for reading");
                if (timeStepIndex < 0 || timeStepIndex >= _timeStepOffsets.Count)
                    throw new IndexOutOfRangeException(string.Format("Requested time step index {0} is invalid.  Valid values are 0 - {1}", timeStepIndex, _timeStepOffsets.Count));
                if (_timeStepOffsets[timeStepIndex] <= 0)
                    throw new IndexOutOfRangeException("Requested time step index {0} is invalid.  The number of time steps indicates that this index SHOULD be valid, but no data were found for the requested time step.  It appears that this simulation did not complete successfully.");
                return SimulationTimeStepRecord.ReadHeader(_reader, _timeStepOffsets[timeStepIndex]);
            }
        }

        public void Add(SimulationTimeStepRecord timeStep)
        {
            using (var writer = new BinaryWriter(new MemoryStream()))
            {
                _timeStepOffsets.Add(_writer.BaseStream.Seek(0, SeekOrigin.End));
                timeStep.Write(writer, _curTimeStep);
                _curStepIndex++;
                _curTimeStep += TimeStepSize;
                writer.BaseStream.Flush();
                _writer.Write(((MemoryStream)writer.BaseStream).GetBuffer());
            }
        }

        public void AddRange(IEnumerable<SimulationTimeStepRecord> timeSteps)
        {
            using (var writer = new BinaryWriter(new MemoryStream()))
            {
                _writer.BaseStream.Seek(0, SeekOrigin.End);
                foreach (var timeStep in timeSteps)
                {
                    _timeStepOffsets[_curStepIndex] = _writer.BaseStream.Position + writer.BaseStream.Position;
                    //timeStep.Write(_writer, _curTimeStep);
                    timeStep.Write(writer, _curTimeStep);
                    _curStepIndex++;
                    _curTimeStep += TimeStepSize;
                }
                writer.BaseStream.Flush();
                _writer.Write(((MemoryStream)writer.BaseStream).GetBuffer());
            }
        }

        public void Close()
        {
            if (_reader != null) _reader.Close();
            if (_writer == null) return;
            WriteTrailer();
            _writer.Close();
        }

        #region IDisposable implementation
        // Implement IDisposable.
        // Do not make this method virtual.
        // A derived class should not be able to override this method.
        public void Dispose()
        {
            Dispose(true);
            // This object will be cleaned up by the Dispose method.
            // Therefore, you should call GC.SupressFinalize to
            // take this object off the finalization queue
            // and prevent finalization code for this object
            // from executing a second time.
            GC.SuppressFinalize(this);
        }

        bool _disposed;
        // Dispose(bool disposing) executes in two distinct scenarios.
        // If disposing equals true, the method has been called directly
        // or indirectly by a user's code. Managed and unmanaged resources
        // can be disposed.
        // If disposing equals false, the method has been called by the
        // runtime from inside the finalizer and you should not reference
        // other objects. Only unmanaged resources can be disposed.
        protected virtual void Dispose(bool disposing)
        {
            // Check to see if Dispose has already been called.
            if (_disposed) return;
            // If disposing equals true, dispose all managed
            // and unmanaged resources.
            if (disposing)
            {
                Close();
                // Dispose managed resources.
                if (_reader != null) _reader.Dispose();
                if (_writer != null) _writer.Dispose();
            }

            // Note disposing has been done.
            _disposed = true;
        }

        ~SimulationLog()
        {
            // Do not re-create Dispose clean-up code here.
            // Calling Dispose(false) is optimal in terms of
            // readability and maintainability.
            Dispose(false);
        }
        #endregion
    }

    public class ScenarioIdentityRecord
    {
        internal ScenarioIdentityRecord(Scenario scenario) : this(scenario.Name, scenario.Guid) {}
        internal ScenarioIdentityRecord(string scenarioName, Guid guid)
        {
            ScenarioName = scenarioName;
            Guid = guid;
        }

        public string ScenarioName { get; private set; }
        public Guid Guid { get; private set; }

        public static ScenarioIdentityRecord Read(BinaryReader reader) { return new ScenarioIdentityRecord(reader.ReadString(), new Guid(reader.ReadBytes(16))); }
        public void Write(BinaryWriter writer)
        {
            writer.Write(ScenarioName);
            writer.Write(Guid.ToByteArray());
        }
    }

    public class PlatformIdentityRecord
    {
        internal PlatformIdentityRecord(Platform platform) : this(platform.ActorID, platform.Guid) { }
        internal PlatformIdentityRecord(int actorID, Guid guid)
        {
            ActorID = actorID;
            Guid = guid;
        }

        public int ActorID { get; set; }
        public Guid Guid { get; set; }

        public static PlatformIdentityRecord Read(BinaryReader reader) { return new PlatformIdentityRecord(reader.ReadInt32(), new Guid(reader.ReadBytes(16))); }

        public void Write(BinaryWriter writer)
        {
            writer.Write(ActorID);
            writer.Write(Guid.ToByteArray());
        }
    }

    public class SpeciesIdentityRecord
    {
        internal SpeciesIdentityRecord(ScenarioSpecies species) : this(species.StartActorID, species.Animat.Locations.Count, species.Guid) { }
        internal SpeciesIdentityRecord(int startActorID, int animatCount, Guid guid)
        {
            StartActorID = startActorID;
            AnimatCount = animatCount;
            Guid = guid;
        }

        public int StartActorID { get; set; }
        public int AnimatCount { get; set; }
        public Guid Guid { get; set; }

        public static SpeciesIdentityRecord Read(BinaryReader reader) { return new SpeciesIdentityRecord(reader.ReadInt32(), reader.ReadInt32(), new Guid(reader.ReadBytes(16))); }

        public void Write(BinaryWriter writer)
        {
            writer.Write(StartActorID);
            writer.Write(AnimatCount);
            writer.Write(Guid.ToByteArray());
        }
    }
}