using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
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
        public NameGuidRecord ScenarioRecord { get; set; }
        [Initialize] public List<ActorNameGuid> PlatformRecords { get; set; }
        [Initialize] public List<ActorNameGuid> ModeRecords { get; set; }
        [Initialize] public List<SpeciesNameGuid> SpeciesRecords { get; set; }
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
                ScenarioRecord = new NameGuidRecord(scenario),
            };
            var actorID = 0;
            var modeID = 0;
            foreach (var platform in scenario.Platforms)
            {
                platform.ActorID = actorID++;
                result.PlatformRecords.Add(new ActorNameGuid(platform));
                foreach (var mode in platform.Sources.SelectMany(source => source.Modes)) 
                {
                    mode.ModeID = modeID++;
                    result.ModeRecords.Add(new ActorNameGuid(mode));
                }
            }
            foreach (var species in scenario.ScenarioSpecies)
            {
                species.StartActorID = actorID;
                actorID += species.Animat.Locations.Count;
                result.SpeciesRecords.Add(new SpeciesNameGuid(species));
            }
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

            ScenarioRecord.Write(_writer);

            _writer.Write(PlatformRecords.Count);
            foreach (var record in PlatformRecords) record.Write(_writer);

            _writer.Write(ModeRecords.Count);
            foreach (var record in ModeRecords) record.Write(_writer);

            _writer.Write(SpeciesRecords.Count);
            foreach (var record in SpeciesRecords) record.Write(_writer);

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

            ScenarioRecord = NameGuidRecord.Read(_reader);

            var count = _reader.ReadInt32();
            for (var i = 0; i < count; i++) PlatformRecords.Add(ActorNameGuid.Read(_reader));

            count = _reader.ReadInt32();
            for (var i = 0; i < count; i++) ModeRecords.Add(ActorNameGuid.Read(_reader));

            count = _reader.ReadInt32();
            for (var i = 0; i < count; i++) SpeciesRecords.Add(SpeciesNameGuid.Read(_reader));

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

    public static class SimulationLogExtensions
    {
        public static NameGuidRecord RecordFromActorID(this SimulationLog log, int actorID) 
        {
            if (log == null) throw new ArgumentNullException("log");
            if (actorID < 0) throw new ArgumentOutOfRangeException("actorID");
            foreach (var platformRecord in log.PlatformRecords.Where(r => r.ActorID == actorID)) return platformRecord;
            return log.SpeciesRecords.FirstOrDefault(species => species.ActorID <= actorID && actorID < species.ActorID + species.AnimatCount);
        }

        public static string NameFromActorID(this SimulationLog log, int actorID)
        {
            var result = log.RecordFromActorID(actorID);
            return result != null ? result.Name : "<Unknown>";
        }

        public static Guid GuidFromActorID(this SimulationLog log, int actorID)
        {
            var result = log.RecordFromActorID(actorID);
            return result != null ? result.Guid : Guid.Empty;
        }

        public static NameGuidRecord RecordFromModeID(this SimulationLog log, int modeID)
        {
            if (log == null) throw new ArgumentNullException("log");
            if (modeID < 0) throw new ArgumentOutOfRangeException("modeID");
            return log.ModeRecords.FirstOrDefault(r => r.ActorID == modeID);
        }
        public static string NameFromModeID(this SimulationLog log, int modeID)
        {
            var result = log.RecordFromModeID(modeID);
            return result != null ? result.Name : "<Unknown>";
        }
        public static Guid GuidFromModeID(this SimulationLog log, int modeID)
        {
            var result = log.RecordFromModeID(modeID);
            return result != null ? result.Guid : Guid.Empty;
        }
    }

    public class NameGuidRecord
    {
        internal NameGuidRecord(Scenario scenario) : this(scenario.Name, scenario.Guid) {}
        internal NameGuidRecord(string name, Guid guid)
        {
            Name = name;
            Guid = guid;
        }

        public string Name { get; private set; }
        public Guid Guid { get; private set; }

        public static NameGuidRecord Read(BinaryReader reader) { return new NameGuidRecord(reader.ReadString(), new Guid(reader.ReadBytes(16))); }
        public virtual void Write(BinaryWriter writer)
        {
            writer.Write(Name);
            writer.Write(Guid.ToByteArray());
        }
    }

    public class ActorNameGuid : NameGuidRecord
    {
        internal ActorNameGuid(Platform platform) : this(platform.ActorID, platform.PlatformName, platform.Guid) { }
        internal ActorNameGuid(Mode mode) : this(mode.ModeID, mode.ModeName, mode.Guid) { }
        internal ActorNameGuid(int actorID, string name, Guid guid) : base(name, guid) { ActorID = actorID; }

        public int ActorID { get; set; }

        public static new ActorNameGuid Read(BinaryReader reader) { return new ActorNameGuid(reader.ReadInt32(), reader.ReadString(), new Guid(reader.ReadBytes(16))); }

        public override void Write(BinaryWriter writer)
        {
            writer.Write(ActorID);
            base.Write(writer);
        }
    }

    public class SpeciesNameGuid : ActorNameGuid
    {
        internal SpeciesNameGuid(ScenarioSpecies species) : this(species.Animat.Locations.Count,species.StartActorID, species.LatinName, species.Guid) { }
        internal SpeciesNameGuid(int animatCount, int startActorID, string name, Guid guid) : base(startActorID, name, guid) { AnimatCount = animatCount; }

        public int AnimatCount { get; set; }

        public static new SpeciesNameGuid Read(BinaryReader reader) { return new SpeciesNameGuid(reader.ReadInt32(), reader.ReadInt32(), reader.ReadString(), new Guid(reader.ReadBytes(16))); }

        public override void Write(BinaryWriter writer)
        {
            writer.Write(AnimatCount);
            base.Write(writer);
        }
    }
}