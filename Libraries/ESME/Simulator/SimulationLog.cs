using System;
using System.Collections.Generic;
using System.IO;
using HRC.Utility;

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
        public int TimeStepCount { get; private set; }
        public PercentProgress<SimulationLog> PercentProgress { get; private set; }
        // Add any further metadata here

        readonly List<long> _timeStepOffsets;
        BinaryWriter _writer;
        BinaryReader _reader;
        int _curStepIndex;
        TimeSpan _curTimeStep;

        SimulationLog()
        {
            _timeStepOffsets = new List<long>();
        }

        public static SimulationLog Create(string fileName, int timeStepCount, TimeSpan timeStepSize)
        {
            if (timeStepSize.TotalMilliseconds <= 0) throw new ArgumentOutOfRangeException("timeStepSize", "The time step size must be greater than zero");
            var result = new SimulationLog
            {
                _writer = new BinaryWriter(new FileStream(fileName, FileMode.Create, FileAccess.Write, FileShare.None)),
                StartTime = DateTime.Now,
                CreatingUser = System.Environment.UserName,
                CreatingComputer = System.Environment.MachineName,
                TimeStepSize = timeStepSize,
                TimeStepCount = timeStepCount,
            };
            for (var i = 0; i < timeStepCount; i++) result._timeStepOffsets.Add(-1);
            result.WriteHeader();
            result.PercentProgress = new PercentProgress<SimulationLog>(result) { MinimumValue = 0, MaximumValue = timeStepCount - 1 };
            return result;
        }

        public static SimulationLog Open(string fileName)
        {
            var result = new SimulationLog
            {
                _reader = new BinaryReader(new FileStream(fileName, FileMode.Open, FileAccess.Read, FileShare.Read)),
            };
            result.ReadHeader();
            return result;
        }

        public void WriteHeader()
        {
            if (_writer == null) throw new IOException("The simulation log file has not been opened for writing");
            _writer.Seek(0, SeekOrigin.Begin);
            _writer.Write(Magic);
            _writer.Write(TimeStepSize.Ticks);
            _writer.Write(StartTime.Ticks);
            _writer.Write(DateTime.Now.Ticks);
            _writer.Write(CreatingUser);
            _writer.Write(CreatingComputer);
            _writer.Write(_timeStepOffsets.Count);
            foreach (var offset in _timeStepOffsets) _writer.Write(offset);
            //_writer.BaseStream.Flush();
        }

        void ReadHeader()
        {
            if (_reader == null) throw new IOException("The simulation log file has not been opened for reading");
            _reader.BaseStream.Seek(0, SeekOrigin.Begin);
            if (Magic != _reader.ReadUInt64()) throw new FileFormatException("Error reading simulation log file, magic number not seen at position 0");
            TimeStepSize = new TimeSpan(_reader.ReadInt64());
            StartTime = new DateTime(_reader.ReadInt64());
            EndTime = new DateTime(_reader.ReadInt64());
            CreatingUser = _reader.ReadString();
            CreatingComputer = _reader.ReadString();
            TimeStepCount = _reader.ReadInt32();
            for (var i = 0; i < TimeStepCount; i++) _timeStepOffsets.Add(_reader.ReadInt64());
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
                return SimulationTimeStepRecord.Read(_reader, _timeStepOffsets[timeStepIndex]);
            }
        }

        public void Add(SimulationTimeStepRecord timeStep)
        {
            using (var writer = new BinaryWriter(new MemoryStream()))
            {
                _timeStepOffsets[_curStepIndex] = _writer.BaseStream.Seek(0, SeekOrigin.End);
                timeStep.Write(writer, _curTimeStep);
                _curStepIndex++;
                PercentProgress.Report(_curStepIndex);
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
            WriteHeader();
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
}