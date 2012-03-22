using System;
using System.Collections.Generic;
using System.IO;
using HRC;
using HRC.Aspects;

namespace ESME.Simulator
{
    public class SimulationTimeStepRecord
    {
        public const ulong Magic = 0xd3c603dd0d7a1ee6;

        public TimeSpan StartTime { get; private set; }
        [Initialize, UsedImplicitly] public List<ActorPositionRecord> ActorPositionRecords { get; private set; }

        readonly List<int> _actorOffsets;

        public SimulationTimeStepRecord() { _actorOffsets = new List<int>();}
        
        int _actorCount;
        BinaryReader _reader;
        long _offsetFromBeginningOfFile;

        internal static SimulationTimeStepRecord ReadAll(BinaryReader reader, long offsetFromBeginningOfFile)
        {
            var result = Read(reader, offsetFromBeginningOfFile);
            for (var i = 0; i < result._actorCount; i++)
                result.ActorPositionRecords.Add(ActorPositionRecord.Read(reader));
            return result;
        }

        public SimulationTimeStepRecord ReadAll()
        {
            if (_reader == null) throw new IOException("The simulation log file has not been opened for reading");
            for (var i = 0; i < _actorCount; i++)
                ActorPositionRecords.Add(ActorPositionRecord.Read(_reader));
            return this;
        }

        internal static SimulationTimeStepRecord Read(BinaryReader reader, long offsetFromBeginningOfFile)
        {
            reader.BaseStream.Seek(offsetFromBeginningOfFile, SeekOrigin.Begin);
            if (Magic != reader.ReadUInt64()) throw new FileFormatException("Error reading simulation time step record, magic number not seen at proper file position");
            var result = new SimulationTimeStepRecord
            {
                StartTime = new TimeSpan(reader.ReadInt64()),
                _actorCount = reader.ReadInt32(),
                _reader = reader,
                _offsetFromBeginningOfFile = offsetFromBeginningOfFile,
            };
            for (var i = 0; i < result._actorCount; i++) result._actorOffsets.Add(reader.ReadInt32());
            return result;
        }

        internal void Write(BinaryWriter writer, TimeSpan startTime)
        {
            StartTime = startTime;
            _actorOffsets.Clear();
            if (writer == null) throw new IOException("The simulation log file has not been opened for writing");
            _offsetFromBeginningOfFile = writer.BaseStream.Position;
            WriteHeader(writer);
            foreach (var actorPositionRecord in ActorPositionRecords) 
            {
                _actorOffsets.Add((int)(writer.BaseStream.Position - _offsetFromBeginningOfFile));
                actorPositionRecord.Write(writer);
            }
            // Go back and write the header again to get the proper offsets
            WriteHeader(writer);
        }

        void WriteHeader(BinaryWriter writer)
        {
            writer.BaseStream.Seek(_offsetFromBeginningOfFile, SeekOrigin.Begin);
            writer.Write(Magic);
            writer.Write(StartTime.Ticks);
            _actorCount = ActorPositionRecords.Count;
            writer.Write(_actorCount);
            foreach (var actorOffset in _actorOffsets)
                writer.Write(actorOffset);
        }

        public ActorPositionRecord this[int actorIndex]
        {
            get
            {
                if (ActorPositionRecords == null)
                {
                    if (_reader == null) throw new IOException("The simulation log file has not been opened for reading");
                    if (actorIndex < 0 || actorIndex >= _actorOffsets.Count) throw new IndexOutOfRangeException(string.Format("Requested actor index {0} is invalid.  Valid values are 0 - {1}", actorIndex, _actorOffsets.Count));
                    return ActorPositionRecord.Read(_reader, _offsetFromBeginningOfFile + _actorOffsets[actorIndex]);
                }
                return ActorPositionRecords[actorIndex];
            }
        }
    }
}