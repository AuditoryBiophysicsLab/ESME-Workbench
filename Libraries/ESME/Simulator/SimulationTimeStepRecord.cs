using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using HRC;
using HRC.Aspects;

namespace ESME.Simulator
{
    public class SimulationTimeStepRecord : IEnumerable<ActorPositionRecord>
    {
        public const ulong Magic = 0xd3c603dd0d7a1ee6;

        public TimeSpan StartTime { get; private set; }
        [Initialize, UsedImplicitly] public List<ActorPositionRecord> ActorPositionRecords { get; private set; }

        int _actorCount;
        BinaryReader _reader;
        long _offsetFromBeginningOfFile;
        int _exposureRecordCount;

        internal static SimulationTimeStepRecord ReadAll(BinaryReader reader, long offsetFromBeginningOfFile)
        {
            var result = ReadHeader(reader, offsetFromBeginningOfFile);
            for (var i = 0; i < result._actorCount; i++)
                result.ActorPositionRecords.Add(ActorPositionRecord.Read(reader));
            return result;
        }

        public void ReadAll()
        {
            if (_reader == null) throw new IOException("The simulation log file has not been opened for reading");
            _reader.BaseStream.Seek(_offsetFromBeginningOfFile, SeekOrigin.Begin);
            for (var i = 0; i < _actorCount; i++) ActorPositionRecords.Add(ActorPositionRecord.Read(_reader));
            _exposureRecordCount = _reader.ReadInt32();
            for (var i = 0; i < _exposureRecordCount; i++)
            {
                var exposure = ActorExposureRecord.Read(_reader);
                ActorPositionRecords[exposure.ActorID].Exposures.Add(exposure);
            }
        }

        internal static SimulationTimeStepRecord ReadHeader(BinaryReader reader, long offsetFromBeginningOfFile)
        {
            if (reader == null) throw new IOException("The simulation log file has not been opened for reading");
            reader.BaseStream.Seek(offsetFromBeginningOfFile, SeekOrigin.Begin);
            if (Magic != reader.ReadUInt64()) throw new FileFormatException("Error reading simulation time step record, magic number not seen at proper file position");
            var result = new SimulationTimeStepRecord
            {
                StartTime = new TimeSpan(reader.ReadInt64()),
                _actorCount = reader.ReadInt32(),
                _reader = reader,
                _offsetFromBeginningOfFile = reader.BaseStream.Position,
            };
            return result;
        }

        internal void Write(BinaryWriter writer, TimeSpan startTime)
        {
            if (writer == null) throw new IOException("The simulation log file has not been opened for writing");
            StartTime = startTime;
            _offsetFromBeginningOfFile = writer.BaseStream.Position;
            writer.Write(Magic);
            writer.Write(StartTime.Ticks);
            _actorCount = ActorPositionRecords.Count;
            writer.Write(_actorCount);
            _exposureRecordCount = 0;
            foreach (var actorPositionRecord in ActorPositionRecords)
            {
                actorPositionRecord.Write(writer);
                _exposureRecordCount += actorPositionRecord.Exposures.Count;
            }
            writer.Write(_exposureRecordCount);
            foreach (var exposure in ActorPositionRecords.SelectMany(actorPositionRecord => actorPositionRecord.Exposures)) exposure.Write(writer);
        }

        public ActorPositionRecord this[int actorIndex]
        {
            get
            {
                if (ActorPositionRecords.Count == 0) ReadAll();
                return ActorPositionRecords[actorIndex];
            }
        }

        public IEnumerator<ActorPositionRecord> GetEnumerator()
        {
            if (ActorPositionRecords.Count == 0) ReadAll();
            return ActorPositionRecords.GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator() { return GetEnumerator(); }
    }
}