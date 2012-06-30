using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using HRC.Aspects;
using HRC.Navigation;

namespace ESME.Simulator
{
    public class ActorPositionRecord
    {
        public float Latitude { get; set; }
        public float Longitude { get; set; }
        public float Depth { get; set; }
        [Initialize] public ConcurrentBag<ActorExposureRecord> Exposures { get; set; }

        public ActorPositionRecord(float latitude, float longitude, float depth)
        {
            Latitude = latitude;
            Longitude = longitude;
            Depth = depth;
        }
        public ActorPositionRecord(Geo<float> geo) : this((float)geo.Latitude, (float)geo.Longitude, geo.Data) { }
        ActorPositionRecord() { }

        int _exposureCount;
        
        internal static ActorPositionRecord Read(BinaryReader reader, long offsetFromBeginningOfFile = -1)
        {
            if (offsetFromBeginningOfFile > 0) reader.BaseStream.Seek(offsetFromBeginningOfFile, SeekOrigin.Begin);
            var result = new ActorPositionRecord
            {
                Latitude = reader.ReadSingle(),
                Longitude = reader.ReadSingle(),
                Depth = reader.ReadSingle(),
                _exposureCount = reader.ReadInt32(),
            };
            for (var i = 0; i < result._exposureCount; i++) result.Exposures.Add(ActorExposureRecord.Read(reader));
            return result;
        }

        internal void Write(BinaryWriter writer)
        {
            writer.Write(Latitude);
            writer.Write(Longitude);
            writer.Write(Depth);
            writer.Write(Exposures.Count);
            foreach (var exposure in Exposures)
                exposure.Write(writer);
        }
    }
}