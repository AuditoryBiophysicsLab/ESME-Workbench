using System.Collections;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using HRC.Aspects;
using HRC.Navigation;

namespace ESME.Simulator
{
    public class ActorPositionRecord : IEnumerable<ActorExposureRecord>
    {
        public float Latitude { get; set; }
        public float Longitude { get; set; }
        public float Depth { get; set; }
        [Initialize] public ConcurrentBag<ActorExposureRecord> Exposures { get; private set; }

        public ActorPositionRecord(float latitude, float longitude, float depth)
        {
            Latitude = latitude;
            Longitude = longitude;
            Depth = depth;
        }
        public ActorPositionRecord(Geo<float> geo) : this((float)geo.Latitude, (float)geo.Longitude, geo.Data) { }
        public ActorPositionRecord(Geo geo, float depth) : this((float)geo.Latitude, (float)geo.Longitude, depth) { }
        ActorPositionRecord() { }

        internal static ActorPositionRecord Read(BinaryReader reader, long offsetFromBeginningOfFile = -1)
        {
            if (offsetFromBeginningOfFile > 0) reader.BaseStream.Seek(offsetFromBeginningOfFile, SeekOrigin.Begin);
            return new ActorPositionRecord
            {
                Latitude = reader.ReadSingle(),
                Longitude = reader.ReadSingle(),
                Depth = reader.ReadSingle(),
            };
        }

        internal void Write(BinaryWriter writer)
        {
            writer.Write(Latitude);
            writer.Write(Longitude);
            writer.Write(Depth);
        }

        public IEnumerator<ActorExposureRecord> GetEnumerator() { return Exposures.GetEnumerator(); }

        IEnumerator IEnumerable.GetEnumerator() { return GetEnumerator(); }
    }
}