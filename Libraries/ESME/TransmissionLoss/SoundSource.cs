using System;
using System.Collections.Generic;
using System.IO;
using ESME.Model;
using HRC.Navigation;

namespace ESME.TransmissionLoss
{
    public class SoundSource : EarthCoordinate, IEquatable<SoundSource>
    {
        const int DefaultRadialCount = 16;
        const float RadialBearingStep = 360.0f / DefaultRadialCount;

        public SoundSource()
        {
            RadialBearings = new List<float>();
            for (var radialBearing = 0.0f; radialBearing < 360.0f; radialBearing += RadialBearingStep) RadialBearings.Add(radialBearing);
            SoundSourceID = Path.GetRandomFileName();
        }

        /// <summary>
        ///   The Acoustic Properties of this sound source
        /// </summary>
        public AcousticProperties AcousticProperties { get; set; }

        /// <summary>
        /// Source Level in dB SPL re: 1uPa
        /// </summary>
        public float SourceLevel { get; set; }

        /// <summary>
        ///   List of radial bearings, in degrees
        /// </summary>
        public List<float> RadialBearings { get; set; }

        /// <summary>
        ///   transmission loss radius, in meters.
        /// </summary>
        public int Radius { get; set; }

        /// <summary>
        ///   The presumptively-unique sound source ID.  Use this as the basis of the filename for transmission loss jobs and TLF files
        /// </summary>
        public string SoundSourceID { get; set; }

        public bool Equals(SoundSource other) 
        {
            if (!Equals(other)) return false;
            if (!AcousticProperties.Equals(other.AcousticProperties)) return false;
            if (RadialBearings.Count != other.RadialBearings.Count) return false;
            for (var bearingIndex = 0; bearingIndex < RadialBearings.Count; bearingIndex++)
                if (RadialBearings[bearingIndex] != other.RadialBearings[bearingIndex]) return false;
            return true;
        }
    }
}