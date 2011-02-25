using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using ESME.Model;
using ESME.NEMO;
using HRC.Navigation;

namespace ESME.TransmissionLoss
{
    public class SoundSource : EarthCoordinate, IEquatable<SoundSource>, INotifyPropertyChanged
    {
        protected SoundSource()
        {
            RadialBearings = new List<float>();
            ShouldBeCalculated = true;
            SoundSourceID = Path.GetFileNameWithoutExtension(Path.GetRandomFileName());
        }

        public SoundSource(EarthCoordinate location, NemoMode nemoMode, int radialCount) : this()
        {
            Latitude = location.Latitude;
            Longitude = location.Longitude;

            AcousticProperties = new AcousticProperties(nemoMode);

            var radialBearingStep = 360.0f / radialCount;
            for (var radialBearing = 0.0f; radialBearing < 360.0f; radialBearing += radialBearingStep) RadialBearings.Add(radialBearing);
            Radius = (int) nemoMode.Radius;
            Name = nemoMode.PSMName;
            SourceLevel = nemoMode.SourceLevel;
        }

        /// <summary>
        ///   The Acoustic Properties of this sound source
        /// </summary>
        public AcousticProperties AcousticProperties { get; set; }

        /// <summary>
        ///   Source Level in dB SPL re: 1uPa
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

        /// <summary>
        ///   Optional name of this sound source.
        /// </summary>
        public string Name { get; set; }

        /// <summary>
        ///   True if the user wants this sound source to be calculated, false otherwise.  Default value is true
        /// </summary>
        public bool ShouldBeCalculated { get; set; }

        #region IEquatable<SoundSource> methods

        public bool Equals(SoundSource other)
        {
            if (!base.Equals(other)) return false; // Compare as an EarthCoordinate first
            if (!AcousticProperties.Equals(other.AcousticProperties)) return false;
            if (RadialBearings.Count != other.RadialBearings.Count) return false;
            return !RadialBearings.Where((t, bearingIndex) => t != other.RadialBearings[bearingIndex]).Any();
        }

        #endregion

        public event PropertyChangedEventHandler PropertyChanged;
        protected void NotifyPropertyChanged(PropertyChangedEventArgs args) { if (PropertyChanged != null) PropertyChanged(this, args); }
    }
}