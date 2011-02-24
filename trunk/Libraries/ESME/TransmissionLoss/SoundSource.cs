using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.IO;
using System.Xml.Serialization;
using Cinch;
using ESME.Model;
using HRC.Navigation;

namespace ESME.TransmissionLoss
{
    public class SoundSource : EarthCoordinate, IEquatable<SoundSource>, INotifyPropertyChanged
    {
        const int DefaultRadialCount = 16;
        const float RadialBearingStep = 360.0f / DefaultRadialCount;

        public SoundSource()
        {
            RadialBearings = new ObservableCollection<float>();
            for (var radialBearing = 0.0f; radialBearing < 360.0f; radialBearing += RadialBearingStep) RadialBearings.Add(radialBearing);
            SoundSourceID = Path.GetRandomFileName();
            ShouldBeCalculated = true;
        }

        /// <summary>
        ///   The Acoustic Properties of this sound source
        /// </summary>
        public AcousticProperties AcousticProperties { get; set; }

        /// <summary>
        ///   Source Level in dB SPL re: 1uPa
        /// </summary>
        public float SourceLevel { get; set; }

        #region public ObservableCollection<float> RadialBearings { get; set; }

        /// <summary>
        ///   List of radial bearings, in degrees
        /// </summary>
        [XmlElement]
        public ObservableCollection<float> RadialBearings
        {
            get { return _radialBearings; }
            set
            {
                if (_radialBearings == value) return;
                if (_radialBearings != null) _radialBearings.CollectionChanged -= RadialBearingsCollectionChanged;
                _radialBearings = value;
                if (_radialBearings != null) _radialBearings.CollectionChanged += RadialBearingsCollectionChanged;
                NotifyPropertyChanged(RadialBearingsChangedEventArgs);
            }
        }

        void RadialBearingsCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(RadialBearingsChangedEventArgs); }
        [XmlIgnore] static readonly PropertyChangedEventArgs RadialBearingsChangedEventArgs = ObservableHelper.CreateArgs<SoundSource>(x => x.RadialBearings);
        [XmlIgnore] ObservableCollection<float> _radialBearings;

        #endregion

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
        /// True if the user wants this sound source to be calculated, false otherwise.  Default value is true
        /// </summary>
        public bool ShouldBeCalculated { get; set; }

        #region IEquatable<SoundSource> methods

        public bool Equals(SoundSource other)
        {
            if (!base.Equals(other)) return false;  // Compare as an EarthCoordinate first
            if (!AcousticProperties.Equals(other.AcousticProperties)) return false;
            if (RadialBearings.Count != other.RadialBearings.Count) return false;
            for (var bearingIndex = 0; bearingIndex < RadialBearings.Count; bearingIndex++) if (RadialBearings[bearingIndex] != other.RadialBearings[bearingIndex]) return false;
            return true;
        }

        #endregion

        public event PropertyChangedEventHandler PropertyChanged;
        protected void NotifyPropertyChanged(PropertyChangedEventArgs args) { if (PropertyChanged != null) PropertyChanged(this, args); }

    }
}