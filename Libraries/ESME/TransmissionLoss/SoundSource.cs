using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Text;
using System.Xml.Serialization;
using Cinch;
using ESME.Environment;
using ESME.Model;
using ESME.NEMO;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.TransmissionLoss
{
    public class SoundSource : EarthCoordinate, IEquatable<SoundSource>, ISupportValidation
    {
        public static WeakReference<Environment2DData> Bathymetry = new WeakReference<Environment2DData>(null);

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

        #region public bool ShouldBeCalculated { get; set; }

        /// <summary>
        ///   True if the user wants this sound source to be calculated, false otherwise.  Default value is true
        /// </summary>
        public bool ShouldBeCalculated
        {
            get { return _shouldBeCalculated; }
            set
            {
                if (_shouldBeCalculated == value) return;
                _shouldBeCalculated = value;
                NotifyPropertyChanged(ShouldBeCalculatedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ShouldBeCalculatedChangedEventArgs = ObservableHelper.CreateArgs<SoundSource>(x => x.ShouldBeCalculated);
        bool _shouldBeCalculated;

        #endregion

        #region public TransmissionLossAlgorithm TransmissionLossAlgorithm { get; set; }

        public TransmissionLossAlgorithm TransmissionLossAlgorithm
        {
            get { return _transmissionLossAlgorithm; }
            set
            {
                if (_transmissionLossAlgorithm == value) return;
                _transmissionLossAlgorithm = value;
                NotifyPropertyChanged(TransmissionLossAlgorithmChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TransmissionLossAlgorithmChangedEventArgs = ObservableHelper.CreateArgs<SoundSource>(x => x.TransmissionLossAlgorithm);
        TransmissionLossAlgorithm _transmissionLossAlgorithm;

        #endregion

        #region IEquatable<SoundSource> methods

        public bool Equals(SoundSource other)
        {
            if (!base.Equals(other)) return false; // Compare as an EarthCoordinate first
            if (!AcousticProperties.Equals(other.AcousticProperties)) return false;
            if (RadialBearings.Count != other.RadialBearings.Count) return false;
            return !RadialBearings.Where((t, bearingIndex) => t != other.RadialBearings[bearingIndex]).Any();
        }

        #endregion

        #region public bool IsValid { get; private set; }

        [XmlIgnore]
        public bool IsValid
        {
            get
            {
                Validate();
                return _isValid;
            }
            private set
            {
                if (_isValid == value) return;
                _isValid = value;
                NotifyPropertyChanged(IsValidChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsValidChangedEventArgs = ObservableHelper.CreateArgs<SoundSource>(x => x.IsValid);
        bool _isValid;

        #endregion

        #region public string ValidationErrorText { get; private set; }

        [XmlIgnore]
        public string ValidationErrorText
        {
            get
            {
                Validate();
                return _validationErrorText;
            }
            private set
            {
                if (_validationErrorText == value) return;
                _validationErrorText = value;
                IsValid = string.IsNullOrEmpty(_validationErrorText);
                NotifyPropertyChanged(ValidationErrorTextChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ValidationErrorTextChangedEventArgs = ObservableHelper.CreateArgs<SoundSource>(x => x.ValidationErrorText);
        string _validationErrorText;

        #endregion

        public void Validate()
        {
            if ((Bathymetry == null) || (Bathymetry.Target == null))
            {
                ValidationErrorText = "Unable to validate";
                return;
            }
            var bathymetry = Bathymetry.Target;
            if (!bathymetry.GeoRect.Contains(this))
            {
                ValidationErrorText = "Sound source not contained within bathymetry bounds";
                return;
            }
            var errors = new StringBuilder();
            foreach (var radialBearing in RadialBearings)
            {
                var radialEndPoint = new EarthCoordinate(this, radialBearing, Radius);
                if (!bathymetry.GeoRect.Contains(radialEndPoint)) errors.AppendLine(string.Format("Radial with bearing {0} extends beyond bathymetry bounds", radialBearing));
            }
            if (errors.Length > 0)
            {
                ValidationErrorText = errors.ToString();
                return;
            }
            ValidationErrorText = null;
        }

        public event PropertyChangedEventHandler PropertyChanged;
        protected void NotifyPropertyChanged(PropertyChangedEventArgs args) { if (PropertyChanged != null) PropertyChanged(this, args); }
    }
}