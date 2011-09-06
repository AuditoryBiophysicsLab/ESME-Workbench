using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.IO;
using System.Text;
using System.Xml.Serialization;
using Cinch;
using ESME.Environment;
using ESME.Model;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.TransmissionLoss
{
    [Serializable]
    public class AnalysisPoint : EarthCoordinate, IEquatable<AnalysisPoint>, ISupportValidation
    {
        private AnalysisPoint()
        {
            TransmissionLossJobs = new ObservableCollection<TransmissionLossJob>();
            TransmissionLossFields = new ObservableCollection<TransmissionLossField>();
            SoundSources = new List<SoundSource>();
            AnalysisPointID = Path.GetFileNameWithoutExtension(Path.GetRandomFileName());
            OldLocation = null;
        }

        public AnalysisPoint(Geo location) : this()
        {
            Latitude = location.Latitude;
            Longitude = location.Longitude;
        }

        /// <summary>
        ///   The presumptively-unique analysis point ID.
        /// </summary>
        public string AnalysisPointID { get; set; }

        public List<SoundSource> SoundSources { get; set; }

        [XmlIgnore]
        public EarthCoordinate OldLocation { get; set; }

        #region public new double Latitude { get; set; }
        [XmlIgnore]
        public new double Latitude
        {
            get { return base.Latitude; }
            set
            {
                if (OldLocation == null) OldLocation = new EarthCoordinate(base.Latitude, base.Longitude);
                base.Latitude = value;
                NotifyPropertyChanged(LatitudeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LatitudeChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPoint>(x => x.Latitude);

        #endregion

        #region public new double Longitude { get; set; }
        [XmlIgnore]
        public new double Longitude
        {
            get { return base.Longitude; }
            set
            {
                if (OldLocation == null) OldLocation = new EarthCoordinate(base.Latitude, base.Longitude);
                base.Longitude = value;
                NotifyPropertyChanged(LongitudeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LongitudeChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPoint>(x => x.Longitude);
        
        #endregion

        #region public WeakReference<Bathymetry> Bathymetry { get; set; }

        [XmlIgnore]
        public WeakReference<Bathymetry> Bathymetry
        {
            get { return _bathymetry ?? (_bathymetry = new WeakReference<Bathymetry>(null)); }
            set
            {
                if (_bathymetry == value) return;
                _bathymetry = value;
                foreach (var soundSource in SoundSources) soundSource.Bathymetry = _bathymetry;
            }
        }

        WeakReference<Bathymetry> _bathymetry;

        #endregion

        #region public ObservableCollection<TransmissionLossJob> TransmissionLossJobs { get; set; }
        [XmlIgnore]
        public ObservableCollection<TransmissionLossJob> TransmissionLossJobs
        {
            get { return _transmissionLossJobs; }
            set
            {
                if (_transmissionLossJobs == value) return;
                if (_transmissionLossJobs != null) _transmissionLossJobs.CollectionChanged -= TransmissionLossJobsCollectionChanged;
                _transmissionLossJobs = value;
                if (_transmissionLossJobs != null) _transmissionLossJobs.CollectionChanged += TransmissionLossJobsCollectionChanged;
            }
        }

        static void TransmissionLossJobsCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { }
        ObservableCollection<TransmissionLossJob> _transmissionLossJobs = new ObservableCollection<TransmissionLossJob>();

        #endregion

        #region public ObservableCollection<TransmissionLossField> TransmissionLossFields { get; private set; }
        [XmlIgnore]
        public ObservableCollection<TransmissionLossField> TransmissionLossFields
        {
            get { return _transmissionLossFields; }
            private set
            {
                if (_transmissionLossFields == value) return;
                if (_transmissionLossFields != null) _transmissionLossFields.CollectionChanged -= TransmissionLossFieldsCollectionChanged;
                _transmissionLossFields = value;
                if (_transmissionLossFields != null) _transmissionLossFields.CollectionChanged += TransmissionLossFieldsCollectionChanged;
            }
        }

        static void TransmissionLossFieldsCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { }
        ObservableCollection<TransmissionLossField> _transmissionLossFields;

        #endregion

        #region IEquatable<AnalysisPoint> Members

        bool IEquatable<AnalysisPoint>.Equals(AnalysisPoint other)
        {
            if (!Equals(other)) return false;
            if (SoundSources.Count != other.SoundSources.Count) return false;
            for (var sourceIndex = 0; sourceIndex < SoundSources.Count; sourceIndex++) 
                if (!SoundSources[sourceIndex].Equals(other.SoundSources[sourceIndex])) return false;
            return true;
        }

        #endregion

        #region public bool IsValid { get; set; }

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

        static readonly PropertyChangedEventArgs IsValidChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPoint>(x => x.IsValid);
        bool _isValid;

        #endregion

        #region public string ValidationErrorText { get; set; }
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

        static readonly PropertyChangedEventArgs ValidationErrorTextChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPoint>(x => x.ValidationErrorText);
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
            if (!bathymetry.Samples.GeoRect.Contains(this))
            {
                ValidationErrorText = "Analysis point not contained within bathymetry bounds";
                return;
            }

            var sourcesInError = new List<string>();
            
            foreach (var source in SoundSources)
            {
                source.Validate();
                if (!source.IsValid) sourcesInError.Add(source.Name);
            }
            switch (sourcesInError.Count)
            {
                case 0:
                    ValidationErrorText = null;
                    break;
                case 1:
                    ValidationErrorText = string.Format("Source {0} has errors", sourcesInError[0]);
                    break;
                default:
                    var result = new StringBuilder();
                    result.AppendLine("The following sources have errors:");
                    foreach (var sourceName in sourcesInError) result.AppendLine(string.Format("  • {0}", sourceName));
                    ValidationErrorText = result.ToString();
                    break;
            }
        }

        #region INotifyPropertyChanged Members

        public event PropertyChangedEventHandler PropertyChanged;
        protected void NotifyPropertyChanged(PropertyChangedEventArgs args) { if (PropertyChanged != null) PropertyChanged(this, args); }

        #endregion

    }
}