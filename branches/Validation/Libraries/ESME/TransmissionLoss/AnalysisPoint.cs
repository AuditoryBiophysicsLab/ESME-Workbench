using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Xml.Serialization;
using Cinch;
using ESME.Environment;
using ESME.Model;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.TransmissionLoss
{
    public class AnalysisPoint : EarthCoordinate, IEquatable<AnalysisPoint>, IHasIDField, ISupportValidation
    {
        public static WeakReference<Environment2DData> Bathymetry = new WeakReference<Environment2DData>(null);

        public AnalysisPoint()
        {
            TransmissionLossJobs = new ObservableCollection<TransmissionLossJob>();
            TransmissionLossFields = new ObservableCollection<TransmissionLossField>();
            SoundSources = new List<SoundSource>();
        }

        public AnalysisPoint(EarthCoordinate location) : this()
        {
            Latitude = location.Latitude;
            Longitude = location.Longitude;
        }

        public List<SoundSource> SoundSources { get; set; }

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

        #region IHasIDField Members

        [XmlElement("AnalysisPointID")]
        public ulong IDField { get; set; }

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
            if (!bathymetry.GeoRect.Contains(this))
            {
                ValidationErrorText = "Analysis point not contained within bathymetry bounds";
                return;
            }

            var errorCount = 0;

            string result = null;
            foreach (var source in SoundSources)
            {
                result = source.ValidationErrorText;
                if (!string.IsNullOrEmpty(result)) errorCount++;
                if (errorCount > 1)
                {
                    ValidationErrorText = "Errors in multiple sound sources";
                    return;
                }
            }
            ValidationErrorText = result;
        }

        #region INotifyPropertyChanged Members

        public event PropertyChangedEventHandler PropertyChanged;
        protected void NotifyPropertyChanged(PropertyChangedEventArgs args) { if (PropertyChanged != null) PropertyChanged(this, args); }

        #endregion

    }

    public class NewAnalysisPointList : UniqueAutoIncrementList<AnalysisPoint> {}
}