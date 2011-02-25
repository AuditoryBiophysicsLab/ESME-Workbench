﻿using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.Xml.Serialization;
using ESME.Model;
using HRC.Navigation;

namespace ESME.TransmissionLoss
{
    public class AnalysisPoint : EarthCoordinate, IEquatable<AnalysisPoint>, IHasIDField
    {
        public AnalysisPoint()
        {
            TransmissionLossJobs = new ObservableCollection<TransmissionLossJob>();
            TransmissionLossFields = new ObservableCollection<TransmissionLossField>();
            SoundSources = new List<SoundSource>();
        }

        public AnalysisPoint(EarthCoordinate location) : this()
        {
            Latitude_degrees = location.Latitude_degrees;
            Longitude_degrees = location.Longitude_degrees;
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
    }

    public class NewAnalysisPointList : UniqueAutoIncrementList<AnalysisPoint> {}
}