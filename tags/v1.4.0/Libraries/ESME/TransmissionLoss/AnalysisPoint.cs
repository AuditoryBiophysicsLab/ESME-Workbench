using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.Xml.Serialization;
using ESME.Model;
using HRC.Navigation;

namespace ESME.TransmissionLoss
{
    public class AnalysisPoint : IEquatable<AnalysisPoint>, IHasIDField
    {
        public AnalysisPoint()
        {
            TransmissionLossJobs = new ObservableCollection<TransmissionLossJob>();
            TransmissionLossFields = new ObservableCollection<TransmissionLossField>();
        }
        /// <summary>
        ///   EarthCoordinate of the analysis point
        /// </summary>
        public EarthCoordinate EarthCoordinate { get; set; }

        /// <summary>
        ///   Bearing of the lowest-numbered radial from this point.  All radials will be evenly spaced starting with this bearing
        /// </summary>
        public float RadialBearing { get; set; }

        /// <summary>
        ///   How many radials will be calculated for all active modes at this analysis point
        /// </summary>
        public int RadialCount { get; set; }

        #region public ObservableCollection<TransmissionLossJob> TransmissionLossJobs { get; set; }

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

        void TransmissionLossJobsCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
        }
        ObservableCollection<TransmissionLossJob> _transmissionLossJobs = new ObservableCollection<TransmissionLossJob>();

        #endregion

        [XmlIgnore]
        #region public ObservableCollection<TransmissionLossField> TransmissionLossFields { get; set; }

        public ObservableCollection<TransmissionLossField> TransmissionLossFields
        {
            get { return _transmissionLossFields; }
            set
            {
                if (_transmissionLossFields == value) return;
                if (_transmissionLossFields != null) _transmissionLossFields.CollectionChanged -= TransmissionLossFieldsCollectionChanged;
                _transmissionLossFields = value;
                if (_transmissionLossFields != null) _transmissionLossFields.CollectionChanged += TransmissionLossFieldsCollectionChanged;
            }
        }

        void TransmissionLossFieldsCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
        }
        ObservableCollection<TransmissionLossField> _transmissionLossFields;

        #endregion
        
        #region IEquatable<AnalysisPoint> Members

        bool IEquatable<AnalysisPoint>.Equals(AnalysisPoint that)
        {
            if (!EarthCoordinate.Equals(that.EarthCoordinate)) return false;
            if (RadialBearing != that.RadialBearing) return false;
            return RadialCount == that.RadialCount;
        }

        #endregion

        #region IHasIDField Members

        [XmlElement("AnalysisPointID")]
        public ulong IDField { get; set; }

        #endregion
    }

    public class NewAnalysisPointList : UniqueAutoIncrementList<AnalysisPoint> {}
}