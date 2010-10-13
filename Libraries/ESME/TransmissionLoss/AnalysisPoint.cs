using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Xml.Serialization;
using ESME.Model;
using HRC.Navigation;

namespace ESME.TransmissionLoss
{
    public class AnalysisPoint : IEquatable<AnalysisPoint>, IHasIDField
    {
        /// <summary>
        /// Location of the analysis point
        /// </summary>
        public EarthCoordinate Location { get; set; }
        /// <summary>
        /// Bearing of the lowest-numbered radial from this point.  All radials will be evenly spaced starting with this bearing
        /// </summary>
        public float RadialBearing { get; set; }
        /// <summary>
        /// How many radials will be calculated for all active modes at this analysis point
        /// </summary>
        public int RadialCount { get; set; }

        #region public ObservableCollection<TransmissionLossJob> TransmissionLossJobs { get; set; }

        public ObservableCollection<TransmissionLossJob> TransmissionLossJobs
        {
            get { return _transmissionLossJobs; }
            set
            {
                if (_transmissionLossJobs == value) return;
                _transmissionLossJobs = value;
            }
        }

        ObservableCollection<TransmissionLossJob> _transmissionLossJobs = new ObservableCollection<TransmissionLossJob>();

        #endregion


        #region IEquatable<AnalysisPoint> Members

        bool IEquatable<AnalysisPoint>.Equals(AnalysisPoint that)
        {
            if (!Location.Equals(that.Location))
                return false;
            if (RadialBearing != that.RadialBearing)
                return false;
            return RadialCount == that.RadialCount;
        }

        #endregion

        #region IHasIDField Members

        [XmlElement("AnalysisPointID")]
        public int IDField { get; set; }

        #endregion
        
    }

    public class AnalysisPointDeletedEventArgs : ItemDeletedEventArgs<AnalysisPoint>
    {
    }

    public class NewAnalysisPointList : UniqueAutoIncrementList<AnalysisPoint>
    {
    }
}