using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using ESME.Scenarios;
using HRC.Aspects;
using HRC.Utility;
using HRC.ViewModels;

namespace ESME.Views.TransmissionLossViewer
{
    public class TransmissionLossViewModel : ViewModelBase
    {
        private ObservableList<Radial> _radials;
        public ObservableList<Radial> Radials
        {
            get { return _radials ?? (_radials = new ObservableList<Radial>(TransmissionLoss.Radials)); }
        }

        public int RadialCount
        {
            get
            {
                if (TransmissionLoss == null) return 0;
                return Radials.Count - 1;
            }
        }

        #region public int SelectedRadialIndex {get; set;}

        [Affects("SelectedBearingGeometry","SelectedRadial")]
        public int SelectedRadialIndex { get; set; }

        #endregion

        #region public Radial SelectedRadial {get; set;}

        public Radial SelectedRadial
        {
            get
            {
                if (TransmissionLoss == null) return null;
                return Radials[SelectedRadialIndex];
            }
        }
        #endregion

        #region public string SelectedBearingGeometry { get; }
        public string SelectedBearingGeometry
        {
            get
            {
                Debug.WriteLine("SelectedBearingGeometry Updated");
                var sb = new StringBuilder();
                var bearing = 0.0;
                if (SelectedRadial != null) bearing = SelectedRadial.Bearing;
                const double radius = 8;
                double x, y;
                for (double angle = 0; angle <= 2 * Math.PI; angle += Math.PI / 32.0)
                {
                    sb.Append(sb.Length == 0 ? "M" : "L");
                    x = (Math.Sin(angle) * radius) + radius;
                    y = (Math.Cos(angle) * radius) + radius;
                    sb.Append(string.Format(" {0:0.###},{1:0.###} ", x, y));
                }
                sb.Append(string.Format("M {0:0.###}, {0:0.###} ", radius));
                x = (Math.Sin(bearing * (Math.PI / 180)) * radius) + radius;
                y = (-Math.Cos(bearing * (Math.PI / 180)) * radius) + radius;
                sb.Append(string.Format("L {0:0.###},{1:0.###} ", x, y));
                return sb.ToString();
            }
        }
        #endregion

        public RadialViewModel RadialViewModel { get; set; }

        private Scenarios.TransmissionLoss _transmissionLoss;
        [Affects("Radials", "RadialCount", "SelectedRadialIndex", "SelectedRadial","SelectedBearingGeometry")]
        public Scenarios.TransmissionLoss TransmissionLoss
        {
            get { return _transmissionLoss; }
            set
            {
                _transmissionLoss = value;
                _radials = null;
                SelectedRadialIndex = 0;
            }
        }
    }
}
