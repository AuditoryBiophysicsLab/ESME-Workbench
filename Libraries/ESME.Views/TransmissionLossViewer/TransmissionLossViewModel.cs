using System;
using System.Diagnostics;
using System.IO;
using System.Text;
using ESME.Scenarios;
using HRC.Aspects;
using HRC.Utility;
using HRC.ViewModels;

namespace ESME.Views.TransmissionLossViewer
{
    public class TransmissionLossViewModel : ViewModelBase
    {
        [Affects("RadialCount")]
        public ObservableList<Radial> Radials { get; set; }

        public int RadialCount
        {
            get
            {
                if (TransmissionLoss == null) return 0;
                return Radials.Count - 1;
            }
        }

        #region public int SelectedRadialIndex {get; set;}
        int _selectedRadialIndex;

        [Affects("SelectedBearingGeometry","SelectedRadial")]
        public int SelectedRadialIndex
        {
            get { return _selectedRadialIndex; }
            set
            {
                _selectedRadialIndex = value;
                RadialViewModel.Radial = Radials[_selectedRadialIndex];
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
                if (RadialViewModel == null || RadialViewModel.Radial == null) return null;
                bearing = RadialViewModel.Radial.Bearing;
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
        [Affects("Radials", "RadialCount", "SelectedRadialIndex", "SelectedRadial", "SelectedBearingGeometry")]
        public Scenarios.TransmissionLoss TransmissionLoss
        {
            get { return _transmissionLoss; }
            set
            {
                _transmissionLoss = value;
                Radials = _transmissionLoss == null ? null : new ObservableList<Radial>(_transmissionLoss.Radials);
                SelectedRadialIndex = 0;
            }
        }
    }
}
