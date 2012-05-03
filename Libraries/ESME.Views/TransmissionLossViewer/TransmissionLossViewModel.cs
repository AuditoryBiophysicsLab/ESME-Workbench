using System;
using System.Linq;
using System.Text;
using System.Windows;
using ESME.Scenarios;
using HRC.Aspects;
using HRC.Services;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Views.TransmissionLossViewer
{
    public class TransmissionLossViewModel : ViewModelBase,IViewStatusAwareInjectionAware
    {
        [Affects("RadialCount")]
        public ObservableList<Radial> Radials { get; set; }

        private string _titleString = "<no radial selected>";
        public string TitleString
        {
            get { return _titleString; }
            set { _titleString = value; }
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
        int _selectedRadialIndex;

        [Affects("SelectedBearingGeometry","SelectedRadial","TitleString")]
        public int SelectedRadialIndex
        {
            get { return _selectedRadialIndex; }
            set
            {
                _selectedRadialIndex = value;
                TitleString = Radials == null ? "<no radial selected>" : string.Format("Radial bearing: {0:000.0} degrees", Radials[_selectedRadialIndex].Bearing);
                if(RadialViewModel!=null) RadialViewModel.Radial = Radials == null ? null:Radials[_selectedRadialIndex];
            }
        }
        #endregion

        #region public string SelectedBearingGeometry { get; }
        public string SelectedBearingGeometry
        {
            get
            {
                var sb = new StringBuilder();
                if (RadialViewModel == null || RadialViewModel.Radial == null) return null;
                var bearing = RadialViewModel.Radial.Bearing;
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
        private Window _window;
        public Window Window
        {
            get { return _window; }
            set
            {
                _window = value;
                if (RadialViewModel == null)
                {
                    RadialViewModel = new RadialViewModel(_window.FindChildren<RadialView>().First())
                                          {Radial = Radials == null ? null : Radials[_selectedRadialIndex]};
                }
            }
        }

        private Scenarios.TransmissionLoss _transmissionLoss;
        [Affects("Radials", "RadialCount", "SelectedRadialIndex", "SelectedRadial", "SelectedBearingGeometry")]
        public Scenarios.TransmissionLoss TransmissionLoss
        {
            get { return _transmissionLoss; }
            set
            {
                _transmissionLoss = value;
                Radials = _transmissionLoss == null ? null : new ObservableList<Radial>(from r in _transmissionLoss.Radials orderby r.Bearing select r);
                SelectedRadialIndex = 0;
            }
        }

        public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService)
        {
            if (RadialViewModel == null)
                RadialViewModel =new RadialViewModel(((Window) viewAwareStatusService.View).FindChildren<RadialView>().First());
        }
    }
}
