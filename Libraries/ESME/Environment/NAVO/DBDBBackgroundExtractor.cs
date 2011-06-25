using System.ComponentModel;
using Cinch;

namespace ESME.Environment.NAVO
{
    public class DBDBBackgroundExtractor : NAVOBackgroundExtractor
    {
        #region public Bathymetry Bathymetry { get; set; }

        public Bathymetry Bathymetry
        {
            get { return _bathymetry; }
            set
            {
                if (_bathymetry == value) return;
                _bathymetry = value;
                NotifyPropertyChanged(BathymetryChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BathymetryChangedEventArgs = ObservableHelper.CreateArgs<DBDBBackgroundExtractor>(x => x.Bathymetry);
        Bathymetry _bathymetry;

        #endregion

        protected override void Run(object sender, DoWorkEventArgs e)
        {
            RunState = "Running";
            TaskName = "Bathymetry data extraction";
            var backgroundExtractor = (NAVOBackgroundExtractor)e.Argument;
            Bathymetry = DigitalBathymetricDatabase.ExtractArea(backgroundExtractor);
        }
    }
}