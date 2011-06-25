using System.ComponentModel;
using Cinch;

namespace ESME.Environment.NAVO
{
    public class SMGCBackgroundExtractor : NAVOBackgroundExtractor
    {
        #region public Wind Wind { get; set; }

        public Wind Wind
        {
            get { return _wind; }
            set
            {
                if (_wind == value) return;
                _wind = value;
                NotifyPropertyChanged(WindChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs WindChangedEventArgs = ObservableHelper.CreateArgs<SMGCBackgroundExtractor>(x => x.Wind);
        Wind _wind;

        #endregion

        protected override void Run(object sender, DoWorkEventArgs e)
        {
            RunState = "Running";
            TaskName = "Wind data extraction";
            var backgroundExtractor = (NAVOBackgroundExtractor)e.Argument;
            Wind = SurfaceMarineGriddedClimatologyDatabase.ExtractArea(backgroundExtractor);
        }
    }
}