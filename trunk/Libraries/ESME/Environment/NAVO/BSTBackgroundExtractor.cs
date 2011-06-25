using System.ComponentModel;
using Cinch;

namespace ESME.Environment.NAVO
{
    public class BSTBackgroundExtractor : NAVOBackgroundExtractor
    {
        #region public Sediment Sediment { get; set; }

        public Sediment Sediment
        {
            get { return _sediment; }
            set
            {
                if (_sediment == value) return;
                _sediment = value;
                NotifyPropertyChanged(SedimentChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SedimentChangedEventArgs = ObservableHelper.CreateArgs<BSTBackgroundExtractor>(x => x.Sediment);
        Sediment _sediment;

        #endregion

        protected override void Run(object sender, DoWorkEventArgs e)
        {
            RunState = "Running";
            TaskName = "Sediment data extraction";
            var backgroundExtractor = (NAVOBackgroundExtractor)e.Argument;
            Sediment = BottomSedimentTypeDatabase.ExtractArea(backgroundExtractor);
        }
    }
}