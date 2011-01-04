using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
using Cinch;
namespace ESMEWorkBench.ViewModels.NAVODataSources
{
    public class DBDBViewModel : ViewModelBase
    {
        #region public string SelectedResolution { get; set; }

        public string SelectedResolution
        {
            get { return _selectedResolution; }
            set
            {
                if (_selectedResolution == value) return;
                _selectedResolution = value;
                NotifyPropertyChanged(SelectedResolutionChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedResolutionChangedEventArgs = ObservableHelper.CreateArgs<DBDBViewModel>(x => x.SelectedResolution);
        string _selectedResolution;

        #endregion

        #region public List<string> Resolutions { get; set; }

        public List<string> Resolutions
        {
            get { return _resolutions; }
            set
            {
                if (_resolutions == value) return;
                _resolutions = value;
                NotifyPropertyChanged(ResolutionsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ResolutionsChangedEventArgs = ObservableHelper.CreateArgs<DBDBViewModel>(x => x.Resolutions);
        List<string> _resolutions;

        #endregion

        

    }
}