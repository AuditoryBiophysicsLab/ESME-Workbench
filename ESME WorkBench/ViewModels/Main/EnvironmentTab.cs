using System.ComponentModel;
using Cinch;
using ESME.Views.Locations;

namespace ESMEWorkBench.ViewModels.Main
{
    public partial class MainViewModel
    {
        #region Bindable Properties

        #region public bool IsEditingLocation { get; set; }

        public bool IsEditingLocation
        {
            get { return _isEditingLocation; }
            set
            {
                if (_isEditingLocation == value) return;
                _isEditingLocation = value;
                NotifyPropertyChanged(IsEditingLocationChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs IsEditingLocationChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsEditingLocation);
        private bool _isEditingLocation;

        #endregion

        #endregion

        #region Commands

        #region NewLocationCommand

        public SimpleCommand<object, object> NewLocationCommand
        {
            get
            {
                return _newLocation ??
                       (_newLocation =
                        new SimpleCommand<object, object>(delegate { NewLocationHandler(); }));
            }
        }

        private SimpleCommand<object, object> _newLocation;

        void NewLocationHandler()
        {
            var vm = new NewLocationViewModel(Globals.AppSettings);
            var result = _visualizerService.ShowDialog("NewLocationView", vm);
            if ((result.HasValue) && (result.Value))
            {
                IsEditingLocation = true;
            }
        }
        #endregion

        #endregion
    }
}
