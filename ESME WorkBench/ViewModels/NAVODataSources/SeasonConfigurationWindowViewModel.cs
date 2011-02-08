using System.ComponentModel;
using Cinch;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.NAVODataSources
{
    [ExportViewModel("SeasonConfigurationWindowViewModel")]
    internal class SeasonConfigurationWindowViewModel : ViewModelBase
    {
        #region public SeasonConfigurationViewModel SeasonConfigurationViewModel { get; set; }

        static readonly PropertyChangedEventArgs SeasonConfigurationViewModelChangedEventArgs = ObservableHelper.CreateArgs<SeasonConfigurationWindowViewModel>(x => x.SeasonConfigurationViewModel);
        SeasonConfigurationViewModel _seasonConfigurationViewModel = new SeasonConfigurationViewModel();

        public SeasonConfigurationViewModel SeasonConfigurationViewModel
        {
            get { return _seasonConfigurationViewModel; }
            set
            {
                if (_seasonConfigurationViewModel == value) return;
                _seasonConfigurationViewModel = value;
                NotifyPropertyChanged(SeasonConfigurationViewModelChangedEventArgs);
            }
        }

        #endregion

        #region OkCommand

        SimpleCommand<object, object> _ok;

        public SimpleCommand<object, object> OkCommand
        {
            get
            {
                return _ok ?? (_ok = new SimpleCommand<object, object>(delegate
                {
                    //if all the database locations have been filled in, etc, then the user can click. 

                    return true;
                    // (!string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.GDEMDirectory) && !string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.SMGCDirectory) && !string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.BSTDirectory) && !string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.DBDBDirectory) && !string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.GDEMEXEPath) && !string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.SMGCEXEPath) && !string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.BSTEXEPath) && !string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.DBDBEXEPath));
                }, delegate
                {
                    //fire off a message, and close the window.
                    SeasonConfigurationViewModel.AppSettings.Save();
                    CloseActivePopUpCommand.Execute(true);
                }));
            }
        }

        #endregion

        #region CancelCommand

        SimpleCommand<object, object> _cancel;

        public SimpleCommand<object, object> CancelCommand
        {
            get
            {
                return _cancel ?? (_cancel = new SimpleCommand<object, object>(delegate
                                                                               {
                                                                                   SeasonConfigurationViewModel.AppSettings.Reload();
                                                                                   CloseActivePopUpCommand.Execute(false);
                                                                               }));
            }
        }

        #endregion

    }
}