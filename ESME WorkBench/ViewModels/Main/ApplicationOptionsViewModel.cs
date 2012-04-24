using System.Collections.Generic;
using ESME.Data;
using ESME.Environment;
using HRC.Services;
using HRC.ViewModels;

namespace ESMEWorkbench.ViewModels.Main
{
    public class ApplicationOptionsViewModel : ViewModelBase
    {
        readonly IMessageBoxService _messageBoxService;
        public ApplicationOptionsViewModel(IMessageBoxService messageBoxService)
        {
            _messageBoxService = messageBoxService;
            Globals.AppSettings = AppSettings.Load();
            AppSettings = Globals.AppSettings;
        }

        public void DesignTimeInitialization() { AppSettings = AppSettings.Load(); }

        public AppSettings AppSettings { get; private set; }

        #region OkCommand

        public SimpleCommand<object, object> OkCommand
        {
            get
            {
                return _ok ??
                       (_ok =
                        new SimpleCommand<object, object>(delegate { return IsOkCommandEnabled; },
                                                          delegate { OkHandler(); }));
            }
        }

        private SimpleCommand<object, object> _ok;

        private bool IsOkCommandEnabled
        {
            get
            {
                return true;
            }
        }

        private void OkHandler()
        {
            AppSettings.Save();
            Globals.AppSettings = AppSettings.Load();
            CloseActivePopUpCommand.Execute(true);
        }

        #endregion

        #region CancelCommand

        public SimpleCommand<object, object> CancelCommand
        {
            get
            {
                return _cancel ??
                       (_cancel =
                        new SimpleCommand<object, object>(delegate { return IsCancelCommandEnabled; },
                                                          delegate { CancelHandler(); }));
            }
        }

        private SimpleCommand<object, object> _cancel;

        private bool IsCancelCommandEnabled
        {
            get { return true; }
        }

        private void CancelHandler()
        {
            AppSettings = AppSettings.Load();
            CloseActivePopUpCommand.Execute(false);
        }

        #endregion

        #region public List<TimePeriod> Months { get; set; }

        public List<TimePeriod> Months
        {
            get
            {
                return _months ?? (_months = new List<TimePeriod>
                                             {
                                                 TimePeriod.January,
                                                 TimePeriod.February,
                                                 TimePeriod.March,
                                                 TimePeriod.April,
                                                 TimePeriod.May,
                                                 TimePeriod.June,
                                                 TimePeriod.July,
                                                 TimePeriod.August,
                                                 TimePeriod.September,
                                                 TimePeriod.October,
                                                 TimePeriod.November,
                                                 TimePeriod.December,
                                             });
            }
        }

        List<TimePeriod> _months;

        #endregion

        #region public Dictionary<string, int> MaxImportThreadCountChoices { get; set; }

        public Dictionary<string, int> MaxImportThreadCountChoices
        {
            get
            {
                return _maxImportThreadCountChoices ?? (_maxImportThreadCountChoices = new Dictionary<string, int>
                {
                    {"1", 1},
                    {"2", 2},
                    {"4", 4},
                    {"6", 6},
                    {"8", 8},
                    {"As many as possible", -1},
                });
            }
        }

        Dictionary<string, int> _maxImportThreadCountChoices;

        #endregion

#if false
        #region public string ScenarioDataDirectory { get; set; }

        public string ScenarioDataDirectory
        {
            get { return Globals.AppSettings.ScenarioDataDirectory; }
            set
            {
                Globals.AppSettings.ValidateScenarioDataDirectory(value, _messageBoxService);
                NotifyPropertyChanged(ScenarioDataDirectoryChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ScenarioDataDirectoryChangedEventArgs = ObservableHelper.CreateArgs<ApplicationOptionsViewModel>(x => x.ScenarioDataDirectory);

        #endregion
#endif
    }
}