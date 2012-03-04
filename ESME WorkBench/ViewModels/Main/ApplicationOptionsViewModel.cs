using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.Composition;
using Cinch;
using ESME.Data;
using ESME.Environment;
using ESME.Plugins;

namespace ESMEWorkbench.ViewModels.Main
{
    public class ApplicationOptionsViewModel : ViewModelBase
    {
        readonly IMessageBoxService _messageBoxService;
        public ApplicationOptionsViewModel(IMessageBoxService messageBoxService, IPluginManagerService pluginManagerService)
        {
            _messageBoxService = messageBoxService;
            _pluginManagerService = pluginManagerService;
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
            AppSettings.DefaultPluginConfigurations = _pluginManagerService.ESMEPluginDictionary.GetDefaultPluginConfigurations();
            AppSettings.Save(null);
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

        #region public string GDEMDirectory { get; set; }

        public string GDEMDirectory
        {
            get { return Globals.AppSettings.NAVOConfiguration.GDEMDirectory; }
            set
            {
                Globals.AppSettings.NAVOConfiguration.ValidateGDEMDirectory(value, _messageBoxService);
                NotifyPropertyChanged(GDEMDirectoryChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs GDEMDirectoryChangedEventArgs = ObservableHelper.CreateArgs<ApplicationOptionsViewModel>(x => x.GDEMDirectory);

        #endregion

        #region public string SMGCDirectory { get; set; }

        public string SMGCDirectory
        {
            get { return Globals.AppSettings.NAVOConfiguration.SMGCDirectory; }
            set
            {
                Globals.AppSettings.NAVOConfiguration.ValidateSMGCDirectory(value, _messageBoxService);
                NotifyPropertyChanged(SMGCDirectoryChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SMGCDirectoryChangedEventArgs = ObservableHelper.CreateArgs<ApplicationOptionsViewModel>(x => x.SMGCDirectory);

        #endregion

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

        readonly IPluginManagerService _pluginManagerService;
        public ESMEPluginDictionary ESMEPluginDictionary
        {
            get { return _pluginManagerService.ESMEPluginDictionary; }
        }
    }
}