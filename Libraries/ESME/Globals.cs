using System.Windows.Threading;
using ESME.Data;
using ESME.Locations;
using ESME.Plugins;
using ESME.TransmissionLoss;
using HRC.Services;

namespace ESME
{
    public static class Globals
    {
        static AppSettings _appSettings;
        public static AppSettings AppSettings
        {
            get { return _appSettings; }
            set
            {
                if (_appSettings == value) return;
                _appSettings = value;
                HRCOpenFileService.InitialDirectories = _appSettings.OpenFileServiceDirectories;
            }
        }

        public static IPluginManagerService PluginManagerService { get; set; }
        public static TransmissionLossCalculatorService TransmissionLossCalculatorService { get; set; }
        public static IMasterDatabaseService MasterDatabaseService { get; set; }
        public static IUIVisualizerService VisualizerService { get; set; }
        public static IHRCSaveFileService SaveFileService { get; set; }
        public static IHRCOpenFileService OpenFileService { get; set; }
        public static EnvironmentalCacheService EnvironmentalCacheService { get; set; }
        public static IViewAwareStatus ViewAwareStatusService { get; set; }
        public static IMessageBoxService MessageBoxService { get; set; }
        public static Dispatcher Dispatcher { get; set; }
    }
}