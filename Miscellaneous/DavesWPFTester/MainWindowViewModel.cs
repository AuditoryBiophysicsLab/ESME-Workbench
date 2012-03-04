using System.Collections.Generic;
using System.ComponentModel;
using Cinch;
using ESME;
using HRC.Collections;
using HRC.Plugins;
using HRC.Utility;
using MEFedMVVM.ViewModelLocator;
using System.ComponentModel.Composition;

namespace DavesWPFTester
{
    [ExportViewModel("MainWindowViewModel")]
    public class MainWindowViewModel : ViewModelBase
    {
        public MainWindowViewModel()
        {
            AllPlugins = PluginManager.FindPlugins<IESMEPlugin>(@"C:\Projects\ESME Deliverables\Plugins\Environmental Data Sources\InstallableNAVO\bin\Debug", 
                p => (p.PluginType == PluginType.EnvironmentalDataSource) && p.IsSelectable, k => k.Subtype);
        }

        [Import] IPluginManagerService _pluginManagerService;

        #region public ObservableConcurrentDictionary<string, Dictionary<string, IESMEPlugin>> AllPlugins { get; set; }

        public ObservableConcurrentDictionary<string, Dictionary<string, IESMEPlugin>> AllPlugins
        {
            get { return _allPlugins; }
            set
            {
                if (_allPlugins == value) return;
                _allPlugins = value;
                NotifyPropertyChanged(AllPluginsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs AllPluginsChangedEventArgs = ObservableHelper.CreateArgs<MainWindowViewModel>(x => x.AllPlugins);
        ObservableConcurrentDictionary<string, Dictionary<string, IESMEPlugin>> _allPlugins;

        #endregion

        public ObservableList<IESMEPlugin> ESMEPlugins
        {
            get { return _pluginManagerService.ESMEPlugins; }
        }
        public ESMEPluginDictionary ESMEPluginDictionary
        {
            get { return _pluginManagerService.ESMEPluginDictionary; }
        }

    }
}