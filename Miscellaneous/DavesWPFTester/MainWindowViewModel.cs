using Cinch;
using ESME.Plugins;
using MEFedMVVM.ViewModelLocator;
using System.ComponentModel.Composition;

namespace DavesWPFTester
{
    [ExportViewModel("MainWindowViewModel")]
    public class MainWindowViewModel : ViewModelBase
    {
        [Import] IPluginManagerService _pluginManagerService;

        public IPluginManagerService PluginManager
        {
            get { return _pluginManagerService; }
        }
    }
}