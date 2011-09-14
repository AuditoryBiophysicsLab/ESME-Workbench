using System.ComponentModel.Composition;
using System.IO;
using Cinch;
using ESME.Data;
using ESME.Environment;
using ESME.Environment.Descriptors;
using ESME.Views.EnvironmentBuilder;
using HRC.Services;
using MEFedMVVM.ViewModelLocator;

namespace DavesWPFTester
{
    [ExportViewModel("MainWindowViewModel")]
    public class MainWindowViewModel : ViewModelBase
    {
        readonly IMessageBoxService _messageBoxService;
        readonly IHRCOpenFileService _openFileService;
        readonly IHRCSaveFileService _saveFileService;
        readonly IViewAwareStatus _viewAwareStatus;
        readonly IUIVisualizerService _visualizerService;

        [ImportingConstructor]
        public MainWindowViewModel(IViewAwareStatus viewAwareStatus, IMessageBoxService messageBoxService, IHRCOpenFileService openFileService, IHRCSaveFileService saveFileService, IUIVisualizerService visualizerService) 
        {
            _viewAwareStatus = viewAwareStatus;
            _messageBoxService = messageBoxService;
            _openFileService = openFileService;
            _saveFileService = saveFileService;
            _visualizerService = visualizerService;
            var settings = Path.Combine(Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData), "ESME WorkBench"), "settings.xml");
            ESME.Globals.AppSettings = AppSettings.Load(settings);
            RangeComplexesViewModel = new RangeComplexesViewModel(RangeComplexes.Singleton);
            ImportProgressCollection = ImportProgressCollection.Singleton;
            RangeComplexes.Singleton.SimAreaCSVFile = Path.Combine(ESME.Globals.AppSettings.ScenarioDataDirectory, "SimAreas.csv");
        }

        public RangeComplexesViewModel RangeComplexesViewModel { get; private set; }
        public ImportProgressCollection ImportProgressCollection { get; private set; }
    }
}
