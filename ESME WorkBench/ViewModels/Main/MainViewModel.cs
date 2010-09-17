using System;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.Windows.Input;
using Cinch;
using ESMEWorkBench.Data;
using ESMEWorkBench.ViewModels.Ribbon;
using ESMEWorkBench.ViewModels.TransmissionLoss;
using MEFedMVVM.ViewModelLocator;
using ThinkGeo.MapSuite.Core;

namespace ESMEWorkBench.ViewModels.Main
{
    [ExportViewModel("MainViewModel")]
    public class MainViewModel : ViewModelBase
    {
        #region Data

        readonly IMessageBoxService _messageBoxService;
        readonly IOpenFileService _openFileService;
        readonly IViewAwareStatus _viewAwareStatusService;
        readonly IUIVisualizerService _visualizerService;

        public static AppSettings AppSettings { get; set; }

        #endregion

        #region Constructors
        static MainViewModel() { AppSettings = AppSettings.Load(); }

        [ImportingConstructor]
        public MainViewModel(IViewAwareStatus viewAwareStatusService, IMessageBoxService messageBoxService,
                             IOpenFileService openFileService, IUIVisualizerService visualizerService)
        {
            _viewAwareStatusService = viewAwareStatusService;
            _messageBoxService = messageBoxService;
            _openFileService = openFileService;
            _visualizerService = visualizerService;

            EditOptionsCommand = new SimpleCommand<object, object>(delegate
            {
                var programOptionsViewModel = new ProgramOptionsViewModel();
                var result = _visualizerService.ShowDialog("OptionsPopup", programOptionsViewModel);
                if ((result.HasValue) && (result.Value))
                    AppSettings.Save();
                else
                    AppSettings.Reload();
            });

            TestTransmissionLossViewCommand = new SimpleCommand<object, object>(delegate
            {
                var transmissionLossViewModel = new TransmissionLossFieldViewModel(@"C:\Users\Dave Anderson\Desktop\Bahamas.tlf");
                _visualizerService.Show("TransmissionLossView", transmissionLossViewModel, true, null);
            });

            LaunchExternalProgramCommand = new SimpleCommand<object, object>(delegate(Object arg)
            {
                var executable = new Process();
                var executablePath = (string)arg;
                executable.StartInfo.FileName = executablePath;
                executable.Start();
            });

            MapViewModel = new MapViewModel(_viewAwareStatusService, _messageBoxService, _openFileService, _visualizerService);
            RibbonViewModel = new RibbonViewModel(this, MapViewModel);

            //RibbonViewModel.RecentExperiments.InsertFile(@"C:\Users\Dave Anderson\Documents\ESME WorkBench\test.esme");
        }

        public MapViewModel MapViewModel { get; set; }
        public RibbonViewModel RibbonViewModel { get; private set; }

        #endregion

        #region Commands

        public SimpleCommand<Object, Object> EditOptionsCommand { get; private set; }
        public SimpleCommand<Object, Object> LaunchExternalProgramCommand { get; private set; }
        public SimpleCommand<Object, Object> TestTransmissionLossViewCommand { get; private set; }

        #endregion
    }
}