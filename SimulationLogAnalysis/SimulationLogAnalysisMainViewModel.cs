using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using System.Text;
using System.Windows;
using ESME.Locations;
using HRC.Services;
using HRC.ViewModels;
using MEFedMVVM.ViewModelLocator;

namespace SimulationLogAnalysis
{
    [ExportViewModel("SimulationLogAnalysisMainViewModel")]
    public class SimulationLogAnalysisMainViewModel : ViewModelBase
    {
        readonly IViewAwareStatus _viewAwareStatus;
        readonly IMessageBoxService _messageBox;
        readonly IUIVisualizerService _visualizer;
        readonly IHRCSaveFileService _saveFile;

        [ImportingConstructor]
        public SimulationLogAnalysisMainViewModel(IViewAwareStatus viewAwareStatus,
                                                  IMessageBoxService messageBox,
                                                  IUIVisualizerService visualizer,
                                                  IHRCSaveFileService saveFile)
        {
            _viewAwareStatus = viewAwareStatus;
            _messageBox = messageBox;
            _visualizer = visualizer;
            _saveFile = saveFile;
            SelectedFileName = (string)Application.Current.Properties["SelectedFileName"];
        }

        public string SelectedFileName { get; set; }
    }
}
