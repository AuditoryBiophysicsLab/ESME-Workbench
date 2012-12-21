using System.Collections.ObjectModel;
using ESME.Scenarios;
using HRC.ViewModels;

namespace ESME.Views.Scenarios
{
    /// <summary>
    /// To create and show the view as a dialog:
    /// var vm = new PSMContextViewModel {...};
    /// var result = _visualizerService.ShowDialog("PSMContextView", vm);
    /// if ((!result.HasValue) || (!result.Value)) return;
    /// 
    /// To create and show the view as a window:
    /// var vm = new PSMContextViewModel {...};
    /// var window = _visualizerService.ShowWindow("PSMContextView", vm);
    /// </summary>
    public class PSMContextViewModel : ViewModelBase
    {
        public string WindowTitle { get; set; }
        public ObservableCollection<PlatformPropertiesControlView> Platforms { get; set; }
        public PlatformPropertiesControlView SelectedPlatform { get; set; }
    }
}
