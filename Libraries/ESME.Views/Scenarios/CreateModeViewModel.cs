using HRC.ViewModels;

namespace ESME.Views.Scenarios
{
    /// <summary>
    /// To create and show the view as a dialog:
    /// var vm = new CreateModeViewModel {...};
    /// var result = _visualizerService.ShowDialog("CreateModeView", vm);
    /// if ((!result.HasValue) || (!result.Value)) return;
    /// 
    /// To create and show the view as a window:
    /// var vm = new CreateModeViewModel {...};
    /// var window = _visualizerService.ShowWindow("CreateModeView", vm);
    /// </summary>
    public class CreateModeViewModel : ViewModelBase
    {
    }
}
