using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Views.Scenarios
{
    /// <summary>
    /// To create and show the view as a dialog:
    /// var vm = new CreatePlatformViewModel {...};
    /// var result = _visualizerService.ShowDialog("CreatePlatformView", vm);
    /// if ((!result.HasValue) || (!result.Value)) return;
    /// 
    /// To create and show the view as a window:
    /// var vm = new CreatePlatformViewModel {...};
    /// var window = _visualizerService.ShowWindow("CreatePlatformView", vm);
    /// </summary>
    public class PropertiesViewModel : ViewModelBase
    {
        public string WindowTitle { get; set; }
        public object PropertyObject { get; set; }

        #region OkCommand
        public SimpleCommand<object, object> OkCommand { get { return _ok ?? (_ok = new SimpleCommand<object, object>(o => CloseDialog(true))); } }
        SimpleCommand<object, object> _ok;
        #endregion
    }
}
