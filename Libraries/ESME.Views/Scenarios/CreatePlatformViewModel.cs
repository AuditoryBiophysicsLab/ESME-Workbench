using HRC.Validation;
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
    public class CreatePlatformViewModel : ValidatingViewModel
    {
        public CreatePlatformViewModel()
        {
            ValidationRules.Add(new ValidationRule
            {
                PropertyName = "PlatformName",
                Description = "Must be unique within the selected scenario and cannot be null or empty",
                RuleDelegate = (o, r) =>
                {
                    var target = (CreatePlatformViewModel)o;
                    return !string.IsNullOrEmpty(target.PlatformName);
                },
            });
        }
        public string PlatformName { get; set; }
        public string Description { get; set; }

        #region OkCommand
        public SimpleCommand<object, object> OkCommand { get { return _ok ?? (_ok = new SimpleCommand<object, object>(o => IsValid, o => CloseDialog(true))); } }
        SimpleCommand<object, object> _ok;
        #endregion

        #region CancelCommand
        public SimpleCommand<object, EventToCommandArgs> CancelCommand { get { return _cancel ?? (_cancel = new SimpleCommand<object, EventToCommandArgs>(o => CloseDialog(false))); } }
        SimpleCommand<object, EventToCommandArgs> _cancel;
        #endregion
    }
}
