using HRC.Validation;
using HRC.ViewModels;
using HRC.WPF;

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
    public class CreateModeViewModel : ValidatingViewModel
    {
        public CreateModeViewModel()
        {
            ValidationRules.Add(new ValidationRule
            {
                PropertyName = "ModeName",
                Description = "Must be unique within the containing source and cannot be null or empty",
                RuleDelegate = (o, r) =>
                {
                    var target = (CreateModeViewModel)o;
                    return !string.IsNullOrEmpty(target.ModeName);
                },
            });
        }

        public string ModeName { get; set; }
        public float Depth { get; set; }
        public float SourceLevel { get; set; }
        public float Frequency { get; set; }
        public float VerticalBeamWidth { get; set; }
        public float DepressionElevationAngle { get; set; }
        public float MaxPropagationRadius { get; set; }

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
