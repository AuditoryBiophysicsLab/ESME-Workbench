using HRC.Validation;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Views.Scenarios
{
    /// <summary>
    /// To create and show the view as a dialog:
    /// var vm = new CreateSourceViewModel {...};
    /// var result = _visualizerService.ShowDialog("CreateSourceView", vm);
    /// if ((!result.HasValue) || (!result.Value)) return;
    /// 
    /// To create and show the view as a window:
    /// var vm = new CreateSourceViewModel {...};
    /// var window = _visualizerService.ShowWindow("CreateSourceView", vm);
    /// </summary>
    public class CreateSourceViewModel : ValidatingViewModel
    {
        public CreateSourceViewModel()
        {
            ValidationRules.Add(new ValidationRule
            {
                PropertyName = "SourceName",
                Description = "Must be unique within the selected scenario and cannot be null or empty",
                RuleDelegate = (o, r) =>
                {
                    var target = (CreateSourceViewModel)o;
                    return !string.IsNullOrEmpty(target.SourceName);
                },
            });
        }

        public string SourceName { get; set; }

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
