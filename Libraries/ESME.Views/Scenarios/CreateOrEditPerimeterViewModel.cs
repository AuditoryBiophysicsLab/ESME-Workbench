using System.Diagnostics;
using ESME.Views.Locations;
using HRC.Validation;
using HRC.ViewModels;

namespace ESME.Views.Scenarios
{
    public sealed class CreateOrEditPerimeterViewModel : ValidatingViewModel
    {
        public CreateOrEditPerimeterViewModel() { ValidationRules.Add(PerimeterNameValidationRule); }

        public EditablePolygonOverlayViewModel EditablePolygonOverlayViewModel { get; set; }
        public string PerimeterName { get; set; }
        public bool IsCanceled { get; private set; }
        public string DialogTitle { get; set; }
        #region Validation Rules
        static readonly ValidationRule<CreateOrEditPerimeterViewModel> PerimeterNameValidationRule = new ValidationRule<CreateOrEditPerimeterViewModel>
        {
            PropertyName = "PerimeterName",
            Description = "Must be unique and cannot be null or empty",
            IsRuleValid = (target, rule) => !string.IsNullOrEmpty(target.PerimeterName),
        };
        #endregion

        #region Commands
        #region OkCommand
        public SimpleCommand<object, object> OkCommand
        {
            get { return _ok ?? (_ok = new SimpleCommand<object, object>(delegate { return IsValid && !EditablePolygonOverlayViewModel.HasErrors; }, o =>
            {
                Debug.Write("PerimeterGeos = new List<Geo>{");
                foreach (var geo in EditablePolygonOverlayViewModel.GeoArray) Debug.Write(string.Format("new Geo({0}, {1}), ", geo.Latitude, geo.Longitude));
                Debug.WriteLine("},");
                CloseDialog(null);
            })); }
        }

        SimpleCommand<object, object> _ok;

        #endregion

        #region CancelCommand
        public SimpleCommand<object, object> CancelCommand
        {
            get
            {
                return _cancel ?? (_cancel = new SimpleCommand<object, object>(o =>
                {
                    IsCanceled = true;
                    CloseDialog(null);
                }));
            }
        }
        SimpleCommand<object, object> _cancel;

        #endregion

        #region ViewClosingCommand
        public SimpleCommand<object, object> ViewClosingCommand { get { return _viewClosing ?? (_viewClosing = new SimpleCommand<object, object>(o => Properties.Settings.Default.Save())); } }
        SimpleCommand<object, object> _viewClosing;
        #endregion
        #endregion
    }
}
