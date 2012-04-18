using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using Cinch;
using ESME.Data;
using ESME.NEMO.Overlay;
using HRC.Navigation;
using HRC.Utility;
using HRC.Validation;
using ValidationRule = HRC.Validation.ValidationRule;

namespace ESME.Views.Locations
{
    public sealed class NewOverlayViewModel : ValidatingViewModel
    {
        public NewOverlayViewModel(AppSettings appSettings, string locationName)
        {
            //SimAreaFolder = appSettings.ScenarioDataDirectory;
            LocationName = locationName;
            RangeComplexAreasFolder = Path.Combine(SimAreaFolder, LocationName, "Areas");
            ValidationRules.AddRange(new List<ValidationRule>
                                         {
                                             new ValidationRule
                                                 {
                                                     PropertyName = "OverlayName",
                                                     Description = "A name must be specified.",
                                                     RuleDelegate = (o, r) => !string.IsNullOrEmpty(((NewOverlayViewModel) o).OverlayName),
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "OverlayCoordinates",
                                                     Description = "Coordinates must be specified.",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((NewOverlayViewModel) o).OverlayCoordinates;
                                                                            return !string.IsNullOrEmpty(ruleTarget);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "OverlayCoordinates",
                                                     Description = null,
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((NewOverlayViewModel) o).OverlayCoordinates;
                                                                            return ValidateOverlayCoordinates(ruleTarget, r);
                                                                        },
                                                 },
                                            new ValidationRule
                                                 {
                                                     PropertyName = "OverlayName",
                                                     Description = "The overlay name contains illegal path or filename characters.",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((NewOverlayViewModel) o).OverlayName;
                                                                            return string.IsNullOrEmpty(ruleTarget) || ruleTarget.IsValidFilename();
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "OverlayName",
                                                     Description = "An overlay file with this name already exists.",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            
                                                                            var ruleTarget = ((NewOverlayViewModel) o).OverlayName;
                                                                            if (string.IsNullOrEmpty(ruleTarget) || !ruleTarget.IsValidFilename())
                                                                                return true;
                                                                            if(ruleTarget.ToLower().EndsWith(".ovr"))
                                                                                ruleTarget = Path.GetFileNameWithoutExtension(ruleTarget);
                                                                            var locationPath = Path.Combine(SimAreaFolder, LocationName,"Areas", ruleTarget+".ovr");
                                                                            return !File.Exists(locationPath);
                                                                        },
                                                 },
                                 
                                 
                                         });

        }

        #region public string  SimAreaFolder { get; set; }

        public string SimAreaFolder
        {
            get { return _simAreaFolder; }
            set
            {
                if (_simAreaFolder == value) return;
                _simAreaFolder = value;
                NotifyPropertyChanged(SimAreaFolderChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs SimAreaFolderChangedEventArgs = ObservableHelper.CreateArgs<NewOverlayViewModel>(x => x.SimAreaFolder);
        private string _simAreaFolder;

        #endregion

        #region public string RangeComplexAreasFolder { get; set; }

        public string RangeComplexAreasFolder
        {
            get { return _rangeComplexAreasFolder; }
            set
            {
                if (_rangeComplexAreasFolder == value) return;
                _rangeComplexAreasFolder = value;
                NotifyPropertyChanged(RangeComplexFolderChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs RangeComplexFolderChangedEventArgs = ObservableHelper.CreateArgs<NewOverlayViewModel>(x => x.RangeComplexAreasFolder);
        private string _rangeComplexAreasFolder;

        #endregion

        #region public string OverlayName { get; set; }

        public string OverlayName
        {
            get { return _overlayName; }
            set
            {
                if (_overlayName == value) return;
                _overlayName = value;
                NotifyPropertyChanged(OverlayNameChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs OverlayNameChangedEventArgs = ObservableHelper.CreateArgs<NewOverlayViewModel>(x => x.OverlayName);
        private string _overlayName;
        #endregion

        #region public string OverlayCoordinates { get; set; }

        public string OverlayCoordinates
        {
            get { return _overlayCoordinates; }
            set
            {
                if (_overlayCoordinates == value) return;
                _overlayCoordinates = value;
                NotifyPropertyChanged(OverlayCoordinatesChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs OverlayCoordinatesChangedEventArgs = ObservableHelper.CreateArgs<NewOverlayViewModel>(x => x.OverlayCoordinates);
        private string _overlayCoordinates;


        #endregion

        private string LocationName { get; set; }

        public static bool ValidateOverlayCoordinates(string fieldData, ValidationRuleBase rule)
        {
            if (string.IsNullOrEmpty(fieldData)) return true;
            List<Geo> geos;
            string validationErrors;
            var result = OverlayFile.ValidateCoordinates(fieldData, null, out geos, out validationErrors);
            if (result != null) return true;
            rule.ValidationErrorMessage = validationErrors;
            return false;
        }

        #region OkCommand

        public SimpleCommand<object, object> OkCommand
        {
            get
            {
                return _ok ??
                       (_ok =
                        new SimpleCommand<object, object>(delegate { return IsValid; },
                                                          delegate { OkCommandHandler(); }));
            }
        }

        public List<Geo> OverlayGeos { get; private set; }

        public GeoRect BoundingBox { get; private set; }

        void OkCommandHandler()
        {
            List<Geo> coords;
            string overlayError;
            BoundingBox = OverlayFile.ValidateCoordinates(OverlayCoordinates, "Op Limits", out coords, out overlayError);
            OverlayGeos = coords;

            CloseActivePopUpCommand.Execute(true);
        }
        private SimpleCommand<object, object> _ok;

        #endregion

        #region ClearCoordinatesCommand

        public SimpleCommand<object, object> ClearCoordinatesCommand
        {
            get
            {
                return _clearCoordinates ??
                       (_clearCoordinates =
                        new SimpleCommand<object, object>(delegate { return ClearCoordinatesIsEnabled; },
                                                          delegate { ClearCoordinatesCommandHandler(); }));
            }
        }

        private SimpleCommand<object, object> _clearCoordinates;

        bool ClearCoordinatesIsEnabled
        {
            get { return (!string.IsNullOrEmpty(OverlayCoordinates)); }
        }

        void ClearCoordinatesCommandHandler()
        {
            OverlayCoordinates = "";
        }

        #endregion

    }
}
