using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using Cinch;
using ESME.Data;
using ESME.Overlay;
using HRC.Navigation;
using HRC.Validation;

namespace ESME.Views.Locations
{
    public sealed class NewRangeComplexViewModel : ValidatingViewModel
    {
        public NewRangeComplexViewModel(AppSettings appSettings)
        {
            SimAreaFolder = appSettings.ScenarioDataDirectory;
            ValidationRules.AddRange(new List<ValidationRule>
            {
                new ValidationRule
                {
                    PropertyName = "LocationName",
                    Description = "Cannot be empty",
                    RuleDelegate = (o, r) => !string.IsNullOrEmpty(((NewRangeComplexViewModel)o).LocationName),
                },
                new ValidationRule
                {
                    PropertyName = "LocationName",
                    Description = "Location already exists.  Choose a different name",
                    RuleDelegate = (o, r) => (string.IsNullOrEmpty(((NewRangeComplexViewModel)o).LocationName)) || ((LocationPath == null) || !Directory.Exists(LocationPath)),
                },
                new ValidationRule
                {
                    PropertyName = "ReferencePointLatitude",
                    Description = "Must be between -90 and +90",
                    RuleDelegate = (o, r) => RangeCheck(((NewRangeComplexViewModel)o).ReferencePointLatitude, -90, 90),
                },
                new ValidationRule
                {
                    PropertyName = "ReferencePointLongitude", 
                    Description = "Must be between -180 and +180",
                    RuleDelegate = (o, r) => RangeCheck(((NewRangeComplexViewModel)o).ReferencePointLongitude, -180, 180),
                },
                new ValidationRule
                {
                    PropertyName = "Height", 
                    Description = "Invalid value", 
                    RuleDelegate = (o, r) => RangeCheck(((NewRangeComplexViewModel)o).Height, double.MinValue),
                },
                new ValidationRule
                {
                    PropertyName = "GeoidSeparation", 
                    Description = "Invalid value", 
                    RuleDelegate = (o, r) => RangeCheck(((NewRangeComplexViewModel)o).GeoidSeparation, double.MinValue),
                },
                new ValidationRule
                {
                    PropertyName = "ExistingOpAreaOverlayFilename", 
                    Description = "If coordinates are not specified, you must provide an overlay file", 
                    RuleDelegate = (o, r) => OnlyOneIsNotEmpty(((NewRangeComplexViewModel)o).NewOpAreaOverlayCoordinates, ((NewRangeComplexViewModel)o).ExistingOpAreaOverlayFilename),
                },
                new ValidationRule
                {
                    PropertyName = "NewOpAreaOverlayCoordinates", 
                    Description = "If an overlay file is not specified, you must provide coordinates", 
                    RuleDelegate = (o, r) => AtLeastOneIsNotEmpty(((NewRangeComplexViewModel)o).NewOpAreaOverlayCoordinates, ((NewRangeComplexViewModel)o).ExistingOpAreaOverlayFilename)
                },
                new ValidationRule
                {
                    PropertyName = "ExistingSimAreaOverlayFilename", 
                    Description = "If coordinates are not specified, you must provide an overlay file", 
                    RuleDelegate = (o, r) => OnlyOneIsNotEmpty(((NewRangeComplexViewModel)o).NewSimAreaOverlayCoordinates, ((NewRangeComplexViewModel)o).ExistingSimAreaOverlayFilename),
                },
                new ValidationRule
                {
                    PropertyName = "NewSimAreaOverlayCoordinates", 
                    Description = "If an overlay file is not specified, you must provide coordinates", 
                    RuleDelegate = (o, r) => AtLeastOneIsNotEmpty(((NewRangeComplexViewModel)o).NewSimAreaOverlayCoordinates, ((NewRangeComplexViewModel)o).ExistingSimAreaOverlayFilename),
                },
                new ValidationRule
                {
                    PropertyName = "NewOpAreaOverlayCoordinates", 
                    Description = "Invalid or incomplete data entered", 
                    RuleDelegate = (o, r) => (string.IsNullOrEmpty(ExistingOpAreaOverlayFilename) && ValidateOverlayCoordinates(((NewRangeComplexViewModel)o).NewOpAreaOverlayCoordinates, r)),
                },
                new ValidationRule
                {
                    PropertyName = "NewSimAreaOverlayCoordinates", 
                    Description = "Invalid or incomplete data entered", 
                    RuleDelegate = (o, r) => ((string.IsNullOrEmpty(ExistingSimAreaOverlayFilename)&& ValidateOverlayCoordinates(((NewRangeComplexViewModel)o).NewSimAreaOverlayCoordinates, r))),
                },
                new ValidationRule
                {
                    PropertyName = "ExistingOpAreaOverlayFilename", 
                    Description = null, 
                    RuleDelegate = (o, r) => ValidateOverlayFile(((NewRangeComplexViewModel)o).ExistingOpAreaOverlayFilename, r),
                },
                new ValidationRule
                {
                    PropertyName = "ExistingSimAreaOverlayFilename", 
                    Description = null, 
                    RuleDelegate = (o, r) => ValidateOverlayFile(((NewRangeComplexViewModel)o).ExistingSimAreaOverlayFilename, r),
                },
            });
        }

        public static bool ValidateOverlayCoordinates(string fieldData, ValidationRuleBase rule)
        {
            if (string.IsNullOrEmpty(fieldData)) return true;

            List<EarthCoordinate> earthCoordinates;
            string validationErrors;
            var result = OverlayFile.ValidateCoordinates(fieldData, null, out earthCoordinates, out validationErrors);
            if (result != null) return true;
            rule.ValidationErrorMessage = validationErrors;
            return false;
        }

        public static bool ValidateOverlayFile(string overlayFilename, ValidationRuleBase rule)
        {
            if (string.IsNullOrEmpty(overlayFilename)) return true;

            string validationErrors;
            var result = OverlayFile.ValidateFile(overlayFilename, "overlay file", out validationErrors);
            if (result != null) return true;
            rule.ValidationErrorMessage = validationErrors;
            return false;
        }

        #region public string LocationName { get; set; } 

        private static readonly PropertyChangedEventArgs LocationNameChangedEventArgs =
            ObservableHelper.CreateArgs<NewRangeComplexViewModel>(x => x.LocationName);

        private string _locationName;

        public string LocationName
        {
            get { return _locationName; }
            set
            {
                if (_locationName == value) return;
                _locationName = value;
                NotifyPropertyChanged(LocationNameChangedEventArgs);
            }
        }

        public string LocationPath
        {
            get
            {
                return string.IsNullOrEmpty(SimAreaFolder) || string.IsNullOrEmpty(LocationName)
                           ? null
                           : Path.Combine(SimAreaFolder, LocationName);
            }
        }
        #endregion

        #region public string ExistingOpAreaOverlayFilename { get; set; }

        private static readonly PropertyChangedEventArgs ExistingOverlayFilenameChangedEventArgs =
            ObservableHelper.CreateArgs<NewRangeComplexViewModel>(x => x.ExistingOpAreaOverlayFilename);

        private string _existingOpAreaOverlayFilename;

        public string ExistingOpAreaOverlayFilename
        {
            get { return _existingOpAreaOverlayFilename; }
            set
            {
                if (_existingOpAreaOverlayFilename == value) return;
                _existingOpAreaOverlayFilename = value;
                NotifyPropertyChanged(ExistingOverlayFilenameChangedEventArgs);
                NotifyPropertyChanged(NewOverlayCoordinatesChangedEventArgs);
            }
        }

        #endregion

        #region public string NewOpAreaOverlayCoordinates { get; set; } 

        private static readonly PropertyChangedEventArgs NewOverlayCoordinatesChangedEventArgs =
            ObservableHelper.CreateArgs<NewRangeComplexViewModel>(x => x.NewOpAreaOverlayCoordinates);

        private string _newOpAreaOverlayCoordinates;

        public string NewOpAreaOverlayCoordinates
        {
            get { return _newOpAreaOverlayCoordinates; }
            set
            {
                //  if (_newOpAreaOverlayCoordinates == value) return;
                _newOpAreaOverlayCoordinates = value;
                NotifyPropertyChanged(NewOverlayCoordinatesChangedEventArgs);
                NotifyPropertyChanged(ExistingOverlayFilenameChangedEventArgs);
            }
        }
     
        #endregion

        #region public string ExistingSimAreaOverlayFilename { get; set; }

        private static readonly PropertyChangedEventArgs ExistingSimAreaOverlayFilenameChangedEventArgs =
            ObservableHelper.CreateArgs<NewRangeComplexViewModel>(x => x.ExistingSimAreaOverlayFilename);

        private string _existingSimAreaOverlayFilename;

        public string ExistingSimAreaOverlayFilename
        {
            get { return _existingSimAreaOverlayFilename; }
            set
            {
                if (_existingSimAreaOverlayFilename == value) return;
                _existingSimAreaOverlayFilename = value;
                NotifyPropertyChanged(ExistingSimAreaOverlayFilenameChangedEventArgs);
                NotifyPropertyChanged(NewSimAreaOverlayCoordinatesChangedEventArgs);
            }
        }

        #endregion

        #region public string NewSimAreaOverlayCoordinates { get; set; }

        private static readonly PropertyChangedEventArgs NewSimAreaOverlayCoordinatesChangedEventArgs =
            ObservableHelper.CreateArgs<NewRangeComplexViewModel>(x => x.NewSimAreaOverlayCoordinates);

        private string _newSimAreaOverlayCoordinates;

        public string NewSimAreaOverlayCoordinates
        {
            get { return _newSimAreaOverlayCoordinates; }
            set
            {
                // if (_newSimAreaOverlayCoordinates == value) return;
                _newSimAreaOverlayCoordinates = value;
                NotifyPropertyChanged(NewSimAreaOverlayCoordinatesChangedEventArgs);
                NotifyPropertyChanged(ExistingSimAreaOverlayFilenameChangedEventArgs);
            }
        }

        #endregion

        #region public float ReferencePointLatitude { get; set; } 

        public float ReferencePointLatitude
        {
            get { return _referencePointLatitude; }
            set
            {
                if (_referencePointLatitude == value) return;
                _referencePointLatitude = value;
                NotifyPropertyChanged(ReferencePointLatitudeChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs ReferencePointLatitudeChangedEventArgs = ObservableHelper.CreateArgs<NewRangeComplexViewModel>(x => x.ReferencePointLatitude);
        private float _referencePointLatitude;
        #endregion

        #region public float ReferencePointLongitude { get; set; } 

        public float ReferencePointLongitude
        {
            get { return _referencePointLongitude; }
            set
            {
                if (_referencePointLongitude == value) return;
                _referencePointLongitude = value;
                NotifyPropertyChanged(ReferencePointLongitudeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ReferencePointLongitudeChangedEventArgs = ObservableHelper.CreateArgs<NewRangeComplexViewModel>(x => x.ReferencePointLongitude);
        float _referencePointLongitude;

        #endregion

        #region public float Height { get; set; } 

        public float Height
        {
            get { return _height; }
            set
            {
                if (_height == value) return;
                _height = value;
                NotifyPropertyChanged(HeightChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs HeightChangedEventArgs = ObservableHelper.CreateArgs<NewRangeComplexViewModel>(x => x.Height);
        private float _height;
        
        #endregion

        #region public float GeoidSeparation { get; set; }

        public float GeoidSeparation
        {
            get { return _geoidSeparation; }
            set
            {
                if (_geoidSeparation == value) return;
                _geoidSeparation = value;
                NotifyPropertyChanged(GeoidSeparationChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs GeoidSeparationChangedEventArgs = ObservableHelper.CreateArgs<NewRangeComplexViewModel>(x => x.GeoidSeparation);
        private float _geoidSeparation;
        
        #endregion

        #region public string SimAreaFolder { get; set; }

        private static readonly PropertyChangedEventArgs SimAreaFolderChangedEventArgs =
            ObservableHelper.CreateArgs<NewRangeComplexViewModel>(x => x.SimAreaFolder);

        private string _simAreaFolder;

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

        #endregion

        public List<EarthCoordinate> NewOpAreaOverlayEarthCoordinates { get; private set; }
        public List<EarthCoordinate> NewSimAreaOverlayEarthCoordinates { get; private set; }
        public GeoRect OpAreaBoundingBox { get; private set; }
        public GeoRect SimAreaBoundingBox { get; private set; }

        #region OkCommand

        private SimpleCommand<object, object> _ok;

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

        private void OkCommandHandler()
        {
            List<EarthCoordinate> opCoords = null;
            List<EarthCoordinate> simCoords = null;
            string opErrors;
            OpAreaBoundingBox = !string.IsNullOrEmpty(NewOpAreaOverlayCoordinates)
                           ? OverlayFile.ValidateCoordinates(NewOpAreaOverlayCoordinates, "Op Limits", out opCoords, out opErrors)
                           : OverlayFile.ValidateFile(ExistingOpAreaOverlayFilename, "Op Limits", out opErrors);
            
            string simErrors;
            SimAreaBoundingBox = !string.IsNullOrEmpty(NewSimAreaOverlayCoordinates)
                            ? OverlayFile.ValidateCoordinates(NewSimAreaOverlayCoordinates, "Sim Limits", out simCoords, out simErrors)
                            : OverlayFile.ValidateFile(ExistingSimAreaOverlayFilename, "Sim Limits", out simErrors);
            
            NewOpAreaOverlayEarthCoordinates = opCoords;
            NewSimAreaOverlayEarthCoordinates = simCoords;

            CloseActivePopUpCommand.Execute(true);
        }

        #endregion

        #region ClearOpAreaCoordinatesCommand

        private SimpleCommand<object, object> _clearOpAreaCoordinates;

        public SimpleCommand<object, object> ClearOpAreaCoordinatesCommand
        {
            get
            {
                return _clearOpAreaCoordinates ??
                       (_clearOpAreaCoordinates =
                        new SimpleCommand<object, object>(delegate { return ClearOpAreaCoordinatesIsEnabled; },
                                                          delegate { ClearOpAreaCoordinatesCommandHandler(); }));
            }
        }

        private bool ClearOpAreaCoordinatesIsEnabled
        {
            get { return (!string.IsNullOrEmpty(NewOpAreaOverlayCoordinates)); }
        }

        private void ClearOpAreaCoordinatesCommandHandler()
        {
            NewOpAreaOverlayCoordinates = "";
        }

        #endregion

        #region ClearSimAreaCoordinatesCommand

        private SimpleCommand<object, object> _clearSimAreaCoordinates;

        public SimpleCommand<object, object> ClearSimAreaCoordinatesCommand
        {
            get
            {
                return _clearSimAreaCoordinates ??
                       (_clearSimAreaCoordinates =
                        new SimpleCommand<object, object>(delegate { return ClearSimAreaCoordinatesIsEnabled; },
                                                          delegate { ClearSimAreaCoordinatesCommandHandler(); }));
            }
        }

        private bool ClearSimAreaCoordinatesIsEnabled
        {
            get { return (!string.IsNullOrEmpty(NewSimAreaOverlayCoordinates)); }
        }

        private void ClearSimAreaCoordinatesCommandHandler()
        {
            NewSimAreaOverlayCoordinates = "";
        }

        #endregion

#if false
        #region ISupportValidation Members

        public void Validate()
        {
            ValidationErrorText = "";
            if ((LocationPath != null) && Directory.Exists(LocationPath))
                ValidationErrorText += "Location already exists, please choose a different name\n";
            if (string.IsNullOrEmpty(LocationName))
                ValidationErrorText += "New location name must be specified\n";
            if (-180 > ReferencePointLatitude || ReferencePointLatitude > 180)
                ValidationErrorText += "Reference Latitude is out of bounds\n";
            if (float.IsNaN(ReferencePointLatitude)) ValidationErrorText += "Latitude is invalid\n";
            if (-90 > ReferencePointLongitude || ReferencePointLongitude > 90)
                ValidationErrorText += "Reference Longitude is out of bounds\n";
            if (float.IsNaN(ReferencePointLongitude)) ValidationErrorText += "Reference Longitude is invalid\n";
            if (float.IsNaN(Height)) ValidationErrorText += "Height is invalid\n.";
            if (float.IsNaN(GeoidSeparation)) ValidationErrorText += "Geoid Separation is invalid\n";
            if (!string.IsNullOrEmpty(ExistingOpAreaOverlayFilename) && !File.Exists(ExistingOpAreaOverlayFilename))
                ValidationErrorText += "Selected overlay file does not exist\n";
            if (!string.IsNullOrEmpty(ExistingSimAreaOverlayFilename) &&
                !string.IsNullOrEmpty(NewSimAreaOverlayCoordinates))
                ValidationErrorText +=
                    "Select EITHER an existing overlay file OR coordinates for a new Simulation Limit overlay\n";
            if (!string.IsNullOrEmpty(ExistingOpAreaOverlayFilename) &&
                !string.IsNullOrEmpty(NewOpAreaOverlayCoordinates))
                ValidationErrorText +=
                    "Select EITHER an existing overlay file OR coordinates for a new Operational Limit overlay\n";
            if (!string.IsNullOrEmpty(ExistingSimAreaOverlayFilename) && !File.Exists(ExistingSimAreaOverlayFilename))
                ValidationErrorText += "Selected overlay file does not exist\n";
            if (string.IsNullOrEmpty(NewOpAreaOverlayCoordinates) &&
                (string.IsNullOrEmpty(ExistingOpAreaOverlayFilename) || !File.Exists(ExistingOpAreaOverlayFilename)))
                ValidationErrorText += "Baseline operational area must be defined\n";
            if (!string.IsNullOrEmpty(NewOpAreaOverlayCoordinates) &&
                (!string.IsNullOrEmpty(ExistingOpAreaOverlayFilename)))
                ValidationErrorText += "Conflicting operational areas defined\n.";
            if (string.IsNullOrEmpty(NewSimAreaOverlayCoordinates) &&
                (string.IsNullOrEmpty(ExistingSimAreaOverlayFilename) || !File.Exists(ExistingSimAreaOverlayFilename)))
                ValidationErrorText += "Baseline simulation area must be defined\n";
            if (!string.IsNullOrEmpty(NewSimAreaOverlayCoordinates) &&
                (!string.IsNullOrEmpty(ExistingSimAreaOverlayFilename)))
                ValidationErrorText += "Conflicting simulation areas defined\n.";
            if (!string.IsNullOrEmpty(ValidationErrorText)) return;

            List<EarthCoordinate> opCoords;
            List<EarthCoordinate> simCoords;
            string opErrors;
            OpBounds = !string.IsNullOrEmpty(NewOpAreaOverlayCoordinates)
                           ? OverlayFile.ValidateCoordinates(NewOpAreaOverlayCoordinates, "Op Limits", out opCoords, out opErrors)
                           : OverlayFile.ValidateFile(ExistingOpAreaOverlayFilename, "Op Limits", out opCoords, out opErrors);
            ValidationErrorText += opErrors;
            string simErrors;
            SimBounds = !string.IsNullOrEmpty(NewSimAreaOverlayCoordinates)
                            ? OverlayFile.ValidateCoordinates(NewSimAreaOverlayCoordinates, "Sim Limits", out simCoords, out simErrors)
                            : OverlayFile.ValidateFile(ExistingSimAreaOverlayFilename, "Sim Limits", out simCoords, out simErrors);
            ValidationErrorText += simErrors;
            if (OpBounds != null) NewOpAreaOverlayEarthCoordinates = opCoords;
            if (SimBounds != null) NewSimAreaOverlayEarthCoordinates = simCoords;

            NotifyPropertyChanged(ErrorVisibilityChangedEventArgs);
        }

        #endregion

        #region public Visibility ErrorVisibility { get; set; }

        private static readonly PropertyChangedEventArgs ErrorVisibilityChangedEventArgs =
            ObservableHelper.CreateArgs<NewRangeComplexViewModel>(x => x.ErrorVisibility);

        public Visibility ErrorVisibility
        {
            get { return IsValid ? Visibility.Collapsed : Visibility.Visible; }
        }

        #endregion

        #region public string ValidationErrorText { get; set; }

        private static readonly PropertyChangedEventArgs ValidationErrorTextChangedEventArgs =
            ObservableHelper.CreateArgs<NewRangeComplexViewModel>(x => x.ValidationErrorText);

        private string _validationErrorText;

        public string ValidationErrorText
        {
            get { return _validationErrorText; }
            set
            {
                if (_validationErrorText == value) return;
                _validationErrorText = value;
                NotifyPropertyChanged(ErrorVisibilityChangedEventArgs);
                NotifyPropertyChanged(ValidationErrorTextChangedEventArgs);
            }
        }

        #endregion 
#endif
    }
}