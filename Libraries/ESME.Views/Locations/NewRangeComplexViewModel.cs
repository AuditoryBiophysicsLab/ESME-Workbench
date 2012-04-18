﻿using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using Cinch;
using ESME.Data;
using ESME.NEMO.Overlay;
using HRC.Navigation;
using HRC.Validation;

namespace ESME.Views.Locations
{
    public sealed class NewRangeComplexViewModel : ValidatingViewModel
    {
        public NewRangeComplexViewModel(AppSettings appSettings)
        {
            //SimAreaFolder = appSettings.ScenarioDataDirectory;
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
                    RuleDelegate = (o, r) =>!string.IsNullOrEmpty(((NewRangeComplexViewModel)o).NewOpAreaOverlayCoordinates) || !string.IsNullOrEmpty(((NewRangeComplexViewModel) o).ExistingOpAreaOverlayFilename)
                },
                new ValidationRule
                {
                    PropertyName = "NewOpAreaOverlayCoordinates", 
                    Description = "If an overlay file is not specified, you must provide coordinates", 
                    RuleDelegate = (o, r) => !string.IsNullOrEmpty(((NewRangeComplexViewModel)o).ExistingOpAreaOverlayFilename) || !string.IsNullOrEmpty(((NewRangeComplexViewModel) o).NewOpAreaOverlayCoordinates)
                },
                new ValidationRule
                {
                    PropertyName = "ExistingOpAreaOverlayFilename", 
                    Description = "If coordinates are specified, you cannot provide an overlay file",
                    RuleDelegate = (o, r) => string.IsNullOrEmpty(((NewRangeComplexViewModel)o).ExistingOpAreaOverlayFilename) || string.IsNullOrEmpty(((NewRangeComplexViewModel)o).NewOpAreaOverlayCoordinates)
                },
                 new ValidationRule
                {
                    PropertyName = "NewOpAreaOverlayCoordinates", 
                    Description = "If an overlay file is specified, you cannot provide coordinates", 
                    RuleDelegate = (o, r) => string.IsNullOrEmpty(((NewRangeComplexViewModel)o).NewOpAreaOverlayCoordinates) || string.IsNullOrEmpty(((NewRangeComplexViewModel)o).ExistingOpAreaOverlayFilename)
                },
               new ValidationRule
                {
                    PropertyName = "ExistingSimAreaOverlayFilename", 
                    Description = "If coordinates are not specified, you must provide an overlay file",
                    RuleDelegate = (o, r) =>!string.IsNullOrEmpty(((NewRangeComplexViewModel)o).NewSimAreaOverlayCoordinates) || !string.IsNullOrEmpty(((NewRangeComplexViewModel) o).ExistingSimAreaOverlayFilename)
                },
                new ValidationRule
                {
                    PropertyName = "NewSimAreaOverlayCoordinates", 
                    Description = "If an overlay file is not specified, you must provide coordinates", 
                    RuleDelegate = (o, r) => !string.IsNullOrEmpty(((NewRangeComplexViewModel)o).ExistingSimAreaOverlayFilename) || !string.IsNullOrEmpty(((NewRangeComplexViewModel) o).NewSimAreaOverlayCoordinates)
                },
                new ValidationRule
                {
                    PropertyName = "ExistingSimAreaOverlayFilename", 
                    Description = "If coordinates are specified, you cannot provide an overlay file",
                    RuleDelegate = (o, r) => string.IsNullOrEmpty(((NewRangeComplexViewModel)o).ExistingSimAreaOverlayFilename) || string.IsNullOrEmpty(((NewRangeComplexViewModel)o).NewSimAreaOverlayCoordinates)
                },
                 new ValidationRule
                {
                    PropertyName = "NewSimAreaOverlayCoordinates", 
                    Description = "If an overlay file is specified, you cannot provide coordinates", 
                    RuleDelegate = (o, r) => string.IsNullOrEmpty(((NewRangeComplexViewModel)o).NewSimAreaOverlayCoordinates) || string.IsNullOrEmpty(((NewRangeComplexViewModel)o).ExistingSimAreaOverlayFilename)
                },
                new ValidationRule
                {
                    PropertyName = "NewOpAreaOverlayCoordinates", 
                    Description = "Invalid or incomplete data entered", 
                    RuleDelegate = (o, r) => string.IsNullOrEmpty(NewOpAreaOverlayCoordinates) || ValidateOverlayCoordinates(((NewRangeComplexViewModel) o).NewOpAreaOverlayCoordinates, r),
                },
                new ValidationRule
                {
                    PropertyName = "NewSimAreaOverlayCoordinates", 
                    Description = "Invalid or incomplete data entered", 
                    RuleDelegate = (o, r) => string.IsNullOrEmpty(NewSimAreaOverlayCoordinates) || ValidateOverlayCoordinates(((NewRangeComplexViewModel)o).NewSimAreaOverlayCoordinates, r),
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

            List<Geo> geos;
            string validationErrors;
            var result = OverlayFile.ValidateCoordinates(fieldData, null, out geos, out validationErrors);
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

        public List<Geo> NewOpAreaOverlayGeos { get; private set; }
        public List<Geo> NewSimAreaOverlayGeos { get; private set; }
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
            List<Geo> opCoords = null;
            List<Geo> simCoords = null;
            string opErrors;
            OpAreaBoundingBox = !string.IsNullOrEmpty(NewOpAreaOverlayCoordinates)
                           ? OverlayFile.ValidateCoordinates(NewOpAreaOverlayCoordinates, "Op Limits", out opCoords, out opErrors)
                           : OverlayFile.ValidateFile(ExistingOpAreaOverlayFilename, "Op Limits", out opErrors);

            string simErrors;
            SimAreaBoundingBox = !string.IsNullOrEmpty(NewSimAreaOverlayCoordinates)
                            ? OverlayFile.ValidateCoordinates(NewSimAreaOverlayCoordinates, "Sim Limits", out simCoords, out simErrors)
                            : OverlayFile.ValidateFile(ExistingSimAreaOverlayFilename, "Sim Limits", out simErrors);

            NewOpAreaOverlayGeos = opCoords;
            NewSimAreaOverlayGeos = simCoords;

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
    }
}