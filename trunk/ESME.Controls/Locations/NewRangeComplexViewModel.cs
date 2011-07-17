using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Windows;
using System.Windows.Media;
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
            ValidationRules.Add(new SimpleRule("LocationName", "Cannot be empty", o => !string.IsNullOrEmpty(((NewRangeComplexViewModel)o).LocationName)));
            ValidationRules.Add(new SimpleRule("LocationName", "Location already exists.  Choose a different name", o => (string.IsNullOrEmpty(((NewRangeComplexViewModel)o).LocationName)) || ((LocationPath == null) || !Directory.Exists(LocationPath))));
            
            ValidationRules.Add(new SimpleRule("ReferencePointLatitude", "Must be between -90 and +90", o => RangeCheck(((NewRangeComplexViewModel)o).ReferencePointLatitude, -90, 90)));
            
            ValidationRules.Add(new SimpleRule("ReferencePointLongitude", "Must be between -180 and +180", o => RangeCheck(((NewRangeComplexViewModel)o).ReferencePointLongitude, -180, 180)));
            
            ValidationRules.Add(new SimpleRule("Height", "Invalid value", o => RangeCheck(((NewRangeComplexViewModel)o).Height, double.MinValue, double.MaxValue)));
            
            ValidationRules.Add(new SimpleRule("GeoidSeparation", "Invalid value", o => RangeCheck(((NewRangeComplexViewModel)o).GeoidSeparation, double.MinValue, double.MaxValue)));

            ValidationRules.Add(new SimpleRule("ExistingOpAreaOverlayFilename", "If coordinates are not specified, you must provide an overlay file", o => OnlyOneIsNotEmpty(((NewRangeComplexViewModel)o).NewOpAreaOverlayCoordinates, ((NewRangeComplexViewModel)o).ExistingOpAreaOverlayFilename)));

            ValidationRules.Add(new SimpleRule("NewOpAreaOverlayCoordinates", "If an overlay file is not specified, you must provide coordinates", o => AtLeastOneIsNotEmpty(((NewRangeComplexViewModel)o).NewOpAreaOverlayCoordinates, ((NewRangeComplexViewModel)o).ExistingOpAreaOverlayFilename)));
            ValidationRules.Add(new SimpleRule("NewOpAreaOverlayCoordinates", "There must be at least four points (lines) given to define an area", o => ValidateMinimumLineCount(((NewRangeComplexViewModel)o).NewOpAreaOverlayCoordinates, 4)));
            ValidationRules.Add(new SimpleRule("NewOpAreaOverlayCoordinates", "Invalid latitude/longitude format. Please use decimal degrees", o => ValidateLatLonFormat(((NewRangeComplexViewModel)o).NewOpAreaOverlayCoordinates)));
            ValidationRules.Add(new SimpleRule("NewOpAreaOverlayCoordinates", "There must be at least four points given to define an area", o => ValidateMinimumCoordinateCount(((NewRangeComplexViewModel)o).NewOpAreaOverlayCoordinates, 4)));
            ValidationRules.Add(new SimpleRule("NewOpAreaOverlayCoordinates", "The points provided are not usable as a perimeter.  Line segments are used in the order given, and cannot cross each other.  The resulting polygon must also be closed.", o => ValidateUsabilityAsPerimeter(((NewRangeComplexViewModel)o).NewOpAreaOverlayCoordinates)));

            ValidationRules.Add(new SimpleRule("ExistingSimAreaOverlayFilename", "If coordinates are not specified, you must provide an overlay file", o => OnlyOneIsNotEmpty(((NewRangeComplexViewModel)o).NewSimAreaOverlayCoordinates, ((NewRangeComplexViewModel)o).ExistingSimAreaOverlayFilename)));

            ValidationRules.Add(new SimpleRule("NewSimAreaOverlayCoordinates", "If an overlay file is not specified, you must provide coordinates", o => AtLeastOneIsNotEmpty(((NewRangeComplexViewModel)o).NewSimAreaOverlayCoordinates, ((NewRangeComplexViewModel)o).ExistingSimAreaOverlayFilename)));
            ValidationRules.Add(new SimpleRule("NewSimAreaOverlayCoordinates", "There must be at least four points (lines) given to define an area", o => ValidateMinimumLineCount(((NewRangeComplexViewModel)o).NewSimAreaOverlayCoordinates, 4)));
            ValidationRules.Add(new SimpleRule("NewSimAreaOverlayCoordinates", "Invalid latitude/longitude format. Please use decimal degrees", o => ValidateLatLonFormat(((NewRangeComplexViewModel)o).NewSimAreaOverlayCoordinates)));
            ValidationRules.Add(new SimpleRule("NewSimAreaOverlayCoordinates", "There must be at least four points given to define an area", o => ValidateMinimumCoordinateCount(((NewRangeComplexViewModel)o).NewSimAreaOverlayCoordinates, 4)));
            ValidationRules.Add(new SimpleRule("NewSimAreaOverlayCoordinates", "The points provided are not usable as a perimeter.  Line segments are used in the order given, and cannot cross each other.  The resulting polygon must also be closed.", o => ValidateUsabilityAsPerimeter(((NewRangeComplexViewModel)o).NewSimAreaOverlayCoordinates)));
        }

        public static bool ValidateMinimumLineCount(string fieldData, int minimumLineCount)
        {
            if (string.IsNullOrEmpty(fieldData)) return true;
            var lineSeparators = new[] { '\r', '\n' };
            var lines = fieldData.Split(lineSeparators, StringSplitOptions.RemoveEmptyEntries);
            return lines.Length >= minimumLineCount;
        }

        public static bool ValidateLatLonFormat(string fieldData)
        {
            if (string.IsNullOrEmpty(fieldData)) return true;
            var lineSeparators = new[] { '\r', '\n' };
            var lines = fieldData.Split(lineSeparators, StringSplitOptions.RemoveEmptyEntries);
            var earthCoordinates = new List<EarthCoordinate>();
            foreach (var line in lines)
            {
                var coordSeparators = new[] { ',', ' ' };
                var coords = line.Split(coordSeparators, StringSplitOptions.RemoveEmptyEntries);
                double lat, lon;
                if (coords.Length == 2 && double.TryParse(coords[0], out lat) && (double.TryParse(coords[1], out lon)))
                    earthCoordinates.Add(new EarthCoordinate(lat, lon));
                else return false;
            }
            return true;
        }

        public static bool ValidateMinimumCoordinateCount(string fieldData, int minimumCoordinateCount)
        {
            if (string.IsNullOrEmpty(fieldData)) return true;
            var lineSeparators = new[] { '\r', '\n' };
            var lines = fieldData.Split(lineSeparators, StringSplitOptions.RemoveEmptyEntries);
            var earthCoordinates = new List<EarthCoordinate>();
            foreach (var line in lines)
            {
                var coordSeparators = new[] { ',', ' ' };
                var coords = line.Split(coordSeparators, StringSplitOptions.RemoveEmptyEntries);
                double lat, lon;
                if (coords.Length == 2 && double.TryParse(coords[0], out lat) && (double.TryParse(coords[1], out lon)))
                    earthCoordinates.Add(new EarthCoordinate(lat, lon));
            }
            return earthCoordinates.Count >= minimumCoordinateCount;
        }

        public static bool ValidateUsabilityAsPerimeter(string fieldData)
        {
            if (string.IsNullOrEmpty(fieldData)) return true;
            var lineSeparators = new[] { '\r', '\n' };
            var lines = fieldData.Split(lineSeparators, StringSplitOptions.RemoveEmptyEntries);
            var earthCoordinates = new List<EarthCoordinate>();
            foreach (var line in lines)
            {
                var coordSeparators = new[] { ',', ' ' };
                var coords = line.Split(coordSeparators, StringSplitOptions.RemoveEmptyEntries);
                double lat, lon;
                if (coords.Length == 2 && double.TryParse(coords[0], out lat) && (double.TryParse(coords[1], out lon)))
                    earthCoordinates.Add(new EarthCoordinate(lat, lon));
            }
            var overlayLineSegments = new OverlayLineSegments(earthCoordinates.ToArray(), Colors.Black);
            return overlayLineSegments.IsUsableAsPerimeter;
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

        private List<EarthCoordinate> NewOpAreaOverlayEarthCoordinates { get; set; }
        private List<EarthCoordinate> NewSimAreaOverlayEarthCoordinates { get; set; }
        private GeoRect OpBounds { get; set; }
        private GeoRect SimBounds { get; set; }

        #region OkCommand

        private SimpleCommand<object, object> _ok;

        public SimpleCommand<object, object> OkCommand
        {
            get
            {
                return _ok ??
                       (_ok =
                        new SimpleCommand<object, object>(delegate { return OkIsEnabled; },
                                                          delegate { OkCommandHandler(); }));
            }
        }

        private bool OkIsEnabled
        {
            get
            {
                Validate();
                if (!IsValid) return false;
                return true;
            }
        }

        private static void WriteOverlayFile(string fileName, IEnumerable<EarthCoordinate> coords)
        {
            using (var writer = new StreamWriter(fileName))
            {
                writer.WriteLine("navigation");
                writer.WriteLine("green");
                writer.WriteLine("solid");
                writer.WriteLine("move");
                var first = true;
                foreach (var coordinate in coords)
                {
                    writer.WriteLine("{0:0.0000}  {1:0.0000}", coordinate.Latitude, coordinate.Longitude);
                    if (first) writer.WriteLine("lines");
                    first = false;
                }
            }
        }

        private void OkCommandHandler()
        {
            Directory.CreateDirectory(LocationPath);
            var areasPath = Path.Combine(LocationPath, "Areas");
            Directory.CreateDirectory(areasPath);
            Directory.CreateDirectory(Path.Combine(LocationPath, "Bathymetry"));
            Directory.CreateDirectory(Path.Combine(LocationPath, "Environment"));
            Directory.CreateDirectory(Path.Combine(LocationPath, "GeographicAreas"));
            Directory.CreateDirectory(Path.Combine(LocationPath, "Images"));
            Directory.CreateDirectory(Path.Combine(LocationPath, "Species"));

            var opsOverlayFilename = Path.Combine(areasPath, String.Format("{0}_OpArea.ovr", LocationName));
            if (!string.IsNullOrEmpty(ExistingOpAreaOverlayFilename))
                File.Copy(ExistingOpAreaOverlayFilename, opsOverlayFilename);
            else WriteOverlayFile(opsOverlayFilename, NewOpAreaOverlayEarthCoordinates);

            var simOverlayFilename = Path.Combine(areasPath, String.Format("{0}_SimArea.ovr", LocationName));
            if (!string.IsNullOrEmpty(ExistingSimAreaOverlayFilename))
                File.Copy(ExistingSimAreaOverlayFilename, simOverlayFilename);
            else WriteOverlayFile(simOverlayFilename, NewSimAreaOverlayEarthCoordinates);

            using (
                var writer = new StreamWriter(Path.Combine(Path.GetDirectoryName(LocationPath), "SimAreas.csv"), true))
                writer.WriteLine("{0},{1:0.0###},{2:0.0###},{3:0.0###},{4:0.0###},{5},{6}", LocationName,
                                 ReferencePointLatitude, ReferencePointLongitude, Height, GeoidSeparation,
                                 Path.GetFileName(opsOverlayFilename), Path.GetFileName(simOverlayFilename));

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
                           ? OverlayFile.ValidateCoordinates(NewOpAreaOverlayCoordinates, "Op Limits", out opCoords,out opErrors)
                           : OverlayFile.ValidateFile(ExistingOpAreaOverlayFilename, "Op Limits", out opCoords,out opErrors);
            ValidationErrorText += opErrors;
            string simErrors;
            SimBounds = !string.IsNullOrEmpty(NewSimAreaOverlayCoordinates)
                            ? OverlayFile.ValidateCoordinates(NewSimAreaOverlayCoordinates, "Sim Limits", out simCoords,out simErrors)
                            : OverlayFile.ValidateFile(ExistingSimAreaOverlayFilename, "Sim Limits", out simCoords,out simErrors);
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
    }
}