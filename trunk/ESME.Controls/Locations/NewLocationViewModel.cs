using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Windows;
using System.Windows.Media;
using Cinch;
using ESME.Data;
using ESME.Environment;
using ESME.Model;
using ESME.Overlay;
using HRC.Navigation;

namespace ESME.Views.Locations
{
    public class NewLocationViewModel : ViewModelBase, ISupportValidation
    {
        public NewLocationViewModel(AppSettings appSettings)
        {
            SimAreaFolder = appSettings.ScenarioDataDirectory;
        }

        #region public string LocationName { get; set; }

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

        private static readonly PropertyChangedEventArgs LocationNameChangedEventArgs = ObservableHelper.CreateArgs<NewLocationViewModel>(x => x.LocationName);
        private string _locationName;

        public string LocationPath
        {
            get
            {
                return string.IsNullOrEmpty(SimAreaFolder) || string.IsNullOrEmpty(LocationName) ? null : Path.Combine(SimAreaFolder, LocationName);
            }
        }

        #endregion

        #region public string ExistingOpAreaOverlayFilename { get; set; }

        public string ExistingOpAreaOverlayFilename
        {
            get { return _existingOpAreaOverlayFilename; }
            set
            {
                if (_existingOpAreaOverlayFilename == value) return;
                _existingOpAreaOverlayFilename = value;
                NotifyPropertyChanged(ExistingOverlayFilenameChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs ExistingOverlayFilenameChangedEventArgs = ObservableHelper.CreateArgs<NewLocationViewModel>(x => x.ExistingOpAreaOverlayFilename);
        private string _existingOpAreaOverlayFilename;

        #endregion

        #region public string NewOpAreaOverlayCoordinates { get; set; }

        public string NewOpAreaOverlayCoordinates
        {
            get { return _newOpAreaOverlayCoordinates; }
            set
            {
              //  if (_newOpAreaOverlayCoordinates == value) return;
                _newOpAreaOverlayCoordinates = value;
                NotifyPropertyChanged(NewOverlayCoordinatesChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs NewOverlayCoordinatesChangedEventArgs = ObservableHelper.CreateArgs<NewLocationViewModel>(x => x.NewOpAreaOverlayCoordinates);
        private string _newOpAreaOverlayCoordinates;

        #endregion

        #region public string ExistingSimAreaOverlayFilename { get; set; }

        public string ExistingSimAreaOverlayFilename
        {
            get { return _existingSimAreaOverlayFilename; }
            set
            {
                if (_existingSimAreaOverlayFilename == value) return;
                _existingSimAreaOverlayFilename = value;
                NotifyPropertyChanged(ExistingSimAreaOverlayFilenameChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs ExistingSimAreaOverlayFilenameChangedEventArgs = ObservableHelper.CreateArgs<NewLocationViewModel>(x => x.ExistingSimAreaOverlayFilename);
        private string _existingSimAreaOverlayFilename;

        #endregion

        #region public string NewSimAreaOverlayCoordinates { get; set; }

        public string NewSimAreaOverlayCoordinates
        {
            get { return _newSimAreaOverlayCoordinates; }
            set
            {
               // if (_newSimAreaOverlayCoordinates == value) return;
                _newSimAreaOverlayCoordinates = value;
                NotifyPropertyChanged(NewSimAreaOverlayCoordinatesChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs NewSimAreaOverlayCoordinatesChangedEventArgs = ObservableHelper.CreateArgs<NewLocationViewModel>(x => x.NewSimAreaOverlayCoordinates);
        private string _newSimAreaOverlayCoordinates;

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

        private static readonly PropertyChangedEventArgs ReferencePointLatitudeChangedEventArgs = ObservableHelper.CreateArgs<NewLocationViewModel>(x => x.ReferencePointLatitude);
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

        private static readonly PropertyChangedEventArgs ReferencePointLongitudeChangedEventArgs = ObservableHelper.CreateArgs<NewLocationViewModel>(x => x.ReferencePointLongitude);
        private float _referencePointLongitude;

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

        private static readonly PropertyChangedEventArgs HeightChangedEventArgs = ObservableHelper.CreateArgs<NewLocationViewModel>(x => x.Height);
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

        private static readonly PropertyChangedEventArgs GeoidSeparationChangedEventArgs = ObservableHelper.CreateArgs<NewLocationViewModel>(x => x.GeoidSeparation);
        private float _geoidSeparation;

        #endregion

        #region public string SimAreaFolder { get; set; }

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

        private static readonly PropertyChangedEventArgs SimAreaFolderChangedEventArgs = ObservableHelper.CreateArgs<NewLocationViewModel>(x => x.SimAreaFolder);
        private string _simAreaFolder;

        #endregion

        List<EarthCoordinate> NewOpAreaOverlayEarthCoordinates { get; set; }
        private List<EarthCoordinate> NewSimAreaOverlayEarthCoordinates { get; set; }
        GeoRect OpBounds { get; set; }
        GeoRect SimBounds { get; set; }


        #region OkCommand

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

        bool OkIsEnabled
        {
            get
            {
                Validate();
                if (!IsValid) return false;
                return true;
            }
        }

        static void WriteOverlayFile(string fileName, IEnumerable<EarthCoordinate> coords)
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

        void OkCommandHandler()
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

#if false
            var location = new Location
                                   {
                                       LocationMetadata =
                                           {
                                               Name = LocationName,
                                               Bounds = OpBounds,
                                               AvailableResolutions = new AvailableResolutions(),
                                               AvailableTimePeriods = new AvailableTimePeriods()
                                           }
                                   };
            location.Save(Path.Combine(LocationPath, "location.xml")); 
#endif
            using(var writer = new StreamWriter(Path.Combine(Path.GetDirectoryName(LocationPath),"SimAreas.csv"),true))
                writer.WriteLine("{0},{1:0.0###},{2:0.0###},{3:0.0###},{4:0.0###},{5},{6}", LocationName, ReferencePointLatitude,ReferencePointLongitude,Height,GeoidSeparation,Path.GetFileName(opsOverlayFilename),Path.GetFileName(simOverlayFilename));

            CloseActivePopUpCommand.Execute(true);
        }
        private SimpleCommand<object, object> _ok;

        #endregion

        #region ClearOpAreaCoordinatesCommand

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

        private SimpleCommand<object, object> _clearOpAreaCoordinates;

        bool ClearOpAreaCoordinatesIsEnabled
        {
            get { return (!string.IsNullOrEmpty(NewOpAreaOverlayCoordinates)); }
        }

        void ClearOpAreaCoordinatesCommandHandler()
        {
            NewOpAreaOverlayCoordinates = "";
        }

        #endregion

        #region ClearSimAreaCoordinatesCommand

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

        private SimpleCommand<object, object> _clearSimAreaCoordinates;

        bool ClearSimAreaCoordinatesIsEnabled
        {
            get { return (!string.IsNullOrEmpty(NewSimAreaOverlayCoordinates)); }
        }

        void ClearSimAreaCoordinatesCommandHandler()
        {
            NewSimAreaOverlayCoordinates = "";
        }

        #endregion

        public bool IsValid { get { return string.IsNullOrEmpty(ValidationErrorText); } }

        #region public Visibility ErrorVisibility { get; set; }

        public Visibility ErrorVisibility
        {
            get { return IsValid ? Visibility.Collapsed : Visibility.Visible; }
        }

        private static readonly PropertyChangedEventArgs ErrorVisibilityChangedEventArgs = ObservableHelper.CreateArgs<NewLocationViewModel>(x => x.ErrorVisibility);

        #endregion

        #region public string ValidationErrorText { get; set; }

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

        static readonly PropertyChangedEventArgs ValidationErrorTextChangedEventArgs = ObservableHelper.CreateArgs<NewLocationViewModel>(x => x.ValidationErrorText);
        string _validationErrorText;

        #endregion

        public void Validate()
        {
            ValidationErrorText = "";
            if ((LocationPath != null) && Directory.Exists(LocationPath))
                ValidationErrorText += "Location already exists, please choose a different name\n";
            if (string.IsNullOrEmpty(LocationName)) 
                ValidationErrorText += "New location name must be specified\n";
            if (-180 > ReferencePointLatitude || ReferencePointLatitude > 180)
                ValidationErrorText += "Reference Latitude is out of bounds\n";
            if (-90 > ReferencePointLongitude || ReferencePointLongitude > 90)
                ValidationErrorText += "Reference Longitude is out of bounds\n";
            if (!string.IsNullOrEmpty(ExistingOpAreaOverlayFilename) && !File.Exists(ExistingOpAreaOverlayFilename))
                ValidationErrorText += "Selected overlay file does not exist\n";
               if (!string.IsNullOrEmpty(ExistingSimAreaOverlayFilename) && !string.IsNullOrEmpty(NewSimAreaOverlayCoordinates))
                ValidationErrorText += "Select EITHER an existing overlay file OR coordinates for a new Simulation Limit overlay\n";
             if (!string.IsNullOrEmpty(ExistingOpAreaOverlayFilename) && !string.IsNullOrEmpty(NewOpAreaOverlayCoordinates))
                ValidationErrorText += "Select EITHER an existing overlay file OR coordinates for a new Operational Limit overlay\n";
            if (!string.IsNullOrEmpty(ExistingSimAreaOverlayFilename) && !File.Exists(ExistingSimAreaOverlayFilename))
                ValidationErrorText += "Selected overlay file does not exist\n";
            if (string.IsNullOrEmpty(NewOpAreaOverlayCoordinates)) 
                ValidationErrorText += "Baseline operational area must be defined\n";
            if (string.IsNullOrEmpty(NewSimAreaOverlayCoordinates)) 
                ValidationErrorText += "Baseline simulation area must be defined\n";
            if (!string.IsNullOrEmpty(ValidationErrorText)) return;
            List<EarthCoordinate> opCoords = null;
            List<EarthCoordinate> simCoords = null;
            OpBounds = ValidateOverlayCoordinates(NewOpAreaOverlayCoordinates, "Op Limits", out opCoords);
            SimBounds = ValidateOverlayCoordinates(NewSimAreaOverlayCoordinates, "Sim Limits", out simCoords);
            if (OpBounds != null) NewOpAreaOverlayEarthCoordinates = opCoords;
            if (SimBounds != null) NewSimAreaOverlayEarthCoordinates = simCoords;

#if false
            var lineSeparators = new[] { '\r', '\n' };
            var lines = NewOpAreaOverlayCoordinates.Split(lineSeparators, StringSplitOptions.RemoveEmptyEntries);
            if (lines.Length < 4) ValidationErrorText += "There must be at least four points given to define an area\n";
            NewOpAreaOverlayEarthCoordinates = new List<EarthCoordinate>();
            var lineCount = 0;
            foreach (var line in lines)
            {
                lineCount++;
                var coordSeparators = new[] { ',', ' ' };
                var coords = line.Split(coordSeparators, StringSplitOptions.RemoveEmptyEntries);
                double lat, lon;
                if (double.TryParse(coords[0], out lat) && (double.TryParse(coords[1], out lon)))
                    NewOpAreaOverlayEarthCoordinates.Add(new EarthCoordinate(lat, lon));
                else
                    ValidationErrorText += string.Format("Invalid latitude/longitude on line {0}. Please use decimal degrees\n", lineCount);
            }
            if (string.IsNullOrEmpty(ValidationErrorText))
            {
                if (NewOpAreaOverlayEarthCoordinates.Count < 4) ValidationErrorText += "There must be at least four points given to define an area\n";
                else
                {
                    var overlayLineSegments = new OverlayLineSegments(NewOpAreaOverlayEarthCoordinates.ToArray(), Colors.Black);
                    if (!overlayLineSegments.IsUsableAsPerimeter)
                        ValidationErrorText += "The points provided are not usable as a perimeter.  Line segments are used in the order given, and cannot cross each other.  The resulting polygon must also be closed\n";
                    else Bounds = new GeoRect(overlayLineSegments.BoundingBox);
                }
            } 
#endif
            NotifyPropertyChanged(ErrorVisibilityChangedEventArgs);
        }

        private GeoRect ValidateOverlayCoordinates(string fieldData, string overlayName, out List<EarthCoordinate> earthCoordinates)
        {
            var lineSeparators = new[] { '\r', '\n' };
            var lines = fieldData.Split(lineSeparators, StringSplitOptions.RemoveEmptyEntries);
            if (lines.Length < 4) ValidationErrorText += overlayName +": There must be at least four points given to define an area\n";
            earthCoordinates = new List<EarthCoordinate>();
            var lineCount = 0;
            foreach (var line in lines)
            {
                lineCount++;
                var coordSeparators = new[] { ',', ' ' };
                var coords = line.Split(coordSeparators, StringSplitOptions.RemoveEmptyEntries);
                double lat, lon;
                if (double.TryParse(coords[0], out lat) && (double.TryParse(coords[1], out lon)))
                    earthCoordinates.Add(new EarthCoordinate(lat, lon));
                else
                    ValidationErrorText += string.Format(overlayName + ": Invalid latitude/longitude on line {0}. Please use decimal degrees\n", lineCount);
            }
            if (string.IsNullOrEmpty(ValidationErrorText))
            {
                if (earthCoordinates.Count < 4) ValidationErrorText += overlayName + ": There must be at least four points given to define an area\n";
                else
                {
                    var overlayLineSegments = new OverlayLineSegments(earthCoordinates.ToArray(), Colors.Black);
                    if (!overlayLineSegments.IsUsableAsPerimeter)
                        ValidationErrorText += overlayName + ": The points provided are not usable as a perimeter.  Line segments are used in the order given, and cannot cross each other.  The resulting polygon must also be closed\n";
                    else return new GeoRect(overlayLineSegments.BoundingBox);
                }
            }
            return null;
        }
    }
}
