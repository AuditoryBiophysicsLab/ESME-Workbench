using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using Cinch;
using ESME.Data;
using ESME.Model;
using ESME.Overlay;
using HRC.Navigation;

namespace ESME.Views.Locations
{
    public class NewRangeComplexViewModel : ViewModelBase, ISupportValidation
    {
        public NewRangeComplexViewModel(AppSettings appSettings)
        {
            SimAreaFolder = appSettings.ScenarioDataDirectory;
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
            }
        }

        #endregion

        #region public float ReferencePointLatitude { get; set; }

        private static readonly PropertyChangedEventArgs ReferencePointLatitudeChangedEventArgs =
            ObservableHelper.CreateArgs<NewRangeComplexViewModel>(x => x.ReferencePointLatitude);

        private float _referencePointLatitude;

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

        #endregion

        #region public float ReferencePointLongitude { get; set; } [changed]

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

        #region ReferencePointLongitudeChangedCommand
        SimpleCommand<object, object> _referencePointLongitudeChanged;

        public SimpleCommand<object, object> ReferencePointLongitudeChangedCommand
        {
            get
            {
                return _referencePointLongitudeChanged ??
                       (_referencePointLongitudeChanged =
                        new SimpleCommand<object, object>(delegate(object cinchArgs)
                        {
                            var sender = (TextBox)((EventToCommandArgs)cinchArgs).Sender;
                            float temp;
                            if (sender != null && !string.IsNullOrEmpty(sender.Text)) ReferencePointLongitude = float.TryParse(sender.Text, out temp) ? temp : float.NaN;

                        }));
            }
        }

        #region public string ReferencePointLongitudeString { get; set; }

        public string ReferencePointLongitudeString
        {
            get { return _referencePointLongitudeString; }
            set
            {
                if (_referencePointLongitudeString == value) return;
                _referencePointLongitudeString = value;
                NotifyPropertyChanged(ReferencePointLongitudeStringChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ReferencePointLongitudeStringChangedEventArgs = ObservableHelper.CreateArgs<NewRangeComplexViewModel>(x => x.ReferencePointLongitudeString);
        string _referencePointLongitudeString;

        #endregion

        #endregion

        #endregion

        #region public float Height { get; set; }

        private static readonly PropertyChangedEventArgs HeightChangedEventArgs =
            ObservableHelper.CreateArgs<NewRangeComplexViewModel>(x => x.Height);

        private float _height;

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

        #endregion

        #region public float GeoidSeparation { get; set; }

        private static readonly PropertyChangedEventArgs GeoidSeparationChangedEventArgs =
            ObservableHelper.CreateArgs<NewRangeComplexViewModel>(x => x.GeoidSeparation);

        private float _geoidSeparation;

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

        #region User Field Changed Commands
        #region LocationNameChangedCommand

        private SimpleCommand<object, object> _locationNameChanged;

        public SimpleCommand<object, object> LocationNameChangedCommand
        {
            get
            {
                return _locationNameChanged ??
                       (_locationNameChanged =
                        new SimpleCommand<object, object>(delegate(object cinchArgs)
                        {
                            var sender =
                                (TextBox)((EventToCommandArgs)cinchArgs).Sender;
                            if (sender != null &&
                                !string.IsNullOrEmpty(sender.Text))
                                LocationName = sender.Text;
                        }));
            }
        }

        #endregion

        #region ReferencePointLatitudeChangedCommand

        private SimpleCommand<object, object> _referencePointLatitudeChanged;

        public SimpleCommand<object, object> ReferencePointLatitudeChangedCommand
        {
            get
            {
                return _referencePointLatitudeChanged ??
                       (_referencePointLatitudeChanged =
                        new SimpleCommand<object, object>(delegate(object cinchArgs)
                        {
                            var sender = (TextBox)((EventToCommandArgs)cinchArgs).Sender;
                            float lat;
                            if (sender != null && !string.IsNullOrEmpty(sender.Text))
                                ReferencePointLatitude = float.TryParse(sender.Text, out lat) ? lat : float.NaN;

                        }));
            }
        }

        #region public string ReferencePointLatitudeString { get; set; }

        public string ReferencePointLatitudeString
        {
            get { return _referencePointLatitudeString; }
            set
            {
                if (_referencePointLatitudeString == value) return;
                _referencePointLatitudeString = value;
                NotifyPropertyChanged(ReferencePointLatitudeStringChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ReferencePointLatitudeStringChangedEventArgs = ObservableHelper.CreateArgs<NewRangeComplexViewModel>(x => x.ReferencePointLatitudeString);
        string _referencePointLatitudeString;

        #endregion

        #endregion

        #region HeightChangedCommand

        private SimpleCommand<object, object> _heightChanged;

        public SimpleCommand<object, object> HeightChangedCommand
        {
            get
            {
                return _heightChanged ??
                       (_heightChanged =
                        new SimpleCommand<object, object>(delegate(object cinchArgs)
                                                              {
                                                                  float height;
                                                                  var sender = (TextBox)((EventToCommandArgs)cinchArgs).Sender;
                                                                  if (sender != null && !string.IsNullOrEmpty(sender.Text))
                                                                      Height = float.TryParse(sender.Text, out height) ? height : float.NaN;
                                                              }));
            }
        }

        #endregion

        #region GeoidSeparationChangedCommand

        private SimpleCommand<object, object> _geoidSeparationChanged;

        public SimpleCommand<object, object> GeoidSeparationChangedCommand
        {
            get
            {
                return _geoidSeparationChanged ??
                       (_geoidSeparationChanged =
                        new SimpleCommand<object, object>(delegate(object cinchArgs)
                                                              {
                                                                  float geoid;
                                                                  var sender = (TextBox)((EventToCommandArgs)cinchArgs).Sender;
                                                                  if (sender != null && !string.IsNullOrEmpty(sender.Text))
                                                                      GeoidSeparation = float.TryParse(sender.Text,out geoid) ? geoid: float.NaN;
                                                              }));
            }
        }

        #endregion

        #region NewOpAreaOverlayCoordinatesChangedCommand

        private SimpleCommand<object, object> _newOpAreaOverlayCoordinatesChanged;

        public SimpleCommand<object, object> NewOpAreaOverlayCoordinatesChangedCommand
        {
            get
            {
                return _newOpAreaOverlayCoordinatesChanged ??
                       (_newOpAreaOverlayCoordinatesChanged =
                        new SimpleCommand<object, object>(delegate(object cinchArgs)
                        {
                            var sender =
                                (TextBox)((EventToCommandArgs)cinchArgs).Sender;
                            if (sender != null &&
                                !string.IsNullOrEmpty(sender.Text))
                                NewOpAreaOverlayCoordinates = sender.Text;
                        }));
            }
        }

        #endregion

        #region NewSimAreaOverlayCoordinatesChangedCommand

        private SimpleCommand<object, object> _newSimAreaOverlayCoordinatesChanged;

        public SimpleCommand<object, object> NewSimAreaOverlayCoordinatesChangedCommand
        {
            get
            {
                return _newSimAreaOverlayCoordinatesChanged ??
                       (_newSimAreaOverlayCoordinatesChanged =
                        new SimpleCommand<object, object>(delegate(object cinchArgs)
                        {
                            var sender =
                                (TextBox)((EventToCommandArgs)cinchArgs).Sender;
                            if (sender != null &&
                                !string.IsNullOrEmpty(sender.Text))
                                NewSimAreaOverlayCoordinates = sender.Text;
                        }));
            }
        }

        #endregion
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
                bool first = true;
                foreach (EarthCoordinate coordinate in coords)
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
            string areasPath = Path.Combine(LocationPath, "Areas");
            Directory.CreateDirectory(areasPath);
            Directory.CreateDirectory(Path.Combine(LocationPath, "Bathymetry"));
            Directory.CreateDirectory(Path.Combine(LocationPath, "Environment"));
            Directory.CreateDirectory(Path.Combine(LocationPath, "GeographicAreas"));
            Directory.CreateDirectory(Path.Combine(LocationPath, "Images"));
            Directory.CreateDirectory(Path.Combine(LocationPath, "Species"));

            string opsOverlayFilename = Path.Combine(areasPath, String.Format("{0}_OpArea.ovr", LocationName));
            if (!string.IsNullOrEmpty(ExistingOpAreaOverlayFilename))
                File.Copy(ExistingOpAreaOverlayFilename, opsOverlayFilename);
            else WriteOverlayFile(opsOverlayFilename, NewOpAreaOverlayEarthCoordinates);

            string simOverlayFilename = Path.Combine(areasPath, String.Format("{0}_SimArea.ovr", LocationName));
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

        public bool IsValid
        {
            get { return string.IsNullOrEmpty(ValidationErrorText); }
        }

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
            OpBounds = !string.IsNullOrEmpty(NewOpAreaOverlayCoordinates)
                           ? ValidateOverlayCoordinates(NewOpAreaOverlayCoordinates, "Op Limits", out opCoords)
                           : ValidateOverlayFile(ExistingOpAreaOverlayFilename, "Op Limits", out opCoords);
            SimBounds = !string.IsNullOrEmpty(NewSimAreaOverlayCoordinates)
                            ? ValidateOverlayCoordinates(NewSimAreaOverlayCoordinates, "Sim Limits", out simCoords)
                            : ValidateOverlayFile(ExistingSimAreaOverlayFilename, "Sim Limits", out simCoords);

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

        private GeoRect ValidateOverlayFile(string overlayFileName, string overlayName,
                                            out List<EarthCoordinate> earthCoordinates)
        {
            earthCoordinates = null;
            try
            {
                var myOvr = new OverlayFile(overlayFileName);
                if (myOvr.Shapes.Length != 1 || !myOvr.Shapes[0].IsUsableAsPerimeter)
                    ValidationErrorText += "Specified " + overlayName + " file is invalid\n";
                else
                {
                    earthCoordinates = myOvr.Shapes[0].EarthCoordinates;
                    return new GeoRect(myOvr.Shapes[0].BoundingBox);
                }
            }
            catch (Exception e)
            {
                ValidationErrorText += "Error loading " + overlayFileName + ": " + e.Message + "\n";
            }
            return null;
        }

        private GeoRect ValidateOverlayCoordinates(string fieldData, string overlayName,
                                                   out List<EarthCoordinate> earthCoordinates)
        {
            var lineSeparators = new[] { '\r', '\n' };
            string[] lines = fieldData.Split(lineSeparators, StringSplitOptions.RemoveEmptyEntries);
            if (lines.Length < 4)
                ValidationErrorText += overlayName + ": There must be at least four points given to define an area\n";
            earthCoordinates = new List<EarthCoordinate>();
            int lineCount = 0;
            foreach (string line in lines)
            {
                lineCount++;
                var coordSeparators = new[] { ',', ' ' };
                string[] coords = line.Split(coordSeparators, StringSplitOptions.RemoveEmptyEntries);
                double lat, lon;
                if (double.TryParse(coords[0], out lat) && (double.TryParse(coords[1], out lon)))
                    earthCoordinates.Add(new EarthCoordinate(lat, lon));
                else
                    ValidationErrorText +=
                        string.Format(
                            overlayName + ": Invalid latitude/longitude on line {0}. Please use decimal degrees\n",
                            lineCount);
            }
            if (string.IsNullOrEmpty(ValidationErrorText))
            {
                if (earthCoordinates.Count < 4)
                    ValidationErrorText += overlayName +
                                           ": There must be at least four points given to define an area\n";
                else
                {
                    var overlayLineSegments = new OverlayLineSegments(earthCoordinates.ToArray(), Colors.Black);
                    if (!overlayLineSegments.IsUsableAsPerimeter)
                        ValidationErrorText += overlayName +
                                               ": The points provided are not usable as a perimeter.  Line segments are used in the order given, and cannot cross each other.  The resulting polygon must also be closed\n";
                    else return new GeoRect(overlayLineSegments.BoundingBox);
                }
            }
            return null;
        }
    }
}