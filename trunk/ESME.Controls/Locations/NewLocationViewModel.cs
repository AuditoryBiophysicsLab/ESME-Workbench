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

        string LocationPath
        {
            get
            {
                return string.IsNullOrEmpty(SimAreaFolder) || string.IsNullOrEmpty(LocationName) ? null : Path.Combine(SimAreaFolder, LocationName);
            }
        }

        #endregion

        #region public string ExistingOverlayFilename { get; set; }

        public string ExistingOverlayFilename
        {
            get { return _existingOverlayFilename; }
            set
            {
                if (_existingOverlayFilename == value) return;
                _existingOverlayFilename = value;
                NotifyPropertyChanged(ExistingOverlayFilenameChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs ExistingOverlayFilenameChangedEventArgs = ObservableHelper.CreateArgs<NewLocationViewModel>(x => x.ExistingOverlayFilename);
        private string _existingOverlayFilename;

        #endregion

        #region public string NewOverlayCoordinates { get; set; }

        public string NewOverlayCoordinates
        {
            get { return _newOverlayCoordinates; }
            set
            {
                if (_newOverlayCoordinates == value) return;
                _newOverlayCoordinates = value;
                NotifyPropertyChanged(NewOverlayCoordinatesChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs NewOverlayCoordinatesChangedEventArgs = ObservableHelper.CreateArgs<NewLocationViewModel>(x => x.NewOverlayCoordinates);
        private string _newOverlayCoordinates;

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

        List<EarthCoordinate> NewOverlayEarthCoordinates { get; set; }
        GeoRect Bounds { get; set; }

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
                if ((LocationPath == null) || Directory.Exists(LocationPath)) return false;
                Validate();
                if (string.IsNullOrEmpty(ExistingOverlayFilename) && ((NewOverlayEarthCoordinates == null) || (NewOverlayEarthCoordinates.Count < 4)))
                    return false;
                if (!IsValid) return false;
                return true;
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

            var newOverlayFilename = Path.Combine(areasPath, String.Format("{0}_OpArea.ovr", LocationName));
            if (!string.IsNullOrEmpty(ExistingOverlayFilename))
                File.Copy(ExistingOverlayFilename, newOverlayFilename);
            else
            {
                using (var writer = new StreamWriter(newOverlayFilename))
                {
                    writer.WriteLine("navigation");
                    writer.WriteLine("green");
                    writer.WriteLine("solid");
                    writer.WriteLine("move");
                    var first = true;
                    foreach (var coordinate in NewOverlayEarthCoordinates)
                    {
                        writer.WriteLine("{0:0.0000}  {1:0.0000}", coordinate.Latitude, coordinate.Longitude);
                        if (first) writer.WriteLine("lines");
                        first = false;
                    }
                }
                Bounds = new GeoRect(NewOverlayEarthCoordinates);
            }
            var location = new Location
                               {
                                   LocationMetadata =
                                       {
                                           Name = LocationName,
                                           Bounds = Bounds,
                                           AvailableResolutions = new AvailableResolutions(),
                                           AvailableTimePeriods = new AvailableTimePeriods()
                                       }
                               };
            location.Save(Path.Combine(LocationPath, "location.xml"));
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
            get { return (!string.IsNullOrEmpty(NewOverlayCoordinates)); }
        }

        void ClearCoordinatesCommandHandler()
        {
            NewOverlayCoordinates = "";
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

        public string ValidationErrorText { get; private set; }

        public void Validate()
        {
            ValidationErrorText = "";
            if ((LocationPath != null) && Directory.Exists(LocationPath))
                ValidationErrorText += "Location already exists, please choose a different name\n";
            if (!string.IsNullOrEmpty(ExistingOverlayFilename) && !string.IsNullOrEmpty(NewOverlayCoordinates))
                ValidationErrorText += "Select EITHER an existing overlay file OR coordinates for a new overlay\n";
            if (!string.IsNullOrEmpty(ExistingOverlayFilename) && !File.Exists(ExistingOverlayFilename))
                ValidationErrorText += "Selected overlay file does not exist\n";
            NotifyPropertyChanged(ErrorVisibilityChangedEventArgs);
            if (!string.IsNullOrEmpty(ValidationErrorText)) return;

            if (string.IsNullOrEmpty(NewOverlayCoordinates)) return;

            var lineSeparators = new [] {'\r', '\n'};
            var lines = NewOverlayCoordinates.Split(lineSeparators, StringSplitOptions.RemoveEmptyEntries);
            if (lines.Length < 4) ValidationErrorText += "There must be at least four points given to define an area\n";
            NewOverlayEarthCoordinates = new List<EarthCoordinate>();
            var lineCount = 0;
            foreach (var line in lines)
            {
                lineCount++;
                var coordSeparators = new[] {',', ' '};
                var coords = line.Split(coordSeparators, StringSplitOptions.RemoveEmptyEntries);
                double lat, lon;
                if (double.TryParse(coords[0], out lat) && (double.TryParse(coords[1], out lon)))
                    NewOverlayEarthCoordinates.Add(new EarthCoordinate(lat, lon));
                else
                    ValidationErrorText += string.Format("Invalid latitude/longitude on line {0}. Please use decimal degrees\n", lineCount);
            }
            if (string.IsNullOrEmpty(ValidationErrorText))
            {
                if (NewOverlayEarthCoordinates.Count < 4) ValidationErrorText += "There must be at least four points given to define an area\n";
                else
                {
                    var overlayLineSegments = new OverlayLineSegments(NewOverlayEarthCoordinates.ToArray(), Colors.Black);
                    if (!overlayLineSegments.IsUsableAsPerimeter)
                        ValidationErrorText += "The points provided are not usable as a perimeter.  Line segments are used in the order given, and cannot cross each other.  The resulting polygon must also be closed\n";
                    else Bounds = new GeoRect(overlayLineSegments.BoundingBox);
                }
            }
            NotifyPropertyChanged(ErrorVisibilityChangedEventArgs);
        }
    }
}
