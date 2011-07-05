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
    public class NewOverlayViewModel : ViewModelBase, ISupportValidation
    {
        public NewOverlayViewModel(AppSettings appSettings)
        {
            SimAreaFolder = appSettings.ScenarioDataDirectory;
            
        }

        #region public string  SimAreaFolder { get; set; }

        public string  SimAreaFolder
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
        private string  _simAreaFolder;

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
        
        List<EarthCoordinate> OverlayEarthCoordinates { get; set; }
      
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
            var locationName = "";
            if (!OverlayName.EndsWith(".ovr")) OverlayName += ".ovr";
            var overlayFileName = Path.Combine(SimAreaFolder,locationName,"Areas",OverlayName);
            WriteOverlayFile(overlayFileName,OverlayEarthCoordinates);
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

        public bool IsValid { get { return string.IsNullOrEmpty(ValidationErrorText); } }

        #region public Visibility ErrorVisibility { get; set; }

        public Visibility ErrorVisibility
        {
            get { return IsValid ? Visibility.Collapsed : Visibility.Visible; }
        }

        private static readonly PropertyChangedEventArgs ErrorVisibilityChangedEventArgs = ObservableHelper.CreateArgs<NewRangeComplexViewModel>(x => x.ErrorVisibility);

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

        static readonly PropertyChangedEventArgs ValidationErrorTextChangedEventArgs = ObservableHelper.CreateArgs<NewRangeComplexViewModel>(x => x.ValidationErrorText);
        string _validationErrorText;

        #endregion

        private GeoRect Bounds { get; set; }
        public void Validate()
        {
            ValidationErrorText = "";
            if (string.IsNullOrEmpty(OverlayName))
                ValidationErrorText += "The Overlay file must have a name\n";
            if (string.IsNullOrEmpty(OverlayCoordinates))
                ValidationErrorText += "No Overlay Coordinates have been entered\n";
            if (!string.IsNullOrEmpty(ValidationErrorText)) return;
            List<EarthCoordinate> coords;
            Bounds = ValidateOverlayCoordinates(OverlayCoordinates, "Op Limits", out coords);
            
            if (Bounds != null) OverlayEarthCoordinates = coords;
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
