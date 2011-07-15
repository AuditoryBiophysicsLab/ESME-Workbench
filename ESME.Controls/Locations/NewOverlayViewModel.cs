using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Windows;
using System.Windows.Controls;
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
        public NewOverlayViewModel(AppSettings appSettings, string locationName)
        {
            SimAreaFolder = appSettings.ScenarioDataDirectory;
            LocationName = locationName;

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
        private string LocationName { get; set; }

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
            if (!OverlayName.EndsWith(".ovr")) OverlayName += ".ovr";
            var overlayFileName = Path.Combine(SimAreaFolder, LocationName, "Areas", OverlayName);
            WriteOverlayFile(overlayFileName, OverlayEarthCoordinates);

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
            if(((OverlayName != null && File.Exists(Path.Combine(SimAreaFolder, LocationName, "Areas", OverlayName))) || File.Exists(Path.Combine(SimAreaFolder, LocationName, "Areas", OverlayName + ".ovr"))))
                ValidationErrorText += "An overlay file with the specified file name already exists.\n";
            if (string.IsNullOrEmpty(OverlayCoordinates))
                ValidationErrorText += "No Overlay Coordinates have been entered\n";
            if (!string.IsNullOrEmpty(ValidationErrorText)) return;
            List<EarthCoordinate> coords;
            string overlayError;
            Bounds = OverlayFile.ValidateCoordinates(OverlayCoordinates, "Op Limits", out coords, out overlayError);
            ValidationErrorText += overlayError;

            if (Bounds != null) OverlayEarthCoordinates = coords;
            NotifyPropertyChanged(ErrorVisibilityChangedEventArgs);
        }
    }
}
