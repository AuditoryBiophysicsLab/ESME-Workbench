using System.Collections.Generic;
using System.ComponentModel;
using Cinch;
using HRC.Navigation;

namespace ESME.Views.EnvironmentBuilder
{
    public class BathymetryExtractionViewModel : ViewModelBase
    {
        public BathymetryExtractionViewModel(string overlayName, GeoRect boundingBox)
        {
            OverlayName = overlayName;
            BoundingBox = boundingBox;
            SelectedResolution = 2.0f;
            UpdateNote();
            UpdatePointNote();
        }

        #region public string OverlayName { get; private set; }

        public string OverlayName
        {
            get { return _overlayName; }
            private set
            {
                if (_overlayName == value) return;
                _overlayName = value;
                NotifyPropertyChanged(OverlayNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs OverlayNameChangedEventArgs = ObservableHelper.CreateArgs<BathymetryExtractionViewModel>(x => x.OverlayName);
        string _overlayName;

        #endregion

        #region public string Note { get; set; }

        public string Note
        {
            get { return _note; }
            set
            {
                if (_note == value) return;
                _note = value;
                NotifyPropertyChanged(NoteChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs NoteChangedEventArgs = ObservableHelper.CreateArgs<BathymetryExtractionViewModel>(x => x.Note);
        string _note;

        void UpdateNote()
        {
            BathymetryName = string.Format("{0}_{1:0.00}min", OverlayName, SelectedResolution);
            Note = string.Format("Note: Bathymetry data will be extracted within the bounds of the overlay {0}. The resulting bathymetry file will be named {1}", OverlayName, BathymetryName);
        }

        #endregion

        #region public GeoRect BoundingBox { get; set; }

        public GeoRect BoundingBox
        {
            get { return _boundingBox; }
            set
            {
                if (_boundingBox == value) return;
                _boundingBox = value;
                NotifyPropertyChanged(BoundingBoxChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BoundingBoxChangedEventArgs = ObservableHelper.CreateArgs<BathymetryExtractionViewModel>(x => x.BoundingBox);
        GeoRect _boundingBox;

        #endregion

        #region public string PointNote { get; set; }

        public string PointNote
        {
            get { return _pointNote; }
            set
            {
                if (_pointNote == value) return;
                _pointNote = value;
                NotifyPropertyChanged(PointNoteChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs PointNoteChangedEventArgs = ObservableHelper.CreateArgs<BathymetryExtractionViewModel>(x => x.PointNote);
        private string _pointNote;

        void UpdatePointNote()
        {
            var samplesPerDegree = 60 / SelectedResolution;
            PointNote = string.Format("Extraction area: {0:0.###}deg (lon) by {1:0.###}deg (lat)\nEstimated point count {2:#,#} x {3:#,#} = {4:#,#}", _boundingBox.Width, _boundingBox.Height, _boundingBox.Width * samplesPerDegree, _boundingBox.Height * samplesPerDegree, _boundingBox.Width * _boundingBox.Height * samplesPerDegree * samplesPerDegree);
        }

        #endregion

        #region public string BathymetryName { get; set; }

        public string BathymetryName
        {
            get { return _bathymetryName; }
            set
            {
                if (_bathymetryName == value) return;
                _bathymetryName = value;
                NotifyPropertyChanged(BathymetryNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BathymetryNameChangedEventArgs = ObservableHelper.CreateArgs<BathymetryExtractionViewModel>(x => x.BathymetryName);
        string _bathymetryName;

        #endregion

        #region public float SelectedResolution { get; set; }

        public float SelectedResolution
        {
            get { return _selectedResolution; }
            set
            {
                if (_selectedResolution == value) return;
                _selectedResolution = value;
                NotifyPropertyChanged(SelectedResolutionChangedEventArgs);
                UpdateNote();
                UpdatePointNote();
            }
        }

        static readonly PropertyChangedEventArgs SelectedResolutionChangedEventArgs = ObservableHelper.CreateArgs<BathymetryExtractionViewModel>(x => x.SelectedResolution);
        float _selectedResolution;

        #endregion

        #region public List<float> AvailableResolutions { get; set; }

        public List<float> AvailableResolutions
        {
            get { return _availableResolutions; }
        }

        readonly List<float> _availableResolutions = new List<float> { 0.05f, 0.10f, 0.50f, 1.0f, 2.0f };

        #endregion
        
        #region OkCommand
        public SimpleCommand<object, object> OkCommand
        {
            get { return _ok ?? (_ok = new SimpleCommand<object, object>(delegate { return true; }, delegate { OkHandler(); })); }
        }

        SimpleCommand<object, object> _ok;

        void OkHandler()
        {
            CloseActivePopUpCommand.Execute(true);
        }
        #endregion
    }
}
