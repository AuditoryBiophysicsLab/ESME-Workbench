using System.Collections.Generic;
using System.ComponentModel;
using System.Windows;
using Cinch;
using HRC.Navigation;

namespace ESME.Views.EnvironmentBuilder
{
    public class BathymetryExtractionViewModel : ViewModelBase
    {
        readonly string _selectedOverlay;
        public BathymetryExtractionViewModel(string selectedOverlay, GeoRect boundingBox)
        {
            _selectedOverlay = selectedOverlay;
            _boundingBox = boundingBox;
            SelectedResolution = 2.0f;
            UpdateNote();
            UpdatePointNote();
        }

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
            BathymetryName = string.Format("{0}_{1:0.00}min", _selectedOverlay, SelectedResolution);
            Note = string.Format("Note: Bathymetry data will be extracted within the bounds of the overlay {0}. The resulting bathymetry file will be named {1}", _selectedOverlay, BathymetryName);
        }

        #endregion

        private readonly GeoRect _boundingBox;

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
            BathymetryName = string.Format("{0}_{1:0.00}min", _selectedOverlay, SelectedResolution);
            Note = string.Format("Note: Bathymetry data will be extracted within the bounds of the overlay {0}. The resulting bathymetry file will be named {1}", _selectedOverlay, BathymetryName);
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
