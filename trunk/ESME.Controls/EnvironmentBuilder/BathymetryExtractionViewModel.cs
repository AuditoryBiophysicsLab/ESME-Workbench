using System.Collections.Generic;
using System.ComponentModel;
using Cinch;

namespace ESME.Views.EnvironmentBuilder
{
    public class BathymetryExtractionViewModel : ViewModelBase
    {
        readonly string _selectedOverlay;
        public BathymetryExtractionViewModel(string selectedOverlay)
        {
            _selectedOverlay = selectedOverlay;
            SelectedResolution = 2.0f;
            UpdateNote();
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
            Note = string.Format("Note: Bathymetry data will be extracted within the bounds of the overlay {0}. The resulting bathymetry file will be named {0}_{1}min", _selectedOverlay, SelectedResolution);
        }

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
