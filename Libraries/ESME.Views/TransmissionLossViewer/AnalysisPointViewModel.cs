using System;
using System.ComponentModel;
using System.Linq;
using System.Reactive.Linq;
using ESME.Scenarios;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Views.TransmissionLossViewer
{
    public class AnalysisPointViewModel : ViewModelBase
    {
        public AnalysisPointViewModel()
        {
        }

        public AnalysisPoint AnalysisPoint { get; private set; }
        public TransmissionLossViewModel TransmissionLossViewModel { get; set; }

        public AnalysisPointViewModel(AnalysisPoint analysisPoint)
        {
            AnalysisPoint = analysisPoint;
            ((INotifyPropertyChanged)AnalysisPoint).PropertyChanged += (s, e) =>
            {
                if (e.PropertyName == "IsDeleted")
                {
                    CloseDialog(null);
                }
            };
            TransmissionLossViewModel = new TransmissionLossViewModel();
            Observable.FromEventPattern<PropertyChangedEventArgs>(TransmissionLossViewModel, "PropertyChanged")
                .Where(e => e.EventArgs.PropertyName == "TransmissionLoss")
                .Select(e => TransmissionLossViewModel.TransmissionLoss)
                .DistinctUntilChanged()
                .ObserveOnDispatcher()
                .Subscribe(transmissionLoss =>
                {
                    
                });
            TransmissionLossViewModel.TransmissionLoss = analysisPoint.TransmissionLosses.FirstOrDefault();
            _oldTL = TransmissionLossViewModel.TransmissionLoss;
            if (_oldTL != null)
            {
                _oldIndex = AnalysisPoint.TransmissionLosses.IndexOf(_oldTL);
                ((INotifyPropertyChanged)_oldTL).PropertyChanged += TransmissionLossChanged;
            }
            TransmissionLossViewModel.PropertyChanged += (s, e) =>
            {
                if (e.PropertyName == "TransmissionLoss")
                {
                    if (_oldTL != null) ((INotifyPropertyChanged)_oldTL).PropertyChanged -= TransmissionLossChanged;
                    if (TransmissionLossViewModel.TransmissionLoss != null)
                    {
                        ((INotifyPropertyChanged)TransmissionLossViewModel.TransmissionLoss).PropertyChanged += TransmissionLossChanged;
                        _oldIndex = AnalysisPoint.TransmissionLosses.IndexOf(_oldTL);
                    }
                    _oldTL = TransmissionLossViewModel.TransmissionLoss;
                    
                }
            };
        }
        void TransmissionLossChanged(object sender, PropertyChangedEventArgs e)
        {
            if (e.PropertyName == "IsDeleted" && AnalysisPoint.TransmissionLosses.Count > 1)
            {
                TransmissionLossViewModel.TransmissionLoss = AnalysisPoint.TransmissionLosses[_oldIndex % (AnalysisPoint.TransmissionLosses.Count - 1)];
            }
            TransmissionLossViewModel.SelectedRadialIndex = 0;
        }
        #region CloseCommand
        public SimpleCommand<object, EventToCommandArgs> CloseCommand { get { return _close ?? (_close = new SimpleCommand<object, EventToCommandArgs>(o => CloseDialog(null))); } }
        SimpleCommand<object, EventToCommandArgs> _close;
        ESME.Scenarios.TransmissionLoss _oldTL;
        int _oldIndex;
        #endregion

        public string TransmissionLossListToolTip
        {
            get
            {
                return "HiFreq = High Frequency in Hertz" + System.Environment.NewLine +
                       "LowFreq = Low Frequency in Hertz" + System.Environment.NewLine +
                       "Depth = Depth below the surface, in meters" + System.Environment.NewLine +
                       "VBW = Vertical beam width of the mode, in degrees" + System.Environment.NewLine +
                       "D/E = Depression/Elevation angle. Vertical look direction of this mode, in degrees" + System.Environment.NewLine +
                       "         Zero is horizontal, positive values are towards the bottom" + System.Environment.NewLine;
            }
        }
    }
}