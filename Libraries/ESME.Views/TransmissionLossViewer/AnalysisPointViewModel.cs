using System;
using System.ComponentModel;
using System.Linq;
using System.Reactive.Linq;
using ESME.Scenarios;
using HRC.Aspects;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Views.TransmissionLossViewer
{
    public class AnalysisPointViewModel : ViewModelBase
    {
        public AnalysisPointViewModel() { }

        public AnalysisPoint AnalysisPoint { get; private set; }
        [Initialize] public TransmissionLossViewModel TransmissionLossViewModel { get; set; }
        public ESME.Scenarios.TransmissionLoss TransmissionLoss { get; set; }

        public AnalysisPointViewModel(AnalysisPoint analysisPoint)
        {
            AnalysisPoint = analysisPoint;
            Observable.FromEventPattern<PropertyChangedEventArgs>(AnalysisPoint, "PropertyChanged")
                .Where(e => e.EventArgs.PropertyName == "IsDeleted")
                .Select(e => AnalysisPoint.IsDeleted)
                .DistinctUntilChanged()
                .ObserveOnDispatcher()
                .Subscribe(isDeleted => { if (isDeleted) CloseDialog(null); });

            Observable.FromEventPattern<PropertyChangedEventArgs>(this, "PropertyChanged")
                .Where(e => e.EventArgs.PropertyName == "TransmissionLoss")
                .Select(e => TransmissionLoss)
                .DistinctUntilChanged()
                .ObserveOnDispatcher()
                .Subscribe(transmissionLoss =>
                {
                    if (_transmissionLossObserver != null) _transmissionLossObserver.Dispose();
                    _transmissionLossObserver = null;
                    if (transmissionLoss == null) return;
                    TransmissionLossViewModel.TransmissionLoss = transmissionLoss;
                    _oldIndex = AnalysisPoint.TransmissionLosses.IndexOf(transmissionLoss);
                    _transmissionLossObserver = Observable.FromEventPattern<PropertyChangedEventArgs>(transmissionLoss, "PropertyChanged")
                        .ObserveOnDispatcher()
                        .Subscribe(e =>
                        {
                            if (e.EventArgs.PropertyName == "IsDeleted" && AnalysisPoint.TransmissionLosses.Count > 1) 
                                TransmissionLossViewModel.TransmissionLoss = AnalysisPoint.TransmissionLosses[_oldIndex % (AnalysisPoint.TransmissionLosses.Count - 1)];
                        });
                    TransmissionLossViewModel.SelectedRadialIndex = 0;
                });
            TransmissionLoss = analysisPoint.TransmissionLosses.FirstOrDefault();
        }

        IDisposable _transmissionLossObserver;

        #region CloseCommand
        public SimpleCommand<object, EventToCommandArgs> CloseCommand { get { return _close ?? (_close = new SimpleCommand<object, EventToCommandArgs>(o => CloseDialog(null))); } }
        SimpleCommand<object, EventToCommandArgs> _close;
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