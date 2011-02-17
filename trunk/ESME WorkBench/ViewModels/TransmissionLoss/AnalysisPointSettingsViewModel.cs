using System;
using System.ComponentModel;
using System.Windows;
using System.Diagnostics;
using System.Windows.Threading;
using Cinch;
using ESME.TransmissionLoss;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    class AnalysisPointSettingsViewModel : ViewModelBase, IViewStatusAwareInjectionAware
    {
        readonly IMessageBoxService _messageBoxService;
        IViewAwareStatus _viewAwareStatus;
        Dispatcher _dispatcher;

        public AnalysisPointSettingsViewModel(AnalysisPoint analysisPoint, IMessageBoxService messageBoxService)
        {
            RegisterMediator();
            _messageBoxService = messageBoxService;
            AnalysisPoint = analysisPoint;
        }

        #region public AnalysisPoint AnalysisPoint { get; set; }

        public AnalysisPoint AnalysisPoint
        {
            get { return _analysisPoint; }
            set
            {
                if (_analysisPoint == value) return;
                _analysisPoint = value;
                NotifyPropertyChanged(AnalysisPointChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs AnalysisPointChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointSettingsViewModel>(x => x.AnalysisPoint);
        AnalysisPoint _analysisPoint;

        #endregion


        #region IViewStatusAwareInjectionAware
        public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService)
        {
            _viewAwareStatus = viewAwareStatusService;
            _dispatcher = ((Window)_viewAwareStatus.View).Dispatcher;
        }
        #endregion

        #region Mediator Registration

        void RegisterMediator()
        {
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine("***********\nAnalysisPointSettingsViewModel: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }
        }

        #endregion
    }
}
