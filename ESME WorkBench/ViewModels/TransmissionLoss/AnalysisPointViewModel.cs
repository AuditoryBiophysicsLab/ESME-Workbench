using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Windows.Threading;
using Cinch;
using ESME.TransmissionLoss;
using HRC.Services;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    [ExportViewModel("AnalysisPointViewModel")]
    class AnalysisPointViewModel : ViewModelBase
    {
        readonly Dispatcher _dispatcher;
        readonly IViewAwareStatus _viewAwareStatus;
        readonly IHRCSaveFileService _saveFileService;

        bool _analysisPointInitialized,
             _fieldInitialized,
             _radialInitialized;

        [ImportingConstructor]
        public AnalysisPointViewModel(IViewAwareStatus viewAwareStatus, IHRCSaveFileService saveFileService)
        {
            RegisterMediator();
            _viewAwareStatus = viewAwareStatus;
            _saveFileService = saveFileService;
            _dispatcher = Dispatcher.CurrentDispatcher;

            _viewAwareStatus.ViewLoaded += () => MediatorMessage.Send(MediatorMessage.AnalysisPointViewInitialized, true);
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
                SelectedField = 0;
            }
        }

        static readonly PropertyChangedEventArgs AnalysisPointChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointViewModel>(x => x.AnalysisPoint);
        AnalysisPoint _analysisPoint;

        #endregion

        #region public int SelectedField { get; set; }

        public int SelectedField
        {
            get { return _selectedField; }
            set
            {
                if (_selectedField == value) return;
                _selectedField = value;
                NotifyPropertyChanged(SelectedFieldChangedEventArgs);
                MediatorMessage.Send(MediatorMessage.TransmissionLossFieldChanged, AnalysisPoint.TransmissionLossFields[SelectedField]);
            }
        }

        static readonly PropertyChangedEventArgs SelectedFieldChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointViewModel>(x => x.SelectedField);
        int _selectedField;

        #endregion

        void RegisterMediator()
        {
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine("***********\nAnalysisPointViewModel: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }
        }

        [MediatorMessageSink(MediatorMessage.AnalysisPointChanged)]
        void AnalysisPointChanged(AnalysisPoint analysisPoint)
        {
            AnalysisPoint = analysisPoint;
        }

        [MediatorMessageSink(MediatorMessage.AnalysisPointViewInitialized)]
        void AnalysisPointViewInitialized(bool dummy)
        {
            _analysisPointInitialized = true;
            InitializeIfViewModelsReady();
        }

        [MediatorMessageSink(MediatorMessage.TransmissionLossFieldViewInitialized)]
        void TransmissionLossFieldViewInitialized(bool dummy)
        {
            _fieldInitialized = true;
            InitializeIfViewModelsReady();
        }

        [MediatorMessageSink(MediatorMessage.TransmissionLossRadialViewInitialized)]
        void TransmissionLossRadialViewInitialized(bool dummy)
        {
            _radialInitialized = true;
            InitializeIfViewModelsReady();
        }

        void InitializeIfViewModelsReady()
        {
            if (_analysisPointInitialized && _fieldInitialized && _radialInitialized)
            {
                MediatorMessage.Send(MediatorMessage.TransmissionLossFieldChanged, AnalysisPoint.TransmissionLossFields[SelectedField]);
            }
        }
    }
}
