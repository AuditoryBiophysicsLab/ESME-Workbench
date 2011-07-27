﻿#if true
using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Threading;
using Cinch;
using ESME;
using ESME.TransmissionLoss;
using ESMEWorkBench.Views;
using HRC.Services;
using MEFedMVVM.Common;
using MEFedMVVM.ViewModelLocator;
using ESME.Views.TransmissionLossViewer;
using AnalysisPointView = ESMEWorkBench.Views.AnalysisPointView;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    [ExportViewModel("AnalysisPointViewModel")]
    internal class AnalysisPointViewModel : ViewModelBase
    {
        readonly Dispatcher _dispatcher;
        readonly IHRCSaveFileService _saveFileService;
        readonly IViewAwareStatus _viewAwareStatus;

        bool _iAmInitialized;
        AnalysisPoint _tempAnalysisPoint;
        TreeView _treeView;

        [ImportingConstructor]
        public AnalysisPointViewModel(IViewAwareStatus viewAwareStatus, IHRCSaveFileService saveFileService)
        {
            RegisterMediator();
            _viewAwareStatus = viewAwareStatus;
            _saveFileService = saveFileService;
            _dispatcher = Dispatcher.CurrentDispatcher;
            _viewAwareStatus.ViewLoaded += () =>
                                           {
                                               if (Designer.IsInDesignMode) return;
                                               _treeView = ((AnalysisPointView) (_viewAwareStatus.View)).TreeView;
                                               MediatorMessage.Send(MediatorMessage.AnalysisPointViewInitialized, true);
                                           };
            TransmissionLossFieldListItems = new ObservableCollection<TransmissionLossFieldListItemViewModel>();
        }

        void InitializeTreeViewModel()
        {
            if (TransmissionLossFieldListItems.Count > 0) TransmissionLossFieldListItems.Clear();
            foreach (var field in _analysisPoint.TransmissionLossFields) TransmissionLossFieldListItems.Add(new TransmissionLossFieldListItemViewModel(field));
        }

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

        [MediatorMessageSink(MediatorMessage.ResetSelectedField)]
        void ResetSelectedField(bool dummy) { SelectedField = _selectedField; }


        [MediatorMessageSink(MediatorMessage.AnalysisPointChanged)]
        void AnalysisPointChanged(AnalysisPoint analysisPoint)
        {
            if (_iAmInitialized)
            {
                Debug.WriteLine("AnalysisPointViewModel: Initializing analysis point");
                AnalysisPoint = analysisPoint;
                InitializeTreeViewModel();
            }
            else
            {
                Debug.WriteLine("AnalysisPointViewModel: Deferring initialization of analysis point");
                _tempAnalysisPoint = analysisPoint;
            }
        }

        [MediatorMessageSink(MediatorMessage.AnalysisPointViewInitialized)]
        void AnalysisPointViewInitialized(bool dummy)
        {
            _iAmInitialized = true;
            if (_tempAnalysisPoint != null)
            {
                AnalysisPoint = _tempAnalysisPoint;
                Debug.WriteLine("AnalysisPointViewModel: Deferred initialization of analysis point completed");
            }
        }

        #region public AnalysisPoint AnalysisPoint { get; set; }

        static readonly PropertyChangedEventArgs AnalysisPointChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointViewModel>(x => x.AnalysisPoint);
        AnalysisPoint _analysisPoint;

        public AnalysisPoint AnalysisPoint
        {
            get { return _analysisPoint; }
            set
            {
                if (_analysisPoint == value) return;
                _analysisPoint = value;
                SelectedField = 0;
                NotifyPropertyChanged(AnalysisPointChangedEventArgs);
                InitializeTreeViewModel();
            }
        }

        #endregion

        #region public ObservableCollection<TransmissionLossFieldListItemViewModel> TransmissionLossFieldListItems { get; set; }

        static readonly PropertyChangedEventArgs TransmissionLossFieldListItemsChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointViewModel>(x => x.TransmissionLossFieldListItems);
        ObservableCollection<TransmissionLossFieldListItemViewModel> _transmissionLossFieldListItems;

        public ObservableCollection<TransmissionLossFieldListItemViewModel> TransmissionLossFieldListItems
        {
            get { return _transmissionLossFieldListItems; }
            set
            {
                if (_transmissionLossFieldListItems == value) return;
                if (_transmissionLossFieldListItems != null) _transmissionLossFieldListItems.CollectionChanged -= TransmissionLossFieldListItemsCollectionChanged;
                _transmissionLossFieldListItems = value;
                if (_transmissionLossFieldListItems != null) _transmissionLossFieldListItems.CollectionChanged += TransmissionLossFieldListItemsCollectionChanged;
                NotifyPropertyChanged(TransmissionLossFieldListItemsChangedEventArgs);
            }
        }

        void TransmissionLossFieldListItemsCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(TransmissionLossFieldListItemsChangedEventArgs); }

        #endregion

        #region TreeViewSelectionChangedCommand

        SimpleCommand<object, object> _treeViewSelectionChanged;

        public SimpleCommand<object, object> TreeViewSelectionChangedCommand
        {
            get
            {
                return _treeViewSelectionChanged ?? (_treeViewSelectionChanged = new SimpleCommand<object, object>(delegate(object cinchArgs)
                                                                                                                   {
                                                                                                                       var args = (RoutedPropertyChangedEventArgs<object>) ((EventToCommandArgs) cinchArgs).EventArgs;
                                                                                                                       if (TransmissionLossFieldListItems.Count == 0) return;
                                                                                                                       TransmissionLossFieldListItemViewModel selectedItem = ((TransmissionLossFieldListItemViewModel) args.NewValue) ?? TransmissionLossFieldListItems[0];
                                                                                                                       MediatorMessage.Send(MediatorMessage.TransmissionLossFieldChanged, selectedItem.TransmissionLossField);
                                                                                                                   }));
            }
        }

        #endregion

        #region public int SelectedField { get; set; }

        static readonly PropertyChangedEventArgs SelectedFieldChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointViewModel>(x => x.SelectedField);
        int _selectedField;

        public int SelectedField
        {
            get { return _selectedField; }
            set
            {
                if (value < 0) return;
                _selectedField = value;
                MediatorMessage.Send(MediatorMessage.TransmissionLossFieldChanged, AnalysisPoint.TransmissionLossFields[SelectedField]);
                NotifyPropertyChanged(SelectedFieldChangedEventArgs);
            }
        }

        #endregion
    }
}
#endif