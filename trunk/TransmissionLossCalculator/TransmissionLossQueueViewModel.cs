using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Threading;
using Cinch;
using ESME.Mapping;
using ESME.TransmissionLoss;
using ESME.Views.TransmissionLoss;
using MEFedMVVM.Common;
using MEFedMVVM.ViewModelLocator;

namespace TransmissionLossCalculator
{
    [ExportViewModel("TransmissionLossQueueViewModel")]
    public class TransmissionLossQueueViewModel : ViewModelBase
    {
        [ImportingConstructor]
        public TransmissionLossQueueViewModel(IViewAwareStatus viewAwareStatus)
        {
            WorkItems = new ObservableCollection<string>();
            DirectoryScanners = new WorkDirectoryScanners(WorkItems);
            QueueViewModel = new TransmissionLossQueueCalculatorViewModel();
            _viewAwareStatus = viewAwareStatus;
            _viewAwareStatus.ViewLoaded += () =>
            {
                if (Designer.IsInDesignMode) return;
                _dispatcher = ((Window)_viewAwareStatus.View).Dispatcher;
            };
        }

        WorkDirectoryScanners DirectoryScanners { get; set; }
        Dispatcher _dispatcher;
        readonly IViewAwareStatus _viewAwareStatus;


        #region public ObservableCollection<string> WorkItems { get; set; }

        public ObservableCollection<string> WorkItems
        {
            get { return _workItems ?? (_workItems = new ObservableCollection<string>()); }
            set
            {
                if (_workItems == value) return;
                if (_workItems != null) _workItems.CollectionChanged -= WorkItemsCollectionChanged;
                _workItems = value;
                if (_workItems != null) _workItems.CollectionChanged += WorkItemsCollectionChanged;
                NotifyPropertyChanged(WorkItemsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs WorkItemsChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossQueueViewModel>(x => x.WorkItems);
        ObservableCollection<string> _workItems;
        void WorkItemsCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            switch (e.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    if (e.NewItems != null)
                        foreach (var item in e.NewItems)
                        {
                            var newFile = (string)item;
                            QueueViewModel.FieldCalculatorViewModels.Add(new TransmissionLossFieldCalculatorViewModel(newFile, _dispatcher));
                        }
                    break;
                case NotifyCollectionChangedAction.Move:
                    Debug.WriteLine("TransmissionLossQueueViewModel: LayerCollection.Move");
                    break;
                case NotifyCollectionChangedAction.Remove:
                    if (e.OldItems != null) 
                        foreach (var item in e.OldItems)
                        { }
                    break;
                case NotifyCollectionChangedAction.Replace:
                    Debug.WriteLine("TransmissionLossQueueViewModel: LayerCollection.Replace");
                    break;
                case NotifyCollectionChangedAction.Reset:
                    Debug.WriteLine("TransmissionLossQueueViewModel: LayerCollection.Reset");
                    break;
            }
        }

        #endregion

        #region public TransmissionLossQueueCalculatorViewModel QueueViewModel { get; set; }

        public TransmissionLossQueueCalculatorViewModel QueueViewModel
        {
            get { return _queueViewModel; }
            set
            {
                if (_queueViewModel == value) return;
                _queueViewModel = value;
                NotifyPropertyChanged(QueueViewModelChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs QueueViewModelChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossQueueViewModel>(x => x.QueueViewModel);
        TransmissionLossQueueCalculatorViewModel _queueViewModel;

        #endregion

    }
}
