using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Windows.Threading;
using Cinch;
using ESME.Data;    
using ESME.Views.TransmissionLoss;
using MEFedMVVM.Common;
using MEFedMVVM.ViewModelLocator;

namespace TransmissionLossCalculator
{
    [ExportViewModel("TransmissionLossQueueViewModel")]
    public class TransmissionLossQueueViewModel : ViewModelBase
    {
        [ImportingConstructor]
        public TransmissionLossQueueViewModel()
        {
            QueueViewModel = new TransmissionLossQueueCalculatorViewModel();
            WorkItems = new ObservableCollection<string>();
            DirectoryScanners = new WorkDirectoryScanners(WorkItems);
            _dispatcher = Dispatcher.CurrentDispatcher;
            if (Designer.IsInDesignMode) return;
            WorkDirectories = WorkDirectories.Load(true);
            WorkDirectories.ReloadOnFileChange = true;
            WorkDirectories.CollectionChanged += (s, e) =>
            {
                switch (e.Action)
                {
                    case NotifyCollectionChangedAction.Add:
                        foreach (var item in e.NewItems)
                            AddWorkDirectory((string)item);
                        break;
                    case NotifyCollectionChangedAction.Remove:
                        foreach (var item in e.OldItems)
                            RemoveWorkDirectory((string)item);
                        break;
                    case NotifyCollectionChangedAction.Replace:
                        foreach (var item in e.OldItems)
                            RemoveWorkDirectory((string)item);
                        foreach (var item in e.NewItems)
                            AddWorkDirectory((string)item);
                        break;
                    case NotifyCollectionChangedAction.Reset:
                        ClearWorkDirectories();
                        AddWorkDirectories(WorkDirectories);
                        break;
                }
            };
            AddWorkDirectories(WorkDirectories);
        }

        WorkDirectories WorkDirectories { get; set; }
        WorkDirectoryScanners DirectoryScanners { get; set; }
        readonly Dispatcher _dispatcher;

        void AddWorkDirectories(IEnumerable<string> workDirectories) { foreach (var directory in workDirectories) AddWorkDirectory(directory); }
        void AddWorkDirectory(string workDirectory)
        {
            DirectoryScanners.Add(workDirectory, "*.bellhop");
            //DirectoryScanners.Add(workDirectory, "*.ramgeo");
        }
        
        void ClearWorkDirectories() { RemoveWorkDirectories(WorkDirectories); }
        void RemoveWorkDirectories(IEnumerable<string> workDirectories) { foreach (var directory in workDirectories) RemoveWorkDirectory(directory); }
        void RemoveWorkDirectory(string workDirectory)
        {
            DirectoryScanners.Remove(workDirectory);
        }

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
                            var runFileName = (string)item;
                            //var runFile = TransmissionLossRunFile.Load(newFileName);
                            var binFileName = Path.Combine(Path.GetDirectoryName(runFileName), Path.GetFileNameWithoutExtension(runFileName) + ".bin");
                            var doCalculation = true;
                            if (File.Exists(binFileName))
                            {
                                var runInfo = new FileInfo(runFileName);
                                var binInfo = new FileInfo(binFileName);
                                if (runInfo.LastWriteTime < binInfo.LastWriteTime)
                                {
                                    Debug.WriteLine("Skipping " + Path.GetFileName(runFileName));
                                    doCalculation = false;
                                }
                            }
                            if (!doCalculation) continue;
                            if (_dispatcher != null) _dispatcher.InvokeIfRequired(() => QueueViewModel.FieldCalculatorViewModels.Add(new TransmissionLossFieldCalculatorViewModel(runFileName, _dispatcher)));
                            else QueueViewModel.FieldCalculatorViewModels.Add(new TransmissionLossFieldCalculatorViewModel(runFileName, _dispatcher));
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

        #region ViewClosingCommand
        public SimpleCommand<object, object> ViewClosingCommand
        {
            get
            {
                return _viewClosing ??
                       (_viewClosing =
                        new SimpleCommand<object, object>(delegate { ViewClosingHandler(); }));
            }
        }

        SimpleCommand<object, object> _viewClosing;

        void ViewClosingHandler() { QueueViewModel.CancelRequested = true; }
        #endregion
    }
}
