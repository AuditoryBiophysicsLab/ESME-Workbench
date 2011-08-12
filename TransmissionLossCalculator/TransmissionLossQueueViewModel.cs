using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Windows;
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
        IMessageBoxService _messageBoxService;

        [ImportingConstructor]
        public TransmissionLossQueueViewModel(IMessageBoxService messageBoxService)
        {
            _messageBoxService = messageBoxService;
            QueueViewModel = new TransmissionLossQueueCalculatorViewModel {IsPaused = !IsAutoStart};
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
                            if (QueueViewModel.FieldCalculatorViewModels.Any(curJob => curJob.TransmissionLossRunFile.Filename == runFileName))
                                doCalculation = false;
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

        #region AutoStartCommand
        public SimpleCommand<object, object> AutoStartCommand
        {
            get
            {
                return _autoStart ??
                       (_autoStart =
                        new SimpleCommand<object, object>(delegate { AutoStartHandler(); }));
            }
        }

        SimpleCommand<object, object> _autoStart;

        void AutoStartHandler() { IsAutoStart = !IsAutoStart; }
        #endregion

        #region public bool IsAutoStart { get; set; }

        public bool IsAutoStart
        {
            get { return Properties.Settings.Default.AutoStart; }
            set
            {
                if (Properties.Settings.Default.AutoStart == value) return;
                Properties.Settings.Default.AutoStart = value;
                NotifyPropertyChanged(IsAutoStartChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsAutoStartChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossQueueViewModel>(x => x.IsAutoStart);

        #endregion

        #region RunCommand
        public SimpleCommand<object, object> RunCommand
        {
            get
            {
                return _run ??
                       (_run =
                        new SimpleCommand<object, object>(delegate { return IsRunCommandEnabled; },
                                                          delegate { RunHandler(); }));
            }
        }

        SimpleCommand<object, object> _run;

        bool IsRunCommandEnabled
        {
            get { return QueueViewModel.IsPaused; }
        }

        void RunHandler() { QueueViewModel.IsPaused = false; }
        #endregion

        #region PauseCommand
        public SimpleCommand<object, object> PauseCommand
        {
            get
            {
                return _pause ??
                       (_pause =
                        new SimpleCommand<object, object>(delegate { return IsPauseCommandEnabled; },
                                                          delegate { PauseHandler(); }));
            }
        }

        SimpleCommand<object, object> _pause;

        bool IsPauseCommandEnabled
        {
            get { return !QueueViewModel.IsPaused; }
        }

        void PauseHandler() { QueueViewModel.IsPaused = true; }
        #endregion

        #region StopCommand
        public SimpleCommand<object, object> StopCommand
        {
            get
            {
                return _stop ??
                       (_stop =
                        new SimpleCommand<object, object>(delegate { return IsStopCommandEnabled; },
                                                          delegate { StopHandler(); }));
            }
        }

        SimpleCommand<object, object> _stop;

        bool IsStopCommandEnabled
        {
            get { return !QueueViewModel.IsPaused; }
        }

        void StopHandler() 
        {
            var result = _messageBoxService.ShowYesNo("Really stop transmission loss calculations immediately?\r\nWarning: This will cancel and discard the currently active calculation.", CustomDialogIcons.Warning);
            if (result != CustomDialogResults.Yes) return;
            QueueViewModel.IsPaused = true;
            QueueViewModel.CancelActiveCalculation();
        }
        #endregion

        #region RescanCommand
        public SimpleCommand<object, object> RescanCommand
        {
            get
            {
                return _rescan ??
                       (_rescan =
                        new SimpleCommand<object, object>(delegate { RescanHandler(); }));
            }
        }

        SimpleCommand<object, object> _rescan;

        void RescanHandler()
        {
            WorkItems.Clear();
            DirectoryScanners.Rescan();
        }
        #endregion

        #region QuitCommand
        public SimpleCommand<object, object> QuitCommand
        {
            get
            {
                return _close ??
                       (_close =
                        new SimpleCommand<object, object>(delegate { QuitHandler(); }));
            }
        }

        SimpleCommand<object, object> _close;

        void QuitHandler()
        {
            var result = _messageBoxService.ShowYesNo("Really exit Transmission Loss Calculator?", CustomDialogIcons.Question);
            if (result != CustomDialogResults.Yes) return;
            QueueViewModel.IsPaused = true;
            QueueViewModel.CancelActiveCalculation();
            Application.Current.Shutdown();
        }
        #endregion

        #region FoldersCommand
        public SimpleCommand<object, object> FoldersCommand
        {
            get
            {
                return _folders ??
                       (_folders =
                        new SimpleCommand<object, object>(delegate { FoldersHandler(); }));
            }
        }

        SimpleCommand<object, object> _folders;

        void FoldersHandler()
        {
            _messageBoxService.ShowInformation("Not yet implemented");
        }
        #endregion

        #region HelpCommand
        public SimpleCommand<object, object> HelpCommand
        {
            get
            {
                return _help ??
                       (_help =
                        new SimpleCommand<object, object>(delegate { HelpHandler(); }));
            }
        }

        SimpleCommand<object, object> _help;

        void HelpHandler()
        {
            _messageBoxService.ShowInformation("Not yet implemented");
        }
        #endregion
    }
}
