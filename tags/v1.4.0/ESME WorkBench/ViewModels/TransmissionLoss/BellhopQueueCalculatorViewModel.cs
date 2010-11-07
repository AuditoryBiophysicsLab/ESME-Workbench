using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Windows;
using System.Windows.Threading;
using Cinch;
using ESME.Model;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    internal class BellhopQueueCalculatorViewModel : ViewModelBase, IViewStatusAwareInjectionAware
    {
        #region public constructor

        readonly string _outputDirectory;
        readonly IMessageBoxService _messageBoxService;
        public BellhopQueueCalculatorViewModel(string outputDirectory, IMessageBoxService messageBoxService)
        {
            _messageBoxService = messageBoxService;
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine("***********\nBellhopQueueCalculatorViewModel: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }
            _outputDirectory = outputDirectory;
            BellhopFieldCalculatorViewModels = new ObservableCollection<BellhopFieldCalculatorViewModel>();
        }

        #endregion

        #region public ObservableCollection<BellhopFieldCalculatorViewModel> BellhopFieldCalculatorViewModels { get; set; }

        public ObservableCollection<BellhopFieldCalculatorViewModel> BellhopFieldCalculatorViewModels
        {
            get { return _bellhopFieldCalculatorViewModels; }
            set
            {
                if (_bellhopFieldCalculatorViewModels == value) return;
                if (_bellhopFieldCalculatorViewModels != null) _bellhopFieldCalculatorViewModels.CollectionChanged -= BellhopFieldCalculatorViewModelsCollectionChanged;
                _bellhopFieldCalculatorViewModels = value;
                if (_bellhopFieldCalculatorViewModels != null) _bellhopFieldCalculatorViewModels.CollectionChanged += BellhopFieldCalculatorViewModelsCollectionChanged;
                NotifyPropertyChanged(BellhopFieldCalculatorViewModelsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BellhopFieldCalculatorViewModelsChangedEventArgs = ObservableHelper.CreateArgs<BellhopQueueCalculatorViewModel>(x => x.BellhopFieldCalculatorViewModels);
        ObservableCollection<BellhopFieldCalculatorViewModel> _bellhopFieldCalculatorViewModels;

        void BellhopFieldCalculatorViewModelsCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            switch (e.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    if (e.NewItems != null)
                        foreach (var newItem in e.NewItems.Cast<BellhopFieldCalculatorViewModel>()) {}
                    break;
                case NotifyCollectionChangedAction.Move:
                    break;
                case NotifyCollectionChangedAction.Remove:
                    if (e.OldItems != null) foreach (var oldItem in e.OldItems.Cast<BellhopFieldCalculatorViewModel>()) {}
                    break;
                case NotifyCollectionChangedAction.Replace:
                    break;
                case NotifyCollectionChangedAction.Reset:
                    break;
            }
            if (BellhopFieldCalculatorViewModels.Count < 1) _window.Visibility = Visibility.Collapsed;
            else _window.Show();
            StartWorkIfNeeded();
            NotifyPropertyChanged(BellhopFieldCalculatorViewModelsChangedEventArgs);
        }

        void StartWorkIfNeeded()
        {
            if (BellhopFieldCalculatorViewModels.Count > 0)
                BellhopFieldCalculatorViewModels[0].Start(delegate { HandleCompletedQueueItem(); });
        }

        void HandleCompletedQueueItem()
        {
            while ((BellhopFieldCalculatorViewModels.Count > 0) && (BellhopFieldCalculatorViewModels[0].IsCompleted))
            {
                var transmissionLossField = BellhopFieldCalculatorViewModels[0].TransmissionLossField;
                var fileName = Path.Combine(_outputDirectory, Path.GetFileNameWithoutExtension(Path.GetRandomFileName()) + ".tlf");
                transmissionLossField.Filename = fileName;
                transmissionLossField.Save();
                BellhopFieldCalculatorViewModels.Remove(BellhopFieldCalculatorViewModels[0]);
            }
            StartWorkIfNeeded();
        }

        #endregion

        #region ViewClosingCommand

        public SimpleCommand<object, EventToCommandArgs> ViewClosingCommand
        {
            get
            {
                return _viewClosing ?? (_viewClosing = new SimpleCommand<object, EventToCommandArgs>(vcArgs =>
                {
                    if (BellhopFieldCalculatorViewModels.Count == 0) return;

                    var ea = (CancelEventArgs)vcArgs.EventArgs;
                    ea.Cancel = true;
                    ((Window) _viewAwareStatus.View).WindowState = WindowState.Minimized;
                }));
            }
        }

        SimpleCommand<object, EventToCommandArgs> _viewClosing;

        #endregion

        [MediatorMessageSink(MediatorMessage.QueueBellhopJob)]
        void QueueBellhopJob(BellhopRunFile bellhopRunFile)
        {
            BellhopFieldCalculatorViewModels.Add(new BellhopFieldCalculatorViewModel(_dispatcher)
            {
                BellhopRunFile = bellhopRunFile,
            });
        }

        [MediatorMessageSink(MediatorMessage.ApplicationClosing)]
        void ApplicationClosing(bool dummy)
        {
            CloseActivePopUpCommand.Execute(true);
        }

        #region IViewStatusAwareInjectionAware Members

        IViewAwareStatus _viewAwareStatus;
        Dispatcher _dispatcher;
        Window _window;

        public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService)
        {
            _viewAwareStatus = viewAwareStatusService;
            _window = (Window) _viewAwareStatus.View;
            _dispatcher = _window.Dispatcher;
            //_window.Visibility = Visibility.Collapsed;
        }

        #endregion
    }
}