using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Threading;
using Cinch;
using ESME.Model;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    internal class BellhopQueueCalculatorViewModel : ViewModelBase, IViewStatusAwareInjectionAware
    {
        #region public constructor

        public BellhopQueueCalculatorViewModel()
        {
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine("***********\nBellhopQueueCalculatorViewModel: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }

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
                        foreach (var newItem in e.NewItems.Cast<BellhopFieldCalculatorViewModel>())
                        {
                            _window.Visibility = Visibility.Visible;
                        }
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
            NotifyPropertyChanged(BellhopFieldCalculatorViewModelsChangedEventArgs);
        }

        #endregion

        [MediatorMessageSink(MediatorMessage.QueueBellhopJob)]
        void QueueBellhopJob(BellhopRunFile bellhopRunFile)
        {
            BellhopFieldCalculatorViewModels.Add(new BellhopFieldCalculatorViewModel
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
            _window.Visibility = Visibility.Hidden;
        }

        #endregion
    }
}