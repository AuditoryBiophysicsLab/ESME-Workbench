using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Threading;
using Cinch;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.Bellhop;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    public class TransmissionLossFieldCalculatorViewModel : ViewModelBase, IViewStatusAwareInjectionAware
    {
        IViewAwareStatus _viewAwareStatus;
        Dispatcher _dispatcher;

        #region public constructor

        public TransmissionLossFieldCalculatorViewModel() { }

        public TransmissionLossFieldCalculatorViewModel(Dispatcher dispatcher)
        {
            _dispatcher = dispatcher;
        }

        #endregion

        #region public TransmissionLossField TransmissionLossField { get; set; }

        public TransmissionLossField TransmissionLossField
        {
            get { return _transmissionLossField; }
            set
            {
                if (_transmissionLossField == value) return;
                _transmissionLossField = value;
                NotifyPropertyChanged(TransmissionLossFieldChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TransmissionLossFieldChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldCalculatorViewModel>(x => x.TransmissionLossField);
        TransmissionLossField _transmissionLossField;

        #endregion

        #region public float TotalProgress { get; set; }

        public float TotalProgress
        {
            get { return _totalProgress; }
            set
            {
                if (_totalProgress == value) return;
                _totalProgress = value;
                NotifyPropertyChanged(TotalProgressChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TotalProgressChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldCalculatorViewModel>(x => x.TotalProgress);
        float _totalProgress;

        #endregion

        #region public TransmissionLossRunFile TransmissionLossRunFile { get; set; }

        public TransmissionLossRunFile TransmissionLossRunFile
        {
            get { return _bellhopRunFile; }
            set
            {
                if (_bellhopRunFile == value) return;
                _bellhopRunFile = value;
                Name = _bellhopRunFile.Name;
                NotifyPropertyChanged(TransmissionLossRunFileChangedEventArgs);
                if (_dispatcher != null) SetupRadialViewModels();
            }
        }

        static readonly PropertyChangedEventArgs TransmissionLossRunFileChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldCalculatorViewModel>(x => x.TransmissionLossRunFile);
        TransmissionLossRunFile _bellhopRunFile;

        void SetupRadialViewModels()
        {
            if ((_dispatcher == null) || (TransmissionLossRunFile == null)) return;

            TransmissionLossField = new TransmissionLossField(TransmissionLossRunFile);
            for (var i = 0; i < TransmissionLossRunFile.TransmissionLossRunFileRadials.Count; i++)
            {
                var radialViewModel = new BellhopRadialCalculatorViewModel((BellhopRunFileRadial)_bellhopRunFile.TransmissionLossRunFileRadials[i], i, _dispatcher);
                radialViewModel.PropertyChanged += (s, e) =>
                                                   {
                                                       if (e.PropertyName != "ProgressPercent") return;
                                                       float radialCount = BellhopRadialCalculatorViewModels.Count;
                                                       var progress = BellhopRadialCalculatorViewModels.Sum(radial => radial.ProgressPercent/radialCount);
                                                       TotalProgress = progress;
                                                   };
                BellhopRadialCalculatorViewModels.Add(radialViewModel);
            }
        }

        public void Start(RunWorkerCompletedEventHandler runWorkerCompletedEventHandler)
        {
            lock (this)
            {
                if (IsStarted) return;
                IsStarted = true;
                var bw = new BackgroundWorker();
                bw.DoWork += Calculate;
                bw.RunWorkerCompleted += delegate { IsCompleted = true; };
                if (runWorkerCompletedEventHandler != null) bw.RunWorkerCompleted += runWorkerCompletedEventHandler;
                bw.RunWorkerAsync(_bellhopRunFile);
            }
        }
        #endregion

        #region public bool IsStarted { get; set; }

        public bool IsStarted
        {
            get { return _isStarted; }
            set
            {
                if (_isStarted == value) return;
                _isStarted = value;
                NotifyPropertyChanged(IsStartedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsStartedChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldCalculatorViewModel>(x => x.IsStarted);
        bool _isStarted;

        #endregion

        #region public bool IsCompleted { get; set; }

        public bool IsCompleted
        {
            get { return _isCompleted; }
            set
            {
                if (_isCompleted == value) return;
                _isCompleted = value;
                NotifyPropertyChanged(IsCompletedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsCompletedChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldCalculatorViewModel>(x => x.IsCompleted);
        bool _isCompleted;

        #endregion

        #region public string Name { get; set; }

        public string Name
        {
            get { return _name; }
            set
            {
                if (_name == value) return;
                _name = value;
                NotifyPropertyChanged(NameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs NameChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldCalculatorViewModel>(x => x.Name);
        string _name;

        #endregion

        #region IViewStatusAwareInjectionAware Members

        public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService)
        {
            _viewAwareStatus = viewAwareStatusService;
            _dispatcher = ((Window) _viewAwareStatus.View).Dispatcher;
            if (TransmissionLossRunFile != null) SetupRadialViewModels();
        }

        #endregion

        #region PropertyChangedCallback PropertyChangedDelegate { get; }

        delegate void PropertyChangedCallback(PropertyChangedEventArgs eventArgs);

        PropertyChangedCallback PropertyChangedDelegate
        {
            get { return _propertyChangedDelegate ?? (_propertyChangedDelegate = new PropertyChangedCallback(NotifyPropertyChanged)); }
        }

        static readonly PropertyChangedEventArgs PropertyChangedDelegateChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldCalculatorViewModel>(x => x.PropertyChangedDelegate);
        PropertyChangedCallback _propertyChangedDelegate;

        #endregion

        #region public ObservableCollection<BellhopRadialCalculatorViewModel> BellhopRadialCalculatorViewModels { get; set; }

        public ObservableCollection<BellhopRadialCalculatorViewModel> BellhopRadialCalculatorViewModels
        {
            get { return _bellhopRadialCalculatorViewModels; }
            set
            {
                if (_bellhopRadialCalculatorViewModels == value) return;
                if (_bellhopRadialCalculatorViewModels != null) _bellhopRadialCalculatorViewModels.CollectionChanged -= BellhopRadialCalculatorViewModelsCollectionChanged;
                _bellhopRadialCalculatorViewModels = value;
                if (_bellhopRadialCalculatorViewModels != null) _bellhopRadialCalculatorViewModels.CollectionChanged += BellhopRadialCalculatorViewModelsCollectionChanged;
                _dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(BellhopRadialCalculatorViewModelsChangedEventArgs));
            }
        }

        void BellhopRadialCalculatorViewModelsCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(BellhopRadialCalculatorViewModelsChangedEventArgs); }
        static readonly PropertyChangedEventArgs BellhopRadialCalculatorViewModelsChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldCalculatorViewModel>(x => x.BellhopRadialCalculatorViewModels);
        ObservableCollection<BellhopRadialCalculatorViewModel> _bellhopRadialCalculatorViewModels = new ObservableCollection<BellhopRadialCalculatorViewModel>();

        #endregion

        #region BackgroundWorker thread that computes the TransmissionLossField in parallel

        void Calculate(object sender, DoWorkEventArgs args)
        {
            var runFile = (TransmissionLossRunFile) args.Argument;
            var radialNum = 0;
            var radialProgress = 100f/runFile.TransmissionLossRunFileRadials.Count;
            TotalProgress = 0f;

            TransmissionLossField.IDField = runFile.IDField;
            Parallel.ForEach<TransmissionLossRunFileRadial, float>(runFile.TransmissionLossRunFileRadials, () => 0, (radial, loopstate, progress) =>
            {
                var localRadialNum = Interlocked.Increment(ref radialNum);
                var radialViewModel = BellhopRadialCalculatorViewModels[localRadialNum - 1];
                radialViewModel.Start();
                return radialProgress;
            }, (finalResult) => TotalProgress += finalResult);
            foreach (var radial in BellhopRadialCalculatorViewModels) TransmissionLossField.AddRadial(radial.TransmissionLossRadial);
            TransmissionLossField.Depths = TransmissionLossField.Radials[0].Depths;
            TransmissionLossField.Ranges = TransmissionLossField.Radials[0].Ranges;
            _dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(TransmissionLossFieldChangedEventArgs));
        }

        #endregion
    }
}