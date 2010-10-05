using System;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.Linq;
using System.Windows;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Threading;
using Cinch;
using ESME.TransmissionLoss;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    [ExportViewModel("TransmissionLossRadialViewModel")]
    public class TransmissionLossRadialViewModel : ViewModelBase
    {
        readonly Dispatcher _dispatcher;
        bool _isRendered;
        readonly IViewAwareStatus _viewAwareStatus;
        ISaveFileService _saveFileService;
        IUIVisualizerService _visualizerService;

        WriteableBitmap _writeableBitmap;

  
        [ImportingConstructor]
        public TransmissionLossRadialViewModel(IViewAwareStatus viewAwareStatus, ISaveFileService saveFileService, IUIVisualizerService visualizerService)
        {
            RegisterMediator();
            _viewAwareStatus = viewAwareStatus;
            _saveFileService = saveFileService;
            _visualizerService = visualizerService;
            _dispatcher = Dispatcher.CurrentDispatcher;
            _viewAwareStatus.ViewLoaded += () => MediatorMessage.Send(MediatorMessage.TransmissionLossRadialViewInitialized, true);
        }

        void RegisterMediator()
        {
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine("***********\nTransmissionLossRadialViewModel: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }


        }
        [MediatorMessageSink(MediatorMessage.TransmissionLossRadialColorMapChanged)]
        void TransmissionLossRadialColorMapChanged(ColorMapViewModel colorMapViewModel)
        {
            ColorMapViewModel = colorMapViewModel;
        }
        [MediatorMessageSink(MediatorMessage.TransmissionLossRadialChanged)]
        void TransmissionLossRadialChanged(TransmissionLossRadial transmissionLossRadial)
        {
            TransmissionLossRadial = transmissionLossRadial;
        }


        #region public ColorMapViewModel ColorMapViewModel { get; set; }

        public ColorMapViewModel ColorMapViewModel
        {
            get { return _colorMapViewModel; }
            set
            {
                if (_colorMapViewModel == value) return;
                _colorMapViewModel = value;
                _colorMapViewModel.PropertyChanged += ColorMapViewModelPropertyChanged;
                NotifyPropertyChanged(ColorMapViewModelChangedEventArgs);
            }
        }

        void ColorMapViewModelPropertyChanged(object sender, PropertyChangedEventArgs e)
        {
            switch (e.PropertyName)
            {
                case "CurMinValue":
                case "CurMaxValue":
                    _isRendered = false;
                    NotifyPropertyChanged(WriteableBitmapChangedEventArgs);
                    break;
            }
        }

        private static readonly PropertyChangedEventArgs ColorMapViewModelChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossRadialViewModel>(x => x.ColorMapViewModel);
        private ColorMapViewModel _colorMapViewModel;

        #endregion

        #region public TransmissionLossRadial TransmissionLossRadial { get; set; }

        public TransmissionLossRadial TransmissionLossRadial
        {
            get { return _transmissionLossRadial; }
            set
            {
                if (_transmissionLossRadial == value) return;
                _transmissionLossRadial = value;
                RangeMin = TransmissionLossRadial.Ranges.First();
                RangeMax = TransmissionLossRadial.Ranges.Last();
                DepthMin = TransmissionLossRadial.Depths.First();
                DepthMax = TransmissionLossRadial.Depths.Last();
               // ColorMapViewModel.MinValue = TransmissionLossRadial.StatMin;
                ColorMapViewModel.MaxValue = TransmissionLossRadial.StatMax;
                ColorMapViewModel.MinValue = TransmissionLossRadial.StatMin;
                NotifyPropertyChanged(TransmissionLossRadialChangedEventArgs);
                RenderBitmap();
            }
        }

        static readonly PropertyChangedEventArgs TransmissionLossRadialChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossRadialViewModel>(x => x.TransmissionLossRadial);
        TransmissionLossRadial _transmissionLossRadial;

        #endregion

        public WriteableBitmap WriteableBitmap
        {
            get
            {
                if (!_isRendered)
                    RenderBitmap();
                return _writeableBitmap;
            }
        }

        private void RenderBitmap()
        {
            if (TransmissionLossRadial == null || ColorMapViewModel == null) return;

            var width = TransmissionLossRadial.Ranges.Length;
            var height = TransmissionLossRadial.Depths.Length;

            if (_writeableBitmap == null)
                _writeableBitmap = new WriteableBitmap(width, height, 96, 96, PixelFormats.Bgr32, null);

            _writeableBitmap.Lock();
            unsafe
            {
                var curOffset = (int)_writeableBitmap.BackBuffer;
                for (var y = 0; y < height; y++)
                {
                    for (var x = 0; x < width; x++)
                    {
                        // Draw from the bottom up, which matches the default render order.  This may change as the UI becomes
                        // more fully implemented, especially if we need to flip the canvas and render from the top.  Time will tell.
                        *((int*)curOffset) = _colorMapViewModel.Lookup(TransmissionLossRadial.TransmissionLoss[y, x]).ToArgb();
                        curOffset += sizeof(Int32);
                    }
                }
            }
            _writeableBitmap.AddDirtyRect(new Int32Rect(0, 0, width, height));
            _writeableBitmap.Unlock();
            _isRendered = true;
            _dispatcher.BeginInvoke(new VoidDelegate(RenderFinished), DispatcherPriority.ApplicationIdle);
        }
        private delegate void VoidDelegate();

        private void RenderFinished()
        {
            NotifyPropertyChanged(WriteableBitmapChangedEventArgs);
        }

        static readonly PropertyChangedEventArgs WriteableBitmapChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossRadialViewModel>(x => x.WriteableBitmap);

        //public TransmissionLossRadial TransmissionLossRadial { get; private set; }
        //public float RangeMin { get; private set; }
        //public float RangeMax { get; private set; }
        //public float DepthMin { get; private set; }
        //public float DepthMax { get; private set; }

        #region public float RangeMin { get; set; }

        public float RangeMin
        {
            get { return _rangeMin; }
            set
            {
                if (_rangeMin == value) return;
                _rangeMin = value;
                NotifyPropertyChanged(RangeMinChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RangeMinChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossRadialViewModel>(x => x.RangeMin);
        float _rangeMin;

        #endregion

        #region public float  RangeMax { get; set; }

        public float  RangeMax
        {
            get { return _rangeMax; }
            set
            {
                if (_rangeMax == value) return;
                _rangeMax = value;
                NotifyPropertyChanged(RangeMaxChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RangeMaxChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossRadialViewModel>(x => x.RangeMax);
        float  _rangeMax;

        #endregion

        #region public float DepthMin { get; set; }

        public float DepthMin
        {
            get { return _depthMin; }
            set
            {
                if (_depthMin == value) return;
                _depthMin = value;
                NotifyPropertyChanged(DepthMinChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DepthMinChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossRadialViewModel>(x => x.DepthMin);
        float _depthMin;

        #endregion

        #region public float  DepthMax { get; set; }

        public float  DepthMax
        {
            get { return _depthMax; }
            set
            {
                if (_depthMax == value) return;
                _depthMax = value;
                NotifyPropertyChanged(DepthMaxChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DepthMaxChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossRadialViewModel>(x => x.DepthMax);
        float  _depthMax;

        #endregion


    }
}