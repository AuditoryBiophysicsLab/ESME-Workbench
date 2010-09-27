using System;
using System.ComponentModel;
using System.Linq;
using System.Windows;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Threading;
using Cinch;
using ESME.TransmissionLoss;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    public class TransmissionLossRadialViewModel : ViewModelBase
    {
        readonly Dispatcher _dispatcher;
        bool _isRendered;
        
        WriteableBitmap _writeableBitmap;

        public TransmissionLossRadialViewModel(TransmissionLossRadial transmissionLossRadial, ColorMapViewModel colorMapViewModel)
        {
            ColorMapViewModel = colorMapViewModel;
            TransmissionLossRadial = transmissionLossRadial;
            RangeMin = TransmissionLossRadial.Ranges.First();
            RangeMax = TransmissionLossRadial.Ranges.Last();
            DepthMin = TransmissionLossRadial.Depths.First();
            DepthMax = TransmissionLossRadial.Depths.Last();
            _dispatcher = Dispatcher.CurrentDispatcher;
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

        public TransmissionLossRadial TransmissionLossRadial { get; private set; }
        public float RangeMin { get; private set; }
        public float RangeMax { get; private set; }
        public float DepthMin { get; private set; }
        public float DepthMax { get; private set; }
    }
}