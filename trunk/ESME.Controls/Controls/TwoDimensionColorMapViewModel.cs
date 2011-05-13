using System.ComponentModel;
using System.Windows.Media.Imaging;
using Cinch;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Views.Controls
{
    [ExportViewModel("TwoDimensionColorMapViewModel")]
    public class TwoDimensionColorMapViewModel : ViewModelBase, IDesignTimeAware
    {
        #region public DataAxisControlViewModel XAxisControl { get; set; }

        public DataAxisControlViewModel XAxisControl
        {
            get { return _xAxisControl ?? (_xAxisControl = new DataAxisControlViewModel()); }
            set
            {
                if (_xAxisControl == value) return;
                _xAxisControl = value;
                NotifyPropertyChanged(XAxisChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs XAxisChangedEventArgs = ObservableHelper.CreateArgs<TwoDimensionColorMapViewModel>(x => x.XAxisControl);
        DataAxisControlViewModel _xAxisControl;

        #endregion

        #region public DataAxisControlViewModel YAxisControl { get; set; }

        public DataAxisControlViewModel YAxisControl
        {
            get { return _yAxisControl ?? (_yAxisControl = new DataAxisControlViewModel()); }
            set
            {
                if (_yAxisControl == value) return;
                _yAxisControl = value;
                NotifyPropertyChanged(YAxisChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs YAxisChangedEventArgs = ObservableHelper.CreateArgs<TwoDimensionColorMapViewModel>(x => x.YAxisControl);
        DataAxisControlViewModel _yAxisControl;

        #endregion

        #region public DataAxisControlViewModel ColorAxisControl { get; set; }

        public DataAxisControlViewModel ColorAxisControl
        {
            get { return _colorAxisControl ?? (_colorAxisControl = new DataAxisControlViewModel()); }
            set
            {
                if (_colorAxisControl == value) return;
                _colorAxisControl = value;
                NotifyPropertyChanged(ColorAxisChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ColorAxisChangedEventArgs = ObservableHelper.CreateArgs<TwoDimensionColorMapViewModel>(x => x.ColorAxisControl);
        DataAxisControlViewModel _colorAxisControl;

        #endregion

        #region public ColorMapViewModel ColorMapViewModel { get; set; }

        public ColorMapViewModel ColorMapViewModel
        {
            get { return _colorMapViewModel ?? (_colorMapViewModel = new ColorMapViewModel()); }
            set
            {
                if (_colorMapViewModel == value) return;
                _colorMapViewModel = value;
                NotifyPropertyChanged(ColorMapViewModelChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ColorMapViewModelChangedEventArgs = ObservableHelper.CreateArgs<TwoDimensionColorMapViewModel>(x => x.ColorMapViewModel);
        ColorMapViewModel _colorMapViewModel;

        #endregion

        #region GridSizeChangedCommandCommand

        public SimpleCommand<object, object> GridSizeChangedCommandCommand
        {
            get { return _gridSizeChangedCommand ?? (_gridSizeChangedCommand = new SimpleCommand<object, object>(delegate { Mediator.Instance.NotifyColleagues("GridSizeChangedCommandCommandMessage"); })); }
        }

        SimpleCommand<object, object> _gridSizeChangedCommand;

        #endregion

        #region public WriteableBitmap WriteableBitmap { get; set; }

        public WriteableBitmap WriteableBitmap
        {
            get { return _writeableBitmap; }
            set
            {
                if (_writeableBitmap == value) return;
                _writeableBitmap = value;
                NotifyPropertyChanged(WriteableBitmapChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs WriteableBitmapChangedEventArgs = ObservableHelper.CreateArgs<TwoDimensionColorMapViewModel>(x => x.WriteableBitmap);
        WriteableBitmap _writeableBitmap;

        #endregion

        #region public string BottomProfileGeometry { get; set; }

        public string BottomProfileGeometry
        {
            get { return _bottomProfileGeometry; }
            set
            {
                if (_bottomProfileGeometry == value) return;
                _bottomProfileGeometry = value;
                NotifyPropertyChanged(BottomProfileGeometryChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BottomProfileGeometryChangedEventArgs = ObservableHelper.CreateArgs<TwoDimensionColorMapViewModel>(x => x.BottomProfileGeometry);
        string _bottomProfileGeometry;

        #endregion

        #region public bool CanvasPathVisibility { get; set; }

        public bool CanvasPathVisibility
        {
            get { return _canvasPathVisibility; }
            set
            {
                if (_canvasPathVisibility == value) return;
                _canvasPathVisibility = value;
                NotifyPropertyChanged(CanvasPathVisibilityChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CanvasPathVisibilityChangedEventArgs = ObservableHelper.CreateArgs<TwoDimensionColorMapViewModel>(x => x.CanvasPathVisibility);
        bool _canvasPathVisibility;

        #endregion

        public void DesignTimeInitialization()
        {
            XAxisControl.AxisLocation = "Top";
            XAxisControl.Label = "XAxisControl";
            YAxisControl.AxisLocation = "Right";
            ColorAxisControl.AxisLocation = "Left";
            ColorAxisControl.TickValueFormat = "0.#";
        }
    }
}