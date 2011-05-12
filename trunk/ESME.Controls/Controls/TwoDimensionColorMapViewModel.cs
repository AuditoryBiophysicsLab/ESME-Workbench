using System.ComponentModel;
using System.Windows.Media.Imaging;
using Cinch;
using MEFedMVVM.Common;

namespace ESME.Views.Controls
{
    class TwoDimensionColorMapViewModel:ViewModelBase
    {
        #region public DataAxisViewModel XAxis { get; set; }

        public DataAxisViewModel XAxis
        {
            get { return _xAxis ?? (_xAxis = new DataAxisViewModel()); }
            set
            {
                if (_xAxis == value) return;
                _xAxis = value;
                NotifyPropertyChanged(XAxisChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs XAxisChangedEventArgs = ObservableHelper.CreateArgs<TwoDimensionColorMapViewModel>(x => x.XAxis);
        DataAxisViewModel _xAxis;

        #endregion

        #region public DataAxisViewModel YAxis { get; set; }

        public DataAxisViewModel YAxis
        {
            get { return _yAxis ?? (_yAxis = new DataAxisViewModel()); }
            set
            {
                if (_yAxis == value) return;
                _yAxis = value;
                NotifyPropertyChanged(YAxisChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs YAxisChangedEventArgs = ObservableHelper.CreateArgs<TwoDimensionColorMapViewModel>(x => x.YAxis);
        DataAxisViewModel _yAxis;

        #endregion

        #region public DataAxisViewModel ColorAxis { get; set; }

        public DataAxisViewModel ColorAxis
        {
            get { return _colorAxis ?? (_colorAxis = new DataAxisViewModel()); }
            set
            {
                if (_colorAxis == value) return;
                _colorAxis = value;
                NotifyPropertyChanged(ColorAxisChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ColorAxisChangedEventArgs = ObservableHelper.CreateArgs<TwoDimensionColorMapViewModel>(x => x.ColorAxis);
        DataAxisViewModel _colorAxis;

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

        public TwoDimensionColorMapViewModel()
        {
            if (Designer.IsInDesignMode)
            {
                XAxis.AxisLocation = DataAxis.AxisLocationEnum.Top;
                XAxis.Label = "XAxis";
                YAxis.AxisLocation = DataAxis.AxisLocationEnum.Right;
                ColorAxis.AxisLocation = DataAxis.AxisLocationEnum.Left;
                ColorAxis.TickValueFormat = "0.#";
            }
        }

    }
    public class DataAxisViewModel:ViewModelBase
    {
        #region public string Label { get; set; }

        public string Label
        {
            get { return _label; }
            set
            {
                if (_label == value) return;
                _label = value;
                NotifyPropertyChanged(LabelChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LabelChangedEventArgs = ObservableHelper.CreateArgs<DataAxisViewModel>(x => x.Label);
        string _label;

        #endregion

        #region public double MaxValue { get; set; }

        public double MaxValue
        {
            get { return _maxValue; }
            set
            {
                if (_maxValue == value) return;
                _maxValue = value;
                NotifyPropertyChanged(MaxValueChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MaxValueChangedEventArgs = ObservableHelper.CreateArgs<DataAxisViewModel>(x => x.MaxValue);
        double _maxValue;

        #endregion

        #region public double MinValue { get; set; }

        public double MinValue
        {
            get { return _minValue; }
            set
            {
                if (_minValue == value) return;
                _minValue = value;
                NotifyPropertyChanged(MinValueChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MinValueChangedEventArgs = ObservableHelper.CreateArgs<DataAxisViewModel>(x => x.MinValue);
        double _minValue;

        #endregion

        #region public string TickValueFormat { get; set; }

        public string TickValueFormat
        {
            get { return _tickValueFormat; }
            set
            {
                if (_tickValueFormat == value) return;
                _tickValueFormat = value;
                NotifyPropertyChanged(TickValueFormatChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TickValueFormatChangedEventArgs = ObservableHelper.CreateArgs<DataAxisViewModel>(x => x.TickValueFormat);
        string _tickValueFormat = "0";

        #endregion


        #region public AxisLocationEnum AxisLocation { get; set; }

        public DataAxis.AxisLocationEnum AxisLocation
        {
            get { return _axisLocation; }
            set
            {
                if (_axisLocation == value) return;
                _axisLocation = value;
                NotifyPropertyChanged(AxisLocationEnumChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs AxisLocationEnumChangedEventArgs = ObservableHelper.CreateArgs<DataAxisViewModel>(x => x.AxisLocation);
        DataAxis.AxisLocationEnum _axisLocation;

        #endregion


    }
}
