using System.ComponentModel;
using Cinch;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Views.Controls
{
    [ExportViewModel("DataAxisControlViewModel")]
    public class DataAxisControlViewModel : ViewModelBase, IDesignTimeAware
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

        static readonly PropertyChangedEventArgs LabelChangedEventArgs = ObservableHelper.CreateArgs<DataAxisControlViewModel>(x => x.Label);
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

        static readonly PropertyChangedEventArgs MaxValueChangedEventArgs = ObservableHelper.CreateArgs<DataAxisControlViewModel>(x => x.MaxValue);
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

        static readonly PropertyChangedEventArgs MinValueChangedEventArgs = ObservableHelper.CreateArgs<DataAxisControlViewModel>(x => x.MinValue);
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

        static readonly PropertyChangedEventArgs TickValueFormatChangedEventArgs = ObservableHelper.CreateArgs<DataAxisControlViewModel>(x => x.TickValueFormat);
        string _tickValueFormat = "0";

        #endregion

        #region public AxisLocationEnum AxisLocation { get; set; }

        public string AxisLocation
        {
            get { return _axisLocation; }
            set
            {
                if (_axisLocation == value) return;
                _axisLocation = value;
                NotifyPropertyChanged(AxisLocationEnumChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs AxisLocationEnumChangedEventArgs = ObservableHelper.CreateArgs<DataAxisControlViewModel>(x => x.AxisLocation);
        string _axisLocation;

        #endregion

        public void DesignTimeInitialization()
        {
            Label = "DataAxisControl.Label";
            MinValue = -10;
            MaxValue = 10;
            TickValueFormat = "0.##";
            AxisLocation = "Left";
        }
    }
}