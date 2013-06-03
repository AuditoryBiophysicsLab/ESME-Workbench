
namespace HRC.Utility
{
    public class ProgressInfo<T> : PropertyChangedBase
    {
        #region public string Status { get; set; }

        public string Status
        {
            get { return _status; }
            set
            {
                if (_status == value) return;
                _status = value;
                OnPropertyChanged("Status");
            }
        }

        string _status;

        #endregion

        #region public T CurrentValue { get; set; }

        public T CurrentValue
        {
            get { return _currentValue; }
            set
            {
                _currentValue = value;
                OnPropertyChanged("CurrentValue");
            }
        }

        T _currentValue;

        #endregion

        #region public double MaximumValue { get; set; }

        public T MaximumValue
        {
            get { return _maximumValue; }
            set
            {
                _maximumValue = value;
                OnPropertyChanged("MaximumValue");
            }
        }

        T _maximumValue;

        #endregion
    }

    public class ProgressInfoInt : ProgressInfo<int> { }
    public class ProgressInfoFloat : ProgressInfo<float> { }
    public class ProgressInfoDouble : ProgressInfo<double> { }
}