using System.ComponentModel;
using Cinch;

namespace HRC.Utility
{
    public class GeoDistanceUnit : PropertyChangedBase
    {
        #region public string Name { get; set; }

        public string Name
        {
            get { return _name; }
            set
            {
                if (_name == value) return;
                _name = value;
                OnPropertyChanged(NameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs NameChangedEventArgs = ObservableHelper.CreateArgs<GeoDistanceUnit>(x => x.Name);
        string _name;

        #endregion

        #region public float ScaleToMeters { get; set; }

        /// <summary>
        /// Multiply Quantity by this number to get meters, divide meters by this number to get Quantity
        /// </summary>
        public float ScaleToMeters
        {
            get { return _scaleToMeters; }
            set
            {
                if (_scaleToMeters == value) return;
                _scaleToMeters = value;
                OnPropertyChanged(ScaleToMetersChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ScaleToMetersChangedEventArgs = ObservableHelper.CreateArgs<GeoDistanceUnit>(x => x.ScaleToMeters);
        float _scaleToMeters;

        #endregion

        #region public float Quantity { get; set; }

        public float Quantity
        {
            get { return _quantity; }
            set
            {
                if (_quantity == value) return;
                _quantity = value;
                OnPropertyChanged(QuantityChangedEventArgs);
                _meters = _quantity * ScaleToMeters;
                OnPropertyChanged(MetersChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs QuantityChangedEventArgs = ObservableHelper.CreateArgs<GeoDistanceUnit>(x => x.Quantity);
        float _quantity;

        #endregion

        #region public float Meters { get; set; }

        public float Meters
        {
            get { return _meters; }
            set
            {
                if (_meters == value) return;
                _meters = value;
                OnPropertyChanged(MetersChangedEventArgs);
                _quantity = _meters / ScaleToMeters;
                OnPropertyChanged(QuantityChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MetersChangedEventArgs = ObservableHelper.CreateArgs<GeoDistanceUnit>(x => x.Meters);
        float _meters;

        #endregion
    }
}
