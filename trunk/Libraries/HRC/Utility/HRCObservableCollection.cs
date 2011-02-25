using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;

namespace HRC.Utility
{
    [Serializable]
    public class HRCObservableCollection<T> : ObservableCollection<T>, INotifyPropertyChanged
    {
        public void Sort()
        {
            var tmp = new List<T>();
            tmp.AddRange(this);
            tmp.Sort();
            Clear();
            foreach (var value in tmp) Add(value);
        }

        #region INotifyPropertyChanged support

        [field: NonSerialized]
        public override event NotifyCollectionChangedEventHandler CollectionChanged;

        [field: NonSerialized] PropertyChangedEventHandler _propertyChangedEventHandler;

        event PropertyChangedEventHandler INotifyPropertyChanged.PropertyChanged
        {
            add { _propertyChangedEventHandler = Delegate.Combine(_propertyChangedEventHandler, value) as PropertyChangedEventHandler; }
            remove { _propertyChangedEventHandler = Delegate.Remove(_propertyChangedEventHandler, value) as PropertyChangedEventHandler; }
        }

        protected override void OnCollectionChanged(NotifyCollectionChangedEventArgs e)
        {
            var handler = CollectionChanged;
            if (handler != null) handler(this, e);
        }

        protected override void OnPropertyChanged(PropertyChangedEventArgs e)
        {
            var handler = _propertyChangedEventHandler;
            if (handler != null) handler(this, e);
        }

        #endregion
    }
}