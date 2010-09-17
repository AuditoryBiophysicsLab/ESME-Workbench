using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;

namespace ESMEWorkBench.Data
{
    public class WatchableList<T> : ObservableCollection<T>
    {
        public WatchableList() { }

        public WatchableList(EventHandler valueChangedHandler)
        {
            if (valueChangedHandler != null)
                ValueChanged += valueChangedHandler;
        }

        protected override void OnCollectionChanged(NotifyCollectionChangedEventArgs e)
        {
            using (BlockReentrancy())
            {
                base.OnCollectionChanged(e);
                if (e.NewItems != null) foreach (var item in e.NewItems.OfType<WatchableDatum>()) (item).ValueChanged += ValueChangedHandler;
                if (e.OldItems != null) foreach (var item in e.OldItems.OfType<WatchableDatum>()) (item).ValueChanged -= ValueChangedHandler;
            }
        }

        protected override void OnPropertyChanged(PropertyChangedEventArgs e)
        {
            base.OnPropertyChanged(e);
            //Console.WriteLine(@"WatchableList property: " + e.PropertyName + @" changed");
            if (e.PropertyName == "Item[]")
                OnValueChanged();
        }

        public event EventHandler ValueChanged;

        protected virtual void OnValueChanged()
        {
            if (ValueChanged != null)
                ValueChanged(this, EventArgs.Empty);
        }

        protected void ValueChangedHandler(object sender, EventArgs e)
        {
            OnValueChanged();
        }
    }
}