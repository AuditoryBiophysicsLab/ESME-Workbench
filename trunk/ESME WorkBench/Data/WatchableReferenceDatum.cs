using System;

namespace ESMEWorkBench.Data
{
    public class WatchableReferenceDatum<T> : WatchableDatum
        where T : class, IEquatable<T>
    {
        public WatchableReferenceDatum() : base(null) { }

        public WatchableReferenceDatum(EventHandler valueChangedHandler) : base(valueChangedHandler) { }

        public T Value
        {
            get { return _value; }
            set
            {
                if ((_value != null) && (_value.Equals(value))) return;
                _value = value;
                OnValueChanged();
            }
        }
        T _value;
    }

    public class WatchableDatum
    {
        protected WatchableDatum(EventHandler valueChangedHandler)
        {
            if (valueChangedHandler != null)
                ValueChanged += valueChangedHandler;
        }

        public event EventHandler ValueChanged;

        protected virtual void OnValueChanged()
        {
            if (ValueChanged != null)
                ValueChanged(this, EventArgs.Empty);
        }
    }
}