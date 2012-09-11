using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows;
using System.Windows.Threading;
using HRC.WPF;

namespace DavesWPFTester
{
    public class Range : INotifyRangeChanged
    {
        public Range() { Min = Max = double.NaN; }

        public Range(double min, double max)
        {
            _min = min;
            _max = max;
        }

        double _min;
        public double Min
        {
            get { return _min; }
            set
            {
                if (Math.Abs(_min - value) < double.Epsilon) return;
                var oldRange = new Range(_min, Max);
                _min = value;
                OnRangeChanged(oldRange);
            }
        }
        double _max;

        public double Max
        {
            get { return _max; }
            set
            {
                if (Math.Abs(_max - value) < double.Epsilon) return;
                var oldRange = new Range(Min, _max);
                _max = value;
                OnRangeChanged(oldRange);
            }
        }
        /// <summary>
        /// Reset the range to the empty state
        /// </summary>
        public void Reset()
        {
            var oldRange = new Range(Min, Max);
            _min = double.NaN;
            _max = double.NaN;
            OnRangeChanged(oldRange);
        }
        public double Size { get { return Max - Min; } }
        /// <summary>
        /// Sets the range to the minimum and maxumum values in the provided enumerable
        /// </summary>
        /// <param name="values"></param>
        public void Update(IEnumerable<double> values)
        {
            var valueList = values.ToList();
            Update(valueList.Min(), valueList.Max());
        }
        public void Update(double min, double max)
        {
            var isChanged = false;
            var oldRange = new Range(Min, Max);
            if (!double.IsNaN(min) && !double.IsInfinity(min) && (double.IsNaN(_min) || (Math.Abs(_min - min) > double.Epsilon)))
            {
                _min = min;
                isChanged = true;
            }
            if (!double.IsNaN(max) && !double.IsInfinity(max) && (double.IsNaN(_max) || (Math.Abs(_max - max) > double.Epsilon)))
            {
                _max = max;
                isChanged = true;
            }
            if (isChanged) OnRangeChanged(oldRange);
        }
        public void Update(Range range) { Update(range.Min, range.Max); }
        public void Update(IEnumerable<Range> ranges)
        {
            var rangeList = ranges.ToList();
            Update(rangeList.Min(r => r.Min), rangeList.Max(r => r.Max));
        }
        /// <summary>
        /// Expands the range to include value.  If the range is empty, the min and max will both be set to value
        /// </summary>
        /// <param name="value"></param>
        public void Add(double value)
        {
            var newMin = double.IsNaN(Min) ? value : Math.Min(Min, value);
            var newMax = double.IsNaN(Max) ? value : Math.Max(Max, value);
            Update(newMin, newMax);
        }
        /// <summary>
        /// Expands the range to include the minimum and maxumum values in the provided enumerable
        /// </summary>
        /// <param name="values"></param>
        public void Add(IEnumerable<double> values)
        {
            var valueList = values.ToList();
            var valuesMin = valueList.Min();
            var valuesMax = valueList.Max();
            var newMin = double.IsNaN(Min) ? valuesMin : Math.Min(Min, valuesMin);
            var newMax = double.IsNaN(Max) ? valuesMax : Math.Max(Max, valuesMax);
            Update(newMin, newMax);
        }
        public void Add(double min, double max)
        {
            min = double.IsNaN(min) ? Min : min;
            max = double.IsNaN(max) ? Max : max;
            var realMin = double.IsNaN(Min) ? min : Math.Min(Min, min);
            var realMax = double.IsNaN(Max) ? max : Math.Max(Max, max);
            Update(realMin, realMax);
        }
        public void Add(Range range) { Add(range.Min, range.Max); }
        public void Add(IEnumerable<Range> ranges)
        {
            var rangeList = ranges.ToList();
            Add(rangeList.Min(r => r.Min), rangeList.Max(r => r.Max));
        }

        public double ValueToRange(double value) { return (value - Min) / Size; }

        public double RangeToValue(double range) { return (range * Size) + Min; }

        public bool Contains(Range otherRange) { return Min <= otherRange.Min && Max >= otherRange.Max; }
        public bool Contains(double value) { return Min <= value && Max >= value; }

        public Range Expand(double amount) { return new Range(Min - amount, Max + amount); }

        public event EventHandler<NotifyRangeChangedEventArgs> RangeChanged;
        protected void OnRangeChanged(Range oldRange)
        {
            var handlers = RangeChanged;
            if (handlers == null) return;
            foreach (EventHandler<NotifyRangeChangedEventArgs> handler in handlers.GetInvocationList())
            {
                if (handler.Target is DispatcherObject)
                {
                    var localHandler = handler;
                    ((DispatcherObject)handler.Target).Dispatcher.InvokeIfRequired(() => localHandler(this, new NotifyRangeChangedEventArgs(oldRange)));
                }
                else
                    handler(this, new NotifyRangeChangedEventArgs(oldRange));
            }
        }
        public override string ToString() { return string.Format("Range {{ Min = {0}, Max = {1} }}", Min, Max); }
    };

    public interface INotifyRangeChanged
    {
        event EventHandler<NotifyRangeChangedEventArgs> RangeChanged;
    }

    public class NotifyRangeChangedEventArgs : EventArgs
    {
        public NotifyRangeChangedEventArgs(Range oldRange)
        {
            OldRange = oldRange;
        }

        public Range OldRange { get; private set; }
    }

    public class NotifyRangeChangedEventManager : WeakEventManager
    {
        public static void AddListener(INotifyRangeChanged rangeChanged, IWeakEventListener listener) { Manager.ProtectedAddListener(rangeChanged, listener); }
        public static void RemoveListener(INotifyRangeChanged rangeChanged, IWeakEventListener listener) { Manager.ProtectedRemoveListener(rangeChanged, listener); }
        void OnRangeChanged(object sender, NotifyRangeChangedEventArgs args) { DeliverEvent(sender, args); }
        protected override void StartListening(object source) { ((INotifyRangeChanged)source).RangeChanged += OnRangeChanged; }
        protected override void StopListening(object source) { ((INotifyRangeChanged)source).RangeChanged -= OnRangeChanged; }

        private static NotifyRangeChangedEventManager Manager
        {
            get
            {
                var managerType = typeof(NotifyRangeChangedEventManager);
                var manager = (NotifyRangeChangedEventManager)GetCurrentManager(managerType);
                if (manager == null)
                {
                    manager = new NotifyRangeChangedEventManager();
                    SetCurrentManager(managerType, manager);
                }
                return manager;
            }
        }
    }
}