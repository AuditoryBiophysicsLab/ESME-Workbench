using System;
using System.Windows.Threading;
using HRC.ViewModels;
using HRC.WPF;

namespace DavesWPFTester.AxisLabeling.Language
{
    public class Range : ViewModelBase
    {
        public Range(double min, double max)
        {
            Min = min;
            Max = max;
            PropertyChanged += (s, e) =>
            {
                switch (e.PropertyName)
                {
                    case "Min":
                    case "Max":
                        OnRangeChanged();
                        break;
                }
            };
        }

        double _min;
        public double Min { get { return _min; } set { _min = value; } }
        double _max;
        public double Max { get { return _max; } set { _max = value; } }
        public double Size { get { return Max - Min; } }
        public void Update(double min, double max)
        {
            var isChanged = false;
            if (Math.Abs(_min - min) > 0.0000000001)
            {
                _min = min;
                isChanged = true;
            }
            if (Math.Abs(_max - max) > 0.0000000001)
            {
                _max = max;
                isChanged = true;
            }
            if (isChanged) OnRangeChanged();
        }

        public double ValueToRange(double value) { return (value - Min) / Size; }

        public double RangeToValue(double range) { return (range * Size) + Min; }

        public bool Contains(Range otherRange) { return Min <= otherRange.Min && Max >= otherRange.Max; }

        public static Range Identity = new Range(0, 1);

        public Range Expand(double amount) { return new Range(Min - amount, Max + amount); }

        public event EventHandler RangeChanged;
        protected void OnRangeChanged()
        {
            var handlers = RangeChanged;
            if (handlers == null) return;
            foreach (EventHandler handler in handlers.GetInvocationList())
            {
                if (handler.Target is DispatcherObject)
                {
                    var localHandler = handler;
                    ((DispatcherObject)handler.Target).Dispatcher.InvokeIfRequired(() => localHandler(this, new EventArgs()));
                }
                else
                    handler(this, new EventArgs());
            }
        }
        public override string ToString() { return string.Format("Range {{ Min = {0}, Max = {1} }}", Min, Max); }
    };
}