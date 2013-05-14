using System;
using System.Reactive.Subjects;
using System.Windows.Threading;
using HRC.WPF;

namespace HRC.Plotting
{
    public abstract class RangeBase : IRange
    {
        protected double Minimum = double.NaN;
        public virtual double Min
        {
            get { return Minimum; }
        }

        protected double Maximum = double.NaN;
        public virtual double Max
        {
            get { return Maximum; }
        }

        public abstract void Add(IRange range);
        /// <summary>
        /// Reset the range to the empty state
        /// </summary>
        public virtual void Reset()
        {
            var oldRange = new Range(Min, Max);
            Minimum = double.NaN;
            Maximum = double.NaN;
            OnRangeChanged(oldRange);
        }

        public virtual void Update(double min, double max)
        {
            var isChanged = false;
            var oldRange = new Range(Min, Max);
            if (!double.IsNaN(min) && !double.IsInfinity(min) && (double.IsNaN(Minimum) || (Math.Abs(Minimum - min) > double.Epsilon)))
            {
                Minimum = min;
                isChanged = true;
            }
            if (!double.IsNaN(max) && !double.IsInfinity(max) && (double.IsNaN(Maximum) || (Math.Abs(Maximum - max) > double.Epsilon)))
            {
                Maximum = max;
                isChanged = true;
            }
            if (isChanged) OnRangeChanged(oldRange);
        }

        public virtual void ForceUpdate(double min, double max)
        {
            var oldRange = new Range(Min, Max);
            if (!double.IsNaN(min) && !double.IsInfinity(min) && (double.IsNaN(Minimum) || (Math.Abs(Minimum - min) > double.Epsilon))) Minimum = min;
            if (!double.IsNaN(max) && !double.IsInfinity(max) && (double.IsNaN(Maximum) || (Math.Abs(Maximum - max) > double.Epsilon))) Maximum = max;
            OnRangeChanged(oldRange);
        }

        public virtual Range Expand(double amount)
        {
            return new Range(Min - amount, Max + amount);
        }

        public double Value { get { return Max - Min; } }

        public event EventHandler<NotifyRangeChangedEventArgs> RangeChanged;
        protected void OnRangeChanged(Range oldRange)
        {
            _subject.OnNext(this);
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
        public virtual bool IsEmpty { get { return double.IsNaN(Minimum) || double.IsNaN(Maximum); } }
        public bool Equals(RangeBase other)
        {
            if (ReferenceEquals(null, other)) return false;
            if (ReferenceEquals(this, other)) return true;
            return other.Minimum.Equals(Minimum) && other.Maximum.Equals(Maximum) && Equals(other._subject, _subject);
        }

        public bool Equals(IRange other) { return Equals((RangeBase)other); }
        public override string ToString() { return string.Format("Range {{ Min = {0}, Max = {1} }}", Min, Max); }
        public static bool operator ==(RangeBase r1, RangeBase r2)
        {
            // If they're both null, they are equal
            if (ReferenceEquals(null, r1) && ReferenceEquals(null, r2)) return true;
            if (ReferenceEquals(null, r1)) return false;
            if (ReferenceEquals(null, r2)) return false;
            return (Math.Abs(r1.Min - r2.Min) < double.Epsilon) && (Math.Abs(r1.Max - r2.Max) < double.Epsilon);
        }
        public static bool operator !=(RangeBase r1, RangeBase r2)
        {
            if (ReferenceEquals(null, r1) && !ReferenceEquals(null, r2)) return true;
            if (!ReferenceEquals(null, r1) && ReferenceEquals(null, r2)) return true;
            return !(r1 == r2);
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            return obj.GetType() == typeof(RangeBase) && Equals((RangeBase)obj);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                return (Minimum.GetHashCode() * 397) ^ Maximum.GetHashCode();
            }
        }

        public IDisposable Subscribe(IObserver<IRange> observer)
        {
            return _subject.Subscribe(observer);
        }

        readonly Subject<IRange> _subject = new Subject<IRange>();
    }
}