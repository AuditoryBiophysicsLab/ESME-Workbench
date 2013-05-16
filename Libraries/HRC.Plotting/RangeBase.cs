using System;
using System.Reactive.Linq;
using System.Windows.Threading;
using HRC.WPF;

namespace HRC.Plotting
{
    public abstract class RangeBase : IRange
    {
        protected RangeBase() { _rangeChangedObservable = Observable.FromEventPattern<NotifyRangeChangedEventArgs>(this, "RangeChanged").Select(e => (IRange)e.Sender); }

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
                else handler(this, new NotifyRangeChangedEventArgs(oldRange));
            }
        }
        public bool Equals(RangeBase other)
        {
            // If they're both null, they are equal
            if (ReferenceEquals(null, this) && ReferenceEquals(null, other)) return true;
            // If I'm null, and we already know that 'other' is not null, so not equal
            if (ReferenceEquals(null, this)) return false;
            // If 'other' is null, and we already know I'm not null, so not equal
            if (ReferenceEquals(null, other)) return false;
            // If other is just a reference to me, they are equal
            if (ReferenceEquals(this, other)) return true;

            // If all the other tests have passed, then the two are equal if their Min and Max properties are equal to each other
            return other.Min.Equals(Min) && other.Max.Equals(Max);
        }

        public static bool operator ==(RangeBase r1, RangeBase r2)
        {
            // If r1 and r2 are both null, they are equal
            if (ReferenceEquals(null, r1) && ReferenceEquals(null, r2)) return true;
            // If r1 is null, and we already know that r2 is not null, they are not equal
            if (ReferenceEquals(null, r1)) return false;
            // If r2 is null, and we already know that r1 is not null, they are not equal
            if (ReferenceEquals(null, r2)) return false;
            // If r1 and r2 refer to the same object, they are equal
            if (ReferenceEquals(r1, r2)) return true;

            // If all the other tests have passed, then the two are equal if their Min and Max properties are equal to each other
            return r1.Min.Equals(r2.Min) && r1.Max.Equals(r2.Max);
        }

        public static bool operator !=(RangeBase r1, RangeBase r2)
        {
            // If r1 and r2 are both null, they are equal (which means we return false in this context)
            if (ReferenceEquals(null, r1) && ReferenceEquals(null, r2)) return false;
            // If r1 is null and we already know that r2 is not null, they are not equal (which means we return true in this context)
            if (ReferenceEquals(null, r1)) return true;
            // If r2 is null and we already know that r1 is not null, they are not equal (which means we return true in this context)
            if (ReferenceEquals(null, r2)) return true;
            // If r1 and r2 refer to the same object, they are equal (which means we return false in this context)
            if (ReferenceEquals(r1, r2)) return false;

            // If all the other tests have passed, then the two are equal if their Min and Max properties are equal to each other
            return !(r1.Min.Equals(r2.Min) && r1.Max.Equals(r2.Max));
        }

        public override bool Equals(object obj)
        {
            // If they're both null, they are equal
            if (ReferenceEquals(null, this) && ReferenceEquals(null, obj)) return true;
            // If I'm null, and we already know that obj is not null, so not equal
            if (ReferenceEquals(null, this)) return false;
            // If obj is null, and we already know I'm not null, so not equal
            if (ReferenceEquals(null, obj)) return false;
            // If this and obj refer to the same object, they are equal, 
            // OR if they don't refer to the same object, then obj is equal to me when treated as a RangeBase
            return ReferenceEquals(this, obj) || Equals(obj as RangeBase);
        }

        public override int GetHashCode() { unchecked { return (Minimum.GetHashCode() * 397) ^ Maximum.GetHashCode(); } }

        public abstract void Add(IRange range);
        public virtual Range Expand(double amount) { return new Range(Min - amount, Max + amount); }

        protected double Minimum = double.NaN;
        protected double Maximum = double.NaN;

        public virtual double Min { get { return Minimum; } }
        public virtual double Max { get { return Maximum; } }
        public double Value { get { return Max - Min; } }

        public bool Equals(IRange other) { return Equals((RangeBase)other); }
        public virtual bool IsEmpty { get { return double.IsNaN(Minimum) || double.IsNaN(Maximum); } }
        
        public override string ToString() { return string.Format("Range {{ Min = {0:0.##}, Max = {1:0.##} }}", Min, Max); }

        public IDisposable Subscribe(IObserver<IRange> observer) { return _rangeChangedObservable.Subscribe(observer); }
        readonly IObservable<IRange> _rangeChangedObservable;
    }
}