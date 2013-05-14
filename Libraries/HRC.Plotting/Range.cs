using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.Linq;
using System.Reactive.Subjects;
using System.Windows;
using System.Windows.Media.Animation;
using System.Windows.Threading;
using HRC.WPF;

namespace HRC.Plotting
{
    public class Range : RangeBase
    {
        public Range()
        {
            Minimum = Maximum = double.NaN;
        }

        public Range(Range source)
        {
            Minimum = source.Minimum;
            Maximum = source.Maximum;
        }

        public Range(double min, double max)
        {
            Minimum = min;
            Maximum = max;
        }

        public new double Min
        {
            get { return Minimum; }
            set
            {
                if (Math.Abs(Minimum - value) < double.Epsilon) return;
                var oldRange = new Range(Minimum, Max);
                Minimum = value;
                OnRangeChanged(oldRange);
            }
        }

        public new double Max
        {
            get { return Maximum; }
            set
            {
                if (Math.Abs(Maximum - value) < double.Epsilon) return;
                var oldRange = new Range(Min, Maximum);
                Maximum = value;
                OnRangeChanged(oldRange);
            }
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

        public void Update(IRange range)
        {
            Update(range.Min, range.Max);
        }
        public void ForceUpdate(IRange range)
        {
            ForceUpdate(range.Min, range.Max);
        }

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
        public override void Add(IRange range)
        {
            Add(range.Min, range.Max);
        }
        public void Add(IEnumerable<Range> ranges)
        {
            var rangeList = ranges.ToList();
            Add(rangeList.Min(r => r.Min), rangeList.Max(r => r.Max));
        }

        public double ValueToRange(double value) { return (value - Min) / Size; }

        public double RangeToValue(double range) { return (range * Size) + Min; }

        public bool Contains(Range otherRange) { return Min <= otherRange.Min && Max >= otherRange.Max; }
        public bool Contains(double value) { return Min <= value && Max >= value; }
    };
    
    public class RangeCollection : RangeBase
    {
        public RangeCollection() 
        {
            _ranges = new ObservableCollection<IRange>();
            _ranges.CollectionChanged += (s, e) =>
            {
                switch (e.Action)
                {
                    case NotifyCollectionChangedAction.Add:
                        foreach (IRange newRange in e.NewItems)
                        {
                            AddInternal(newRange);
                            newRange.RangeChanged += CollectionElementChanged;
                        }
                        break;
                    case NotifyCollectionChangedAction.Remove:
                        var isRescanNeeded = false;
                        foreach (Range oldRange in e.OldItems)
                        {
                            oldRange.RangeChanged -= CollectionElementChanged;
                            isRescanNeeded = (oldRange.Max >= Maximum) || (oldRange.Min <= Minimum);
                        }
                        if (isRescanNeeded) Update(_ranges.Min(r => r.Min), _ranges.Max(r => r.Max));
                        break;
                }
            };
        }

        public override void Reset()
        {
            _ranges.Clear();
            base.Reset();
        }

        void CollectionElementChanged(object sender, NotifyRangeChangedEventArgs notifyRangeChangedEventArgs) 
        {
            AddInternal((IRange)sender);
        }

        void AddInternal(IRange range)
        {
            var min = double.IsNaN(range.Min) ? Min : range.Min;
            var max = double.IsNaN(range.Max) ? Max : range.Max;
            var realMin = double.IsNaN(Min) ? min : Math.Min(Min, min);
            var realMax = double.IsNaN(Max) ? max : Math.Max(Max, max);
            Update(realMin, realMax);
        }

        public RangeCollection(IEnumerable<IRange> ranges) : this() { foreach (var range in ranges) _ranges.Add(range); }

        readonly ObservableCollection<IRange> _ranges;

        public override void Add(IRange range)
        {
            if (range == null) throw new ArgumentNullException("range");
            if (_ranges.Contains(range)) return;
            _ranges.Add(range);
        }

        public void Remove(IRange range)
        {
            if (range == null) throw new ArgumentNullException("range");
            _ranges.Remove(range);
        }
    }

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

    public interface IRange : INotifyRangeChanged, IEquatable<IRange>, IObservable<IRange>
    {
        double Min { get; }
        double Max { get; }
        double Value { get; }
        bool IsEmpty { get; }
    }
    public interface INotifyRangeChanged
    {
        event EventHandler<NotifyRangeChangedEventArgs> RangeChanged;
    }

    public class NotifyRangeChangedEventArgs : EventArgs
    {
        public NotifyRangeChangedEventArgs(IRange oldRange) { OldRange = oldRange; }
        public IRange OldRange { get; private set; }
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
    public class RangeAnimation : AnimationTimeline
    {
        #region dependency property Range From

        public static DependencyProperty FromProperty = DependencyProperty.Register("From",
                                                                                 typeof(Range),
                                                                                 typeof(RangeAnimation),
                                                                                 new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, FromPropertyChanged));

        public Range From { get { return (Range)GetValue(FromProperty); } set { SetValue(FromProperty, value); } }

        static void FromPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((RangeAnimation)obj).FromPropertyChanged(); }
        void FromPropertyChanged() { _from = new Range(From); }
        #endregion

        #region dependency property Range To

        public static DependencyProperty ToProperty = DependencyProperty.Register("To",
                                                                                 typeof(Range),
                                                                                 typeof(RangeAnimation),
                                                                                 new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, ToPropertyChanged));

        public Range To { get { return (Range)GetValue(ToProperty); } set { SetValue(ToProperty, value); } }

        static void ToPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((RangeAnimation)obj).ToPropertyChanged(); }
        void ToPropertyChanged() { }
        #endregion

        #region dependency property IEasingFunction EasingFunction

        public static DependencyProperty EasingFunctionProperty = DependencyProperty.Register("EasingFunction",
                                                                                 typeof(IEasingFunction),
                                                                                 typeof(RangeAnimation),
                                                                                 new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, EasingFunctionPropertyChanged));

        public IEasingFunction EasingFunction { get { return (IEasingFunction)GetValue(EasingFunctionProperty); } set { SetValue(EasingFunctionProperty, value); } }

        static void EasingFunctionPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((RangeAnimation)obj).EasingFunctionPropertyChanged(); }
        void EasingFunctionPropertyChanged() { }
        #endregion

        public RangeAnimation() {}
        public RangeAnimation(Range from, Range to, Duration duration, FillBehavior fillBehavior = FillBehavior.Stop, IEasingFunction easingFunction = null)
        {
            From = from;
            To = to;
            Duration = duration;
            FillBehavior = fillBehavior;
            EasingFunction = easingFunction;
        }

        Range _from;
        protected override Freezable CreateInstanceCore() { return new RangeAnimation(); }
        public override Type TargetPropertyType { get { return typeof(Range); } }
        public override object GetCurrentValue(object defaultOriginValue, object defaultDestinationValue, AnimationClock animationClock)
        {
            if (!animationClock.CurrentProgress.HasValue) return From;

            var progress = (EasingFunction != null) ? EasingFunction.Ease(animationClock.CurrentProgress.Value) : animationClock.CurrentProgress.Value;

            double newMin, newMax;
            if (_from.Min > To.Min) newMin = (1 - progress) * (_from.Min - To.Min) + To.Min;
            else newMin = progress * (To.Min - _from.Min) + _from.Min;
            if (_from.Max > To.Max) newMax = (1 - progress) * (_from.Max - To.Max) + To.Max;
            else newMax = progress * (To.Max - _from.Max) + _from.Max;
            From.Update(newMin, newMax);
            //Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} GetCurrentValue returning: {1}", DateTime.Now, From));
            return From;
        }
    }
}