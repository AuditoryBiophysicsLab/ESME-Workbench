using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.Linq;

namespace HRC.Plotting
{
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
}