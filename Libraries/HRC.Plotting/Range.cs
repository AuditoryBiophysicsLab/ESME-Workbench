using System;
using System.Collections.Generic;
using System.Linq;

namespace HRC.Plotting
{
    public class Range : RangeBase
    {
        public Range()
        {
            Minimum = Maximum = double.NaN;
        }

        public Range(IRange source)
        {
            Minimum = source.Min;
            Maximum = source.Max;
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
        public override void Add(IRange range) { Add(range.Min, range.Max); }
        public void Add(IEnumerable<Range> ranges)
        {
            var rangeList = ranges.ToList();
            Add(rangeList.Min(r => r.Min), rangeList.Max(r => r.Max));
        }
    };
}