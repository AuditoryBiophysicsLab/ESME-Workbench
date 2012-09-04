using System;
using System.Collections.Generic;
using System.Linq;
using DavesWPFTester.AxisLabeling.Language;

namespace DavesWPFTester.AxisLabeling.Layout.Formatters
{
    public abstract class Format
    {
        protected double weight;

        protected Format(double weight) { this.weight = weight; }

        public abstract double Score(ICollection<Object> val);

        public abstract Tuple<ICollection<String>, String> FormalLabels(ICollection<Object> o);
    }

    public abstract class NumericFormat : Format
    {
        protected bool factored; // if true, 10^power portion will be placed on the axis title
        protected bool decimalExtend; // if true, labels will be extended to the same number of decimal places

        protected NumericFormat(bool factored, bool decimalExtend, double weight) : base(weight)
        {
            this.factored = factored;
            this.decimalExtend = decimalExtend;
        }

        public override double Score(ICollection<Object> val) { return 0.9 * val.Select(x => (double)x == 0.0 ? 1 : weight * Score((double)x)).Average() + 0.1 * (decimalExtend ? 1 : 0); }

        public abstract double Score(double d);

        public override Tuple<ICollection<string>, string> FormalLabels(ICollection<Object> o) { return FormatLabels(o.Cast<double>().ToList()); }

        public abstract Tuple<ICollection<string>, string> FormatLabels(ICollection<double> d);

        protected int pot(double val) { return (int)Math.Floor(Math.Log10(Math.Abs(val))); }

        protected int decimalPlaces(double i)
        {
            var t = i.ToString("G29");
            var s = t.IndexOf(".", StringComparison.Ordinal);
            if (s < 0) return 0;
            return (t.Length - (s + 1));
        }
    }

    public class UnitFormat : NumericFormat
    {
        readonly double _unit;
        readonly string _name;
        readonly Range _potRange;

        public UnitFormat(double unit, string name, Range potRange, bool factored, bool decimalExtend, double weight)
            : base(factored, decimalExtend, weight)
        {
            _unit = unit;
            _name = name;
            _potRange = potRange;
        }

        public override double Score(double d) { return (pot(d) >= _potRange.Min && pot(d) <= _potRange.Max) ? 1 : 0; }

        public override Tuple<ICollection<string>, string> FormatLabels(ICollection<double> d)
        {
            var r = (from x in d select x / _unit).ToList();
            var decimals = r.Select(decimalPlaces).Concat(new[] { int.MinValue }).Max();
            return new Tuple<ICollection<string>, string>((from x in r select x.ToString(decimalExtend ? "N" + decimals : "G29") + (factored ? "" : _name)).ToList(), (factored ? _name : ""));
        }
    }

    public class ScientificFormat : NumericFormat
    {
        public ScientificFormat(bool factored, bool decimalExtend, double weight)
            : base(factored, decimalExtend, weight) { }

        //scientific format for general numbers
        public override double Score(double d) { return 1; }

        public override Tuple<ICollection<string>, string> FormatLabels(ICollection<double> d)
        {
            if (d == null) return null;
            var avgpot = (int)Math.Round((from x in d.Where(x => x != 0) select pot(x)).Average());
            var s = Math.Pow(10.0, avgpot);
            var r = (from x in d select x / s).ToList();
            var decimals = (from x in r select decimalPlaces(x)).Max();
            var label = "x10\\^" + avgpot + "\\^";
            return new Tuple<ICollection<string>, string>((from x in r select x.ToString(decimalExtend ? "N" + decimals : "0.#") + (factored ? "" : label)).ToList(), (factored ? label : ""));
        }
    }
}