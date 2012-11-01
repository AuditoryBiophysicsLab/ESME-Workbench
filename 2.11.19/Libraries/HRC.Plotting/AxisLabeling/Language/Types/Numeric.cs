using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;

namespace HRC.Plotting.AxisLabeling.Language.Types
{
    public class Numeric : Vector
    {
        public List<double> vector;

        public override int Length { get { return vector.Count(); } }

        public enum NumericDomain
        {
            COUNT,
            RATIO,
            AMOUNT,
            BALANCE,
            TEMPORAL
        };

        readonly NumericDomain domain;
        public NumericDomain Domain { get { return domain; } }

        readonly Range range;
        public override Range Range { get { return range; } }

        public override string Type { get { return new CultureInfo("en").TextInfo.ToTitleCase(Domain.ToString().ToLower()); } }

        //public SType labelType;
        public int numUniqueValues;

        public Numeric(List<double> vector, NumericDomain domain)
        {
            this.vector = vector;
            this.domain = domain;
            range = new Range(vector.Min(), vector.Max());
            numUniqueValues = vector.Distinct().Count();
        }

        public Numeric(List<double> vector, NumericDomain domain, Range range, int numUniqueValues /*, Language.Types.SType type*/)
        {
            this.vector = vector;
            this.domain = domain;
            this.range = range;
            this.numUniqueValues = numUniqueValues;
            //this.labelType = type;
        }

        public Factor partition(int levels)
        {
            throw new NotImplementedException();
        }

        public virtual string samplePoints(int strata)
        {
            throw new NotImplementedException();
        }

        public override List<T> Select<T>() { return vector.Select(v => (T)Convert.ChangeType(v, typeof(T))).ToList(); }
    }
}