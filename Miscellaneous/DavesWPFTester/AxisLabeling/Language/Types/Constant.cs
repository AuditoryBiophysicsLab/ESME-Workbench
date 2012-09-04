using System;
using System.Collections.Generic;
using System.Linq;

namespace DavesWPFTester.AxisLabeling.Language.Types
{
    public class Constant : Vector
    {
        readonly Object value;
        readonly int length;
        public override int Length { get { return length; } }

        public override string Type { get { return "Constant"; } }

        public override Range Range { get { return new Range(-0.5, 0.5); } }

        public Constant(Object value, int length)
        {
            this.value = value;
            this.length = length;
        }

        public override List<T> Select<T>() { return Enumerable.Repeat((T)Convert.ChangeType(value, typeof(T)), length).ToList(); }
    }
}