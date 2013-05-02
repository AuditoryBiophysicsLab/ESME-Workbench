using System;
using System.Collections.Generic;
using HRC.Plotting.AxisLabeling.Language.Types;

namespace HRC.Plotting.AxisLabeling.Language.Expressions
{
    public class Symbol : Expression
    {
        protected readonly string name;

        public string Name
        {
            get { return name; }
        }

        public Symbol(string name) { this.name = name; }

        public override IEnumerable<Value> Eval(ICollection<Value> args)
        {
            foreach (var v in args)
                if (v is Frame) yield return (v as Frame).Project(this);
                else throw new InvalidOperationException("Attempt to apply a symbol to a non-data source");
        }

        public override bool Equals(object obj) { return obj is Symbol && name == (obj as Symbol).name; }

        public override int GetHashCode() { return name.GetHashCode(); }

        public override string ToString() { return name; }

        public static readonly Symbol Constant = new Symbol("Constant");
    }
}