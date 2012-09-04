using System;
using System.Collections.Generic;
using System.Linq;
using DavesWPFTester.AxisLabeling.Language.Types;

namespace DavesWPFTester.AxisLabeling.Language.Expressions
{
    public class EmptyFrame : Expression
    {
        public override IEnumerable<Value> Eval(ICollection<Value> args) { yield return new Frame(); }
    }

    public class Formula : Expression
    {
        readonly Expressions lhs;
        readonly List<Expressions> rhs;

        readonly Expression labels;

        public Expressions Response { get { return lhs; } }
        public List<Expressions> Predictors { get { return rhs; } }
        public Expression Labels { get { return labels; } }

        public Formula()
        {
            lhs = new Expressions();
            lhs.All.Add(Symbol.Constant);
            rhs = new List<Expressions> { new Expressions() };
            rhs[0].All.Add(Symbol.Constant);
            labels = new EmptyFrame();
        }

        public Formula(Expression predictor, Expression response)
        {
            lhs = new Expressions();
            lhs.All.Add(predictor);
            rhs = new List<Expressions> { new Expressions() };
            rhs[0].All.Add(response);
            labels = new EmptyFrame();
        }

        public override IEnumerable<Value> Eval(ICollection<Value> args)
        {
            foreach (var v in lhs.Eval(args))
            {
                if (!(v is Frame)) throw new InvalidOperationException("Formula must get frames");

                foreach (var r in Cross(args, 0)) yield return new ModelFrame(v as Frame, r, labels.Eval(args) as Frame);
            }
        }

        public IEnumerable<Frame> Cross(ICollection<Value> args, int i)
        {
            if (i < rhs.Count())
                foreach (var v in rhs[i].Eval(args))
                {
                    if (!(v is Frame)) throw new InvalidOperationException("Formula must get frames");

                    foreach (var w in Cross(args, i + 1)) yield return Frame.Cbind(v as Frame, w);
                }
            else yield return new Frame();
        }

        public override string ToString() { return "Formula"; }
    }
}