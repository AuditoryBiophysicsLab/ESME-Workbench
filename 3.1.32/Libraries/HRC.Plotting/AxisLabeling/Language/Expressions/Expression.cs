using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;

namespace HRC.Plotting.AxisLabeling.Language.Expressions
{
    public interface Value {}

    public delegate void ChangedHandler(Expression value);

    public delegate void ViewChangedHandler(Expression value);

    // Named values in the language
    public abstract class Expression : Value, INotifyPropertyChanged
    {
        public abstract IEnumerable<Value> Eval(ICollection<Value> args);
        public IEnumerable<Value> Eval(params Value[] args) { return Eval(args.ToList()); }

        // Flag changes to value
        public ChangedHandler Changed;

        // Flag changes to visible state (not to actual value)
        public ViewChangedHandler ViewChanged;

        // Hack around fact that Binding List doesn't get the parameter to PropertyChanged.
        public string LastPropertyChange;

        public virtual void NotifyChanged()
        {
            LastPropertyChange = "ValueChanged";

            if (Changed != null) Changed(this);

            if (PropertyChanged != null) PropertyChanged(this, new PropertyChangedEventArgs("ValueChanged"));
        }

        public virtual void NotifyViewChanged()
        {
            LastPropertyChange = "ViewChanged";

            if (ViewChanged != null) ViewChanged(this);

            if (PropertyChanged != null) PropertyChanged(this, new PropertyChangedEventArgs("ViewChanged"));
        }

        public event PropertyChangedEventHandler PropertyChanged;

        //
        // Visible properties of the variable
        //

        // Is it highlighted?
        protected bool highlighted = true;

        public virtual bool Highlighted
        {
            get { return highlighted; }
            set
            {
                if (highlighted == value) return;
                highlighted = value;
                NotifyViewChanged();
            }
        }

        // Can be moved around
        protected bool moveable = true;

        public virtual bool Moveable
        {
            get { return moveable; }
            set
            {
                if (moveable == value) return;
                moveable = value;
                NotifyViewChanged();
            }
        }
    }

    // List of expressions
    public class Expressions : Expression
    {
        bool compared = true;

        public bool Compared
        {
            get { return compared; }
            set
            {
                compared = value;
                NotifyChanged();
            }
        }

        readonly List<Expression> expressions = new List<Expression>();
        public List<Expression> All { get { return expressions; } }

        public override IEnumerable<Value> Eval(ICollection<Value> args)
        {
            if (compared) foreach (var v in expressions.SelectMany(e => e.Eval(args))) yield return v;
            else if (expressions.Any()) foreach (var v in expressions[0].Eval(args)) yield return v;
        }
    }
}