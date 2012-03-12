using System;
using PostSharp.Aspects;
using PostSharp.Extensibility;

namespace HRC.Aspects
{
    [Serializable]
    [MulticastAttributeUsage(MulticastTargets.Property)]
    public class InitializeAttribute : LocationInterceptionAspect, IInstanceScopedAspect
    {
        public InitializeAttribute() { }
        public InitializeAttribute(object defaultValue) { _defaultValue = defaultValue; }

        readonly object _defaultValue;
        bool _firstTime = true;
        public override void OnSetValue(LocationInterceptionArgs args)
        {
            _firstTime = false;
            args.ProceedSetValue();
        }
        public override void OnGetValue(LocationInterceptionArgs args)
        {
            if (_firstTime)
            {
                _firstTime = false;
                if (_defaultValue != null)
                {
                    //Console.WriteLine("Initializing {0} to |{1}|", args.LocationName, _defaultValue); 
                    args.SetNewValue(_defaultValue);
                }
                else if (args.Location.PropertyInfo.GetType().IsClass)
                {
                    var t = args.Location.PropertyInfo.PropertyType;
                    if (t.GetConstructor(Type.EmptyTypes) != null)
                    {
                        //Console.WriteLine("Initializing {0}", args.LocationName);
                        args.SetNewValue(Activator.CreateInstance(t));
                    }
                    else throw new InvalidOperationException(string.Format("The property {0} (type {1}) has no default constructor and must either be provided with a default value or have the [Initialize] attribute removed", args.LocationFullName, t));
                }
            }
            //Console.WriteLine("Before get called.  Current value is |{0}|", args.GetCurrentValue());
            args.ProceedGetValue();
            //Console.WriteLine("After get called.  Current value is |{0}|", args.Value);
        }

        public object CreateInstance(AdviceArgs adviceArgs) { return MemberwiseClone(); }
        public void RuntimeInitializeInstance() { }
    }
}
