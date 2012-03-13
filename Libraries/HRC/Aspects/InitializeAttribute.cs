using System;
using PostSharp.Aspects;
using PostSharp.Aspects.Dependencies;
using PostSharp.Extensibility;

namespace HRC.Aspects
{
    [Serializable]
    [MulticastAttributeUsage(MulticastTargets.Property)]
    [AspectTypeDependency(AspectDependencyAction.Commute, typeof(NotifyPropertyChangedAttribute))]
    [AspectTypeDependency(AspectDependencyAction.Commute, typeof(AffectsAttribute))]
    public sealed class InitializeAttribute : LocationInterceptionAspect, IInstanceScopedAspect
    {
        public override bool CompileTimeValidate(PostSharp.Reflection.LocationInfo locationInfo)
        {
            if (IsGuid && locationInfo.LocationType != typeof(string))
                Message.Write(SeverityType.Error, "Initialize01", "Only string types can be initialized with IsGuid = true");
            if (IsGuid && _defaultValue != null)
                Message.Write(SeverityType.Error, "Initialize02", "Cannot specify a default value with IsGuid = true");
            return true;
        }

        public InitializeAttribute() { }
        public InitializeAttribute(object defaultValue) { _defaultValue = defaultValue; }

        public bool IsGuid { get; set; }
        readonly object _defaultValue;
        bool _valueSet;
        public override void OnSetValue(LocationInterceptionArgs args)
        {
            _valueSet = true;
            args.ProceedSetValue();
        }
        public override void OnGetValue(LocationInterceptionArgs args)
        {
            if (!_valueSet)
            {
                _valueSet = true;
                if (_defaultValue != null || IsGuid)
                {
                    args.SetNewValue(IsGuid ? Guid.NewGuid().ToString() : _defaultValue);
                    //Console.WriteLine("Initialized {0} to |{1}|", args.LocationName, args.GetCurrentValue()); 
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
