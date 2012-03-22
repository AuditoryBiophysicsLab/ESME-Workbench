using System;
using PostSharp;
using PostSharp.Aspects;
using PostSharp.Reflection;
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
        public override bool CompileTimeValidate(LocationInfo locationInfo)
        {
            if (IsGuid && (locationInfo.LocationType != typeof(String) && locationInfo.LocationType != typeof(Guid)))
            {
                //Message.Write(SeverityType.Warning, "Initialize00", string.Format("[Initialize] on {0}.{1}", locationInfo.DeclaringType.Name, locationInfo.Name));
                //Message.Write(SeverityType.Warning, "Initialize00", string.Format("IsGuid == {0}", IsGuid));
                //Message.Write(SeverityType.Warning, "Initialize00", string.Format("LocationType == {0}", locationInfo.LocationType));
                //Message.Write(SeverityType.Warning, "Initialize00", string.Format("LocationType != typeof(string) == {0}", locationInfo.LocationType != typeof(string)));
                //Message.Write(SeverityType.Warning, "Initialize00", string.Format("LocationType != typeof(Guid) == {0}", locationInfo.LocationType != typeof(Guid)));
                Message.Write(MessageLocation.Of(locationInfo), SeverityType.Error, "Initialize01", "Only string and Guid types can be initialized with IsGuid = true");
            }
            if (IsGuid && _defaultValue != null)
                Message.Write(MessageLocation.Of(locationInfo), SeverityType.Error, "Initialize02", "Cannot specify a default value with IsGuid = true");
            if (locationInfo.LocationType.IsInterface)
                Message.Write(MessageLocation.Of(locationInfo), SeverityType.Error, "Initialize03", string.Format("Cannot initialize a property that is an interface type (in {0}.{1})", locationInfo.DeclaringType.Module, locationInfo.Name));
            return true;
        }

        public override void CompileTimeInitialize(LocationInfo targetLocation, AspectInfo aspectInfo)
        {
            if (targetLocation.LocationType == typeof(Guid)) IsGuid = true;
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
                if (IsGuid)
                {
                    //Console.WriteLine("Initializing Guid"); 
                    if (args.Location.LocationType == typeof(string))
                        args.SetNewValue(Guid.NewGuid().ToString());
                    else if (args.Location.LocationType == typeof(Guid))
                        args.SetNewValue(Guid.NewGuid());
                }
                else if (_defaultValue != null)
                {
                    args.SetNewValue(_defaultValue);
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
