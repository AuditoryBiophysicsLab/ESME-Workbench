using System;
using PostSharp.Aspects;
using PostSharp.Aspects.Advices;
using PostSharp.Aspects.Dependencies;

namespace HRC.Aspects
{
    [Serializable]
    [AttributeUsage(AttributeTargets.Property, AllowMultiple = true)]
    [ProvideAspectRole(StandardRoles.DataBinding)]
    [AspectEffectDependency(AspectDependencyAction.Order, AspectDependencyPosition.After, StandardEffects.InterfaceIntroduction)]
    //[AspectTypeDependency(AspectDependencyAction.Commute, typeof(NotifyPropertyChangedAttribute))]
    public sealed class AffectsAttribute : LocationInterceptionAspect, IInstanceScopedAspect
    {
        public AffectsAttribute(params string[] propertyNames) { PropertyNames = propertyNames; }
        public string[] PropertyNames { get; set; }
        /// <summary>
        /// Field bound at runtime to a delegate of the method <c>OnPropertyChanged</c>.
        /// </summary>
        [ImportMember("OnPropertyChanged", IsRequired = false, Order = ImportMemberOrder.AfterIntroductions)]
        public Action<string> OnPropertyChangedMethod;

        public override void OnSetValue(LocationInterceptionArgs args)
        {
            // Don't go further if the new value is equal to the old one.
            // (Possibly use object.Equals here).
            if (args.Value == args.GetCurrentValue()) return;

            // Actually sets the value.
            args.ProceedSetValue();

            //var instance = args.Instance as INotifyPropertyChanged;
            //instance.PropertyChanged(instance, new PropertyChangedEventArgs("foo"));
            // Invoke method OnPropertyChanged (our, the base one, or the overridden one).
            foreach (var propertyName in PropertyNames)
                OnPropertyChangedMethod.Invoke(propertyName);
        }

        public object CreateInstance(AdviceArgs adviceArgs)
        {
            return MemberwiseClone();
        }
        public void RuntimeInitializeInstance() { }
    }
}