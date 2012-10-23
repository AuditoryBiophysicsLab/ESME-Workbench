using System;
using System.ComponentModel;
using PostSharp.Aspects;
using PostSharp.Aspects.Advices;
using PostSharp.Aspects.Dependencies;
using PostSharp.Reflection;

namespace HRC.Aspects
{
    [Serializable]
    [AttributeUsage(AttributeTargets.Property, AllowMultiple = true)]
    [ProvideAspectRole(StandardRoles.DataBinding)]
    [ProvideAspectRole(StandardEffects.InterfaceIntroduction)]
    [ProvideAspectRole(StandardEffects.MemberIntroduction)]
    [AspectTypeDependency(AspectDependencyAction.Commute, typeof(InitializeAttribute))]
    [AspectTypeDependency(AspectDependencyAction.Commute, typeof(NotifyPropertyChangedAttribute))]
    public sealed class AffectsAttribute : LocationInterceptionAspect, IInstanceScopedAspect
    {
        public AffectsAttribute(params string[] propertyNames) { PropertyNames = propertyNames; }
        public string[] PropertyNames { get; set; }
        object _instance;
        /// <summary>
        /// Field bound at runtime to a delegate of the method <c>OnPropertyChanged</c>.
        /// </summary>
        [ImportMember("OnPropertyChanged", IsRequired = false, Order = ImportMemberOrder.AfterIntroductions)]
        public Action<string> OnPropertyChangedMethod;

        /// <summary>
        /// Method introduced in the target type (unless it is already present);
        /// raises the <see cref="PropertyChanged"/> event.
        /// </summary>
        /// <param name="propertyName">Name of the property.</param>
        [IntroduceMember(Visibility = Visibility.Family, IsVirtual = true, OverrideAction = MemberOverrideAction.Ignore)]
        //[ProvideAspectRole(StandardRoles.DataBinding)]
        public void OnPropertyChanged(string propertyName)
        {
            if (PropertyChanged != null)
                PropertyChanged(_instance, new PropertyChangedEventArgs(propertyName));
        }

        /// <summary>
        /// Event introduced in the target type (unless it is already present);
        /// raised whenever a property has changed.
        /// </summary>
        [IntroduceMember(Visibility = Visibility.Public, OverrideAction = MemberOverrideAction.Ignore)]
        public event PropertyChangedEventHandler PropertyChanged;

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
            _instance = adviceArgs.Instance;
            return MemberwiseClone();
        }
        public void RuntimeInitializeInstance() { }
    }
}