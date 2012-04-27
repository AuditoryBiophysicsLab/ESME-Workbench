using System;
using System.ComponentModel;
using System.Windows.Threading;
using HRC.WPF;
using PostSharp.Aspects;
using PostSharp.Aspects.Advices;
using PostSharp.Aspects.Dependencies;
using PostSharp.Extensibility;
using PostSharp.Reflection;

namespace HRC.Aspects
{
    /// <summary>
    /// Aspect that, when apply on a class, fully implements the interface 
    /// <see cref="INotifyPropertyChanged"/> into that class, and overrides all properties to
    /// that they raise the event <see cref="INotifyPropertyChanged.PropertyChanged"/>.
    /// </summary>
    [Serializable]
    [IntroduceInterface(typeof(INotifyPropertyChanged), OverrideAction = InterfaceOverrideAction.Ignore)]
    [MulticastAttributeUsage(MulticastTargets.Class, Inheritance = MulticastInheritance.Strict)]
    [ProvideAspectRole(StandardRoles.DataBinding)]
    [ProvideAspectRole(StandardEffects.InterfaceIntroduction)]
    [ProvideAspectRole(StandardEffects.MemberIntroduction)]
    [AspectTypeDependency(AspectDependencyAction.Commute, typeof(InitializeAttribute))]
    [AspectTypeDependency(AspectDependencyAction.Commute, typeof(AffectsAttribute))]
    public sealed class NotifyPropertyChangedAttribute : InstanceLevelAspect, INotifyPropertyChanged
    {
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
            var handlers = PropertyChanged;
            if (handlers == null) return;
            foreach (PropertyChangedEventHandler handler in handlers.GetInvocationList())
            {
                if (handler.Target is DispatcherObject)
                {
                    var localHandler = handler;
                    ((DispatcherObject)handler.Target).Dispatcher.InvokeIfRequired(() => localHandler(Instance, new PropertyChangedEventArgs(propertyName)));
                }
                else
                    handler(Instance, new PropertyChangedEventArgs(propertyName));
            }
        }

        /// <summary>
        /// Event introduced in the target type (unless it is already present);
        /// raised whenever a property has changed.
        /// </summary>
        [IntroduceMember(Visibility = Visibility.Public, OverrideAction = MemberOverrideAction.Ignore)]
        public event PropertyChangedEventHandler PropertyChanged;

        /// <summary>
        /// Method intercepting any call to a property setter.
        /// </summary>
        /// <param name="args">Aspect arguments.</param>
        [OnLocationSetValueAdvice, MulticastPointcut(Targets = MulticastTargets.Property, Attributes = MulticastAttributes.Instance)]
        //[ProvideAspectRole(StandardRoles.DataBinding)]
        public void OnPropertySet(LocationInterceptionArgs args)
        {
            // Don't go further if the new value is equal to the old one.
            // (Possibly use object.Equals here).
            var currentValue = args.GetCurrentValue();
            if (args.Value == null && currentValue == null) return;
            if (args.Value == currentValue) return;

            // Actually sets the value.
            args.ProceedSetValue();

            //Console.WriteLine("Calling OnPropertyChanged(\"{0}\"), new value is |{1}|", args.LocationName, args.Value);
            // Invoke method OnPropertyChanged (our, the base one, or the overridden one).
            OnPropertyChangedMethod.Invoke(args.Location.Name);
        }
    }
}
