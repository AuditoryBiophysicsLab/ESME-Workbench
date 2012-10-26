using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;

namespace HRC.ViewModels
{
    public class WeakEventAction
    {
        #region Data

        readonly MethodInfo _method;
        readonly Type _delegateType;
        #endregion

        #region Public Properties
        public WeakReference TargetObject { get; private set; }
        #endregion

        #region Internal Methods

        /// <summary>
        /// Constructs a new WeakAction
        /// </summary>
        /// <param name="target">The sender</param>
        /// <param name="method">The _method to call on sender</param>
        /// <param name="parameterType">The parameter type if using generics</param>
        public WeakEventAction(object target, MethodInfo method, Type parameterType)
        {
            TargetObject = new WeakReference(target);
            _method = method;
            _delegateType = parameterType == null ? typeof (Action) : typeof (Action<>).MakeGenericType(parameterType);
        }

        /// <summary>
        /// Creates callback delegate
        /// </summary>
        /// <returns>Callback delegate</returns>
        internal Delegate CreateAction()
        {
            var target = TargetObject.Target;
            return target != null ? Delegate.CreateDelegate(_delegateType, TargetObject.Target, _method) : null;
        }
        #endregion
    }

    public class WeakActionEvent<T>
    {
        public static WeakActionEvent<T> operator +(WeakActionEvent<T> wre, Action<T> handler)
        {
            wre.Add(handler);
            return wre;
        }

        void Add(Action<T> handler)
        {
            var parameters = handler.Method.GetParameters();
            if (parameters != null && parameters.Length > 1) throw new InvalidOperationException("Action should have only 0 or 1 parameter");
            if (_delegates.Any(del => del.TargetObject.Target == handler.Target)) return;
            var parameterType = (parameters.Length == 0) ? null : parameters[0].ParameterType;
            _delegates.Add(new WeakEventAction(handler.Target, handler.Method, parameterType));
        }

        public static WeakActionEvent<T> operator -(WeakActionEvent<T> wre, Action<T> handler)
        {
            wre.Remove(handler);
            return wre;
        }

        void Remove(Action<T> handler)
        {
            _delegates.RemoveAll(d => d.TargetObject.Target == handler.Target);
        }

        readonly List<WeakEventAction> _delegates = new List<WeakEventAction>();

        internal void Invoke(T arg)
        {
            for (var i = _delegates.Count - 1; i > -1; --i)
            {
                var weakAction = _delegates[i];
                if (!weakAction.TargetObject.IsAlive) _delegates.RemoveAt(i);
                else
                {
                    var action = weakAction.CreateAction();
                    action.DynamicInvoke(arg);
                }
            }
        }
    }
}
