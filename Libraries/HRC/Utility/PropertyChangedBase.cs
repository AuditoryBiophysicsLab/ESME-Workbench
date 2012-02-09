using System;
using System.ComponentModel;
using System.Linq.Expressions;
using System.Runtime.CompilerServices;
using System.Windows.Threading;
using Cinch;

namespace HRC.Utility
{
    [Serializable]
    public abstract class PropertyChangedBase : INotifyPropertyChanged
    {
        #region INotifyPropertyChanged Members

        [NonSerialized]
        private PropertyChangedEventHandler _propertyChanged;
        public event PropertyChangedEventHandler PropertyChanged
        {
            [MethodImpl(MethodImplOptions.Synchronized)]
            add
            {
                _propertyChanged = (PropertyChangedEventHandler)Delegate.Combine(_propertyChanged, value);
            }
            [MethodImpl(MethodImplOptions.Synchronized)]
            remove
            {
                _propertyChanged = (PropertyChangedEventHandler)Delegate.Remove(_propertyChanged, value);
            }
        }
        protected void NotifyPropertyChanged(PropertyChangedEventArgs e)
        {
            var handlers = _propertyChanged;
            if (handlers == null) return;
            foreach (PropertyChangedEventHandler handler in handlers.GetInvocationList())
            {
                if (handler.Target is DispatcherObject)
                    ((DispatcherObject)handler.Target).Dispatcher.InvokeIfRequired(() => handler(this, e));
                else
                    handler(this, e);
            }
        }
        protected void NotifyPropertyChanged(string propertyName)
        {
            var handlers = _propertyChanged;
            if (handlers == null) return;
            foreach (PropertyChangedEventHandler handler in handlers.GetInvocationList())
            {
                if (handler.Target is DispatcherObject)
                    ((DispatcherObject)handler.Target).Dispatcher.InvokeIfRequired(() => handler(this, new PropertyChangedEventArgs(propertyName)));
                else
                    handler(this, new PropertyChangedEventArgs(propertyName));
            }
        }

        #endregion

        #region Public Methods
        /// <summary>
        /// Creates PropertyChangedEventArgs
        /// </summary>
        /// <param name="propertyExpression">Expression to make 
        /// PropertyChangedEventArgs out of</param>
        /// <returns>PropertyChangedEventArgs</returns>
        public static PropertyChangedEventArgs CreateArgs<T>(Expression<Func<T, Object>> propertyExpression)
        {
            return new PropertyChangedEventArgs(GetPropertyName(propertyExpression));
        }

        /// <summary>
        /// Creates PropertyChangedEventArgs
        /// </summary>
        /// <param name="propertyExpression">Expression to make 
        /// PropertyChangedEventArgs out of</param>
        /// <returns>PropertyChangedEventArgs</returns>
        public static string GetPropertyName<T>(Expression<Func<T, Object>> propertyExpression)
        {
            var lambda = propertyExpression as LambdaExpression;
            MemberExpression memberExpression;
            if (lambda.Body is UnaryExpression)
                memberExpression = (MemberExpression)((UnaryExpression)lambda.Body).Operand;
            else
                memberExpression = (MemberExpression)lambda.Body;

            return memberExpression.Member.Name;
        }
        #endregion
    }
}
