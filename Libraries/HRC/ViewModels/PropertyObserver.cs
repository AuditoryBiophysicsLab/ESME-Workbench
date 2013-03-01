using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using System.Windows;

namespace HRC.ViewModels
{
    /// <summary>
    /// Monitors the PropertyChanged event of an object that implements INotifyPropertyChanged,
    /// and executes callback methods (i.e. handlers) registered for properties of that object.
    /// </summary>
    /// <typeparam name="TPropertySource">The type of object to monitor for property changes.</typeparam>
    public class PropertyObserver<TPropertySource> : IWeakEventListener
        where TPropertySource : class
    {
        #region Constructor

        /// <summary>
        /// Initializes a new instance of PropertyObserver, which
        /// observes the 'propertySource' object for property changes.
        /// </summary>
        /// <param name="propertySource">The object to monitor for property changes.</param>
        public PropertyObserver(TPropertySource propertySource)
        {
            if (propertySource == null)
                throw new ArgumentNullException("propertySource");

            _propertySourceRef = new WeakReference(propertySource);
            _propertyNameToHandlerWithArgumentMap = new Dictionary<string, Action<TPropertySource>>();
            _propertyNameToHandlerWithoutArgumentMap = new Dictionary<string, Action>();
        }

        #endregion // Constructor

        #region Public Methods

        #region RegisterHandler

        /// <summary>
        /// Registers a callback to be invoked when the PropertyChanged event has been raised for the specified property.
        /// </summary>
        /// <param name="handler">The callback to invoke when the property has changed.</param>
        /// <param name="expression">A lambda expression like 'n => n.PropertyName'.</param>
        /// <returns>The object on which this method was invoked, to allow for multiple invocations chained together.</returns>
        public PropertyObserver<TPropertySource> RegisterHandler(Expression<Func<TPropertySource, object>> expression, Action<TPropertySource> handler)
        {
            if (expression == null)
                throw new ArgumentNullException("expression");

            var propertyName = GetPropertyName(expression);
            if (String.IsNullOrEmpty(propertyName))
                throw new ArgumentException("'expression' did not provide a property name.");

            if (handler == null)
                throw new ArgumentNullException("handler");

            var propertySource = GetPropertySource();
            if (propertySource != null && propertySource is INotifyPropertyChanged)
            {
                _propertyNameToHandlerWithArgumentMap[propertyName] = handler;
                PropertyChangedEventManager.AddListener((INotifyPropertyChanged)propertySource, this, propertyName);
            }

            return this;
        }

        /// <summary>
        /// Registers a callback to be invoked when the PropertyChanged event has been raised for the specified property.
        /// </summary>
        /// <param name="handler">The callback to invoke when the property has changed.</param>
        /// <param name="expression">A lambda expression like 'n => n.PropertyName'.</param>
        /// <returns>The object on which this method was invoked, to allow for multiple invocations chained together.</returns>
        public PropertyObserver<TPropertySource> RegisterHandler(Expression<Func<TPropertySource, object>> expression, Action handler)
        {
            if (expression == null)
                throw new ArgumentNullException("expression");

            var propertyName = GetPropertyName(expression);
            if (String.IsNullOrEmpty(propertyName))
                throw new ArgumentException("'expression' did not provide a property name.");

            if (handler == null)
                throw new ArgumentNullException("handler");

            var propertySource = GetPropertySource();
            if (propertySource != null && propertySource is INotifyPropertyChanged)
            {
                _propertyNameToHandlerWithoutArgumentMap[propertyName] = handler;
                PropertyChangedEventManager.AddListener((INotifyPropertyChanged)propertySource, this, propertyName);
            }

            return this;
        }

        #endregion // RegisterHandler

        #region UnregisterHandler
        /// <summary>
        /// Removes the callback associated with the specified property.
        /// </summary>
        /// <returns>The object on which this method was invoked, to allow for multiple invocations chained together.</returns>
        public PropertyObserver<TPropertySource> UnregisterHandler(Expression<Func<TPropertySource, object>> expression)
        {
            if (expression == null)
                throw new ArgumentNullException("expression");

            var propertyName = GetPropertyName(expression);
            if (String.IsNullOrEmpty(propertyName))
                throw new ArgumentException("'expression' did not provide a property name.");

            var propertySource = GetPropertySource();
            if (propertySource != null && propertySource is INotifyPropertyChanged)
            {
                if (_propertyNameToHandlerWithArgumentMap.ContainsKey(propertyName))
                {
                    _propertyNameToHandlerWithArgumentMap.Remove(propertyName);
                    PropertyChangedEventManager.RemoveListener((INotifyPropertyChanged)propertySource, this, propertyName);
                }
                if (_propertyNameToHandlerWithoutArgumentMap.ContainsKey(propertyName))
                {
                    _propertyNameToHandlerWithoutArgumentMap.Remove(propertyName);
                    PropertyChangedEventManager.RemoveListener((INotifyPropertyChanged)propertySource, this, propertyName);
                }
            }

            return this;
        }
        #endregion // UnregisterHandler

        #endregion // Public Methods

        #region Private Helpers

        #region GetPropertyName
        static string GetPropertyName(Expression<Func<TPropertySource, object>> expression)
        {
            var lambda = expression as LambdaExpression;
            MemberExpression memberExpression = null;
            if (lambda.Body is UnaryExpression)
            {
                var unaryExpression = lambda.Body as UnaryExpression;
                if (unaryExpression != null) memberExpression = unaryExpression.Operand as MemberExpression;
            }
            else memberExpression = lambda.Body as MemberExpression;

            Debug.Assert(memberExpression != null, "Please provide a lambda expression like 'n => n.PropertyName'");

            var propertyInfo = memberExpression.Member as PropertyInfo;

            return propertyInfo == null ? null : propertyInfo.Name;
        }

        #endregion // GetPropertyName

        #region GetPropertySource

        TPropertySource GetPropertySource()
        {
            try
            {
                return (TPropertySource)_propertySourceRef.Target;
            }
            catch 
            {
                return default(TPropertySource);
            }
        }

        #endregion // GetPropertySource

        #endregion // Private Helpers

        #region Fields

        readonly Dictionary<string, Action<TPropertySource>> _propertyNameToHandlerWithArgumentMap;
        readonly Dictionary<string, Action> _propertyNameToHandlerWithoutArgumentMap;
        readonly WeakReference _propertySourceRef;

        #endregion // Fields

        #region IWeakEventListener Members

        bool IWeakEventListener.ReceiveWeakEvent(Type managerType, object sender, EventArgs e)
        {
            if (managerType == typeof(PropertyChangedEventManager))
            {
                var propertyName = ((PropertyChangedEventArgs)e).PropertyName;
                var propertySource = (TPropertySource)sender;

                if (String.IsNullOrEmpty(propertyName))
                {
                    // When the property name is empty, all properties are considered to be invalidated.
                    // Iterate over a copy of the list of handlers, in case a handler is registered by a callback.
                    foreach (var curHandler in _propertyNameToHandlerWithArgumentMap.Values.ToArray()) curHandler(propertySource);
                    foreach (var curHandler in _propertyNameToHandlerWithoutArgumentMap.Values.ToArray()) curHandler();
                    return true;
                }
                Action<TPropertySource> handlerWithArgument;
                if (_propertyNameToHandlerWithArgumentMap.TryGetValue(propertyName, out handlerWithArgument))
                {
                    handlerWithArgument(propertySource);
                    return true;
                }
                Action handlerWithoutArgument;
                if (_propertyNameToHandlerWithoutArgumentMap.TryGetValue(propertyName, out handlerWithoutArgument))
                {
                    handlerWithoutArgument();
                    return true;
                }
            }

            return false;
        }

        #endregion
    }
}