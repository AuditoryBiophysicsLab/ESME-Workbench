using System;
using System.Diagnostics;

namespace HRC.Utility
{
    #region Types

    public delegate void UnregisterCallback<TEventHandler>(EventHandler<TEventHandler> eventHandler)
      where TEventHandler : EventArgs;

    public interface IWeakEventHandler<TEventHandler> where TEventHandler : EventArgs
    {
        EventHandler<TEventHandler> Handler { get; }
    }
    #endregion

    /// <summary>
    /// Provides methods for creating WeakEvent handlers
    /// </summary>
    /// <typeparam name="TSender">The type of the event source</typeparam>
    /// <typeparam name="TEventArgs">The EventArgs</typeparam>
    public class WeakEventHandler<TSender, TEventArgs> : IWeakEventHandler<TEventArgs>
        where TSender : class
        where TEventArgs : EventArgs
    {
        #region Data
        private delegate void OpenEventHandler(TSender @this, object sender, TEventArgs e);
        private readonly WeakReference _targetRef;
        private readonly OpenEventHandler _openHandler;
        private readonly EventHandler<TEventArgs> _handler;
        private UnregisterCallback<TEventArgs> _unregister;
        #endregion

        #region Ctor
        /// <summary>
        /// Constructs a new WeakEventHandler
        /// </summary>
        /// <param name="eventHandler">The Event handler</param>
        /// <param name="unregister">Unregister delegate</param>
        public WeakEventHandler(EventHandler<TEventArgs> eventHandler, UnregisterCallback<TEventArgs> unregister)
        {
            _targetRef = new WeakReference(eventHandler.Target);
            _openHandler = (OpenEventHandler)Delegate.CreateDelegate(typeof(OpenEventHandler),
              null, eventHandler.Method);
            _handler = Invoke;
            _unregister = unregister;
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Invokes the event handler if the source is still alive
        /// </summary>
        public void Invoke(object sender, TEventArgs e)
        {
            var target = (TSender)_targetRef.Target;

            if (target != null)
                _openHandler.Invoke(target, sender, e);
            else if (_unregister != null)
            {
                _unregister(_handler);
                _unregister = null;
            }
        }

        public EventHandler<TEventArgs> Handler
        {
            get { return _handler; }
        }

        public static implicit operator EventHandler<TEventArgs>(WeakEventHandler<TSender, TEventArgs> weh)
        {
            return weh._handler;
        }
        #endregion
    }


    /// <summary>
    /// Provides extension method for EventHandler&lt;E&gt;
    /// </summary>
    /// <example>
    /// <![CDATA[
    /// 
    ///    //SO DECLARE LISTENERS LIKE
    ///    workspace.CloseWorkSpace +=
    ///        new EventHandler<EventArgs>(OnCloseWorkSpace).
    ///           MakeWeak(eh => workspace.CloseWorkSpace -= eh);
    ///           
    ///    private void OnCloseWorkSpace(object sender, EventArgs e)
    ///    {
    ///
    ///    }
    ///    
    ///    //OR YOU COULD CREATE ACTUAL EVENTS LIKE
    ///    public class EventProvider
    ///    {
    ///         private EventHandler<EventArgs> closeWorkSpace;
    ///         public event EventHandler<EventArgs> CloseWorkSpace
    ///         {
    ///             add
    ///             {
    ///                 closeWorkSpace += value.MakeWeak(eh => closeWorkSpace -= eh);
    ///             }
    ///             remove
    ///             {
    ///             }
    ///         }
    ///    }
    /// ]]>
    /// </example>
    public static class EventHandlerUtils
    {
        /// <summary>
        /// Extension method for EventHandler
        /// </summary>
        /// <typeparam name="TEventHandler">The type</typeparam>
        /// <param name="eventHandler">The EventHandler</param>
        /// <param name="unregister">EventHandler unregister delegate</param>
        /// <returns>An EventHandler</returns>
        public static EventHandler<TEventHandler> MakeWeak<TEventHandler>(this EventHandler<TEventHandler> eventHandler,
            UnregisterCallback<TEventHandler> unregister) where TEventHandler : EventArgs
        {
            if (eventHandler == null) throw new ArgumentNullException("eventHandler");
            if (eventHandler.Method.IsStatic || eventHandler.Target == null) throw new ArgumentException("Only instance methods are supported.", "eventHandler");
            var wehType = typeof(WeakEventHandler<,>).MakeGenericType(eventHandler.Method.DeclaringType, typeof(TEventHandler));
            var wehConstructor = wehType.GetConstructor(new[] { typeof(EventHandler<TEventHandler>), typeof(UnregisterCallback<TEventHandler>) });
            Debug.Assert(wehConstructor != null, "wehConstructor != null");
            var weh = (IWeakEventHandler<TEventHandler>)wehConstructor.Invoke(new object[] { eventHandler, unregister });
            return weh.Handler;
        }
    }
}
