using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using HRC.ViewModels;

namespace HRC.Utility
{
#if true
    public abstract class MediatorBase
    {
        public static void Send<T>(string key, T message)
        {
            try
            {
                Mediator.Instance.NotifyColleagues(key, message);
            }
            catch (Exception) {}
        }

        public static void Send(string key) { Mediator.Instance.NotifyColleagues(key, true); }
        public static void SendAsync<T>(string key, T message) { Mediator.Instance.NotifyColleaguesAsync(key, message); }
        public static void SendAsync(string key) { Mediator.Instance.NotifyColleaguesAsync(key, true); }
    }

    /// <summary>
    /// This attribute allows a method to be targeted as a recipient for a message.
    /// It requires that the Type is registered with the MessageMediator through the
    /// <seealso cref="HRCMediator.Register"/> method
    /// </summary>
    /// <example>
    /// <![CDATA[
    /// [HRCMediatorMessageSinkAttribute("MyScope", "DoBackgroundCheck")]
    /// void OnBackgroundCheck(object parameter) { ... }
    /// 
    /// mediator.NotifyColleagues("MyScope", "DoBackgroundCheck", new CheckParameters());
    /// ...
    /// mediator.NotifyColleagues(new SomeDataClass(...));
    /// 
    /// ]]>
    /// </example>
    [AttributeUsage(AttributeTargets.Method)]
    public sealed class HRCMediatorMessageSinkAttribute : Attribute
    {
        /// <summary>
        /// Scope
        /// </summary>
        public string Scope { get; private set; }

        /// <summary>
        /// Message key
        /// </summary>
        public object MessageKey { get; private set; }

        /// <summary>
        /// Default constructor
        /// </summary>
        public HRCMediatorMessageSinkAttribute()
        {
            Scope = null;
            MessageKey = null;
        }

        /// <summary>
        /// Constructor that takes a message key
        /// </summary>
        /// <param name="scope">Scope of this message</param>
        /// <param name="messageKey">Message Key</param>
        public HRCMediatorMessageSinkAttribute(string scope, object messageKey)
        {
            Scope = scope;
            MessageKey = messageKey;
        }
    }

    /// <summary>
    ///   This class creates a simple Mediator which loosely connects different objects together. 
    ///   The message handlers are organized using string-based message keys and are held in a WeakReference
    ///   collection.
    /// </summary>
    public class HRCMediator
    {
        #region Data

        public static HRCMediator Instance { get; private set; }
        readonly Dictionary<string, Dictionary<object, List<WeakAction>>> _registeredScopes = new Dictionary<string, Dictionary<object, List<WeakAction>>>();

        #endregion

        #region Ctor

        static HRCMediator() { Instance = new HRCMediator(); }
        HRCMediator() { }

        #endregion

        #region Private Methods

        /// <summary>
        ///   Performs the actual registration of a target
        /// </summary>
        /// <param name="scope">Scope of the handler being registered</param>
        /// <param name = "key">Key to store in dictionary</param>
        /// <param name = "actionType">Delegate type</param>
        /// <param name = "handler">Method</param>
        void RegisterHandler(string scope, object key, Type actionType, Delegate handler)
        {
            var action = new WeakAction(handler.Target, actionType, handler.Method);
            lock (_registeredScopes)
            {
                Dictionary<object, List<WeakAction>> scopeResult;
                if (_registeredScopes.TryGetValue(scope, out scopeResult))
                {
                    if (scopeResult.Count > 0)
                    {
                        lock (scopeResult)
                        {
                            List<WeakAction> wr;
                            if (scopeResult.TryGetValue(key, out wr))
                            {
                                if (wr.Count > 0)
                                {
                                    var wa = wr[0];
                                    if (wa.ActionType != actionType && !wa.ActionType.IsAssignableFrom(actionType)) throw new ArgumentException("Invalid key passed to RegisterHandler - existing handler has incompatible parameter type");
                                }

                                wr.Add(action);
                            }
                            else
                            {
                                wr = new List<WeakAction>
                                     {
                                         action
                                     };
                                scopeResult.Add(key, wr);
                            }
                        }
                    }
                }
            }
        }

        /// <summary>
        ///   Performs the unregistration from a target
        /// </summary>
        /// <param name="scope">Scope of the handler being unregistered</param>
        /// <param name = "key">Key to store in dictionary</param>
        /// <param name = "actionType">Delegate type</param>
        /// <param name = "handler">Method</param>
        void UnregisterHandler(string scope, object key, Type actionType, Delegate handler)
        {
            lock (_registeredScopes)
            {
                Dictionary<object, List<WeakAction>> scopeResult;
                if (!_registeredScopes.TryGetValue(scope, out scopeResult)) return;
                lock (scopeResult)
                {
                    List<WeakAction> wr;
                    if (scopeResult.TryGetValue(key, out wr))
                    {
                        wr.RemoveAll(wa => handler == wa.GetMethod() && actionType == wa.ActionType);

                        if (wr.Count == 0) scopeResult.Remove(key);
                    }
                }
            }
        }

        /// <summary>
        ///   This method broadcasts a message to all message targets for a given
        ///   message key and passes a parameter.
        /// </summary>
        /// <param name="scope">Scope of the handler being notified</param>
        /// <param name = "key">Message key</param>
        /// <param name = "message">Message parameter</param>
        /// <returns>True/False if any handlers were invoked.</returns>
        bool NotifyColleagues(string scope, object key, object message)
        {
            List<WeakAction> wr = null;
            lock (_registeredScopes)
            {
                Dictionary<object, List<WeakAction>> scopeResult;
                if (_registeredScopes.TryGetValue(scope, out scopeResult))
                {
                    lock (scopeResult)
                    {
                        if (!scopeResult.TryGetValue(key, out wr)) return false;
                    }
                }
            }

            if (wr != null)
            {
                foreach (var action in wr.Select(cb => cb.GetMethod()).Where(action => action != null)) 
                    action.DynamicInvoke(message);

                lock (_registeredScopes)
                {
                    Dictionary<object, List<WeakAction>> scopeResult;
                    if (_registeredScopes.TryGetValue(scope, out scopeResult))
                    {
                        lock (scopeResult)
                        {
                            wr.RemoveAll(wa => wa.HasBeenCollected);
                        }
                    }

                }
            }
            return true;
        }

        #endregion

        #region Public Properties/Methods

        /// <summary>
        ///   This registers a Type with the mediator.  Any methods decorated with <seealso cref = "HRCMediatorMessageSinkAttribute" /> will be 
        ///   registered as target method handlers for the given message key.
        /// </summary>
        /// <param name="scope"></param>
        /// <param name = "view">Object to register</param>
        public void Register(string scope, object view)
        {
            // Look at all instance/static methods on this object type.
            foreach (var mi in view.GetType().GetMethods(BindingFlags.Instance | BindingFlags.Static | BindingFlags.NonPublic | BindingFlags.Public))
            {
                // See if we have a target attribute - if so, register the method as a handler.
                foreach (var att in mi.GetCustomAttributes(typeof (MediatorMessageSinkAttribute), true))
                {
                    var mha = (MediatorMessageSinkAttribute) att;
                    var pi = mi.GetParameters();
                    if (pi.Length != 1) throw new InvalidCastException("Cannot cast " + mi.Name + " to Action<T> delegate type.");

                    var actionType = typeof (Action<>).MakeGenericType(pi[0].ParameterType);
                    var key = (mha.MessageKey) ?? actionType;

                    RegisterHandler(scope, key, actionType, mi.IsStatic ? Delegate.CreateDelegate(actionType, mi) : Delegate.CreateDelegate(actionType, view, mi.Name));
                }
            }
        }

        /// <summary>
        ///   This method unregisters a type from the message mediator.
        /// </summary>
        /// <param name="scope"></param>
        /// <param name = "view">Object to unregister</param>
        public void Unregister(string scope, object view)
        {
            foreach (var mi in view.GetType().GetMethods(BindingFlags.Instance | BindingFlags.Static | BindingFlags.NonPublic | BindingFlags.Public))
            {
                foreach (var att in mi.GetCustomAttributes(typeof (MediatorMessageSinkAttribute), true))
                {
                    var mha = (MediatorMessageSinkAttribute) att;
                    var pi = mi.GetParameters();
                    if (pi.Length != 1) throw new InvalidCastException("Cannot cast " + mi.Name + " to Action<T> delegate type.");

                    var actionType = typeof (Action<>).MakeGenericType(pi[0].ParameterType);
                    var key = (mha.MessageKey) ?? actionType;

                    UnregisterHandler(scope, key, actionType, mi.IsStatic ? Delegate.CreateDelegate(actionType, mi) : Delegate.CreateDelegate(actionType, view, mi.Name));
                }
            }
        }

        /// <summary>
        ///   This registers a specific method as a message handler for a specific type.
        /// </summary>
        /// <param name="scope"></param>
        /// <param name = "key">Message key</param>
        /// <param name = "handler">Handler method</param>
        public void RegisterHandler<T>(string scope, string key, Action<T> handler) { RegisterHandler(scope, key, handler.GetType(), handler); }

        /// <summary>
        ///   This unregisters a method as a handler.
        /// </summary>
        /// <param name="scope"></param>
        /// <param name = "key">Message key</param>
        /// <param name = "handler">Handler</param>
        public void UnregisterHandler<T>(string scope, string key, Action<T> handler) { UnregisterHandler(scope, key, handler.GetType(), handler); }

        /// <summary>
        ///   This method broadcasts a message to all message targets for a given
        ///   message key and passes a parameter.
        /// </summary>
        /// <param name="scope"></param>
        /// <param name = "key">Message key</param>
        /// <param name = "message">Message parameter</param>
        /// <returns>True/False if any handlers were invoked.</returns>
        public bool NotifyColleagues<T>(string scope, string key, T message) { return NotifyColleagues(scope, (object) key, message); }

        /// <summary>
        ///   This method broadcasts a message to all message targets for a given parameter type.
        ///   If a derived type is passed, any handlers for interfaces or base types will also be
        ///   invoked.
        /// </summary>
        /// <param name="scope"></param>
        /// <param name = "message">Message parameter</param>
        /// <returns>True/False if any handlers were invoked.</returns>
        public bool NotifyColleagues<T>(string scope, T message)
        {
            var actionType = typeof (Action<>).MakeGenericType(typeof (T));
            var keyList = from key in _registeredScopes[scope].Keys
                          where key is Type && ((Type) key).IsAssignableFrom(actionType)
                          select key;

            return keyList.Aggregate(false, (current, key) => current | NotifyColleagues(scope, key, message));
        }

        /// <summary>
        ///   This method broadcasts a message to all message targets for a given
        ///   message key and passes a parameter.  The message targets are all called
        ///   asynchronously and any resulting exceptions are ignored.
        /// </summary>
        /// <param name="scope"></param>
        /// <param name = "key">Message key</param>
        /// <param name = "message">Message parameter</param>
        public void NotifyColleaguesAsync<T>(string scope, string key, T message)
        {
            Func<string, T, bool> smaFunc = NotifyColleagues;
            smaFunc.BeginInvoke(scope, message, ia =>
                                              {
                                                  try
                                                  {
                                                      smaFunc.EndInvoke(ia);
                                                  }
                                                  catch {}
                                              }, null);
        }

        #endregion
    }
#endif
}