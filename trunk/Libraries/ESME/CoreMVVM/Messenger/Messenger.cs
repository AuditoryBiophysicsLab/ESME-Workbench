using System;
using System.Collections.Generic;
using System.Reflection;

namespace ESME.CoreMVVM.Messenger
{
	/// <summary>
	/// Provides loosely-coupled messaging between
	/// various colleagues.  All references to objects
	/// are stored weakly, to prevent memory leaks.
	/// </summary>
	public class MessageMediator
    {
        readonly MessageToActionsMap invocationList = new MessageToActionsMap();

        internal MessageMediator()
        {
        }

        #region Public Methods

        /// <summary>
        /// Register particular callback delegate for Messenger message callbacks
        /// </summary>
        /// <param name="message"></param>
        /// <param name="callback"></param>
		public void Register(string message, Delegate callback)
		{
			if (callback.Target == null)
				throw new InvalidOperationException("Delegate cannot be static");

			ParameterInfo[] parameters = callback.Method.GetParameters();

			// JAS - Changed this logic to allow for 0 or 1 parameter.
			if (parameters != null && parameters.Length > 1)
				throw new InvalidOperationException(
                    "The registered delegate should only have 0 or 1 parameter" +
                    "since the Messenger has up to 1 argument to pass");

			Type parameterType = (parameters == null || parameters.Length == 0) ? null : parameters[0].ParameterType;

			invocationList.AddAction(message, callback.Target, callback.Method, parameterType);
		}
        /// <summary>
        /// Notify collegues use weak callback for those that
        /// registered for message
        /// </summary>
        /// <typeparam name="T">The type for the message</typeparam>
        /// <param name="message">The message to lookup weak callbacks
        /// for</param>
        /// <param name="parameter">The state parameter</param>
		public void BroadcastMessage<T>(string message, T parameter)
		{
			var actions = invocationList.GetActions(message);

			if (actions != null)
				actions.ForEach(action => action.DynamicInvoke(parameter));
		}

        /// <summary>
        /// Notify collegues use weak callback for those that
        /// registered for message
        /// </summary>
        /// <typeparam name="T">The type for the message</typeparam>
        /// <param name="message">The message to lookup weak callbacks
        /// for</param>
        public void BroadcastMessage<T>(string message)
		{
			var actions = invocationList.GetActions(message);

			if (actions != null)
				actions.ForEach(action => action.DynamicInvoke());
        }
        #endregion
    }
}