using System;
using System.Windows.Threading;

namespace HRC.WPF
{
    public static class DispatcherExtensions
    {
        #region Dispatcher Extensions
        /// <summary>
        /// A simple threading extension method, to invoke a delegate
        /// on the correct thread if it is not currently on the correct thread
        /// which can be used with DispatcherObject types.
        /// </summary>
        /// <param name="dispatcher">The Dispatcher object on which to 
        /// perform the Invoke</param>
        /// <param name="action">The delegate to run</param>
        /// <param name="priority">The DispatcherPriority for the invoke.</param>
        public static void InvokeIfRequired(this Dispatcher dispatcher, Action action, DispatcherPriority priority = DispatcherPriority.Normal)
        {
            object result;
            if (!dispatcher.CheckAccess()) result = dispatcher.Invoke(priority, action);
            else action();
        }

        /// <summary>
        /// A simple threading extension method, to invoke a delegate
        /// on the correct thread if it is not currently on the correct thread
        /// which can be used with DispatcherObject types.
        /// </summary>
        /// <param name="dispatcher">The Dispatcher object on which to 
        /// perform the Invoke</param>
        /// <param name="action">The delegate to run</param>
        public static void InvokeInBackgroundIfRequired(this Dispatcher dispatcher, Action action) { dispatcher.InvokeIfRequired(action, DispatcherPriority.Background); }

        /// <summary>
        /// A simple threading extension method, to invoke a delegate
        /// on the correct thread asynchronously if it is not currently 
        /// on the correct thread which can be used with DispatcherObject types.
        /// </summary>
        /// <param name="dispatcher">The Dispatcher object on which to 
        /// perform the Invoke</param>
        /// <param name="action">The delegate to run</param>
        public static void InvokeAsynchronouslyInBackground(this Dispatcher dispatcher, Action action)
        {
            if (dispatcher != null)
                dispatcher.BeginInvoke(DispatcherPriority.Background, action);
            else
                action();
        }
        #endregion
    }
}
