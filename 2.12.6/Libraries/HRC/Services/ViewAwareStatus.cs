using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Windows;
using System.Windows.Threading;
using HRC.ViewModels;
using MEFedMVVM.Services.Contracts;
using MEFedMVVM.ViewModelLocator;

namespace HRC.Services
{
    /// <summary>
    /// View aware service that provides the following
    /// 1. Events for ViewLoaded / ViewUnloaded (WPF and SL)
    /// 2. Events for ViewActivated / ViewDeactivated (WPF Only)
    /// 3. Views current Dispatcher
    /// 4. If the view implements <c>IViewCreationContextProvider</c>
    ///    the current Views Context will also be available to allow
    ///    the ViewModel to obtain some view specific contextual information
    /// </summary>
    [PartCreationPolicy(CreationPolicy.NonShared)]
    [ExportService(ServiceType.Both, typeof(IViewAwareStatus))]
    public class ViewAwareStatus : IViewAwareStatus
    {

        #region Data
        WeakReference _weakViewInstance;
        #endregion

        #region IViewAwareStatus Members

        readonly IList<WeakAction> _loadedHandlers = new List<WeakAction>();
        public event Action ViewLoaded
        {
            add
            {
                _loadedHandlers.Add(new WeakAction(value.Target, typeof(Action), value.Method));
            }
            remove
            {

            }
        }


        readonly IList<WeakAction> _unloadedHandlers = new List<WeakAction>();
        public event Action ViewUnloaded
        {
            add
            {
                _unloadedHandlers.Add(new WeakAction(value.Target, typeof(Action), value.Method));
            }
            remove
            {

            }
        }


        readonly IList<WeakAction> _activatedHandlers = new List<WeakAction>();
        public event Action ViewActivated
        {
            add
            {
                _activatedHandlers.Add(new WeakAction(value.Target, typeof(Action), value.Method));
            }
            remove
            {

            }
        }


        readonly IList<WeakAction> _deactivatedHandlers = new List<WeakAction>();
        public event Action ViewDeactivated
        {
            add
            {
                _deactivatedHandlers.Add(new WeakAction(value.Target, typeof(Action), value.Method));
            }
            remove
            {

            }
        }

        public Dispatcher ViewsDispatcher { get; private set; }

        public Object View
        {
            get
            {
                return _weakViewInstance.Target;
            }
        }


        #endregion

        #region IContextAware Members

        public void InjectContext(object view)
        {
            if (_weakViewInstance != null)
            {
                if (_weakViewInstance.Target == view)
                    return;
            }

            // unregister before hooking new events
            Window w;
            if (_weakViewInstance != null && _weakViewInstance.Target != null)
            {
                var targ = _weakViewInstance.Target;


                ((FrameworkElement)targ).Loaded -= OnViewLoaded;
                ((FrameworkElement)targ).Unloaded -= OnViewUnloaded;

                w = targ as Window;
                if (w != null)
                {
                    w.Activated -= OnViewActivated;
                    w.Deactivated -= OnViewDeactivated;
                }
            }

            var x = view as FrameworkElement;

            if (x == null) return;
            x.Loaded += OnViewLoaded;
            x.Unloaded += OnViewUnloaded;

            w = x as Window;
            if (w != null)
            {
                w.Activated += OnViewActivated;
                w.Deactivated += OnViewDeactivated;
            }

            //get the Views Dispatcher
            ViewsDispatcher = x.Dispatcher;
            _weakViewInstance = new WeakReference(x);
        }


        #endregion

        #region Private Helpers

        private void OnViewLoaded(object sender, RoutedEventArgs e)
        {
            foreach (var loadedHandler in _loadedHandlers)
            {
                loadedHandler.GetMethod().DynamicInvoke();
            }
        }

        private void OnViewUnloaded(object sender, RoutedEventArgs e)
        {
            foreach (var unloadedHandler in _unloadedHandlers)
            {
                unloadedHandler.GetMethod().DynamicInvoke();
            }
        }


        private void OnViewActivated(object sender, EventArgs e)
        {
            foreach (var activatedHandler in _activatedHandlers)
            {
                activatedHandler.GetMethod().DynamicInvoke();
            }

        }

        private void OnViewDeactivated(object sender, EventArgs e)
        {
            foreach (var deactivatedHandler in _deactivatedHandlers)
            {
                deactivatedHandler.GetMethod().DynamicInvoke();
            }
        }

        #endregion
    }

    public interface IViewAwareStatus : IContextAware
    {
        event Action ViewLoaded;
        event Action ViewUnloaded;


        event Action ViewActivated;
        event Action ViewDeactivated;

        Dispatcher ViewsDispatcher { get; }
        Object View { get; }
    }
}
