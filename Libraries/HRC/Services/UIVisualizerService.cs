using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Windows;
using HRC.Utility;
using HRC.ViewModels;
using MEFedMVVM.ViewModelLocator;

namespace HRC.Services
{
    /// <summary>
    /// This class implements the IUIVisualizerService for WPF purposes.
    /// If you have attributed up your views using the ViewnameToViewLookupKeyMetadataAttribute
    /// Registration of Views with the IUIVisualizerService service is automatic.
    /// However you can still register views manually, to do this simply put some lines like this in you App.Xaml.cs
    /// ViewModelRepository.Instance.Resolver.Container.GetExport().Value.Register("MainWindow", typeof(MainWindow));
    /// </summary>
    [PartCreationPolicy(CreationPolicy.Shared)]
    [ExportService(ServiceType.Both, typeof(IUIVisualizerService))]
    public class UIVisualizerService : IUIVisualizerService
    {
        #region Data
        private readonly Dictionary<string, Type> _registeredWindows;
        #endregion

        #region Ctor
        public UIVisualizerService()
        {
            _registeredWindows = new Dictionary<string, Type>();
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Registers a collection of entries
        /// </summary>
        /// <param name="startupData"></param>
        public void Register(Dictionary<string, Type> startupData)
        {
            foreach (var entry in startupData)
                Register(entry.Key, entry.Value);
        }

        /// <summary>
        /// Registers a type through a key.
        /// </summary>
        /// <param name="key">Key for the UI dialog</param>
        /// <param name="winType">Type which implements dialog</param>
        public void Register(string key, Type winType)
        {
            if (string.IsNullOrEmpty(key))
                throw new ArgumentNullException("key");
            if (winType == null)
                throw new ArgumentNullException("winType");
            if (!typeof(Window).IsAssignableFrom(winType))
                throw new ArgumentException("winType must be of type Window");

            lock (_registeredWindows)
            {
                _registeredWindows.Add(key, winType);
            }
        }

        /// <summary>
        /// This unregisters a type and removes it from the mapping
        /// </summary>
        /// <param name="key">Key to remove</param>
        /// <returns>True/False success</returns>
        public bool Unregister(string key)
        {
            if (string.IsNullOrEmpty(key))
                throw new ArgumentNullException("key");

            lock (_registeredWindows)
            {
                return _registeredWindows.Remove(key);
            }
        }

        /// <summary>
        /// This method displays a modaless dialog associated with the given key.
        /// </summary>
        /// <param name="key">Key previously registered with the UI controller.</param>
        /// <param name="state">Object state to associate with the dialog</param>
        /// <param name="setOwner">Set the owner of the window</param>
        /// <param name="completedProc">Callback used when UI closes (may be null)</param>
        /// <returns>True/False if UI is displayed</returns>
        public bool Show(string key, object state, bool setOwner,
            EventHandler<UICompletedEventArgs> completedProc)
        {
            var win = CreateWindow(key, state, setOwner, completedProc, false);
            if (win != null)
            {
                win.Show();
                return true;
            }
            return false;
        }

        public Window ShowWindow(string key, object state, bool setOwner,
            EventHandler<UICompletedEventArgs> completedProc)
        {
            var win = CreateWindow(key, state, setOwner, completedProc, false);
            if (win != null)
            {
                win.Show();
                return win;
            }
            return null;
        }

        /// <summary>
        /// This method displays a modal dialog associated with the given key.
        /// </summary>
        /// <param name="key">Key previously registered with the UI controller.</param>
        /// <param name="state">Object state to associate with the dialog</param>
        /// <returns>True/False if UI is displayed.</returns>
        public bool? ShowDialog(string key, object state)
        {
            var win = CreateWindow(key, state, true, null, true);
            if (win != null) return win.ShowDialog();
            return false;
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// This creates the WPF window from a key.
        /// </summary>
        /// <param name="key">Key</param>
        /// <param name="dataContext">DataContext (state) object</param>
        /// <param name="setOwner">True/False to set ownership to MainWindow</param>
        /// <param name="completedProc">Callback</param>
        /// <param name="isModal">True if this is a ShowDialog request</param>
        /// <returns>Success code</returns>
        private Window CreateWindow(string key, object dataContext, bool setOwner,
            EventHandler<UICompletedEventArgs> completedProc, bool isModal)
        {
            if (string.IsNullOrEmpty(key))
                throw new ArgumentNullException("key");

            Type winType;
            lock (_registeredWindows)
            {
                if (!_registeredWindows.TryGetValue(key, out winType))
                    return null;
            }

            var win = (Window)Activator.CreateInstance(winType);

            var viewStatusAwareInjectionAware = dataContext as IViewStatusAwareInjectionAware;
            if (viewStatusAwareInjectionAware != null) 
            {
                var export = ViewModelRepository.Instance.Resolver.Container.GetExport<IViewAwareStatus>();
                if (export != null) 
                {
                    var viewAwareStatus = export.Value;
                    viewAwareStatus.InjectContext(win);
                    (viewStatusAwareInjectionAware).InitialiseViewAwareService(viewAwareStatus);
                }
            }

            win.DataContext = dataContext;


            if (setOwner)
                win.Owner = Application.Current.MainWindow;

            if (dataContext != null)
            {
                var bvm = dataContext as ViewModelBase;
                if (bvm != null)
                {
                    if (isModal)
                    {
                        bvm.CloseRequest += ((EventHandler<CloseRequestEventArgs>)((s, e) =>
                        {
                            try
                            {
                                win.DialogResult = e.Result;
                            }
                            catch (InvalidOperationException)
                            {
                                win.Close();
                            }
                        })).MakeWeak(eh => bvm.CloseRequest -= eh);


                    }
                    else
                    {
                        bvm.CloseRequest += ((EventHandler<CloseRequestEventArgs>)((s, e) => win.Close()))
                                .MakeWeak(eh => bvm.CloseRequest -= eh);
                    }
                    bvm.ActivateRequest += ((EventHandler<EventArgs>)((s, e) => win.Activate()))
                        .MakeWeak(eh => bvm.ActivateRequest -= eh);
                }
            }

            win.Closed += (s, e) =>
            {
                if (completedProc != null)
                {
                    completedProc(this, new UICompletedEventArgs
                    {
                        State = dataContext,
                        Result = (isModal) ? win.DialogResult : null
                    });
                }
            };


            return win;
        }
        #endregion
    }
    /// <summary>
    /// This interface defines a UI controller which can be used to display dialogs
    /// in either modal or modaless form from a ViewModel.
    /// </summary>
    public interface IUIVisualizerService
    {
        /// <summary>
        /// Registers a type through a key.
        /// </summary>
        /// <param name="key">Key for the UI dialog</param>
        /// <param name="winType">Type which implements dialog</param>
        void Register(string key, Type winType);

        /// <summary>
        /// This unregisters a type and removes it from the mapping
        /// </summary>
        /// <param name="key">Key to remove</param>
        /// <returns>True/False success</returns>
        bool Unregister(string key);

        /// <summary>
        /// This method displays a modaless dialog associated with the given key.
        /// </summary>
        /// <param name="key">Key previously registered with the UI controller.</param>
        /// <param name="state">Object state to associate with the dialog</param>
        /// <param name="setOwner">Set the owner of the window</param>
        /// <param name="completedProc">Callback used when UI closes (may be null)</param>
        /// <returns>True/False if UI is displayed</returns>
        bool Show(string key, object state, bool setOwner,
            EventHandler<UICompletedEventArgs> completedProc);

        Window ShowWindow(string key, object state, bool setOwner=false, EventHandler<UICompletedEventArgs> completedProc=null);

        /// <summary>
        /// This method displays a modal dialog associated with the given key.
        /// </summary>
        /// <param name="key">Key previously registered with the UI controller.</param>
        /// <param name="state">Object state to associate with the dialog</param>
        /// <returns>True/False if UI is displayed.</returns>
        bool? ShowDialog(string key, object state);
    }
    /// <summary>
    /// This is the EventArgs return value for the IUIVisualizer.Show completed event.
    /// </summary>
    public class UICompletedEventArgs : EventArgs
    {
        /// <summary>
        /// Data passed to the Show method.
        /// </summary>
        public object State { get; set; }
        /// <summary>
        /// Final result of the UI dialog
        /// </summary>
        public bool? Result { get; set; }
    }
    public interface IViewStatusAwareInjectionAware
    {
        void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService);
    }

}
