using System;
using System.ComponentModel;
using System.Diagnostics;
using ESME.CoreMVVM.Events;
using ESME.CoreMVVM.Services;
using ESME.CoreMVVM.Messenger;

namespace ESME.CoreMVVM.ViewModels
{
    /// <summary>
    /// Provides common functionality for ViewModel classes
    /// </summary>
    public abstract class ViewModelBase : INotifyPropertyChanged, IDisposable
    {
        /// <summary>
        /// Service resolver for view models.  Allows derived types to add/remove
        /// services from mapping.
        /// </summary>
        public static readonly ServiceProvider ServiceProvider = new ServiceProvider();

        /// <summary>
        /// This event should be raised to close the view.  Any view tied to this
        /// ViewModel should register a handler on this event and close itself when
        /// this event is raised.  If the view is not bound to the lifetime of the
        /// ViewModel then this event can be ignored.
        /// </summary>
        public event EventHandler<CloseRequestEventArgs> CloseRequest = delegate { };

        /// <summary>
        /// This event should be raised to activate the UI.  Any view tied to this
        /// ViewModel should register a handler on this event and close itself when
        /// this event is raised.  If the view is not bound to the lifetime of the
        /// ViewModel then this event can be ignored.
        /// </summary>
        public event EventHandler ActivateRequest = delegate { };

        static ViewModelBase()
        {
            ServiceProvider.RegisterService<MessageMediator>(new MessageMediator());

            // You could register all services. However, these services may not apply for all ViewModels.
            // Hence registration of services is deferred to when the VM is created.
            // RegisterKnownServiceTypes();
        }

        /// <summary>
        /// This method registers known WPF services with the service provider.
        /// </summary>
        public static void RegisterKnownServiceTypes()
        {
            ServiceProvider.RegisterService<IOpenFileService>(new OpenFileService());
            ServiceProvider.RegisterService<IUIVisualizerService>(new UIVisualizerService());

            // Add other services - some may be specific to certain viewmodels
        }

        /// <summary>
        /// This resolves a service type and returns the implementation.
        /// </summary>
        /// <typeparam name="T">Type to resolve</typeparam>
        /// <returns>Implementation</returns>
        protected T GetService<T>()
        {
            return ServiceProvider.GetService<T>();
        }

        /// <summary>
        /// This raises the CloseRequest event to close the UI.
        /// </summary>
        public virtual void RaiseCloseRequest()
        {
            CloseRequest(this, new CloseRequestEventArgs(null));
        }

        /// <summary>
        /// This raises the CloseRequest event to close the UI.
        /// </summary>
        public virtual void RaiseCloseRequest(bool? dialogResult)
        {
            CloseRequest(this, new CloseRequestEventArgs(dialogResult));
        }

        /// <summary>
        /// This raises the ActivateRequest event to activate the UI.
        /// </summary>
        public virtual void RaiseActivateRequest()
        {
            ActivateRequest(this, EventArgs.Empty);
        }

        #region INotifyPropertyChanged

        /// <summary>
        /// Occurs when a property value changes.
        /// </summary>
        public event PropertyChangedEventHandler PropertyChanged = delegate { };

        /// <summary>
        /// This raises the INotifyPropertyChanged.PropertyChanged event to indicate
        /// a specific property has changed value.
        /// </summary>
        /// <param name="name"></param>
        protected void OnPropertyChanged(string propertyName)
        {
            this.VerifyPropertyName(propertyName);
            Debug.Assert(String.IsNullOrEmpty(propertyName) || GetType().GetProperty(propertyName) != null);
            PropertyChanged(this, new PropertyChangedEventArgs(propertyName));
        }

        #endregion

        #region IDisposable Members

        /// <summary>
        /// This disposes of the view model.  It unregisters from the message mediator.
        /// </summary>
        /// <param name="isDisposing">True if IDisposable.Dispose was called</param>
        protected virtual void Dispose(bool isDisposing)
        {
        }

        /// <summary>
        /// Implementation of IDisposable.Dispose.
        /// </summary>
        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        #endregion

        #region ModelDataChanged event

        /// <summary>
        /// Raised when the view model changes the underlying model data.
        /// </summary>
        public event EventHandler ModelDataChanged;

        /// <summary>
        /// Raises this object's ModelDataChanged event.
        /// </summary>
        protected virtual void OnModelDataChanged()
        {
            if (ModelDataChanged != null)
                ModelDataChanged(this, new EventArgs());
        }

        #endregion

        #region Debugging Aides

        /// <summary>
        /// Warns the developer if this object does not have
        /// a public property with the specified name. This 
        /// method does not exist in a Release build.
        /// </summary>
        [Conditional("DEBUG")]
        [DebuggerStepThrough]
        public void VerifyPropertyName(string propertyName)
        {
            // Verify that the property name matches a real,  
            // public, instance property on this object.
            if (TypeDescriptor.GetProperties(this)[propertyName] == null)
            {
                string msg = "Invalid property name: " + propertyName;

                if (this.ThrowOnInvalidPropertyName)
                    throw new Exception(msg);
                else
                    Debug.Fail(msg);
            }
        }

        /// <summary>
        /// Returns whether an exception is thrown, or if a Debug.Fail() is used
        /// when an invalid property name is passed to the VerifyPropertyName method.
        /// The default value is false, but subclasses used by unit tests might 
        /// override this property's getter to return true.
        /// </summary>
        protected virtual bool ThrowOnInvalidPropertyName { get; private set; }

        #endregion
    }
}
