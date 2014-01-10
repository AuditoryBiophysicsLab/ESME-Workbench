using System;
using System.ComponentModel;
using System.ComponentModel.DataAnnotations.Schema;
using System.Diagnostics;
using System.Windows.Threading;
using System.Xml.Serialization;
using HRC.Aspects;
using HRC.WPF;

namespace HRC.ViewModels
{
    [NotifyPropertyChanged]
    public abstract class ViewModelBase : INotifyPropertyChanged
    {
        protected ViewModelBase() { CloseActivePopUpCommand = new SimpleCommand<object, bool>(CloseDialog); }

        /// <summary>
        /// CloseActivePopUpCommand : Close popup command
        /// </summary>
        [XmlIgnore, NotMapped]
        public SimpleCommand<object, bool> CloseActivePopUpCommand { get; private set; }

        #region Private Methods

        /// <summary>
        /// Raises RaiseCloseRequest event, passing back correct DialogResult
        /// </summary>
        private void CloseDialog(bool param)
        {
            CloseDialog((bool?)param);
        }

        /// <summary>
        /// Child classes can override this method to perform 
        /// clean-up logic, such as removing event handlers.
        /// </summary>
        protected virtual void OnDispose()
        {
        }

        #endregion

        #region Debugging

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
                var msg = "Invalid property name: " + propertyName;

                if (ThrowOnInvalidPropertyName) throw new Exception(msg);
                Debug.Fail(msg);
            }
        }

        /// <summary>
        /// Returns whether an exception is thrown, or if a Debug.Fail() is used
        /// when an invalid property name is passed to the VerifyPropertyName method.
        /// The default value is false, but subclasses used by unit tests might 
        /// override this property's getter to return true.
        /// </summary>
        [XmlIgnore, UsedImplicitly, NotMapped] protected virtual bool ThrowOnInvalidPropertyName { get; private set; }

        #endregion // Debugging Aides

        public event PropertyChangedEventHandler PropertyChanged;
        protected void OnPropertyChanged(string propertyName)
        {
            var handlers = PropertyChanged;
            if (handlers == null) return;
            foreach (PropertyChangedEventHandler handler in handlers.GetInvocationList())
            {
                if (handler.Target is DispatcherObject)
                {
                    var localHandler = handler;
                    ((DispatcherObject)handler.Target).Dispatcher.InvokeIfRequired(() => localHandler(this, new PropertyChangedEventArgs(propertyName)));
                }
                else
                    handler(this, new PropertyChangedEventArgs(propertyName));
            }
        }

        #region Events
        /// <summary>
        /// This event should be raised to activate the UI.  Any view tied to this
        /// ViewModel should register a handler on this event and close itself when
        /// this event is raised.  If the view is not bound to the lifetime of the
        /// ViewModel then this event can be ignored.
        /// </summary>
        public event EventHandler<EventArgs> ActivateRequest;

        /// <summary>
        /// This event should be raised to close the view.  Any view tied to this
        /// ViewModel should register a handler on this event and close itself when
        /// this event is raised.  If the view is not bound to the lifetime of the
        /// ViewModel then this event can be ignored.
        /// </summary>
        public event EventHandler<CloseRequestEventArgs> CloseRequest;

        #endregion

        #region Public Methods

        /// <summary>
        /// This raises the CloseRequest event to close the UI.
        /// </summary>
        public virtual void CloseDialog(bool? dialogResult)
        {
            var handlers = CloseRequest;

            // Invoke the event handlers
            if (handlers == null) return;
            try
            {
                handlers(this, new CloseRequestEventArgs(dialogResult));
            }
            catch (Exception ex)
            {
                Debug.WriteLine(ex);
            }
        }


        /// <summary>
        /// This raises the ActivateRequest event to activate the UI.
        /// </summary>
        public virtual void ActivateDialog()
        {
            var handlers = ActivateRequest;

            // Invoke the event handlers
            if (handlers == null) return;
            try
            {
                handlers(this, EventArgs.Empty);
            }
            catch (Exception ex)
            {
                Debug.WriteLine(ex);
            }
        }
        #endregion

    }

    /// <summary>
    /// This is used to send result parameters to a CloseRequest
    /// </summary>
    public class CloseRequestEventArgs : EventArgs
    {
        ///<summary>
        /// Final result for ShowDialog
        ///</summary>
        public bool? Result { get; private set; }

        internal CloseRequestEventArgs(bool? result) { Result = result; }
    }
}
