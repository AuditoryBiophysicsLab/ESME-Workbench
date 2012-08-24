using System;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Windows;

namespace DavesWPFTester
{
    /// <summary>
    /// Monitors the CollectionChanged event of an object that implements INotifyPropertyChanged,
    /// and executes callback methods (i.e. handlers) registered for properties of that object.
    /// </summary>
    public class CollectionObserver : IWeakEventListener
    {
        #region Constructor

        /// <summary>
        /// Initializes a new instance of PropertyObserver, which
        /// observes the 'propertySource' object for property changes.
        /// </summary>
        /// <param name="collectionSource">The object to monitor for property changes.</param>
        public CollectionObserver(object collectionSource)
        {
            if (collectionSource == null)
                throw new ArgumentNullException("collectionSource");
            if (!(collectionSource is INotifyPropertyChanged)) throw new ArgumentException("Argument is not of type INotifyPropertyChanged", "collectionSource");

            _collectionSourceRef = new WeakReference(collectionSource);
        }

        #endregion // Constructor

        #region Public Methods

        #region RegisterHandler

        /// <summary>
        /// Registers a callback to be invoked when the PropertyChanged event has been raised for the specified property.
        /// </summary>
        /// <param name="handler">The callback to invoke when the property has changed.</param>
        /// <returns>The object on which this method was invoked, to allow for multiple invocations chained together.</returns>
        public CollectionObserver RegisterHandler(Action<INotifyCollectionChanged, NotifyCollectionChangedEventArgs> handler)
        {
            if (handler == null)
                throw new ArgumentNullException("handler");

            var propertySource = GetCollectionSource();
            if (propertySource != null)
            {
                _handler = handler;
                CollectionChangedEventManager.AddListener(propertySource, this);
            }

            return this;
        }

        #endregion // RegisterHandler

        #region UnregisterHandler
        /// <summary>
        /// Removes the callback associated with the specified property.
        /// </summary>
        /// <returns>The object on which this method was invoked, to allow for multiple invocations chained together.</returns>
        public CollectionObserver UnregisterHandler(Action<INotifyPropertyChanged, NotifyCollectionChangedEventArgs> handler)
        {
            var collectionSource = GetCollectionSource();
            if (collectionSource != null)
            {
                if (_handler != null)
                {
                    _handler = null;
                    CollectionChangedEventManager.RemoveListener(collectionSource, this);
                }
            }

            return this;
        }
        #endregion // UnregisterHandler

        #endregion // Public Methods

        #region Private Helpers

        #region GetCollectionSource

        INotifyCollectionChanged GetCollectionSource()
        {
            try
            {
                return (INotifyCollectionChanged)_collectionSourceRef.Target;
            }
            catch
            {
                return null;
            }
        }

        #endregion // GetPropertySource

        #endregion // Private Helpers

        #region Fields

        Action<INotifyCollectionChanged, NotifyCollectionChangedEventArgs> _handler;
        readonly WeakReference _collectionSourceRef;

        #endregion // Fields

        #region IWeakEventListener Members

        bool IWeakEventListener.ReceiveWeakEvent(Type managerType, object sender, EventArgs e)
        {
            if (managerType == typeof(CollectionChangedEventManager))
            {
                var eventArgs = (NotifyCollectionChangedEventArgs)e;
                var collectionSource = (INotifyCollectionChanged)sender;
                if (_handler != null)
                {
                    _handler(collectionSource, eventArgs);
                    return true;
                }
            }

            return false;
        }

        #endregion
    }
}