using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel;
using System.Windows.Threading;

namespace ESME.DataModel
{
    public class DataModelBase : INotifyPropertyChanged
    {
        #region Constructor
        public DataModelBase()
        {
            _dispatcher = Dispatcher.CurrentDispatcher;
        }
        #endregion

        #region ModelState enum
        public enum ModelState
        {
            Fetching,   // The model is fetching data
            Invalid,    // The model is in an invalid state
            Active      // The model has fetched its data
        }
        #endregion

        #region Public properties
        #region State
        public ModelState State
        {
            get
            {
                VerifyCalledOnUIThread();
                return _state;
            }

            set
            {
                VerifyCalledOnUIThread();
                if (value != _state)
                {
                    _state = value;
                    SendPropertyChanged("State");
                }
            }
        }
        #endregion
        #region FetchProgress
        public int FetchProgress
        {
            get
            {
                VerifyCalledOnUIThread();
                if (_state == ModelState.Fetching)
                    return _fetchProgress;
                else
                    return 100;
            }

            protected set
            {
                VerifyCalledOnUIThread();
                if (_state == ModelState.Fetching)
                {
                    _fetchProgress = value;
                    SendPropertyChanged("FetchProgress");
                }
            }
        }
        #endregion
        #region IsActive
        public bool IsActive
        {
            get
            {
                VerifyCalledOnUIThread();
                return _isActive;
            }

            private set
            {
                VerifyCalledOnUIThread();
                if (value != _isActive)
                {
                    _isActive = value;
                    SendPropertyChanged("IsActive");
                }
            }
        }
        #endregion
        #endregion

        #region Public Methods
        #region Activate
        public void Activate()
        {
            VerifyCalledOnUIThread();

            if (!_isActive)
            {
                this.IsActive = true;
                OnActivated();
            }
        }
        #endregion
        #region Deactivate
        public void Deactivate()
        {
            VerifyCalledOnUIThread();

            if (_isActive)
            {
                this.IsActive = false;
                OnDeactivated();
            }
        }
        #endregion
        #endregion

        #region PropertyChanged event handler
        public event PropertyChangedEventHandler PropertyChanged
        {
            add
            {
                VerifyCalledOnUIThread();
                _propertyChangedEvent += value;
            }

            remove
            {
                VerifyCalledOnUIThread();
                _propertyChangedEvent -= value;
            }
        }        
        #endregion

        #region Protected methods
        /// <summary>
        /// To override, see http://blogs.msdn.com/dancre/archive/2006/08/25/724938.aspx
        /// </summary>
        protected virtual void OnActivated()
        {
        }

        protected virtual void OnDeactivated()
        {
        }

        [System.Diagnostics.Conditional("Debug")]
        protected void VerifyCalledOnUIThread()
        {
            System.Diagnostics.Debug.Assert(Dispatcher.CurrentDispatcher == _dispatcher, "Call must be made on UI thread.");
        }

        protected virtual void SendPropertyChanged(string propertyName)
        {
            VerifyCalledOnUIThread();
            if (_propertyChangedEvent != null)
            {
                _propertyChangedEvent(this, new PropertyChangedEventArgs(propertyName));
            }
        }        
        #endregion

        #region Private data members
        private ModelState _state;
        private Dispatcher _dispatcher;
        private PropertyChangedEventHandler _propertyChangedEvent;
        private bool _isActive;
        private int _fetchProgress;
 	    #endregion    
    }
}
