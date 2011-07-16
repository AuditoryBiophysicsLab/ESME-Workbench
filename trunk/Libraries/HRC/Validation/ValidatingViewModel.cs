using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Windows.Controls;
using System.Windows.Input;
using Cinch;

namespace HRC.Validation
{
    public abstract class ValidatingViewModel : ViewModelBase, IDataErrorInfo
    {
// ReSharper disable DoNotCallOverridableMethodsInConstructor
        protected ValidatingViewModel() { ValidationRules = new List<SimpleRule>(); }
// ReSharper restore DoNotCallOverridableMethodsInConstructor

        protected virtual List<SimpleRule> ValidationRules { get; set; }

        #region Implementation of IDataErrorInfo
        /// <summary>
        /// Gets the error message for the property with the given name.
        /// </summary>
        /// <returns>
        /// The error message for the property. The default is an empty string ("").
        /// </returns>
        /// <param name="columnName">The name of the property whose error message to get. </param>
        public virtual string this[string columnName]
        {
            get
            {
                var result = string.Empty;
                var errStr = string.Empty;
                var allRulesValid = true;
                foreach (var rule in ValidationRules)
                {
                    var ruleIsBroken = !rule.ValidateRule(this);
                    if (!ruleIsBroken) continue;
                    allRulesValid = false;
                    errStr += rule.Description + Environment.NewLine;
                    if (rule.PropertyName == columnName) result += rule.Description + Environment.NewLine;
                }
                IsValid = allRulesValid;
                Error = !string.IsNullOrEmpty(errStr) ? errStr.Remove(errStr.Length - 2, 2) : errStr;
                return !string.IsNullOrEmpty(result) ? result.Remove(result.Length - 2, 2) : result;
            }
        }

        #region public virtual bool IsValid { get; private set; }

        public virtual bool IsValid
        {
            get { return _isValid; }
            private set
            {
                if (_isValid == value) return;
                _isValid = value;
                NotifyPropertyChanged(IsValidChangedEventArgs);
                CommandManager.InvalidateRequerySuggested();
            }
        }

        static readonly PropertyChangedEventArgs IsValidChangedEventArgs = ObservableHelper.CreateArgs<ValidatingViewModel>(x => x.IsValid);
        bool _isValid;

        #endregion

        #region public string Error { get; private set; }
        /// <summary>
        /// Gets an error message indicating what is wrong with this object.
        /// </summary>
        /// <returns>
        /// An error message indicating what is wrong with this object. The default is an empty string ("").
        /// </returns>
        public string Error
        {
            get { return _error; }
            private set
            {
                if (_error == value) return;
                _error = value;
                NotifyPropertyChanged(ErrorChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ErrorChangedEventArgs = ObservableHelper.CreateArgs<ValidatingViewModel>(x => x.Error);
        string _error;

        #endregion

        #endregion

        #region ValidationErrorCommand
        public SimpleCommand<object, ValidationErrorEventArgs> ValidationErrorCommand
        {
            get
            {
                return _validationError ??
                       (_validationError =
                        new SimpleCommand<object, ValidationErrorEventArgs>(args =>
                        {
                            if ((args == null) || (args.Action != ValidationErrorEventAction.Added)) return;
                            var message = (args.Error.Exception != null) ? args.Error.Exception.Message : "An exception has occurred";
                            if (!string.IsNullOrEmpty(Error)) Error += Environment.NewLine + message;
                            else Error = message;
                            IsValid = false;
                        }));
            }
        }

        SimpleCommand<object, ValidationErrorEventArgs> _validationError;
        #endregion

    }
}
