using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Linq;
using System.Windows.Controls;
using System.Windows.Input;
using System.Xml.Serialization;
using Cinch;

namespace HRC.Validation
{
    public abstract class ValidatingViewModel : ViewModelBase, IDataErrorInfo
    {
        protected ValidatingViewModel() { ValidationRules = new List<ValidationRule>(); }

        protected List<ValidationRule> ValidationRules { get; set; }

        #region Implementation of IDataErrorInfo
        /// <summary>
        /// Gets the error message for the property with the given name.
        /// </summary>
        /// <returns>
        /// The error message for the property. The default is an empty string ("").
        /// </returns>
        /// <param name="columnName">The name of the property whose error message to get. </param>
        [XmlIgnore]
        public virtual string this[string columnName]
        {
            get
            {
                var result = string.Empty;
                var errStr = string.Empty;
                var allRulesValid = true;
                foreach (var rule in ValidationRules)
                {
                    rule.ValidationErrorMessage = null;
                    var ruleIsBroken = !rule.Validate(this, rule);
                    if (!ruleIsBroken) continue;
                    allRulesValid = false;
                    Debug.WriteLine("{0}: Broken rule on {1} : {2}{3}", DateTime.Now, rule.PropertyName, rule.Description, !string.IsNullOrEmpty(rule.ValidationErrorMessage) ? "\n" + rule.ValidationErrorMessage : "");
                    if (!string.IsNullOrEmpty(rule.Description)) errStr += rule.Description + Environment.NewLine;
                    if (!string.IsNullOrEmpty(rule.ValidationErrorMessage)) errStr += rule.ValidationErrorMessage + Environment.NewLine;
                    if (string.IsNullOrEmpty(rule.PropertyName) || (rule.PropertyName != columnName)) continue;
                    if (!string.IsNullOrEmpty(rule.Description)) result += rule.Description + Environment.NewLine;
                    if (!string.IsNullOrEmpty(rule.ValidationErrorMessage)) result += rule.ValidationErrorMessage + Environment.NewLine;
                }
                IsValid = allRulesValid;
                Error = !string.IsNullOrEmpty(errStr) ? errStr.Remove(errStr.Length - 2, 2) : errStr;
                Debug.WriteLine("{0}: this[{1}] : {2}", DateTime.Now, columnName, !string.IsNullOrEmpty(result) ? result.Remove(result.Length - 2, 2) : result == string.Empty ? "(empty)" : "(null)");
                return !string.IsNullOrEmpty(result) ? result.Remove(result.Length - 2, 2) : result;
            }
        }

        #region public virtual bool IsValid { get; private set; }

        [XmlIgnore]
        public virtual bool IsValid
        {
            get { return _isValid; }
            private set
            {
                if (_isValid == value) return;
                _isValid = value;
                Debug.WriteLine("{0}: IsValid property is now : {1}", DateTime.Now, _isValid);
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
        [XmlIgnore]
        public string Error
        {
            get { return _error; }
            private set
            {
                if (_error == value) return;
                _error = value;
                //Debug.WriteLine("{0}: Error property is now : {1}", DateTime.Now, !string.IsNullOrEmpty(_error) ? _error : _error == string.Empty ? "(empty)" : "(null)");
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
                            var curException = args.Error.Exception;
                            var message = string.Empty;
                            while (curException != null)
                            {
                                if (!string.IsNullOrEmpty(message)) message += Environment.NewLine;
                                message += string.IsNullOrEmpty(curException.Message) ? "(empty)" : curException.Message;
                                curException = curException.InnerException;
                            }
                            if (string.IsNullOrEmpty(message)) message += "An unknown exception has occurred";
                            if (!string.IsNullOrEmpty(Error)) Error += Environment.NewLine + message;
                            else Error = message;
                            IsValid = false;
                        }));
            }
        }

        SimpleCommand<object, ValidationErrorEventArgs> _validationError;
        #endregion

        public static bool RangeCheck(double valueToCheck, double minimum = double.MinValue, double maximum = double.MaxValue, bool includeEndpoints = true)
        {
            if (includeEndpoints)
                return (!double.IsNaN(valueToCheck) && (valueToCheck >= minimum)) && (valueToCheck <= maximum);
            return (!double.IsNaN(valueToCheck) && (valueToCheck > minimum)) && (valueToCheck < maximum);
        }
        public static bool RangeCheck(float valueToCheck, float minimum = float.MinValue, float maximum = float.MaxValue, bool includeEndpoints = true)
        {
            if (includeEndpoints)
                return (!float.IsNaN(valueToCheck) && (valueToCheck >= minimum)) && (valueToCheck <= maximum);
            return (!float.IsNaN(valueToCheck) && (valueToCheck > minimum)) && (valueToCheck < maximum);
        }

        public static bool RangeCheck(int valueToCheck, int minimum = int.MinValue, int maximum = int.MaxValue, bool includeEndpoints = true)
        {
            if (includeEndpoints)
                return ((valueToCheck >= minimum)) && (valueToCheck <= maximum);
            return ((valueToCheck > minimum)) && (valueToCheck < maximum);
        }

        public static bool AllAreFull(params string[] fields)
        {
            return EmptyCount(fields) == 0;
        }

        public static bool AllAreEmpty(params string[] fields)
        {
            return FullCount(fields) == 0;
        }
        public static bool OnlyOneIsFull(params string[] fields)
        {
            return FullCount(fields) == 1;
        }
        public static bool OnlyOneIsEmpty(params string[] fields)
        {
            return EmptyCount(fields) == 1;
        }
        public static int FullCount(params string[] fields)
        {
            var nonEmptyCount = fields.Count(field => !string.IsNullOrEmpty(field));
            return nonEmptyCount;
        }

        public static int EmptyCount(params string[] fields)
        {
            var emptyCount = fields.Where(string.IsNullOrEmpty).Count();
            return emptyCount;
        }
    }
}
