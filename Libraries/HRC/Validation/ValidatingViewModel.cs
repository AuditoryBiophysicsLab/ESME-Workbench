using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.DataAnnotations.Schema;
using System.Windows.Controls;
using System.Xml.Serialization;
using HRC.Aspects;
using HRC.ViewModels;

namespace HRC.Validation
{
    public abstract class ValidatingViewModel : ViewModelBase, IDataErrorInfo
    {
        [XmlIgnore, NotMapped, Initialize] protected List<ValidationRuleBase> ValidationRules { get; set; }
        protected void AddValidationRules(params ValidationRuleBase[] validationRules) { foreach (var rule in validationRules) ValidationRules.Add(rule); }

        #region Implementation of IDataErrorInfo
        /// <summary>
        /// Gets the error message for the property with the given name.
        /// </summary>
        /// <returns>
        /// The error message for the property. The default is an empty string ("").
        /// </returns>
        /// <param name="columnName">The name of the property whose error message to get. </param>
        [XmlIgnore, NotMapped]
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
                    if (rule.Validate(this, rule)) continue;
                    allRulesValid = false;
                    //Debug.WriteLine("{0}: Broken rule on {1} : {2}{3}", DateTime.Now, rule.PropertyName, rule.Description, !string.IsNullOrEmpty(rule.ValidationErrorMessage) ? "\n" + rule.ValidationErrorMessage : "");
                    if (!string.IsNullOrEmpty(rule.Description))
                    {
                        errStr += rule.Description + Environment.NewLine;
                        if (rule.PropertyName == columnName) result += rule.Description + Environment.NewLine;
                    }
                    if (!string.IsNullOrEmpty(rule.ValidationErrorMessage))
                    {
                        errStr += rule.ValidationErrorMessage + Environment.NewLine;
                        if (rule.PropertyName == columnName) result += rule.ValidationErrorMessage + Environment.NewLine;
                    }
                }
                IsValid = allRulesValid;
                Error = !string.IsNullOrEmpty(errStr) ? errStr.Remove(errStr.Length - 2, 2) : errStr;
                //Debug.WriteLine("{0}: this[{1}] : {2}", DateTime.Now, columnName, !string.IsNullOrEmpty(result) ? result.Remove(result.Length - 2, 2) : result == string.Empty ? "(empty)" : "(null)");
                return !string.IsNullOrEmpty(result) ? result.Remove(result.Length - 2, 2) : result;
            }
        }

        protected void Validate()
        {
            var errStr = string.Empty;
            var allRulesValid = true;
            foreach (var rule in ValidationRules)
            {
                rule.ValidationErrorMessage = null;
                if (rule.Validate(this, rule)) continue;
                allRulesValid = false;
                //Debug.WriteLine("{0}: Broken rule on {1} : {2}{3}", DateTime.Now, rule.PropertyName, rule.Description, !string.IsNullOrEmpty(rule.ValidationErrorMessage) ? "\n" + rule.ValidationErrorMessage : "");
                if (!string.IsNullOrEmpty(rule.Description)) errStr += rule.Description + Environment.NewLine;
                if (!string.IsNullOrEmpty(rule.ValidationErrorMessage)) errStr += rule.ValidationErrorMessage + Environment.NewLine;
            }
            IsValid = allRulesValid;
            Error = !string.IsNullOrEmpty(errStr) ? errStr.Remove(errStr.Length - 2, 2) : errStr;
        }

        [XmlIgnore, NotMapped] public virtual bool IsValid { get; private set; }

        /// <summary>
        /// Gets an error message indicating what is wrong with this object.
        /// </summary>
        /// <returns>
        /// An error message indicating what is wrong with this object. The default is an empty string ("").
        /// </returns>
        [XmlIgnore, NotMapped]
        public string Error { get; private set; }

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
    }
}
