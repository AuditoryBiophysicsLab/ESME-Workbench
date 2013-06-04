using System;
using System.Text.RegularExpressions;

namespace HRC.Validation
{
    /// <summary>
    /// A class to define a RegEx rule, using a delegate for validation.
    /// </summary>
    public class RegexRule<T> : ValidationRule<T> where T: ValidatingViewModel
    {
        string _regularExpression;

        public string RegularExpression
        {
            get { return _regularExpression; }
            set
            {
                _regularExpression = value;
                if (_regularExpression == null) return;
                IsRuleValid = (target, rule) =>
                {
                    if (string.IsNullOrEmpty(PropertyName)) return false;
                    var pi = target.GetType().GetProperty(PropertyName);
                    var theValue = pi.GetValue(target, null) as String;
                    if (String.IsNullOrEmpty(theValue)) return false;
                    var m = Regex.Match(value, RegularExpression);
                    return m.Success;
                };
            }
        }
    }
}
