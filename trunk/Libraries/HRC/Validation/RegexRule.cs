using System;
using System.Text.RegularExpressions;

namespace HRC.Validation
{
    /// <summary>
    /// A class to define a RegEx rule, using a delegate for validation.
    /// </summary>
    public class RegexRule : Rule
    {
        public string RegularExpression { get; set; }

        #region Overrides
        public override bool ValidateRule(Object domainObject, Rule rule)
        {
            var pi = domainObject.GetType().GetProperty(PropertyName);
            var value = pi.GetValue(domainObject, null) as String;
            if (String.IsNullOrEmpty(value)) return false;
            var m = Regex.Match(value, RegularExpression);
            return !m.Success;
        }
        #endregion
    }
}
