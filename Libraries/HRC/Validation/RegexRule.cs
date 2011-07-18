using System;
using System.Text.RegularExpressions;

namespace HRC.Validation
{
    /// <summary>
    /// A class to define a RegEx rule, using a delegate for validation.
    /// </summary>
    public class RegexRule : ValidationRuleBase
    {
        public string RegularExpression { get; set; }

        #region Overrides
        public override bool Validate(Object domainObject, ValidationRuleBase rule)
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
