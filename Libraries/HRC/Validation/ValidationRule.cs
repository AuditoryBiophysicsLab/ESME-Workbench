using System;

namespace HRC.Validation
{
    public class ValidationRule<T> : ValidationRuleBase where T : ValidatingViewModel
    {
        #region Public Methods/Properties
        /// <summary>
        /// Gets or sets the delegate used to validate this rule.  Should return true if the rule is being followed, or false if it is not.
        /// </summary>
        public virtual Func<T, ValidationRule<T>, bool> IsRuleValid { get; set; }
        #endregion

        public override bool Validate(object domainObject, ValidationRuleBase rule) { return IsRuleValid((T)domainObject, (ValidationRule<T>)rule); }
    }
}
