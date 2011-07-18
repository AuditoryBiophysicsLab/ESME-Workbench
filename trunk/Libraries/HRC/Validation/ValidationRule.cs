using System;

namespace HRC.Validation
{
    public class ValidationRule : ValidationRuleBase
    {
        #region Public Methods/Properties
        /// <summary>
        /// Gets or sets the delegate used to validate this rule.
        /// </summary>
        public virtual Func<object, ValidationRuleBase, bool> RuleDelegate { get; set; }
        #endregion

        #region Overrides
        /// <summary>
        /// Validates that the rule has not been broken.
        /// </summary>
        /// <param name="domainObject">The domain object being validated.</param>
        /// <param name="rule"></param>
        /// <returns>True if the rule has not been broken, or false if it has.</returns>
        public override bool Validate(Object domainObject, ValidationRuleBase rule)
        {
            return RuleDelegate(domainObject, rule);
        }
        #endregion
    }
}
