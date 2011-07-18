using System;

namespace HRC.Validation
{
    public class SimpleRule : Rule
    {
        #region Data
        #endregion

        #region Public Methods/Properties
        /// <summary>
        /// Gets or sets the delegate used to validate this rule.
        /// </summary>
        protected virtual Func<object, bool> RuleDelegate { get; set; }
        #endregion

        #region Overrides
        /// <summary>
        /// Validates that the rule has not been broken.
        /// </summary>
        /// <param name="domainObject">The domain object being validated.</param>
        /// <returns>True if the rule has not been broken, or false if it has.</returns>
        public override bool ValidateRule(Object domainObject)
        {
            return RuleDelegate(domainObject);
        }
        #endregion
    }
}
