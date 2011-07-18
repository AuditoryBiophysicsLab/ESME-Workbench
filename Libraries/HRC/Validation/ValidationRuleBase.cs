using System;

// A simple base rule that can be used for more specific rules
// that may be used to validate objects
namespace HRC.Validation
{
    /// <summary>
    /// An abstract class that contains information about a rule as well as a method to validate it.
    /// </summary>
    /// <remarks>
    /// This class is primarily designed to be used on a domain object to validate a business rule. In most cases, you will want to use the 
    /// concrete class ValidationRule, which just needs you to supply a delegate used for validation. For custom, complex business rules, you can 
    /// extend this class and provide your own method to validate the rule.
    /// </remarks>
    public abstract class ValidationRuleBase
    {
        #region Public Methods/Properties
        /// <summary>
        /// Gets descriptive text about this broken rule.
        /// </summary>
        public virtual string Description { get; set; }

        /// <summary>
        /// Gets a string that is intended to be written by a validation routine, 
        /// describing what's wrong with the object being validated.
        /// </summary>
        public virtual string ValidationErrorMessage { get; set; }

        /// <summary>
        /// Gets the name of the property the rule belongs to.
        /// </summary>
        public virtual string PropertyName { get; set; }

        /// <summary>
        /// Validates that the rule has been followed.
        /// </summary>
        public abstract bool Validate(Object domainObject, ValidationRuleBase rule);

        /// <summary>
        /// Gets a string representation of this rule.
        /// </summary>
        /// <returns>A string containing the description of the rule.</returns>
        public override string ToString()
        {
            return Description;
        }

        /// <summary>
        /// Serves as a hash function for a particular type. System.Object.GetHashCode()
        /// is suitable for use in hashing algorithms and data structures like a hash
        /// table.
        /// </summary>
        /// <returns>A hash code for the current rule.</returns>
        public override int GetHashCode()
        {
            return ToString().GetHashCode();
        }
        #endregion
    }
}
