using System;
using System.Runtime.Serialization;

namespace HRC.Utility
{
    public class SettingTypeNotFoundException : Exception
    {
        public SettingTypeNotFoundException() { }
        public SettingTypeNotFoundException(string message) : base(message) { }
        public SettingTypeNotFoundException(string message, Exception innerException) : base(message, innerException) { }
        public SettingTypeNotFoundException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }

    public class InvalidReferencedTypesAttributeException : Exception
    {
        public InvalidReferencedTypesAttributeException() { }
        public InvalidReferencedTypesAttributeException(string message) : base(message) { }
        public InvalidReferencedTypesAttributeException(string message, Exception innerException) : base(message, innerException) { }
        public InvalidReferencedTypesAttributeException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }
}
