using System;
using System.Runtime.Serialization;

namespace ESME.Behaviors
{
    [Serializable]
    public class PlatformMovementException : Exception
    {
        public PlatformMovementException() { }
        public PlatformMovementException(string message) : base(message) { }
        public PlatformMovementException(string message, Exception innerException) : base(message, innerException) { }
        public PlatformMovementException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }

    [Serializable]
    public class PlatformBehaviorException : Exception
    {
        public PlatformBehaviorException() { }
        public PlatformBehaviorException(string message) : base(message) { }
        public PlatformBehaviorException(string message, Exception innerException) : base(message, innerException) { }
        public PlatformBehaviorException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }

    [Serializable]
    public class PerimeterInvalidException : Exception
    {
        public PerimeterInvalidException() { }
        public PerimeterInvalidException(string message) : base(message) { }
        public PerimeterInvalidException(string message, Exception inner) : base(message, inner) { }
        protected PerimeterInvalidException(SerializationInfo info,StreamingContext context) : base(info, context) { }
    }
}
