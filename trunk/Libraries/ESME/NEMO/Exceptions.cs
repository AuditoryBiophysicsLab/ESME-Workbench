using System;
using System.Runtime.Serialization;

namespace ESME.NEMO
{
    public class ParameterOutOfRangeException : Exception
    {
        public ParameterOutOfRangeException() { }
        public ParameterOutOfRangeException(string message) : base(message) { }
        public ParameterOutOfRangeException(string message, Exception innerException) : base(message, innerException) { }
        public ParameterOutOfRangeException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }
    public class PlatformException : Exception
    {
        public PlatformException() { }
        public PlatformException(string message) : base(message) { }
        public PlatformException(string message, Exception innerException) : base(message, innerException) { }
        public PlatformException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }
    public class ScenarioException : Exception
    {
        public ScenarioException() { }
        public ScenarioException(string message) : base(message) { }
        public ScenarioException(string message, Exception innerException) : base(message, innerException) { }
        public ScenarioException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }
}
