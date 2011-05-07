using System;
using System.Runtime.Serialization;

namespace ESMEWorkBench
{
    public class UserCanceledOperationException : Exception
    {
        public UserCanceledOperationException() { }
        public UserCanceledOperationException(string message) : base(message) { }
        public UserCanceledOperationException(string message, Exception innerException) : base(message, innerException) { }
        public UserCanceledOperationException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }
}
