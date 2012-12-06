using System;
using System.Runtime.Serialization;

namespace ESME
{
    public class ServiceNotFoundException : Exception
    {
        public ServiceNotFoundException() { }
        public ServiceNotFoundException(string message) : base(message) { }
        public ServiceNotFoundException(string message, Exception innerException) : base(message, innerException) { }
        public ServiceNotFoundException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }

    [Serializable]
    public class RadialDeletedByUserException : Exception
    {
        //
        // For guidelines regarding the creation of new exception types, see
        //    http://msdn.microsoft.com/library/default.asp?url=/library/en-us/cpgenref/html/cpconerrorraisinghandlingguidelines.asp
        // and
        //    http://msdn.microsoft.com/library/default.asp?url=/library/en-us/dncscol/html/csharp07192001.asp
        //

        public RadialDeletedByUserException() { }
        public RadialDeletedByUserException(string message) : base(message) { }
        public RadialDeletedByUserException(string message, Exception inner) : base(message, inner) { }
        protected RadialDeletedByUserException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }

    [Serializable]
    public class SpeciesSeedingException : Exception
    {
        //
        // For guidelines regarding the creation of new exception types, see
        //    http://msdn.microsoft.com/library/default.asp?url=/library/en-us/cpgenref/html/cpconerrorraisinghandlingguidelines.asp
        // and
        //    http://msdn.microsoft.com/library/default.asp?url=/library/en-us/dncscol/html/csharp07192001.asp
        //

        public SpeciesSeedingException() { }
        public SpeciesSeedingException(string message) : base(message) { }
        public SpeciesSeedingException(string message, Exception inner) : base(message, inner) { }

        protected SpeciesSeedingException(
            SerializationInfo info,
            StreamingContext context) : base(info, context) { }
    }
}
