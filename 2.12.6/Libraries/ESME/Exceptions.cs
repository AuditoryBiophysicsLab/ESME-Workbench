using System;
using System.Runtime.Serialization;
using System.Text;

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

    public static class ExceptionHelpers
    {
        public static string ToString(this AggregateException ex, int indentLevel = 0)
        {
            var sb = new StringBuilder();
            if (ex.InnerExceptions.Count > 1)
            {
                sb.AppendLine("Multiple exceptions occurred");
                indentLevel += 2;
            }
            foreach (var e in ex.InnerExceptions) sb.AppendLine(e.ToString(indentLevel));
            return sb.ToString();
        }

        public static string ToString(this Exception ex, int indentLevel = 0)
        {
            var sb = new StringBuilder();
            sb.AppendLine(new string(' ', indentLevel) + ex.Message);
            if (ex.InnerException != null) sb.AppendLine(ex.InnerException.ToString(indentLevel + 2));
            return sb.ToString();
        }
    }
}
