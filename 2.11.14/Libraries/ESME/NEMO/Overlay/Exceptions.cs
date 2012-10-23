using System;
using System.Runtime.Serialization;
using ESME.Model;

namespace ESME.NEMO.Overlay
{
    public class GeometricException : ESMEModelException
    {
        public GeometricException() { }
        public GeometricException(string message) : base(message) { }
        public GeometricException(string message, Exception innerException) : base(message, innerException) { }
        public GeometricException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }

    public class AlgebraicException : ESMEModelException
    {
        public AlgebraicException() { }
        public AlgebraicException(string message) : base(message) { }
        public AlgebraicException(string message, Exception innerException) : base(message, innerException) { }
        public AlgebraicException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }
}