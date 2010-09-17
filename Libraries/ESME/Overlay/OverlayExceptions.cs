using System;

namespace ESME.Overlay
{
    public class AlgebraicException : Exception
    {
        public AlgebraicException(string message)
            : base(message)
        {
        }
    }

    public class GeometricException : Exception
    {
        public GeometricException(string message)
            : base(message)
        {
        }
    }
}