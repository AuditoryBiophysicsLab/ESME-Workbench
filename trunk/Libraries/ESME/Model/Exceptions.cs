using System;
using System.Runtime.Serialization;

namespace ESME.Model
{
    public class ESMEModelException : Exception 
    {
        public ESMEModelException() { }
        public ESMEModelException(string message) : base(message) { }
        public ESMEModelException(string message, Exception innerException) : base(message, innerException) { }
        public ESMEModelException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }

    public class DuplicateListItemException : ESMEModelException
    {
        public DuplicateListItemException() { }
        public DuplicateListItemException(string message) : base(message) { }
        public DuplicateListItemException(string message, Exception innerException) : base(message, innerException) { }
        public DuplicateListItemException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }

    public class FileNameFormatException : ESMEModelException
    {
        public FileNameFormatException() { }
        public FileNameFormatException(string message) : base(message) { }
        public FileNameFormatException(string message, Exception innerException) : base(message, innerException) { }
        public FileNameFormatException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }

    public class FileIsEmptyException : ESMEModelException
    {
        public FileIsEmptyException() { }
        public FileIsEmptyException(string message) : base(message) { }
        public FileIsEmptyException(string message, Exception innerException) : base(message, innerException) { }
        public FileIsEmptyException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }

    public class FileFormatException : ESMEModelException
    {
        public FileFormatException() { }
        public FileFormatException(string message) : base(message) { }
        public FileFormatException(string message, Exception innerException) : base(message, innerException) { }
        public FileFormatException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }

    public class BathymetryOutOfBoundsException : ESMEModelException
    {
        public BathymetryOutOfBoundsException() { }
        public BathymetryOutOfBoundsException(string message) : base(message) { }
        public BathymetryOutOfBoundsException(string message, Exception innerException) : base(message, innerException) { }
        public BathymetryOutOfBoundsException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }

    public class BathymetryTooShallowException : ESMEModelException
    {
        public BathymetryTooShallowException() { }
        public BathymetryTooShallowException(string message) : base(message) { }
        public BathymetryTooShallowException(string message, Exception innerException) : base(message, innerException) { }
        public BathymetryTooShallowException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }

    public class LongitudeOutOfRangeException : ESMEModelException
    {
        public LongitudeOutOfRangeException() { }
        public LongitudeOutOfRangeException(string message) : base(message) { }
        public LongitudeOutOfRangeException(string message, Exception innerException) : base(message, innerException) { }
        public LongitudeOutOfRangeException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }

    public class LatitudeOutOfRangeException : ESMEModelException
    {
        public LatitudeOutOfRangeException() { }
        public LatitudeOutOfRangeException(string message) : base(message) { }
        public LatitudeOutOfRangeException(string message, Exception innerException) : base(message, innerException) { }
        public LatitudeOutOfRangeException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }

    public class SpeciesNotFoundException : ESMEModelException
    {
        public SpeciesNotFoundException() { }
        public SpeciesNotFoundException(string message) : base(message) { }
        public SpeciesNotFoundException(string message, Exception innerException) : base(message, innerException) { }
        public SpeciesNotFoundException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }

    public class PropertyNotInitializedException : ESMEModelException
    {
        public PropertyNotInitializedException() { }
        public PropertyNotInitializedException(string message) : base(message) { }
        public PropertyNotInitializedException(string message, Exception innerException) : base(message, innerException) { }
        public PropertyNotInitializedException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }
    public class AnimatInterfaceConfigurationException : Exception
    {
        public AnimatInterfaceConfigurationException() { }
        public AnimatInterfaceConfigurationException(string message) : base(message) { }
        public AnimatInterfaceConfigurationException(string message, Exception innerException) : base(message, innerException) { }
        public AnimatInterfaceConfigurationException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }

    public class AnimatInterfaceMMBSException : Exception
    {
        public AnimatInterfaceMMBSException() { }
        public AnimatInterfaceMMBSException(string message) : base(message) { }
        public AnimatInterfaceMMBSException(string message, Exception innerException) : base(message, innerException) { }
        public AnimatInterfaceMMBSException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }

    public class AnimatLocationFileException : Exception
    {
        public AnimatLocationFileException() { }
        public AnimatLocationFileException(string message) : base(message) { }
        public AnimatLocationFileException(string message, Exception innerException) : base(message, innerException) { }
        public AnimatLocationFileException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }

    public class AnimatMMBSException : Exception
    {
        public AnimatMMBSException() { }
        public AnimatMMBSException(string message) : base(message) { }
        public AnimatMMBSException(string message, Exception innerException) : base(message, innerException) { }
        public AnimatMMBSException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }

}
