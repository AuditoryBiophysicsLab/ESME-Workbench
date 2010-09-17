using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ESME.Model
{
    public class ESMEModelException : Exception 
    {
        public ESMEModelException(string Message) : base(Message) { }
    }

    public class DuplicateListItemException : ESMEModelException
    {
        public DuplicateListItemException(string Message) : base(Message) { }
    }

    public class FileNameFormatException : ESMEModelException
    {
        public FileNameFormatException(string Message) : base(Message) { }
    }

    public class FileIsEmptyException : ESMEModelException
    {
        public FileIsEmptyException(string Message) : base(Message) { }
    }

    public class FileFormatException : ESMEModelException
    {
        public FileFormatException(string Message) : base(Message) { }
    }

    public class BathymetryOutOfBoundsException : ESMEModelException
    {
        public BathymetryOutOfBoundsException(string Message) : base(Message) { }
    }

    public class SpeciesNotFoundException : ESMEModelException
    {
        public SpeciesNotFoundException(string Message) : base(Message) { }
    }

    public class PropertyNotInitializedException : ESMEModelException
    {
        public PropertyNotInitializedException(string Message) : base(Message) { }
    }

}
