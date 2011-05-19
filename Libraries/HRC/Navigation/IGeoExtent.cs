using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace HRC.Navigation
{
    public interface IGeoExtent
    {
        BoundingCircle BoundingCircle { get; }
    }
}
