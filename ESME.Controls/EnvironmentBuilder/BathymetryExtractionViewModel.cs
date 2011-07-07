using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Cinch;

namespace ESME.Views.EnvironmentBuilder
{
    public class BathymetryExtractionViewModel : ViewModelBase
    {
        public readonly float[] AvailableResolutions = {0.05f, 0.10f, 0.50f, 1.0f, 2.0f};
    }
}
