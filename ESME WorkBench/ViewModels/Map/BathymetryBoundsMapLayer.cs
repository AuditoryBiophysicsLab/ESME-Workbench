using System.IO;
using ESME.Environment;

namespace ESMEWorkBench.ViewModels.Map
{
    public class BathymetryBoundsMapLayer : OverlayShapesMapLayer
    {
        public BathymetryBoundsMapLayer(string bathymetryFileName)
        {
            var bathymetry = new Bathymetry(bathymetryFileName);
            Name = Path.GetFileNameWithoutExtension(bathymetry + " bathymetry");
            OverlayShapes.Add(bathymetry.BoundingBox);
            CommitShapes();
        }

    }
}