using ESME.Environment;

namespace ESMEWorkBench.ViewModels.Map
{
    public class BathymetryBoundsMapLayer : OverlayShapesMapLayer
    {
        public BathymetryBoundsMapLayer(string bathymetryFileName)
        {
            var bathymetry = new Bathymetry(bathymetryFileName);
            OverlayShapes.Add(bathymetry.BoundingBox);
            CommitShapes();
        }

    }
}