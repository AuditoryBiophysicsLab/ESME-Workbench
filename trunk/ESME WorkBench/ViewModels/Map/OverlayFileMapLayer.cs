using ESME.Overlay;

namespace ESMEWorkBench.ViewModels.Map
{
    public class OverlayFileMapLayer : OverlayShapesMapLayer
    {
        public OverlayFileMapLayer(string overlayFileName)
        {
            var overlayFile = new OverlayFile(overlayFileName);
            foreach (var s in overlayFile.Shapes) OverlayShapes.Add(s);
            CommitShapes();
        }
    }

}