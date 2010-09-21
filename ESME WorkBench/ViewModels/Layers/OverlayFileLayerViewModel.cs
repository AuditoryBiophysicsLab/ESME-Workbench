using System.IO;
using ESME.Overlay;
using ESMEWorkBench.ViewModels.Main;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESMEWorkBench.ViewModels.Layers
{
    public class OverlayFileLayerViewModel : OverlayShapesLayerViewModel
    {
        public OverlayFileLayerViewModel(string overlayFileName, MapViewModel mapViewModel) : base((Overlay) null, Path.GetFileNameWithoutExtension(overlayFileName), mapViewModel)
        {
            Overlay = new LayerOverlay
                      {
                          TileType = TileType.SingleTile
                      };
            mapViewModel.Overlays.Add(Overlay);

            var overlayFile = new OverlayFile(overlayFileName);
            foreach (var s in overlayFile.Shapes) OverlayShapes.Add(s);
            CommitShapes();
        }
    }
}