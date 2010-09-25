using ESMEWorkBench.ViewModels.Layers;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESMEWorkBench.ViewModels.Map
{
    public class MapLayerReorderDescriptor
    {
        public MapLayerReorderCommand MapLayerReorderCommand { get; set; }
        public LayerViewModel SourceLayer { get; set; }
        public LayerViewModel TargetLayer { get; set; }
    }

    public enum MapLayerReorderCommand
    {
        MoveToBack,
        MoveBackward,
        MoveForward,
        MoveToFront
    }
}