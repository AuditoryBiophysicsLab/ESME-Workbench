using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESMEWorkBench.ViewModels.Map
{
    public class MapLayerReorderDescriptor
    {
        public Overlay Overlay { get; set; }
        public int DestinationIndex { get; set; }
    }
}