using System.IO;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESMEWorkBench.ViewModels.Map
{
    public class MapLayer : LayerOverlay
    {
        public MapLayer() { }

        public MapLayer(string fileName) 
        { 
            FileName = fileName;
            Name = Path.GetFileNameWithoutExtension(fileName);
        }

        public string FileName { get; protected set; }
    }
}