using System.IO;
using System.Windows.Media;
using ESME.NEMO;
using ESME.Platform;
using ESMEWorkBench.ViewModels.Map;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESMEWorkBench.ViewModels.Layers
{
#if false
    public class ScenarioFileLayerViewModel : LayerViewModel
    {
        public ScenarioFileLayerViewModel(NemoFile nemoFile, GeoCollection<Overlay> overlays)
        {
            LayerName = Path.GetFileNameWithoutExtension(nemoFile.FileName) + "sim area";
            overlays.Add(new OverlayShapesMapLayer(nemoFile.Scenario.OverlayFile.Shapes));
            var platformCount = 0;
            foreach (var platform in nemoFile.Scenario.Platforms)
            {
                platformCount++;
                var layerName = "Platform " + platformCount + ": " + platform.Name;
                var trackLayerViewModel = new PlatformTrackLayerViewModel(platform, layerName + " track", overlays);
                var opAreaCount = 0;
                foreach (var trackdef in platform.Trackdefs)
                {
                    opAreaCount++;
                    var opAreaName = layerName + " op area" + (platform.Trackdefs.Count >= 1 ? " " + opAreaCount : "");
                    var opAreaLayerViewModel = new PlatformOpAreaLayerViewModel(trackdef, opAreaName, overlays);
                    //Children.Add(opAreaLayerViewModel);
                }

                //Children.Add(new ScenarioPlatformLayerViewModel(platform, platformLayerName, overlays));
            }
        }
    }

    public class ScenarioPlatformLayerViewModel : LayerViewModel
    {
        public ScenarioPlatformLayerViewModel(NemoPlatform nemoPlatform, string layerName, GeoCollection<Overlay> overlays) 
            : base(layerName)
        {
            var trackLayerViewModel = new PlatformTrackLayerViewModel(nemoPlatform, layerName + " track", overlays);
            //Children.Add(trackLayerViewModel);
            var opAreaCount = 0;
            foreach (var trackdef in nemoPlatform.Trackdefs)
            {
                opAreaCount++;
                var opAreaName = nemoPlatform.Trackdefs.Count == 1 ? "OpArea" : "OpArea " + opAreaCount;
                var opAreaLayerViewModel = new PlatformOpAreaLayerViewModel(trackdef, opAreaName, overlays);
                //Children.Add(opAreaLayerViewModel);
            }
        }
    }

    public class PlatformOpAreaLayerViewModel : LayerViewModel
    {
        public PlatformOpAreaLayerViewModel(NemoTrackdef nemoTrackdef, string layerName, GeoCollection<Overlay> overlays)
            : base(layerName)
        {
            var overlay = new OverlayShapesMapLayer();
            foreach (var shape in nemoTrackdef.OverlayFile.Shapes) overlay.OverlayShapes.Add(shape);
            overlay.CommitShapes();
            overlays.Add(layerName + Path.GetFileNameWithoutExtension(Path.GetRandomFileName()), overlay);
        }
        
    }

    public class PlatformTrackLayerViewModel : LayerViewModel
    {
        public PlatformTrackLayerViewModel(NemoPlatform nemoPlatform, string layerName, GeoCollection<Overlay> overlays)
            : base(layerName)
        {
            var behavior = new BehaviorModel(nemoPlatform);
            var lineStyle = new CustomStartEndLineStyle(PointSymbolType.Circle, Colors.Green, 5, PointSymbolType.Square, Colors.Red, 5, Colors.DarkGray, 1);
            var overlay = new OverlayShapesMapLayer { LineStyle = lineStyle };
            overlay.OverlayShapes.Add(behavior.CourseOverlay);
            overlay.CommitShapes();
            overlays.Add(layerName + Path.GetFileNameWithoutExtension(Path.GetRandomFileName()), overlay);
        }
    }
#endif
}