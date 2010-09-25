using System.IO;
using ThinkGeo.MapSuite.Core;

namespace ESMEWorkBench.ViewModels.Map
{
    public class ShapefileMapLayer : MapLayer
    {
        public ShapefileMapLayer(string shapefileFileName) : base(shapefileFileName)
        {
            string projection = null;
            var projectionFile = Path.Combine(Path.GetDirectoryName(shapefileFileName), "projection.txt");
            if (File.Exists(projectionFile))
            {
                using (var sr = new StreamReader(projectionFile)) projection = sr.ReadToEnd();
            }
            var newLayer = new ShapeFileFeatureLayer(shapefileFileName);
            newLayer.ZoomLevelSet.ZoomLevel01.DefaultAreaStyle = AreaStyles.County1;
            newLayer.ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;
            newLayer.RequireIndex = false;
            if (projection != null)
                newLayer.FeatureSource.Projection = new ManagedProj4Projection
                                                    {
                                                        InternalProjectionParameters = projection,
                                                        ExternalProjectionParameters = ManagedProj4Projection.GetEpsgParameters(4326),
                                                    };
            Layers.Add(newLayer);
        }
    }
}