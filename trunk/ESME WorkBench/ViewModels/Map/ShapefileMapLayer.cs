using System.IO;
using System.Xml.Serialization;
using ESMEWorkBench.ViewModels.Layers;
using ThinkGeo.MapSuite.Core;

namespace ESMEWorkBench.ViewModels.Map
{
    public class ShapefileMapLayer : MapLayerViewModel
    {
        public ShapefileMapLayer() { LayerType = LayerType.Shapefile; LayerOverlay.Layers.Clear(); }

        #region public string ShapefileName { get; set; }
        [XmlElement]
        public string ShapefileName
        {
            get { return _shapefileName; }
            set
            {
                if (_shapefileName == value) return;
                _shapefileName = value;

                switch (LayerType)
                {
                    case LayerType.Shapefile:
                        Name = Path.GetFileNameWithoutExtension(_shapefileName);
                        break;
                    case LayerType.BaseMap:
                        Name = "Base Map";
                        break;
                }
                    
                string projection = null;
                var projectionFile = Path.Combine(Path.GetDirectoryName(_shapefileName), "projection.txt");
                if (File.Exists(projectionFile))
                {
                    using (var sr = new StreamReader(projectionFile)) projection = sr.ReadToEnd();
                }
                var newLayer = new ShapeFileFeatureLayer(_shapefileName);
#if true
                if (AreaStyle == null)
                {
                    newLayer.ZoomLevelSet.ZoomLevel01.DefaultAreaStyle = AreaStyles.County1;
                    newLayer.ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;
                }
                else
                {
                    newLayer.ZoomLevelSet.ZoomLevel01.DefaultAreaStyle = AreaStyle;
                    newLayer.ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;
                }
#endif
                newLayer.RequireIndex = false;
                if (projection != null)
                    newLayer.FeatureSource.Projection = new ManagedProj4Projection
                    {
                        InternalProjectionParameters = projection,
                        ExternalProjectionParameters = ManagedProj4Projection.GetEpsgParameters(4326),
                    };
                LayerOverlay.Layers.Add(newLayer);
            }
        }

        string _shapefileName;

        #endregion
    }
}