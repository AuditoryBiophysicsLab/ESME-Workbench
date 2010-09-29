using System.IO;
using System.Xml.Serialization;
using ESMEWorkBench.ViewModels.Layers;
using ThinkGeo.MapSuite.Core;

namespace ESMEWorkBench.ViewModels.Map
{
    public class ShapefileMapLayer : MapLayer
    {
        public ShapefileMapLayer() { LayerType = LayerType.Shapefile; }

        #region public string ShapefileName { get; set; }
        [XmlElement]
        public string ShapefileName
        {
            get { return _shapefileName; }
            set
            {
                if (_shapefileName == value) return;
                _shapefileName = value;

                Name = Path.GetFileNameWithoutExtension(_shapefileName);
                string projection = null;
                var projectionFile = Path.Combine(Path.GetDirectoryName(_shapefileName), "projection.txt");
                if (File.Exists(projectionFile))
                {
                    using (var sr = new StreamReader(projectionFile)) projection = sr.ReadToEnd();
                }
                var newLayer = new ShapeFileFeatureLayer(_shapefileName);
                if (AreaStyle == null)
                {
                    newLayer.ZoomLevelSet.ZoomLevel01.DefaultAreaStyle = AreaStyles.County1;
                    newLayer.ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;
                }
                else
                {
                    newLayer.ZoomLevelSet.ZoomLevel01.CustomStyles.Clear();
                    newLayer.ZoomLevelSet.ZoomLevel01.CustomStyles.Add(AreaStyle);
                    newLayer.ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;
                }
                newLayer.RequireIndex = false;
                if (projection != null)
                    newLayer.FeatureSource.Projection = new ManagedProj4Projection
                    {
                        InternalProjectionParameters = projection,
                        ExternalProjectionParameters = ManagedProj4Projection.GetEpsgParameters(4326),
                    };
                Layers.Clear();
                Layers.Add(newLayer);
            }
        }

        string _shapefileName;

        #endregion
    }
}