using System.IO;
using System.Windows.Media;
using System.Xml.Serialization;
using ESME.Environment;
using ESME.Overlay;
using ESMEWorkBench.ViewModels.Layers;
using ThinkGeo.MapSuite.Core;

namespace ESMEWorkBench.ViewModels.Map
{
    public class BathymetryBoundsMapLayer : OverlayShapeMapLayer
    {
        public BathymetryBoundsMapLayer() { LayerType = LayerType.OverlayFile; Layers.Clear(); }

        #region public string BathymetryFileName { get; set; }
        [XmlElement]
        public string BathymetryFileName
        {
            get { return _bathymetryFileName; }
            set
            {
                if (_bathymetryFileName == value) return;
                _bathymetryFileName = value;
                Name = Path.GetFileNameWithoutExtension(_bathymetryFileName);

                var bathymetry = new Bathymetry(_bathymetryFileName);
                if (LineStyle == null)
                {
                    LineColor = Colors.Black;
                    LineWidth = 1;
                }
                Layers.Clear();
                Add(bathymetry.BoundingBox);
                Done();
            }
        }
        [XmlIgnore]
        string _bathymetryFileName;

        #endregion
    }
}