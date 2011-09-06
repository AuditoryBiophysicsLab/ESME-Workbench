using System;
using System.IO;
using System.Text;
using System.Xml.Serialization;
using ThinkGeo.MapSuite.Core;

namespace ESME.Mapping
{
    [Serializable]
    public class MyGdiPlusRasterLayer : GdiPlusRasterLayer
    {
        public MyGdiPlusRasterLayer(string fileName) : base(fileName) { }
        public new RasterSource ImageSource
        {
            get { return base.ImageSource; }
            set { base.ImageSource = value; }
        }
    }

    [Serializable]
    public class RasterMapLayer : MapLayerViewModel
    {
        public RasterMapLayer() {LayerOverlay.Layers.Clear();}

        GdiPlusRasterLayer _layer;

        public float North { get; set; }
        public float South { get; set; }
        public float East { get; set; }
        public float West { get; set; }

        /// <summary>
        /// Pixel size, in degrees.  This value is used to calculate the true bounding box extent in the case where
        /// North, South, East and West refer to pixel centers.  If this calculation should not be performed, this
        /// value should be set to zero
        /// </summary>
        public float PixelSize { get; set; }

        #region public string RasterFilename { get; set; }

        public string RasterFilename
        {
            get { return _rasterFilename; }
            set
            {
                LayerOverlay.Layers.Clear();
                _rasterFilename = value;
                _worldFilename = Path.Combine(Path.GetDirectoryName(_rasterFilename), Path.GetFileNameWithoutExtension(_rasterFilename)) + ".bpw";
                if (!File.Exists(_worldFilename)) using (var writer = new StreamWriter(_worldFilename, false)) writer.Write(WorldFileContents);

                _layer = new GdiPlusRasterLayer(_rasterFilename, new RectangleShape(West, North, East, South))
                {
                    UpperThreshold = double.MaxValue,
                    LowerThreshold = 0,
                    IsGrayscale = false
                };
                LayerOverlay.Layers.Add(_layer);
            }
        }

        string _rasterFilename;
        string _worldFilename;

        #endregion

        [XmlIgnore]
        string WorldFileContents
        {
            get
            {
                var sb = new StringBuilder();
                sb.AppendLine(PixelSize.ToString());
                sb.AppendLine("0.0");
                sb.AppendLine("0.0");
                sb.AppendLine(PixelSize.ToString());
                sb.AppendLine(West.ToString());
                sb.AppendLine(North.ToString());
                return sb.ToString();
            }
        }
    }
}