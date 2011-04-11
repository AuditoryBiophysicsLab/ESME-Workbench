using System.IO;
using System.Text;
using System.Xml.Serialization;
using ThinkGeo.MapSuite.Core;

namespace ESMEWorkBench.ViewModels.Map
{
    public class MyGdiPlusRasterLayer : GdiPlusRasterLayer
    {
        public MyGdiPlusRasterLayer(string fileName) : base(fileName) { }
        public new RasterSource ImageSource
        {
            get { return base.ImageSource; }
            set { base.ImageSource = value; }
        }
    }

    public class RasterMapLayer : MapLayerViewModel
    {
        public RasterMapLayer() {LayerOverlay.Layers.Clear();}

        MyGdiPlusRasterLayer _layer;
        byte[] _fileBuffer;
        MemoryStream _memoryStream;

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
                _rasterFilename = value;
                var fi = new FileInfo(_rasterFilename);
                _fileBuffer = new byte[fi.Length];
                using (var fileStream = File.OpenRead(_rasterFilename))
                    fileStream.Read(_fileBuffer, 0, _fileBuffer.Length);
                _memoryStream = new MemoryStream(_fileBuffer);
                RasterStream = _memoryStream;
            }
        }

        string _rasterFilename;

        #endregion

        #region public Stream RasterStream { get; set; }
        [XmlIgnore]
        public Stream RasterStream
        {
            get { return _rasterStream; }
            set
            {
                //if (_rasterStream == value) return;
                _rasterStream = value;
                _layer = new MyGdiPlusRasterLayer("foo");

                var halfPixel = PixelSize / 2.0;
                var north = North + halfPixel;
                var south = South - halfPixel;
                var east = East + halfPixel;
                var west = West - halfPixel;
                //var imageSource = new GdiPlusRasterSource(_layer.PathFilename, new RectangleShape(west, north, east, south));
                var imageSource = new GdiPlusRasterSource("foo.jpg");
#if true
                imageSource.StreamLoading += (s, e) =>
                                                    {
#if false
                                                        // Create a simple bitmap and color it purple
                                                        Bitmap bitmap = new Bitmap(100, 100);
                                                        using (Graphics g = Graphics.FromImage(bitmap))
                                                        {
                                                            g.Clear(Color.Purple);
                                                        }

                                                        // Lock the bits of the bitmap
                                                        BitmapData bitmapData = bitmap.LockBits(new Rectangle(0, 0, bitmap.Width, bitmap.Height),
                                                                                                ImageLockMode.ReadWrite,
                                                                                                PixelFormat.Format32bppArgb);

                                                        // Copy the bytes of the bitmap to a byte array
                                                        byte[] bitmapBytes = new byte[bitmapData.Stride * bitmapData.Height];
                                                        Marshal.Copy(bitmapData.Scan0, bitmapBytes, 0, bitmapBytes.Length);

                                                        // Unlock the bitmap bits
                                                        bitmap.UnlockBits(bitmapData);

                                                        // Create a memory stream of the bitmap bytes
                                                        MemoryStream bitmapStream = new MemoryStream(bitmapBytes);
                                                        return;
#endif
                                                        if (e.AlternateStreamName.ToLower().Contains(".jpg"))
                                                        {
                                                            //e.AlternateStream = File.OpenRead(@"C:\Users\Dave Anderson\Desktop\bathymetric_small.jpg");
                                                            e.AlternateStream = _rasterStream;
                                                            e.AlternateStreamName = "bathymetric_small.jpg";
                                                        }
                                                        else if (e.AlternateStreamName.ToLower().Contains(".jgw"))
                                                        {
                                                            //e.AlternateStream = File.OpenRead(@"C:\Users\Dave Anderson\Desktop\bathymetric_small.jgw");
                                                            e.AlternateStream = WorldFileStream;
                                                            e.AlternateStreamName = "bathymetric_small.jgw";
                                                        }
                                                    };
                _layer.ImageSource = imageSource;

#endif
                _layer.UpperThreshold = double.MaxValue;
                _layer.LowerThreshold = 1;
                _layer.IsGrayscale = false;

                LayerOverlay.Layers.Add(_layer);
            }
        }

        Stream _rasterStream;

        #endregion
#if true
        [XmlIgnore]
        MemoryStream WorldFileStream
        {
            get
            {
                //var wf = new WorldFile(PixelSize, 0, 0, -PixelSize, West, North);
                var sb = new StringBuilder();
                sb.AppendLine(PixelSize.ToString());
                sb.AppendLine("0.0");
                sb.AppendLine("0.0");
                sb.AppendLine(PixelSize.ToString());
                sb.AppendLine(West.ToString());
                sb.AppendLine(North.ToString());
                var byteArray = Encoding.ASCII.GetBytes(sb.ToString());
                return new MemoryStream(byteArray);
            }
        }
#endif    

    }
}