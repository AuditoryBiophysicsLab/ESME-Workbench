using System;
using System.IO;
using System.Text;
using System.Windows;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Xml.Xsl;

namespace HRC.WPF
{
    public static class VisualExtensions
    {
        // adapted from http://blogs.msdn.com/b/jaimer/archive/2009/07/03/rendertargetbitmap-tips.aspx
        public static BitmapSource ToBitmapSource(this Visual thevisual)
        {
            var m = PresentationSource.FromVisual(thevisual).CompositionTarget.TransformToDevice;
            var dpiX = 96 * m.M11;
            var dpiY = 96 * m.M22;

            var bounds = VisualTreeHelper.GetDescendantBounds(thevisual);
            var rtb = new RenderTargetBitmap((int)(bounds.Width * dpiX / 96.0),
                                                            (int)(bounds.Height * dpiY / 96.0),
                                                            dpiX,
                                                            dpiY,
                                                            PixelFormats.Pbgra32);
            var dv = new DrawingVisual();
            using (var ctx = dv.RenderOpen())
            {
                var vb = new VisualBrush(thevisual);
                ctx.DrawRectangle(vb, null, new Rect(new Point(), bounds.Size));
            }
            rtb.Render(dv);
            return rtb;
        }

        public static void ToImageFile(this Visual theVisual, string fileName)
        {
            BitmapEncoder encoder;
            switch (Path.GetExtension(fileName).ToLower())
            {
                case ".jpg":
                case ".jpeg":
                    encoder = new JpegBitmapEncoder();
                    break;
                case ".png":
                    encoder = new PngBitmapEncoder();
                    break;
                case ".bmp":
                    encoder = new BmpBitmapEncoder();
                    break;
                case ".gif":
                    encoder = new GifBitmapEncoder();
                    break;
                case ".tiff":
                    encoder = new TiffBitmapEncoder();
                    break;
                default:
                    throw new EncoderFallbackException("The Specified Filename is not a known image type.  Supported image formats are jpeg, png, bmp, gif, and tiff.");
            }
            encoder.Frames.Add(BitmapFrame.Create(theVisual.ToBitmapSource()));
            using (var stream = new FileStream(fileName, FileMode.Create)) encoder.Save(stream);
        }

        public static void ToSVG(this Visual theVisual, string fileName)
        {
           throw new NotImplementedException(""); 
            var transform = new XslTransform();
            transform.Load("xaml2svg.xsl");
            //transform.Transform(sourceXML, fileName);

        }
    }
}
