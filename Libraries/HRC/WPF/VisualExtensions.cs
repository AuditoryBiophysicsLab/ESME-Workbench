using System.Windows;
using System.Windows.Media;
using System.Windows.Media.Imaging;

namespace HRC.WPF
{
    public static class VisualExtensions
    {
        // adapted from http://blogs.msdn.com/b/jaimer/archive/2009/07/03/rendertargetbitmap-tips.aspx
        public static BitmapSource ToBitmapSource(this Visual thevisual)
        {
            // ReSharper disable PossibleNullReferenceException
            var m = PresentationSource.FromVisual(thevisual).CompositionTarget.TransformToDevice;
            // ReSharper restore PossibleNullReferenceException
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
    }
}
