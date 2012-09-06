using System.Windows;
using System.Windows.Media;

namespace DavesWPFTester.Transforms
{
    /// <summary>
    /// Swaps X and Y coordinate
    /// </summary>
    public class SwapTransform : GeneralTransform
    {
        public SwapTransform() { }

        protected override Freezable CreateInstanceCore() { return new SwapTransform(); }
        public override bool TryTransform(Point inPoint, out Point result)
        {
            result = new Point(inPoint.Y, inPoint.X);
            return true;
        }
        public override Rect TransformBounds(Rect rect) { return new Rect(Transform(rect.TopLeft), Transform(rect.BottomRight)); }
        public override GeneralTransform Inverse { get { return new SwapTransform(); } }
    }
}