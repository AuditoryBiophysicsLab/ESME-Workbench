using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Shapes;
using HRC.ViewModels;

namespace HRC.Plotting
{
    public class ImageSeriesViewModel : SeriesViewModelBase
    {
        [UsedImplicitly] PropertyObserver<ImageSeriesViewModel> _propertyObserver;

        public ImageSeriesViewModel()
        {
            _propertyObserver = new PropertyObserver<ImageSeriesViewModel>(this)
                .RegisterHandler(d => d.ImageSource, RenderShapes)
                .RegisterHandler(d => d.Top, RenderShapes)
                .RegisterHandler(d => d.Left, RenderShapes)
                .RegisterHandler(d => d.Bottom, RenderShapes)
                .RegisterHandler(d => d.Right, RenderShapes);
        }
        protected override void RenderSample() { }
        public ImageSource ImageSource { get; set; }
        public double Top { get; set; }
        public double Left { get; set; }
        public double Bottom { get; set; }
        public double Right { get; set; }
        public override void RenderShapes()
        {
            if (XAxis == null || XAxis.ValueToPosition == null || YAxis == null || YAxis.ValueToPosition == null || ImageSource == null) return;
            var topLeft = new Point(XAxis.ValueToPosition(Left), YAxis.ValueToPosition(Top));
            var bottomRight = new Point(XAxis.ValueToPosition(Right), YAxis.ValueToPosition(Bottom));
            var rect = new Rect(topLeft, bottomRight);
            var imageBrush = new ImageBrush(ImageSource);
            var imageShape = new Path
            {
                Fill = imageBrush,
                Data = new RectangleGeometry(rect),
            };
            Panel.SetZIndex(imageShape, -1);
            if (Shapes.Count == 0) Shapes.Add(imageShape);
            else Shapes[0] = imageShape;
        }

        protected override void AddPoint(Point newPoint) { throw new NotImplementedException(); }
        protected override void RemovePoint(Point oldPoint) { throw new NotImplementedException(); }

        public int ZIndex
        {
            get
            {
                if (Shapes == null || Shapes.Count == 0) return -1;
                return Panel.GetZIndex(Shapes[0]);
            }
            set
            {
                if (Shapes == null || Shapes.Count == 0) return;
                Panel.SetZIndex(Shapes[0], value);
            }
        }

    }
}