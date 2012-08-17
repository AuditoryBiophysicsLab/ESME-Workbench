using System.Collections.Specialized;
using System.Diagnostics;
using System.Linq;
using System.Windows;
using System.Windows.Media;
using System.Windows.Shapes;
using ESME.Views.Controls;
using HRC.Aspects;
using HRC.Services;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;
using MEFedMVVM.ViewModelLocator;
using System.ComponentModel.Composition;

namespace DavesWPFTester
{
    [ExportViewModel("MainWindowViewModel")]
    public class MainWindowViewModel : ViewModelBase
    {
        readonly IViewAwareStatus _viewAwareStatus;

        [ImportingConstructor]
        public MainWindowViewModel(IViewAwareStatus viewAwareStatus)
        {
            _viewAwareStatus = viewAwareStatus;
            _viewAwareStatus.ViewLoaded += () =>
            {
                ShapeCanvas = ((MainWindow)_viewAwareStatus.View).ShapeCanvas;
                DrawGridShapes();
            };
            PropertyChanged += (s, e) =>
            {
                Debug.WriteLine(string.Format("PropertyChanged: {0}", e.PropertyName));
                switch (e.PropertyName)
                {
                    case "XAxisMajorTicks":
                        if (XAxisMajorTicks != null) XAxisMajorTicks.CollectionChanged += TickCollectionChanged;
                        break;
                    case "XAxisMinorTicks":
                        if (XAxisMinorTicks != null) XAxisMinorTicks.CollectionChanged += TickCollectionChanged;
                        break;
                    case "YAxisMajorTicks":
                        if (YAxisMajorTicks != null) YAxisMajorTicks.CollectionChanged += TickCollectionChanged;
                        break;
                    case "YAxisMinorTicks":
                        if (YAxisMinorTicks != null) YAxisMinorTicks.CollectionChanged += TickCollectionChanged;
                        break;
                }
            };
        }

        #region ViewClosingCommand

        public SimpleCommand<object, EventToCommandArgs> ViewClosingCommand { get { return _viewClosing ?? (_viewClosing = new SimpleCommand<object, EventToCommandArgs>(vcArgs => Properties.Settings.Default.Save())); } }

        SimpleCommand<object, EventToCommandArgs> _viewClosing;

        #endregion

        public ShapeCanvas ShapeCanvas { get; private set; }
        public ObservableList<AxisTick> XAxisMajorTicks { get; set; }
        public ObservableList<AxisTick> XAxisMinorTicks { get; set; }
        public ObservableList<AxisTick> YAxisMajorTicks { get; set; }
        public ObservableList<AxisTick> YAxisMinorTicks { get; set; }

        public DrawingImage DisplayImage { get; set; }

        void TickCollectionChanged(object sender, NotifyCollectionChangedEventArgs args) { DrawGridShapes(); }

        public void DrawGridDrawingContext()
        {
            if (XAxisMajorTicks == null || XAxisMinorTicks == null || YAxisMajorTicks == null || YAxisMinorTicks == null) return;
            if (XAxisMajorTicks.Count == 0 || XAxisMinorTicks.Count == 0 || YAxisMajorTicks.Count == 0 || YAxisMinorTicks.Count == 0) return;
            var grayPen = new Pen(Brushes.LightGray, 1);
            var blackPen = new Pen(Brushes.Black, 1);
            var drawing = new DrawingGroup();
            var endX = ShapeCanvas.ActualWidth;
            var endY = ShapeCanvas.ActualHeight;
            using (var dc = drawing.Open())
            {
                foreach (var xMinor in XAxisMinorTicks) dc.DrawLine(grayPen, new Point(xMinor.Location, 0), new Point(xMinor.Location, endY));
                foreach (var yMinor in YAxisMinorTicks) dc.DrawLine(grayPen, new Point(0, yMinor.Location), new Point(endX, yMinor.Location));
                foreach (var xMajor in XAxisMajorTicks.Skip(1).Take(XAxisMajorTicks.Count - 2)) dc.DrawLine(blackPen, new Point(xMajor.Location, 0), new Point(xMajor.Location, endY));
                foreach (var yMajor in YAxisMajorTicks.Skip(1).Take(YAxisMajorTicks.Count - 2)) dc.DrawLine(blackPen, new Point(0, yMajor.Location), new Point(endX, yMajor.Location));
            }
            DisplayImage = new DrawingImage(drawing);
        }

        public void DrawGridShapes()
        {
            if (ShapeCanvas == null) return;
            if (XAxisMajorTicks == null || XAxisMinorTicks == null || YAxisMajorTicks == null || YAxisMinorTicks == null) return;
            if (XAxisMajorTicks.Count == 0 || XAxisMinorTicks.Count == 0 || YAxisMajorTicks.Count == 0 || YAxisMinorTicks.Count == 0) return;
            var endX = ShapeCanvas.ActualWidth;
            var endY = ShapeCanvas.ActualHeight;

            var minorGeometry = new StreamGeometry();
            Shapes.Clear();

            using (var ctx = minorGeometry.Open())
            {
                foreach (var coordinate in XAxisMinorTicks)
                {
                    ctx.BeginFigure(new Point(coordinate.Location, 0), false, false);
                    ctx.LineTo(new Point(coordinate.Location, endY), true, false);
                }
                foreach (var coordinate in YAxisMinorTicks)
                {
                    ctx.BeginFigure(new Point(0, coordinate.Location), false, false);
                    ctx.LineTo(new Point(endX, coordinate.Location), true, false);
                }
            }
            minorGeometry.Freeze();
            Shapes.Add(new Path
            {
                Stroke = Brushes.LightGray,
                StrokeThickness = 1,
                StrokeMiterLimit = 1,
                StrokeStartLineCap = PenLineCap.Flat,
                StrokeEndLineCap = PenLineCap.Flat,
                SnapsToDevicePixels = true,
                Data = minorGeometry,
                ToolTip = "Minor axis tick"
            });
            var majorGeometry = new StreamGeometry();

            using (var ctx = majorGeometry.Open())
            {
                foreach (var coordinate in XAxisMajorTicks.Skip(1).Take(XAxisMajorTicks.Count - 2))
                {
                    ctx.BeginFigure(new Point(coordinate.Location, 0), false, false);
                    ctx.LineTo(new Point(coordinate.Location, endY), true, false);
                }
                foreach (var coordinate in YAxisMajorTicks.Skip(1).Take(YAxisMajorTicks.Count - 2))
                {
                    ctx.BeginFigure(new Point(0, coordinate.Location), false, false);
                    ctx.LineTo(new Point(endX, coordinate.Location), true, false);
                }
            }
            majorGeometry.Freeze();
            Shapes.Add(new Path
            {
                Stroke = Brushes.Black,
                StrokeThickness = 1,
                StrokeMiterLimit = 1,
                StrokeStartLineCap = PenLineCap.Flat,
                StrokeEndLineCap = PenLineCap.Flat,
                SnapsToDevicePixels = true,
                Data = majorGeometry,
                ToolTip = "Major axis tick"
            });
        }

        [Initialize] public ObservableList<Shape> Shapes { get; set; }
    }

}