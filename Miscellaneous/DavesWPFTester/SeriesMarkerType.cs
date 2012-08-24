using System;
using System.Windows;
using System.Windows.Media;
using HRC.Navigation;

namespace DavesWPFTester
{
    public static class SeriesMarkerType
    {
        public static Action<StreamGeometryContext, Point, double> Circle
        {
            get
            {
                return (ctx, point, size) =>
                {
                    var halfSize = size / 2;
                    ctx.BeginFigure(new Point(point.X - halfSize, point.Y), true, true);
                    ctx.ArcTo(new Point(point.X + halfSize, point.Y), new Size(halfSize, halfSize), 180, false, SweepDirection.Clockwise, true, true);
                    ctx.ArcTo(new Point(point.X - halfSize, point.Y), new Size(halfSize, halfSize), 180, false, SweepDirection.Clockwise, true, true);
                };
            }
        }

        public static Action<StreamGeometryContext, Point, double> Square { get { return (ctx, point, size) => Polygon(ctx, point, size, Math.PI / 4, 4); } }
        public static Action<StreamGeometryContext, Point, double> Diamond { get { return (ctx, point, size) => Polygon(ctx, point, size, 0, 4); } }
        public static Action<StreamGeometryContext, Point, double> UpTriangle { get { return (ctx, point, size) => Polygon(ctx, point, size, 0, 3); } }
        public static Action<StreamGeometryContext, Point, double> DownTriangle { get { return (ctx, point, size) => Polygon(ctx, point, size, Math.PI / 3, 3); } }
        public static Action<StreamGeometryContext, Point, double> LeftTriangle { get { return (ctx, point, size) => Polygon(ctx, point, size, -Math.PI / 2, 3); } }
        public static Action<StreamGeometryContext, Point, double> RightTriangle { get { return (ctx, point, size) => Polygon(ctx, point, size, Math.PI / 2, 3); } }

        public static Action<StreamGeometryContext, Point, double> Plus { get { return (ctx, point, size) => OpenFigure(ctx, point, size, 0, 2); } }
        public static Action<StreamGeometryContext, Point, double> Cross { get { return (ctx, point, size) => OpenFigure(ctx, point, size, Math.PI / 4, 2); } }
        public static Action<StreamGeometryContext, Point, double> Asterisk { get { return (ctx, point, size) => OpenFigure(ctx, point, size, 0, 3); } }

        public static Action<StreamGeometryContext, Point, double> Pentagram { get { return (ctx, point, size) => Star(ctx, point, size, 0, 5); } }
        public static Action<StreamGeometryContext, Point, double> Hexagram { get { return (ctx, point, size) => Star(ctx, point, size, 0, 6); } }

        static void OpenFigure(StreamGeometryContext ctx, Point point, double size, double startAngle, int steps)
        {
            var halfSize = size / 2;
            for (var angle = startAngle; angle <= Math.PI; angle += Math.PI / steps)
            {
                var xOffset = halfSize * Math.Sin(angle);
                var yOffset = halfSize * Math.Cos(angle);
                ctx.BeginFigure(new Point(point.X + xOffset, point.Y - yOffset), false, false);
                ctx.LineTo(new Point(point.X - xOffset, point.Y + yOffset), true, false);
            }            
        }

        static void Polygon(StreamGeometryContext ctx, Point point, double size, double startAngle, int steps)
        {
            var halfSize = size / 2;
            var xOffset = halfSize * Math.Sin(startAngle);
            var yOffset = halfSize * Math.Cos(startAngle);
            ctx.BeginFigure(new Point(point.X + xOffset, point.Y - yOffset), true, true);
            for (var angle = startAngle + (MoreMath.TwoPi / steps); angle < MoreMath.TwoPi; angle += MoreMath.TwoPi / steps) ctx.LineTo(new Point(point.X + (halfSize * Math.Sin(angle)), point.Y - (halfSize * Math.Cos(angle))), true, true);
        }

        static void Star(StreamGeometryContext ctx, Point point, double size, double startAngle, int steps)
        {
            var halfSize = size / 2;
            var quarterSize = halfSize / 2;
            var xOffset = halfSize * Math.Sin(startAngle);
            var yOffset = halfSize * Math.Cos(startAngle);
            ctx.BeginFigure(new Point(point.X + xOffset, point.Y - yOffset), true, true);
            for (var angle = Math.PI / steps; angle < MoreMath.TwoPi; angle += Math.PI / steps)
            {
                ctx.LineTo(new Point(point.X + (quarterSize * Math.Sin(angle)), point.Y - (quarterSize * Math.Cos(angle))), true, true);
                angle += Math.PI / steps;
                ctx.LineTo(new Point(point.X + (halfSize * Math.Sin(angle)), point.Y - (halfSize * Math.Cos(angle))), true, true);
            }
        }
    }
}