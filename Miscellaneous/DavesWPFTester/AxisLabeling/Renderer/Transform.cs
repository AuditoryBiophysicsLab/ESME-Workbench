using System;
using System.Windows;
using DavesWPFTester.AxisLabeling.Language;

namespace DavesWPFTester.AxisLabeling.Renderer
{
    public class Transform
    {
        public Func<Point, Point> transform = (p => p);
        public Func<Point, Point> untransform = (p => p);

        public Transform Concat(Transform t)
        {
            return new Transform
            {
                transform = point => t.transform(transform(point)),
                untransform = point => untransform(t.untransform(point))
            };
        }

        public static Transform Translate(Point p)
        {
            return new Transform
            {
                transform = point => new Point(point.X + p.X, point.Y+p.Y),
                untransform = point => new Point(point.X - p.X, point.Y - p.Y)
            };
        }

        public static Transform Range(Range x, Range y)
        {
            return new Transform
            {
                transform = point => new Point((float)x.ValueToRange(point.X), (float)y.ValueToRange(point.Y)),
                untransform = point => new Point((float)x.RangeToValue(point.X), (float)y.RangeToValue(point.Y)),
            };
        }

        public static Transform Range(Func<Range> x, Func<Range> y)
        {
            return new Transform
            {
                transform = point => new Point((float)x().ValueToRange(point.X), (float)y().ValueToRange(point.Y)),
                untransform = point => new Point((float)x().RangeToValue(point.X), (float)y().RangeToValue(point.Y)),
            };
        }
    }

#if false
    public abstract class SelectionManager
    {
        public abstract bool InteractiveUpdate(SelectionMode mode, IEnumerable<Mark> marks);
        public abstract bool Done(bool commit);
    }

    public abstract class Mark
    {
        public Func<Pen> fStroke;
        public Pen Stroke { get { return fStroke(); } set { fStroke = () => value; } }

        public String Name { get; set; }
        public SelectionManager SelectionManager { get; set; }
        public Brush Fill { get; set; }
        public object Data { get; set; }
        public double Order { get; set; }
        public Panel Parent { get; set; }
        public bool Visible { get; set; }        

        public Func<Point, bool> MouseOver, MouseEnter, MouseExit;

        protected Mark()
        {
            Name = null;
            SelectionManager = null;
            Stroke = new Pen(Brushes.Transparent, 1);
            Fill = new SolidColorBrush(Colors.Transparent);            
            Order = 0;
            MouseOver = p => false;
            MouseEnter = p => false;
            MouseExit = p => false;
            Visible = true;
        }

        public abstract void render(DrawingContext dc, Transform transform);
        public abstract bool hitTest(Point p, Transform transform);
        public abstract bool hitTest(Rect hit, Transform transform);
        public virtual Rect bounds(Transform transform)
        {
            throw new NotImplementedException();
        }


        // Interaction state
        public bool hovered;
    }

    public class BarMark : Mark
    {
        public Func<double> x0, y0, x1, y1;
        public Func<double> Width, Height;

        public BarMark()
        {
            x0 = () => 0;
            y0 = () => 0;
            x1 = () => 1;
            y1 = () => 1;

            Width = () => x1() - x0();
            Height = () => y1() - y0();
        }

        public BarMark(double x0, double y0, double x1, double y1)
        {
            this.x0 = () => x0;
            this.y0 = () => y0;
            this.x1 = () => x1;
            this.y1 = () => y1;

            Width = () => this.x1() - this.x0();
            Height = () => this.y1() - this.y0();
        }

        public override void render(DrawingContext dc, Transform t)
        {
            var zero = t.transform(new Point((float)x0(), (float)y0()));
            var one = t.transform(new Point((float)x1(), (float)y1()));
            var rect = RendererUtilities.MakeRect(zero.X, zero.Y, one.X, one.Y);

            System.Drawing.Drawing2D.SmoothingMode oldMode = g.SmoothingMode;
            g.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.Default;
            g.FillRectangle(Fill, rect);
            g.DrawRectangle(Stroke, rect.X, rect.Y, rect.Width, rect.Height);
            g.SmoothingMode = oldMode;
        }

        public override bool hitTest(Point p, Transform t)
        {
            var rect = bounds(t);
            return rect.Contains(p);
        }

        public override bool hitTest(Rect hit, Transform t)
        {
            var rect = bounds(t);
            return rect.IntersectsWith(hit);
        }

        public override Rect bounds(Transform t)
        {
            var zero = t.transform(new Point((float)x0(), (float)y0()));
            var one = t.transform(new Point((float)x1(), (float)y1()));
            var r = RendererUtilities.MakeRect(zero.X, zero.Y, one.X, one.Y);
            r.Inflate(Stroke.Thickness / 2.0f, Stroke.Thickness / 2.0f);
            return r;
        }
    }

    public class Panel : BarMark
    {
        public List<Mark> Children { get; set; }
        // transformation of children
        public Transform Transform { get; set; }

        public bool Clip = false;

        public Color color = Colors.Transparent;

        public Panel()
        {
            Transform = new Transform();
            Children = new List<Mark>();
            this.SetStrokeColor(Brushes.Transparent);
            this.SetFillColor(Colors.Transparent);
        }

        public void Add(Mark mark)
        {
            Children.Add(mark);
            mark.Parent = this;
        }

        public Transform ToScreen()
        {
            return Parent != null ? Transform.Concat(Transform.Translate(new Point((float)x0(), (float)y0()))).Concat(Parent.ToScreen()) : Transform;
        }

        public IEnumerable<Mark> hit(Point p, Transform t)
        {
            var nt = Transform.Concat(Transform.Translate(new Point((float)x0(), (float)y0()))).Concat(t);
            foreach (var c in Children.OrderBy(m => m.Order))
            {
                if (c.hitTest(p, nt))
                    yield return c;

                if (!(c is Panel)) continue;
                foreach (var m in (c as Panel).hit(p, nt))
                    yield return m;
            }
        }

        public IEnumerable<Mark> hit(Rect p, Transform t)
        {
            var nt = Transform.Concat(Transform.Translate(new Point((float)x0(), (float)y0()))).Concat(t);
            foreach (var c in Children.OrderBy(m => m.Order))
            {
                if (c.hitTest(p, nt))
                    yield return c;

                if (c is Panel)
                {
                    foreach (var m in (c as Panel).hit(p, nt))
                        yield return m;
                }
            }
        }  
    }

    public class DotMark : Mark
    {
        double x, y;
        float size = 5;
        public enum Shape { CIRCLE, X };
        Shape shape;

        public DotMark(double x, double y) : base()
        {
            this.x = x;
            this.y = y;
            this.shape = Shape.CIRCLE;
        }

        public DotMark SetSize(double d)
        {
            size = (float)d;
            return this;
        }

        public DotMark SetShape(Shape shape)
        {
            this.shape = shape;
            return this;
        }

        public override void render(DrawingContext dc, Transform transform)
        {
            if (!Visible)
                return;

            Point s = transform.transform(new Point((float)x, (float)y));

            switch(shape)
            {
                case Shape.CIRCLE:
                    g.FillEllipse(Fill, s.X - (size / 2), s.Y - (size / 2), size, size);
                    g.DrawEllipse(Stroke, s.X - (size / 2), s.Y - (size / 2), size, size);
                    break;
                case Shape.X:
                    g.DrawLine(Stroke, (float)(s.X - (size / 2)), (float)(s.Y - (size / 2)), (float)(s.X + (size / 2)), (float)(s.Y + (size / 2)));
                    g.DrawLine(Stroke, (float)(s.X - (size / 2)), (float)(s.Y + (size / 2)), (float)(s.X + (size / 2)), (float)(s.Y - (size / 2)));
                    break;
            }
        }

        public override bool hitTest(Point p, Transform transform)
        {
            Point s = transform.transform(new Point((float)x, (float)y));

            float pointsize = (float)(size * 1.3333);
            
            GraphicsPath gp = new GraphicsPath();
            gp.AddEllipse(s.X - (pointsize / 2), s.Y - (pointsize / 2), pointsize, pointsize);
            return gp.IsVisible(p);
        }

        public override bool hitTest(Rect hit, Transform transform)
        {
            var s = transform.transform(new Point((float)x, (float)y));

            var pointsize = (float)(size * 1.3333);

            GraphicsPath gp = new GraphicsPath();
            gp.AddEllipse(s.X - (pointsize / 2), s.Y- (pointsize / 2), pointsize, pointsize);
            Region rgn = new Region(gp);
            return rgn.IsVisible(hit);
        }

        public override Rect bounds(Transform t)
        {
            Point zero = t.transform(new Point((float)x, (float)y));
            Rect r = RendererUtilities.MakeRect(zero.X, zero.Y, zero.X, zero.Y);
            r.Inflate(size / 2.0f + Stroke.Thickness / 2.0f, size / 2.0f + Stroke.Thickness / 2.0f);
            return r;
            //return Children.OrderBy(m => m.Order).Aggregate(Rect.Empty, (t, m) => t.Union(m.bounds(transform.Concat(Transform))));
        }
    }

    public class LineMark : Mark
    {
        List<double> _x, _y;
        
        public LineMark(IEnumerable<double> x, IEnumerable<double> y) : base()
        {
            _x = x.ToList();
            _y = y.ToList();
            Stroke.Thickness = 2;
        }

        public LineMark(double x1, double x2, double y1, double y2) :
            this(new List<double> {x1, x2}, new List<double>{y1, y2})
        {}

        public LineMark SetPosition(double x1, double x2, double y1, double y2)
        {
            _x = new List<double> { x1, x2 };
            _y = new List<double> { y1, y2 };
            return this;
        }

        public override void render(DrawingContext dc, Transform t)
        {
            if (!Visible)
                return;

            if(_x.Count() < 2)
                return;

            var points = _x.Zip(_y, (px, py) => t.transform(new Point((float)px, (float)py))).ToArray();

            System.Drawing.Drawing2D.SmoothingMode oldMode = g.SmoothingMode;

            if(points.Count() == 2 && (points[0].X == points[1].X || points[0].Y == points[1].Y))
                g.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.Default;

            g.DrawLines(Stroke, points);

            g.SmoothingMode = oldMode;
        }

        public override bool hitTest(Point p, Transform t)
        {
            if(_x.Count() < 2)
                return false;

            var points = _x.Zip(_y, (px, py) => t.transform(new Point((float)px, (float)py)));

            GraphicsPath gp = new GraphicsPath();
            gp.AddLines(points.ToArray());
            return gp.IsVisible(p) || gp.IsOutlineVisible(p, Stroke);
        }

        public override bool hitTest(Rect hit, Transform t)
        {
            if (_x.Count() < 2)
                return false;

            var points = _x.Zip(_y, (px, py) => t.transform(new Point((float)px, (float)py)));

            GraphicsPath gp = new GraphicsPath();
            gp.AddLines(points.ToArray());
            return new Region(gp).IsVisible(hit);
        }

        public override Rect bounds(Transform t)
        {
            var points = _x.Zip(_y, (px, py) => t.transform(new Point((float)px, (float)py))).ToArray();
            var x1 = points.Select(p => p.X).Min();
            var y1 = points.Select(p => p.Y).Min();
            var x2 = points.Select(p => p.X).Max();
            var y2 = points.Select(p => p.Y).Max();
            var r = new Rect(x1, y1, x2 - x1, y2 - y1);
            r.Inflate(Stroke.Thickness / 2.0f, Stroke.Thickness / 2.0f);
            return r;
        }
    }

    public class AreaMark : Mark
    {
        readonly List<double> _x;
        List<double> _y;

        public AreaMark(IEnumerable<double> x, IEnumerable<double> y) : base()
        {
            _x = x.ToList();
            _y = y.ToList();
            Stroke.Brush = Brushes.Transparent;
            Fill = new SolidColorBrush(Colors.LightBlue);
        }

        public AreaMark(IEnumerable<double> xb, IEnumerable<double> yb, 
                                IEnumerable<double> xt, IEnumerable<double> yt)
            : this(xb.Concat(xt.Reverse()), yb.Concat(yt.Reverse()))
        {}

        public override void render(DrawingContext dc, Transform t)
        {
            if (!Visible)
                return;

            if(_x.Count() < 3)
                return;

            var points = _x.Zip(_y, (px, py) => t.transform(new Point((float)px, (float)py))).ToArray();

            g.FillPolygon(Fill, points);
            //g.DrawPolygon(Stroke, points);
            g.DrawLines(Stroke, points.Take(points.Count() / 2).ToArray());
            g.DrawLines(Stroke, points.Skip(points.Count() / 2).ToArray());
        }

        public override bool hitTest(Point p, Transform t)
        {
            if(_x.Count() < 3)
                return false;

            var points = _x.Zip(_y, (px, py) => t.transform(new Point((float)px, (float)py))).ToArray();

            GraphicsPath gp = new GraphicsPath();
            gp.AddPolygon(points);
            return gp.IsVisible(p);
        }

        public override bool hitTest(Rect hit, Transform t)
        {
            if (_x.Count() < 3)
                return false;

            var points = _x.Zip(_y, (px, py) => t.transform(new Point((float)px, (float)py))).ToArray();

            GraphicsPath gp = new GraphicsPath();
            gp.AddPolygon(points.ToArray());
            return new Region(gp).IsVisible(hit);
        }

        public override Rect bounds(Transform t)
        {
            var points = _x.Zip(_y, (px, py) => t.transform(new Point((float)px, (float)py))).ToArray();
            var x1 = points.Select(p => p.X).Min();
            var y1 = points.Select(p => p.Y).Min();
            var x2 = points.Select(p => p.X).Max();
            var y2 = points.Select(p => p.Y).Max();
            var r = new Rect(x1, y1, x2 - x1, y2 - y1);
            r.Inflate(Stroke.Thickness / 2.0f, Stroke.Thickness / 2.0f);
            return r;
        }
    }

    public class LabelMark : Mark
    {
        public double _x, _y;
        string originalText;
        readonly TextAlignment _textAlignment;
        readonly FontFamily _fontFamily;
        float _fontSize = 8f;
        Size size;
        bool horizontal;

        static readonly Dictionary<Tuple<string, FontFamily, float, FontStyle>, Size> CachedSizes = new Dictionary<Tuple<string, FontFamily, float, FontStyle>, Size>();

        enum BlockPlacement { SUPERSCRIPT, SUBSCRIPT };
        struct Block
        {
            public string text;
            public FontStyle style;
            public List<BlockPlacement> placement;
        }

        readonly List<Block> blocks = new List<Block>();

        public LabelMark(string text, double x, double y, Brush strokeBrush, TextAlignment textAlignment, FontFamily fontFamily, bool formatted = false)
        {
            originalText = text;
            _x = x;
            _y = y;
            _fontFamily = fontFamily;
            Stroke.Brush = strokeBrush;
            Fill = new SolidColorBrush(Colors.Transparent);
            _textAlignment = textAlignment;
            horizontal = true;
            if (formatted)
                parseText();
            else
                blocks.Add(new Block { text = text, style = FontStyles.Normal, placement = new List<BlockPlacement>()});
            FontSize(8);
        }

        public LabelMark SetPosition(double x, double y)
        {
            _x = x;
            _y = y;
            return this;
        }

        public LabelMark SetText(string text, bool formatted=false)
        {
            originalText = text;
            blocks.Clear();
            if (formatted)
                parseText();
            else
                blocks.Add(new Block { text = text, style = FontStyles.Normal, placement = new List<BlockPlacement>() });
            size = measureString();
            return this;
        }

        public LabelMark FontSize(float fontSize)
        {
            _fontSize = fontSize;
            size = measureString();
            return this;
        }

        private void parseText()
        {
            blocks.Clear();

            var state = new List<char>();
            var text = new List<char>();

            var useNext = false;
            for (var i = 0; i < originalText.Count(); i++)
            {
                var c = originalText[i];

                switch (c)
                {
                    case '\\':
                        useNext = true;
                        break;
                    case '_':
                    case '*':
                    case '-':
                    case '+':
                    case '^':
                    case '~':
                        if (useNext)
                        {
                            if (text.Any())
                            {
                                addBlock(new string(text.ToArray()), state);
                            }

                            if (state.Any() && state.Last() == c)
                            {
                                state.RemoveAt(state.Count() - 1);
                            }
                            else
                            {
                                state.Add(c);
                            }
                            text.Clear();
                        }
                        else
                        {
                            text.Add(c);
                        }
                        useNext = false;
                        break;
                    default:
                        if (useNext)
                        {
                            text.Add('\\');
                        }
                        text.Add(c);
                        useNext = false;
                        break;
                }
            }
            state.Clear();
            if (text.Any())
                addBlock(new string(text.ToArray()), state);
        }

        private void addBlock(string text, IEnumerable<char> state)
        {
            var style = FontStyles.Normal;
            var placement = new List<BlockPlacement>();

            foreach(var c in state)
            {
                switch (c)
                {
                    case '_':
                        style |= FontStyle.Italic;
                        break;
                    case '*':
                        style |= FontStyle.Bold;
                        break;
                    case '-':
                        style |= FontStyle.Strikeout;
                        break;
                    case '+':
                        style |= FontStyle.Underline;
                        break;
                    case '^':
                        placement.Add(BlockPlacement.SUPERSCRIPT);
                        break;
                    case '~':
                        placement.Add(BlockPlacement.SUBSCRIPT);
                        break;
                    default:
                        break;
                }
            }

            blocks.Add(new Block { text = text, style = style, placement = placement });
        }

        private Size measureString()
        {
            var result = new Size(0, 0);
            foreach (var b in blocks)
            {
                var fontsize = _fontSize;
                foreach (var bp in b.placement)
                {
                    if (bp == BlockPlacement.SUBSCRIPT)
                    {
                        fontsize *= 0.62f;
                    }
                    else
                    {
                        fontsize *= 0.6f;
                    }
                }
                var tuple = Tuple.Create(b.text, _fontFamily, fontsize, b.style);
                if (!CachedSizes.ContainsKey(tuple))
                {
                    Font font = new Font(_fontFamily, fontsize, b.style);
                    SizeF csize = dummyG.MeasureString(b.text, font, new Point(0, 0), StringFormat.GenericTypographic);
                    CachedSizes.Add(tuple, csize);
                }
                this.size = CachedSizes[tuple];
                result.Width += this.size.Width;
                result.Height = Math.Max(result.Height, size.Height);
            }
            return result;
        }

        public LabelMark SetHorizontal(bool horizontal)
        {
            this.horizontal = horizontal;
            return this;
        }

        private double offsetX(Size size)
        {
            if (horizontal)
            {
                if (_textAlignment == TextAlignment.Left) return 0;
                if (_textAlignment == TextAlignment.Center) return -size.Width / 2;
                return -size.Width;
            }
            if (_textAlignment == TextAlignment.Left) return 0;
            if (_textAlignment == TextAlignment.Center) return -size.Height / 2;
            return -size.Height;
        }

        private double offsetY(Size size)
        {
            if (horizontal)
            {
                if (_textAlignment == TextAlignment.Left) return -size.Height;
                if (_textAlignment == TextAlignment.Center) return -size.Height / 2;
                return 0;
            }
            if (_textAlignment == TextAlignment.Left) return 0;
            if (_textAlignment == TextAlignment.Center) return -size.Width / 2;
            return -size.Width;
        }

        public override void render(DrawingContext dc, Transform t)
        {
            if (!Visible)
                return;

            if(originalText == "")
                return;

            var s = t.transform(new Point((float)_x, (float)_y));

            System.Drawing.Drawing2D.SmoothingMode oldMode = g.SmoothingMode;
            //g.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.Default;

            if (!horizontal)
            {
                g.TranslateTransform(s.X, s.Y);
                g.RotateTransform(-90);
                g.FillRectangle(Fill, bounds(t));
                drawBlocks(g, offsetY(size), offsetX(size));
                g.ResetTransform();
            }
            else
            {
                g.FillRectangle(Fill, bounds(t));
                s.X += offsetX(size);
                s.Y += offsetY(size);                
                drawBlocks(g, s.X, s.Y);
            }

            g.SmoothingMode = oldMode;
        }

        public void drawBlocks(DrawingContext dc, float x, float y)
        {            
            Font basicFont = new Font(fontFamily, _fontSize);
            float lineHeight = basicFont.GetHeight(g);

            Brush strokeBrush = new SolidBrush(Stroke.Color);

            foreach (Block b in blocks)
            {
                float fontscale = 1;
                float baseline = 0;
                foreach (BlockPlacement bp in b.placement)
                {
                    if (bp == BlockPlacement.SUPERSCRIPT)
                    {
                        baseline = baseline + 0.44f * fontscale;
                        fontscale *= 0.62f;                        
                    }
                    else
                    {
                        baseline = baseline - 0.16f * fontscale;
                        fontscale *= 0.6f;
                    }
                }

                Font font = new Font(fontFamily, _fontSize*fontscale, b.style);
                float fontHeight = font.GetHeight(g);
                
                g.DrawString(b.text, font, strokeBrush, x, y+(1-baseline)*lineHeight-fontHeight, StringFormat.GenericTypographic);
                SizeF size = dummyG.MeasureString(b.text, font, new Point(0, 0), StringFormat.GenericTypographic);
                x += size.Width+0.75f;
            }
        }

        public override bool hitTest(Point p, Transform t)
        {
            if (originalText == "")
                return false;

            GraphicsPath gp = new GraphicsPath();
            gp.AddRectangle(bounds(t));
            return gp.IsVisible(p);
        }

        public override bool hitTest(Rect hit, Transform t)
        {
            if (originalText == "")
                return false;

            GraphicsPath gp = new GraphicsPath();
            gp.AddRectangle(bounds(t));
            return new Region(gp).IsVisible(hit);
        }

        public override Rect bounds(Transform t)
        {
            var s = t.transform(new Point((float)_x, (float)_y));
            s.X += offsetX(size);
            s.Y += offsetY(size);
            if(horizontal)
                return new Rect(s.X, s.Y, size.Width, size.Height);
            else
                return new Rect(s.X, s.Y, size.Height, size.Width);
        }

        public Size Size()
        {
            return size;
        }
    }

    public static class RendererUtilities
    {
        public static Rect Union(this Rect r1, Rect r2)
        {
            if (r1.IsEmpty) return new Rect(r2.X, r2.Y, r2.Width, r2.Height);
            if (r2.IsEmpty) return new Rect(r1.X, r1.Y, r1.Width, r1.Height);
            var x = Math.Min(r1.X, r2.X);
            var y = Math.Min(r1.Y, r2.Y);
            return new Rect(x, y, Math.Max(r1.X + r1.Width, r2.X + r2.Width) - x, Math.Max(r1.Y + r1.Height, r2.Y + r2.Height) - y);
        }

        public static Rect MakeRect(double x1, double y1, double x2, double y2)
        {
            var xm = Math.Min(x1, x2);
            var xM = Math.Max(x1, x2);

            var ym = Math.Min(y1, y2);
            var yM = Math.Max(y1, y2);

            return new Rect(xm, ym, xM - xm, yM - ym);
        }
    }

    public static class MarkUtilities
    {
        public static T SetName<T>(this T t, String name) where T : Mark
        {
            t.Name = name;
            return t;
        }

        public static T SetSelectionManager<T>(this T t, SelectionManager manager) where T : Mark
        {
            t.SelectionManager = manager;
            return t;
        }

        public static T SetStroke<T>(this T t, Func<Pen> stroke) where T : Mark
        {
            t.fStroke = stroke;
            return t;
        }

        public static T SetStroke<T>(this T t, Pen stroke) where T : Mark
        {
            t.Stroke = stroke;
            return t;
        }

        public static T SetFill<T>(this T t, Brush fill) where T : Mark
        {
            t.Fill = fill;
            return t;
        }

        public static T SetStroke<T>(this T t, Brush stroke, double width) where T : Mark
        {
            t.Stroke.Brush = stroke;
            t.Stroke.Thickness = (float)width;
            return t;
        }

        public static T SetStrokeColor<T>(this T t, Brush stroke) where T : Mark
        {
            t.Stroke.Brush = stroke;
            return t;
        }

        public static T SetStrokeWidth<T>(this T t, double width) where T : Mark
        {
            t.Stroke.Thickness = (float)width;
            return t;
        }

        public static T SetFillColor<T>(this T t, Color fill) where T : Mark
        {
            t.Fill = new SolidColorBrush(fill);
            return t;
        }

        public static T SetData<T>(this T t, object data) where T : Mark
        {
            t.Data = data;
            return t;
        }

        public static T SetOrder<T>(this T t, double order) where T : Mark
        {
            t.Order = order;
            return t;
        }

        public static T SetVisible<T>(this T t, bool v) where T : Mark
        {
            t.Visible = v;
            return t;
        }

        public static T SetMouseOver<T>(this T t, Func<Point, bool> f) where T : Mark
        {
            t.MouseOver = f;
            return t;
        }

        public static T SetMouseEnter<T>(this T t, Func<Point, bool> f) where T : Mark
        {
            t.MouseEnter = f;
            return t;
        }

        public static T SetMouseExit<T>(this T t, Func<Point, bool> f) where T : Mark
        {
            t.MouseExit = f;
            return t;
        }
    }
#endif
}
