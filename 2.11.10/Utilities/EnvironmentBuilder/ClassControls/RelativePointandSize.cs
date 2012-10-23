using System;
using System.Collections.Generic;
using System.Text;
using System.Drawing;
using System.Drawing.Drawing2D;

namespace EnvironmentBuilder.ClassControls
{
    public class RelativePoint
    {
        #region Relative Point Constructor
        public RelativePoint(float X, float Y)
        {
            this.X = X;
            this.Y = Y;
        }
        #endregion

#if false
        // can't have both implicit and explicit conversions to the same external type
        // but this is how you'd write something that used an explicit conversion (cast)
        // RelativePoint rp = new RelativePoint(.5, .5);
        // PointF pf = (PointF)rp;
        public static explicit operator PointF(RelativePoint p)
        {
            return new PointF(p.X, p.Y);
        }
#endif
        #region Relative Point Static Implicit Operator Methods
        public static implicit operator PointF(RelativePoint p)
        {
            return new PointF(p.X, p.Y);
        }

        public static implicit operator RelativePoint(PointF p)
        {
            return new RelativePoint(p.X, p.Y);
        }
        #endregion

        #region Relative Point Properties
        public float X { get; set; }
        public float Y { get; set; }
        #endregion

        #region Relative Point Static Operator Override Methods
        public static RelativePoint operator *(RelativePoint lhs, RelativeSize rhs)
        {
            return new RelativePoint(lhs.X * rhs.Width, lhs.Y * rhs.Height);
        }

        public static PointF operator *(RelativePoint lhs, Size rhs)
        {
            return new PointF(lhs.X * rhs.Width, lhs.Y * rhs.Height);
        }

        public static PointF operator *(Size lhs, RelativePoint rhs)
        {
            return new PointF(rhs.X * lhs.Width, rhs.Y * lhs.Height);
        }

        public static PointF operator /(RelativePoint lhs, Size rhs)
        {
            return new PointF(lhs.X / rhs.Width, lhs.Y / rhs.Height);
        }

        public static PointF operator /(Size lhs, RelativePoint rhs)
        {
            return new PointF(rhs.X / lhs.Width, rhs.Y / lhs.Height);
        }

        public static RelativePoint operator /(RelativePoint lhs, RelativeSize rhs)
        {
            return new RelativePoint(lhs.X / rhs.Width, lhs.Y / rhs.Height);
        }

        public static RelativePoint operator *(RelativePoint lhs, float rhs)
        {
            return new RelativePoint(lhs.X * rhs, lhs.Y * rhs);
        }

        public static RelativePoint operator /(RelativePoint lhs, float rhs)
        {
            return lhs * (1.0f / rhs);
        }

        public static RelativePoint operator +(RelativePoint lhs, RelativePoint rhs)
        {
            return new RelativePoint(lhs.X + rhs.X, lhs.Y + rhs.Y);
        }

        public static RelativePoint operator -(RelativePoint lhs, RelativePoint rhs)
        {
            return new RelativePoint(lhs.X - rhs.X, lhs.Y - rhs.Y);
        }

        public static RelativePoint operator +(RelativePoint lhs, RelativeSize rhs)
        {
            return new RelativePoint(lhs.X + rhs.Width, lhs.Y + rhs.Height);
        }

        public static RelativePoint operator -(RelativePoint lhs, RelativeSize rhs)
        {
            return new RelativePoint(lhs.X - rhs.Width, lhs.Y - rhs.Height);
        }

        public static RelativePoint Max(RelativePoint arg1, RelativePoint arg2)
        {
            return new RelativePoint(Math.Max(arg1.X, arg2.X), Math.Max(arg1.Y, arg2.Y));
        }

        public static RelativePoint Min(RelativePoint arg1, RelativePoint arg2)
        {
            return new RelativePoint(Math.Min(arg1.X, arg2.X), Math.Min(arg1.Y, arg2.Y));
        }
        #endregion

        public override string ToString()
        {
            return "{X = " + X.ToString("g") + ", Y = " + Y.ToString("g") + "}";
        }
    }

    public class RelativeSize
    {
        #region Relative Size Constructor
        public RelativeSize(float Width, float Height)
        {
            this.Height = Height;
            this.Width = Width;
        }
        #endregion

        #region Relative Size Properties
        public float Width { get; set; }
        public float Height { get; set; }
        #endregion

        #region Relative Size Static Implicit Operator Overrides 
        public static implicit operator SizeF(RelativeSize s)
        {
            return new SizeF(s.Width, s.Height);
        }

        public static implicit operator RelativeSize(SizeF s)
        {
            return new RelativeSize(s.Width, s.Height);
        }
        #endregion

        #region Relative Size Static Operator Overrides
        public static RelativeSize operator *(RelativeSize lhs, float rhs)
        {
            return new RelativeSize(lhs.Width * rhs, lhs.Height * rhs);
        }

        public static RelativeSize operator /(RelativeSize lhs, float rhs)
        {
            return lhs * (1.0f / rhs);
        }

        public static RelativeSize operator *(RelativeSize lhs, RelativeSize rhs)
        {
            return new RelativeSize(lhs.Width * rhs.Width, lhs.Height * rhs.Height);
        }

        public static RelativeSize operator /(RelativeSize lhs, RelativeSize rhs)
        {
            return new RelativeSize(lhs.Width / rhs.Width, lhs.Height / rhs.Width);
        }

        public static RelativeSize operator +(RelativeSize lhs, RelativePoint rhs)
        {
            return new RelativeSize(lhs.Width + rhs.X, lhs.Height + rhs.Y);
        }

        public static RelativeSize operator -(RelativeSize lhs, RelativePoint rhs)
        {
            return new RelativeSize(lhs.Width - rhs.X, lhs.Height - rhs.Y);
        }

        public static RelativeSize operator +(RelativeSize lhs, RelativeSize rhs)
        {
            return new RelativeSize(lhs.Width + rhs.Width, lhs.Height + rhs.Height);
        }

        public static RelativeSize operator -(RelativeSize lhs, RelativeSize rhs)
        {
            return new RelativeSize(lhs.Width - rhs.Width, lhs.Height - rhs.Height);
        }

        public static RelativeSize Max(RelativeSize arg1, RelativeSize arg2)
        {
            return new RelativeSize(Math.Max(arg1.Width, arg2.Width), Math.Max(arg1.Height, arg2.Height));
        }

        public static RelativeSize Min(RelativeSize arg1, RelativeSize arg2)
        {
            return new RelativeSize(Math.Min(arg1.Width, arg2.Width), Math.Min(arg1.Height, arg2.Height));
        }

        public static SizeF operator *(RelativeSize lhs, Size rhs)
        {
            return new SizeF(lhs.Width * rhs.Width, lhs.Height * rhs.Height);
        }

        public static SizeF operator *(Size lhs, RelativeSize rhs)
        {
            return new SizeF(rhs.Width * lhs.Width, rhs.Height * lhs.Height);
        }

        public static SizeF operator /(RelativeSize lhs, Size rhs)
        {
            return new SizeF(lhs.Width / rhs.Width, lhs.Height / rhs.Height);
        }

        public static SizeF operator /(Size lhs, RelativeSize rhs)
        {
            return new SizeF(rhs.Width / lhs.Width, rhs.Height / lhs.Height);
        }

        public static bool operator ==(RelativeSize lhs, RelativeSize rhs)
        {
            return ((rhs.Width == lhs.Width) && (rhs.Height == lhs.Height));
        }

        public static bool operator !=(RelativeSize lhs, RelativeSize rhs)
        {
            return ((rhs.Width != lhs.Width) || (rhs.Height != lhs.Height));
        }
        #endregion

        public override string ToString()
        {
            return "{Width = " + Width.ToString("g") + ", Height = " + Height.ToString("g") + "}";
        }

        public override int GetHashCode()
        {
            return ((int)(Width * int.MaxValue)) | ((int)(Height * int.MaxValue));
        }

        public override bool Equals(Object that)
        {
            if (that.GetType() != this.GetType())
                return false;
            return ((this.Width == ((RelativeSize)that).Width) && (this.Height == ((RelativeSize)that).Height));
        }
    }
}
