using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Windows.Media;

namespace ESME.Database
{
    [ComplexType]
    public class DbColor
    {
        public DbColor() { }
        public DbColor(Color color)
        {
            Alpha = color.A;
            Red = color.R;
            Green = color.G;
            Blue = color.B;
        }

        public static implicit operator DbColor(Color color) { return new DbColor(color); }
        public static implicit operator Color(DbColor dbColor) { return Color.FromArgb(dbColor.Alpha, dbColor.Red, dbColor.Green, dbColor.Blue); }
        [NotMapped]
        public Color Color
        {
            get { return Color.FromArgb(Alpha, Red, Green, Blue); }
            set { Alpha = value.A; Red = value.R; Green = value.G; Blue = value.B; }
        }

        public byte Alpha { get; set; }
        public byte Red { get; set; }
        public byte Green { get; set; }
        public byte Blue { get; set; }
    }
}