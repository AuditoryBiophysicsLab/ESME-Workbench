using System.Windows.Media;
using System.Text;
using HRC.Navigation;

namespace ESME.NEMO.Overlay
{
    public class OverlayPoint : OverlayShape
    {
        public OverlayPoint(EarthCoordinate point) : this(point, Colors.Black, 1) { }

        public OverlayPoint(EarthCoordinate point, Color color, float width)
            : base(color, width)
        {
            Add(point);
        }

        public override string WellKnownText
        {
            get
            {
                if (MyWellKnownText == null)
                {
                    var retval = new StringBuilder();
                    retval.Append("POINT(");
                    foreach (var coord in _earthCoordinates)
                        if (coord != null) retval.Append(string.Format("{0} {1}, ", coord.Longitude, coord.Latitude));
                    retval.Remove(retval.Length - 2, 2); // Lose the last comma and space
                    retval.Append(")");
                    MyWellKnownText = retval.ToString();
                }
                return MyWellKnownText;
            }
        }
    }
}
