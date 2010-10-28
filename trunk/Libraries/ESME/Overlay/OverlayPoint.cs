﻿using System.Windows.Media;
using System.Text;
using HRC.Navigation;
namespace ESME.Overlay
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
                    foreach (var coord in EarthCoordinates)
                        retval.Append(string.Format("{0} {1}, ", coord.Longitude_degrees, coord.Latitude_degrees));
                    retval.Remove(retval.Length - 2, 2); // Lose the last comma and space
                    retval.Append(")");
                    MyWellKnownText = retval.ToString();
                }
                return MyWellKnownText;
            }
        }
    }
}
