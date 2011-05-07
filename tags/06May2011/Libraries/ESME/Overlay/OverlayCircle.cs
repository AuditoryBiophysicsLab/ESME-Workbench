using System;
using System.Windows.Media;
using HRC.Navigation;

namespace ESME.Overlay
{
    public class OverlayCircle : OverlayShape
    {
        public OverlayCircle(EarthCoordinate point, Color color, LineStyle lineStyle, float radiusMeters)
            : base(color, 1, lineStyle)
        {
            Add(point);
            RadiusMeters = radiusMeters;
        }

        public float RadiusMeters { get; set; }

        public override string WellKnownText
        {
            get { throw new NotImplementedException(); }
        }
    }
}