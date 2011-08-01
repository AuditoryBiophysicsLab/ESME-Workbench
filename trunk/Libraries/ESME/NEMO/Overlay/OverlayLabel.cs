using System;
using System.Windows.Media;
using HRC.Navigation;

namespace ESME.NEMO.Overlay
{
    public class OverlayLabel : OverlayShape
    {
        public OverlayLabel(EarthCoordinate point, Color color, string label)
            : base(color)
        {
            Add(point);
            Label = label;
        }

        public string Label { get; set; }

        public override string WellKnownText
        {
            get { throw new NotImplementedException(); }
        }
    }
}