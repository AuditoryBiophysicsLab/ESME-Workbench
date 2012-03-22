using System;
using ESME.Database;
using HRC.Aspects;

namespace ESME.Scenarios
{
    public class PerimeterCoordinate
    {
        [Initialize]
        public Guid Guid { get; set; }
        public int Order { get; set; }
        public DbGeo Geo { get; set; }

        public virtual Perimeter Perimeter { get; set; }
    }
}