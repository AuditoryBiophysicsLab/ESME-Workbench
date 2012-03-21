using System;
using System.ComponentModel.DataAnnotations;
using ESME.Database;
using HRC.Aspects;

namespace ESME.Scenarios
{
    public class PerimeterCoordinate
    {
        [Key, Initialize]
        public Guid PerimeterCoordinateID { get; set; }
        public int Order { get; set; }
        public DbGeo Geo { get; set; }

        public virtual Perimeter Perimeter { get; set; }
    }
}