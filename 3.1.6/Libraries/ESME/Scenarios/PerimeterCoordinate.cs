using System;
using System.ComponentModel.DataAnnotations;
using ESME.Database;
using HRC.Aspects;
using HRC.Navigation;

namespace ESME.Scenarios
{
    public class PerimeterCoordinate
    {
        public PerimeterCoordinate() {}

        public PerimeterCoordinate(PerimeterCoordinate coordinate)
        {
            Order = coordinate.Order;
            Geo = new Geo(coordinate.Geo);
        }
        [Key, Initialize]
        public Guid Guid { get; set; }
        public int Order { get; set; }
        public DbGeo Geo { get; set; }

        public virtual Perimeter Perimeter { get; set; }
    }
}