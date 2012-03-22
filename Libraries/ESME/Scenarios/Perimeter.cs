using System;
using System.Collections.Generic;
using ESME.Locations;
using HRC.Aspects;

namespace ESME.Scenarios
{
    public class Perimeter : IHaveGuid
    {
        [Initialize]
        public Guid Guid { get; set; }
        public string Name { get; set; }
        public virtual Scenario Scenario { get; set; }
        public virtual ICollection<PerimeterCoordinate> PerimeterCoordinates { get; set; }
        public virtual ICollection<LogEntry> Logs { get; set; }
    }
}