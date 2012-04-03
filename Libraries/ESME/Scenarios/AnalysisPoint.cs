using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using ESME.Database;
using ESME.Locations;
using HRC.Aspects;

namespace ESME.Scenarios
{
    public class AnalysisPoint : IHaveGuid
    {
        [Key, Initialize]
        public Guid Guid { get; set; }
        public DbGeo Geo { get; set; }

        public virtual Scenario Scenario { get; set; }

        public virtual ICollection<TransmissionLoss> TransmissionLosses { get; set; }
    }

    public class TransmissionLoss : IHaveGuid
    {
        [Key, Initialize]
        public Guid Guid { get; set; }
        public bool IsReadyToCalculate { get; set; }
        public virtual AnalysisPoint AnalysisPoint { get; set; }
        public virtual Mode Mode { get; set; }

        public virtual ICollection<Radial> Radials { get; set; }
    }

    public class Radial : IHaveGuid
    {
        [Key, Initialize]
        public Guid Guid { get; set; }
        public bool IsCalculated { get; set; }
        public DbDateTime CalculationStarted { get; set; }
        public DbDateTime CalculationCompleted { get; set; }
        public double Bearing { get; set; }
        public double Length { get; set; }

        public virtual TransmissionLoss TransmissionLoss { get; set; }
        public virtual ICollection<LevelRadius> LevelRadii { get; set; }
    }

    public class LevelRadius : IHaveGuid
    {
        [Key, Initialize]
        public Guid Guid { get; set; }

        public double Level { get; set; }
        public double Radius { get; set; }

        public virtual Radial Radial { get; set; }
    }
}
