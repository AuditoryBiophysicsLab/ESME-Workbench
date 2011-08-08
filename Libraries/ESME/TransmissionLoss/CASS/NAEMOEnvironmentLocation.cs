using System;
using System.Collections.Generic;
using ESME.Environment;
using HRC.Navigation;

namespace ESME.TransmissionLoss.CASS
{
    public class NAEMOEnvironmentLocation : EarthCoordinate<NAEMOEnvironmentLocation>, IEquatable<NAEMOEnvironmentLocation>
    {
        public NAEMOEnvironmentLocation() {  }
        public NAEMOEnvironmentLocation(Geo location) : base(location) {  }
        public List<double> Depths;
        public List<double> Soundspeeds;
        public string BottomType;
        public double WindSpeed;
        public string Filename;

        public bool Equals(NAEMOEnvironmentLocation that)
        {
            if (!base.Equals(that)) return false;
            if (!BottomType.Equals(that.BottomType)) return false;
            if (!WindSpeed.Equals(that.WindSpeed)) return false;
            if (!Depths.Count.Equals(that.Depths.Count)) return false;
            if (!Soundspeeds.Count.Equals(that.Soundspeeds.Count)) return false;
            for (var i = 0; i < Depths.Count; i++)
                if (!Depths[i].Equals(that.Depths[i])) return false;
            for (var i = 0; i < Soundspeeds.Count; i++)
                if (!Soundspeeds[i].Equals(that.Soundspeeds[i])) return false;
            return true;
        }
    }
}