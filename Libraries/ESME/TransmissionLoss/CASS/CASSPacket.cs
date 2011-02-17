using System;
using System.Collections.Generic;
using System.Text;
using HRC.Navigation;

namespace ESME.TransmissionLoss.CASS
{
    public class CASSPacket:IEquatable<CASSPacket>
    {
        public EarthCoordinate Location;
        public List<double> Depths;
        public List<double> Soundspeeds;
        public string BottomType;
        public double WindSpeed;
        public string Filename;

        public bool Equals(CASSPacket that)
        {
            if (!Location.Equals(that.Location)) return false;
            if (!BottomType.Equals(that.BottomType)) return false;
            if (!WindSpeed.Equals(that.WindSpeed)) return false;
            if (!Depths.Count.Equals(that.Depths.Count)) return false;
            if (!Soundspeeds.Count.Equals(that.Soundspeeds.Count)) return false;
            for (var i = 0; i < Depths.Count; i++)
            {
                if (!Depths[i].Equals(that.Depths[i])) return false;
            }
            for (var i = 0; i < Soundspeeds.Count; i++)
            {
                if (!Soundspeeds[i].Equals(that.Soundspeeds[i])) return false;
            }
            return true;
        }

        public bool WithinDeltaOf(CASSPacket that) {
            return Location.GetDistanceTo_Meters(that.Location) < 10000;
        }

        public string CompareTo(CASSPacket that)
        {
            var sb = new StringBuilder();
            if (!Location.Equals(that.Location))
            {
                sb.Append(string.Format("locations "));
            }
            return sb.ToString();
    
        }

        
    }
    
}