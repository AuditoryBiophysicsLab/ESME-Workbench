using System.Collections.Generic;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment.NAVO
{
    public class SerializedOutput : SerializableData
    {
        //other header things here, before DataPoints.
        public List<EnvironmentalDataPoint> DataPoints { get; set; }
    }

     public class EnvironmentalDataPoint : SerializableData
    {
        public EarthCoordinate EarthCoordinate { set; get; }
        public List<DepthValuePair> Data { get; set; }
    }
    public class DepthValuePair : SerializableData
    {
        public float? Depth { get; set; }
        public float? Value { get; set; }
    }
}