using System.Collections.Generic;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment.NAVO
{
    public class SerializedOutput : SerializableData<SerializedOutput>
    {
        //other header things here, before DataPoints.
        public List<EnvironmentalDataPoint> DataPoints { get; set; }
        public List<float> DepthAxis { get; set; }
    }

     public class EnvironmentalDataPoint : SerializableData<EnvironmentalDataPoint>
    {
        public EarthCoordinate EarthCoordinate { set; get; }
        public List<float> Data { get; set; }
    }
}