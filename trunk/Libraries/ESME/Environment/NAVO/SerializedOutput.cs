using System.Collections.Generic;
using System.Linq;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment.NAVO
{
    public class SerializedOutput : SerializableData<SerializedOutput>
    {
        public SerializedOutput()
        {
            DataPoints = new List<EnvironmentalDataPoint>();
            DepthAxis = new List<double>();
        }

        /// <summary>
        ///   List of latitudes (in degrees) for which we have values
        /// </summary>
        public IEnumerable<double> Latitudes
        {
            get
            {
                var result = from value in DataPoints
                             select value.Latitude_degrees;
                var list = result.Distinct().ToList();
                list.Sort();
                return list;
            }
        }

        /// <summary>
        ///   List of longitudes (in degrees) for which we have values
        /// </summary>
        public IEnumerable<double> Longitudes
        {
            get
            {
                var result = from value in DataPoints
                             select value.Longitude_degrees;
                var list = result.Distinct().ToList();
                list.Sort();
                return list;
            }
        }

        //other header things here, before DataPoints.
        public List<EnvironmentalDataPoint> DataPoints { get; set; }
        public List<double> DepthAxis { get; set; }
    }

    public class EnvironmentalDataPoint : EarthCoordinate
    {
        public EnvironmentalDataPoint() { Data = new List<double>(); }

        public List<double> Data { get; set; }
    }
}