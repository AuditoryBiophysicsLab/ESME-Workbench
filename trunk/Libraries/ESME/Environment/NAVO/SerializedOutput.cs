using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment.NAVO
{
    public class SerializedOutput : PropertyChangedBase
    {
        public static readonly List<Type> ReferencedTypes = new List<Type>
        {
                typeof (EnvironmentalDataPoint),
                typeof (EarthCoordinate),
                typeof (EarthCoordinate<List<double>>),
                typeof (List<double>),
        };

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
                             select value.Latitude;
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
                             select value.Longitude;
                var list = result.Distinct().ToList();
                list.Sort();
                return list;
            }
        }

        //other header things here, before DataPoints.
        public List<EnvironmentalDataPoint> DataPoints { get; set; }

        public List<double> DepthAxis { get; set; }

        public void Save(string fileName)
        {
            var serializer = new XmlSerializer<SerializedOutput> { Data = this };
            serializer.Save(fileName, ReferencedTypes);
        }

        public void Save(string fileName, List<Type> referencedTypes)
        {
            if (referencedTypes == null) referencedTypes = ReferencedTypes;
            else referencedTypes.AddRange(ReferencedTypes);
            var serializer = new XmlSerializer<SerializedOutput> { Data = this };
            serializer.Save(fileName, referencedTypes);
        }

        public static SerializedOutput Load(string fileName)
        {
            return XmlSerializer<SerializedOutput>.Load(fileName, ReferencedTypes);
        }

        public static SerializedOutput Load(string fileName, List<Type> referencedTypes)
        {
            if (referencedTypes == null) referencedTypes = ReferencedTypes;
            else referencedTypes.AddRange(ReferencedTypes);
            return XmlSerializer<SerializedOutput>.Load(fileName, referencedTypes);
        }
    }

    public class EnvironmentalDataPoint : EarthCoordinate<List<double>>
    {
        public EnvironmentalDataPoint() { Data = new List<double>(); }
    }
}