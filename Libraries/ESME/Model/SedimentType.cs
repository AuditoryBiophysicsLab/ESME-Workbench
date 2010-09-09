using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using System.IO;
using System.Xml;
using System.Xml.Linq;
using System.Xml.Schema;
using System.Xml.Serialization;

namespace ESME.Model
{
    public class SedimentType
    {
        public static SedimentType Default;

        public SedimentType(string Name, float Density_gramsCC, float CompressionWaveSpeed_metersSec,
            float CompressionWaveCoefficient, float ShearWaveSpeed_metersSec,
            float ShearWaveCoefficient)
        {
            this.Name = Name;
            this.Density_gramsCC = Density_gramsCC;
            this.CompressionWaveSpeed_metersSec = CompressionWaveSpeed_metersSec;
            this.CompressionWaveCoefficient = CompressionWaveCoefficient;
            this.ShearWaveSpeed_metersSec = ShearWaveSpeed_metersSec;
            this.ShearWaveCoefficient = ShearWaveCoefficient;
        }

        public SedimentType() { }

        public string Name { get; set; }
        public float Density_gramsCC { get; set; }
        public float CompressionWaveSpeed_metersSec { get; set; }
        public float CompressionWaveCoefficient { get; set; }
        public float ShearWaveSpeed_metersSec { get; set; }
        public float ShearWaveCoefficient { get; set; }
    }

    public static class SedimentTypes
    {
        private static List<SedimentType> myList = new List<SedimentType>();

        static SedimentTypes()
        {
            //List<SedimentType> myList = new List<SedimentType>();
            myList.Add(new SedimentType("Rough Rock", 2.500f, 3750.0f, 1.595f, 699.6f, 46.20f));
            myList.Add(new SedimentType("Rock", 2.500f, 3750.0f, 1.595f, 699.6f, 46.20f));
            myList.Add(new SedimentType("Cobble or Gravel or Pebble", 2.500f, 2700.0f, 1.595f, 699.6f, 46.20f));
            myList.Add(new SedimentType("Sandy Gravel", 2.492f, 2005.5f, 1.595f, 699.6f, 46.20f));
            myList.Add(new SedimentType("Very Coarse Sand", 2.401f, 1960.1f, 1.595f, 518.0f, 46.20f));
            myList.Add(new SedimentType("Muddy Sandy Gravel", 2.314f, 1916.7f, 1.595f, 372.6f, 46.20f));
            myList.Add(new SedimentType("Coarse Sand or Gravelly Sand", 2.231f, 1875.5f, 1.637f, 259.5f, 46.20f));
            myList.Add(new SedimentType("Gravelly Muddy Sand", 2.151f, 1836.2f, 1.680f, 174.2f, 46.20f));
            myList.Add(new SedimentType("Medium Sand or Sand", 1.845f, 1767.3f, 1.723f, 171.2f, 46.20f));
            myList.Add(new SedimentType("Muddy Gravel", 1.615f, 1709.4f, 1.766f, 168.2f, 46.20f));
            myList.Add(new SedimentType("Fine Sand or Silty Sand", 1.451f, 1661.0f, 1.809f, 65.6f, 46.20f));
            myList.Add(new SedimentType("Muddy Sand", 1.339f, 1620.0f, 2.000f, 58.9f, 46.20f));
            myList.Add(new SedimentType("Very Fine Sand", 1.268f, 1585.2f, 2.217f, 52.3f, 46.20f));
            myList.Add(new SedimentType("Clayey Sand", 1.224f, 1554.6f, 2.435f, 65.2f, 46.20f));
            myList.Add(new SedimentType("Coarse Silt", 1.195f, 1526.9f, 2.651f, 78.1f, 16.80f));
            myList.Add(new SedimentType("Gravelly Mud or Sandy Silt", 1.169f, 1499.9f, 1.578f, 659.4f, 16.80f));
            myList.Add(new SedimentType("Medium Silt or Sand-Silt-Clay", 1.149f, 1482.8f, 0.856f, 351.3f, 46.90f));
            myList.Add(new SedimentType("Sandy Mud or Silt", 1.149f, 1481.0f, 0.489f, 310.3f, 46.20f));
            myList.Add(new SedimentType("Fine Silt or Clayey Silt", 1.148f, 1479.2f, 0.388f, 284.8f, 46.20f));
            myList.Add(new SedimentType("Sandy Clay", 1.147f, 1477.4f, 0.307f, 259.3f, 60.55f));
            myList.Add(new SedimentType("Very Fine Silt", 1.147f, 1474.8f, 0.247f, 203.0f, 60.55f));
            myList.Add(new SedimentType("Silty Clay", 1.146f, 1473.6f, 0.207f, 146.7f, 60.55f));
            myList.Add(new SedimentType("Clay", 1.145f, 1470.0f, 0.189f, 146.7f, 60.55f));

            SedimentArray = myList.ToArray();
            SedimentType.Default = SedimentTypes.SedimentArray[0];

            var query = from s in SedimentArray
                        select s.Name;
            Names = query.ToArray();
        }

        public static string[] Names {get; private set;}

        public static SedimentType[] SedimentArray { get; private set; }

        public static SedimentType Find(string Name)
        {
            var q = from s in SedimentArray
                    where s.Name == Name
                    select s;
            return q.FirstOrDefault();
        }
    }
}
