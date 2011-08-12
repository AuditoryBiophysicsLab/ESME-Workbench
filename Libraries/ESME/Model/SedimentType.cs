using System.Collections.Generic;
using System.Linq;

namespace ESME.Model
{
    public class SedimentType
    {
        public static SedimentType Default;

        public SedimentType(string name, int hfevaCategory, float density, float compressionWaveSpeed, float compressionWaveCoefficient, float shearWaveSpeed, float shearWaveCoefficient, float soundSpeedRatio, float bulkGrainSize, float lossParameter)
        {
            Name = name;
            HFEVACategory = hfevaCategory;
            Density = density;
            CompressionWaveSpeed = compressionWaveSpeed;
            CompressionWaveCoefficient = compressionWaveCoefficient;
            ShearWaveSpeed = shearWaveSpeed;
            ShearWaveCoefficient = shearWaveCoefficient;
            SoundSpeedRatio = soundSpeedRatio;
            BulkGrainSize = bulkGrainSize;
            LossParameter = lossParameter;
            BottomInterfaceSoundSpeed = 1500;
        }

        public SedimentType() {}

        public string Name { get; private set; }
        public int HFEVACategory { get; private set; }
        public float Density { get; private set; }
        public float CompressionWaveSpeed { get; private set; }
        public float CompressionWaveCoefficient { get; private set; }
        public float ShearWaveSpeed { get; private set; }
        public float ShearWaveCoefficient { get; private set; }
        public float SoundSpeedRatio { get; private set; }
        public float SoundSpeed { get; private set; }
        public float BulkGrainSize { get; private set; }
        public float LossParameter { get; private set; }
        float _bottomInterfaceSoundSpeed;
        public float BottomInterfaceSoundSpeed
        {
            get { return _bottomInterfaceSoundSpeed; }
            set
            {
                _bottomInterfaceSoundSpeed = value;
                SoundSpeed = _bottomInterfaceSoundSpeed * SoundSpeedRatio;
            }
        }

    }

    public static class SedimentTypes
    {
        static readonly List<SedimentType> SedimentTypeList = new List<SedimentType>();

        static SedimentTypes()
        {
            SedimentTypeList.Add(new SedimentType("Rough Rock",                       1, 2.500f, 3750.0f, 1.595f, 699.6f, 46.20f, 2.5000f, float.NaN,    0.01374f));
            SedimentTypeList.Add(new SedimentType("Rock",                             2, 2.500f, 3750.0f, 1.595f, 699.6f, 46.20f, 2.5000f, float.NaN,    0.01374f));
            SedimentTypeList.Add(new SedimentType("Cobble or Gravel or Pebble",       3, 2.500f, 2700.0f, 1.595f, 699.6f, 46.20f, 1.8000f, float.NaN,    0.01374f));
            SedimentTypeList.Add(new SedimentType("Sandy Gravel",                     4, 2.492f, 2005.5f, 1.595f, 699.6f, 46.20f, 1.3370f, -1.0f,        0.01705f));
            SedimentTypeList.Add(new SedimentType("Very Coarse Sand",                 5, 2.401f, 1960.1f, 1.595f, 518.0f, 46.20f, 1.3067f, -0.5f,        0.01667f));
            SedimentTypeList.Add(new SedimentType("Muddy Sandy Gravel",               6, 2.314f, 1916.7f, 1.595f, 372.6f, 46.20f, 1.2778f,  0.0f,        0.01630f));
            SedimentTypeList.Add(new SedimentType("Coarse Sand or Gravelly Sand",     7, 2.231f, 1875.5f, 1.637f, 259.5f, 46.20f, 1.2503f,  0.5f,        0.01638f));
            SedimentTypeList.Add(new SedimentType("Gravelly Muddy Sand",              8, 2.151f, 1836.2f, 1.680f, 174.2f, 46.20f, 1.2241f,  1.0f,        0.01645f));
            SedimentTypeList.Add(new SedimentType("Medium Sand or Sand",              9, 1.845f, 1767.3f, 1.723f, 171.2f, 46.20f, 1.1782f,  1.5f,        0.01624f));
            SedimentTypeList.Add(new SedimentType("Muddy Gravel",                    10, 1.615f, 1709.4f, 1.766f, 168.2f, 46.20f, 1.1396f,  2.0f,        0.01610f));
            SedimentTypeList.Add(new SedimentType("Fine Sand or Silty Sand",         11, 1.451f, 1661.0f, 1.809f,  65.6f, 46.20f, 1.1073f,  2.5f,        0.01602f));
            SedimentTypeList.Add(new SedimentType("Muddy Sand",                      12, 1.339f, 1620.0f, 2.000f,  58.9f, 46.20f, 1.0800f,  3.0f,        0.01728f));
            SedimentTypeList.Add(new SedimentType("Very Fine Sand",                  13, 1.268f, 1585.2f, 2.217f,  52.3f, 46.20f, 1.0568f,  3.5f,        0.01875f));
            SedimentTypeList.Add(new SedimentType("Clayey Sand",                     14, 1.224f, 1554.6f, 2.435f,  65.2f, 46.20f, 1.0364f,  4.0f,        0.02019f));
            SedimentTypeList.Add(new SedimentType("Coarse Silt",                     15, 1.195f, 1526.9f, 2.651f,  78.1f, 16.80f, 1.0179f,  4.5f,        0.02158f));
            SedimentTypeList.Add(new SedimentType("Gravelly Mud or Sandy Silt",      16, 1.169f, 1499.9f, 1.578f, 659.4f, 16.80f, 0.9999f,  5.0f,        0.01261f));
            SedimentTypeList.Add(new SedimentType("Medium Silt or Sand-Silt-Clay",   17, 1.149f, 1482.8f, 0.856f, 351.3f, 46.90f, 0.9885f,  5.5f,        0.00676f));
            SedimentTypeList.Add(new SedimentType("Sandy Mud or Silt",               18, 1.149f, 1481.0f, 0.489f, 310.3f, 46.20f, 0.9873f,  6.0f,        0.00386f));
            SedimentTypeList.Add(new SedimentType("Fine Silt or Clayey Silt",        19, 1.148f, 1479.2f, 0.388f, 284.8f, 46.20f, 0.9861f,  6.5f,        0.00306f));
            SedimentTypeList.Add(new SedimentType("Sandy Clay",                      20, 1.147f, 1477.4f, 0.307f, 259.3f, 60.55f, 0.9849f,  7.0f,        0.00242f));
            SedimentTypeList.Add(new SedimentType("Very Fine Silt",                  21, 1.147f, 1474.8f, 0.247f, 203.0f, 60.55f, 0.9832f,  7.5f,        0.00194f));
            SedimentTypeList.Add(new SedimentType("Silty Clay",                      22, 1.146f, 1473.6f, 0.207f, 146.7f, 60.55f, 0.9824f,  8.0f,        0.00163f));
            SedimentTypeList.Add(new SedimentType("Clay",                            23, 1.145f, 1470.0f, 0.189f, 146.7f, 60.55f, 0.9800f,  9.0f,        0.00148f));
            SedimentTypeList.Add(new SedimentType("No Data",                        888, 0.000f, 0000.0f, 0.000f, 000.0f, 00.00f, 0.0000f,  0.0f,        0.00000f));
            SedimentTypeList.Add(new SedimentType("Land",                           999, 0.000f, 0000.0f, 0.000f, 000.0f, 00.00f, 0.0000f,  0.0f,        0.00000f));

            SedimentArray = SedimentTypeList.ToArray();
            SedimentType.Default = SedimentArray[0];

            var query = from s in SedimentArray select s.Name;
            Names = query.ToArray();
        }

        public static string[] Names { get; private set; }

        public static SedimentType[] SedimentArray { get; private set; }

        public static SedimentType Find(string name)
        {
            var q = from s in SedimentArray where s.Name.ToLower() == name.ToLower() select s;
            return q.FirstOrDefault();
        }

        public static SedimentType Find(int hfevaCategory)
        {
            var q = from s in SedimentArray where s.HFEVACategory == hfevaCategory select s;
            return q.First();
        }
    }
}