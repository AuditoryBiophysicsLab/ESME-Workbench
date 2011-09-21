using System.Collections.Generic;
using ESME.Model;

namespace ESME.Environment.NAVO
{
    public static class BottomSedimentTypeTable
    {
        static readonly List<ValueToNameMap> Map;

        public static readonly List<ValueToNameMap> HFEVAMap = new List<ValueToNameMap>
                                                  {
                                                      new ValueToNameMap(1, "Rough Rock"),
                                                      new ValueToNameMap(2, "Rock"),
                                                      new ValueToNameMap(3, "Cobble or Gravel or Pebble"),
                                                      new ValueToNameMap(4, "Sandy Gravel"),
                                                      new ValueToNameMap(5, "Very Coarse Sand"),
                                                      new ValueToNameMap(6, "Muddy Sandy Gravel"),
                                                      new ValueToNameMap(7, "Coarse Sand or Gravelly Sand"),
                                                      new ValueToNameMap(8, "Gravelly Muddy Sand"),
                                                      new ValueToNameMap(9, "Medium Sand or Sand"),
                                                      new ValueToNameMap(10, "Muddy Gravel"),
                                                      new ValueToNameMap(11, "Fine Sand or Silty Sand"),
                                                      new ValueToNameMap(12, "Muddy Sand"),
                                                      new ValueToNameMap(13, "Very Fine Sand"),
                                                      new ValueToNameMap(14, "Clayey Sand"),
                                                      new ValueToNameMap(15, "Coarse Silt"),
                                                      new ValueToNameMap(16, "Gravelly Mud or Sandy Silt"),
                                                      new ValueToNameMap(17, "Medium Silt or Sand-Silt-Clay"),
                                                      new ValueToNameMap(18, "Sandy Mud or Silt"),
                                                      new ValueToNameMap(19, "Fine Silt or Clayey Silt"),
                                                      new ValueToNameMap(20, "Sandy Clay"),
                                                      new ValueToNameMap(21, "Very Fine Silt"),
                                                      new ValueToNameMap(22, "Silty Clay"),
                                                      new ValueToNameMap(23, "Clay"),
                                                      new ValueToNameMap(888, "No Data"),
                                                      new ValueToNameMap(999, "Land"),
                                                  };

        public static readonly Dictionary<int, string> SedimentNames = new Dictionary<int, string>
        {
            {1, "Rough Rock"},
            {2, "Rock"},
            {3, "Cobble or Gravel or Pebble"},
            {4, "Sandy Gravel"},
            {5, "Very Coarse Sand"},
            {6, "Muddy Sandy Gravel"},
            {7, "Coarse Sand or Gravelly Sand"},
            {8, "Gravelly Muddy Sand"},
            {9, "Medium Sand or Sand"},
            {10, "Muddy Gravel"},
            {11, "Fine Sand or Silty Sand"},
            {12, "Muddy Sand"},
            {13, "Very Fine Sand"},
            {14, "Clayey Sand"},
            {15, "Coarse Silt"},
            {16, "Gravelly Mud or Sandy Silt"},
            {17, "Medium Silt or Sand-Silt-Clay"},
            {18, "Sandy Mud or Silt"},
            {19, "Fine Silt or Clayey Silt"},
            {20, "Sandy Clay"},
            {21, "Very Fine Silt"},
            {22, "Silty Clay"},
            {23, "Clay"},
            {888, "No Data"},
            {999, "Land"},
        };

        public static readonly Dictionary<int, SedimentType> SedimentTypes = new Dictionary<int, SedimentType>
        {
            //                     name                         category density  compSpd cmpCoef shearSpd shrCoef sspRatio grainSize     lossParam
            {  1, new SedimentType("Rough Rock",                       1, 2.500f, 3750.0f, 1.595f, 699.6f, 46.20f, 2.5000f, float.NaN,    0.01374f)},
            {  2, new SedimentType("Rock",                             2, 2.500f, 3750.0f, 1.595f, 699.6f, 46.20f, 2.5000f, float.NaN,    0.01374f)},
            {  3, new SedimentType("Cobble or Gravel or Pebble",       3, 2.500f, 2700.0f, 1.595f, 699.6f, 46.20f, 1.8000f, float.NaN,    0.01374f)},
            {  4, new SedimentType("Sandy Gravel",                     4, 2.492f, 2005.5f, 1.595f, 699.6f, 46.20f, 1.3370f, -1.0f,        0.01705f)},
            {  5, new SedimentType("Very Coarse Sand",                 5, 2.401f, 1960.1f, 1.595f, 518.0f, 46.20f, 1.3067f, -0.5f,        0.01667f)},
            {  6, new SedimentType("Muddy Sandy Gravel",               6, 2.314f, 1916.7f, 1.595f, 372.6f, 46.20f, 1.2778f,  0.0f,        0.01630f)},
            {  7, new SedimentType("Coarse Sand or Gravelly Sand",     7, 2.231f, 1875.5f, 1.637f, 259.5f, 46.20f, 1.2503f,  0.5f,        0.01638f)},
            {  8, new SedimentType("Gravelly Muddy Sand",              8, 2.151f, 1836.2f, 1.680f, 174.2f, 46.20f, 1.2241f,  1.0f,        0.01645f)},
            {  9, new SedimentType("Medium Sand or Sand",              9, 1.845f, 1767.3f, 1.723f, 171.2f, 46.20f, 1.1782f,  1.5f,        0.01624f)},
            { 10, new SedimentType("Muddy Gravel",                    10, 1.615f, 1709.4f, 1.766f, 168.2f, 46.20f, 1.1396f,  2.0f,        0.01610f)},
            { 11, new SedimentType("Fine Sand or Silty Sand",         11, 1.451f, 1661.0f, 1.809f,  65.6f, 46.20f, 1.1073f,  2.5f,        0.01602f)},
            { 12, new SedimentType("Muddy Sand",                      12, 1.339f, 1620.0f, 2.000f,  58.9f, 46.20f, 1.0800f,  3.0f,        0.01728f)},
            { 13, new SedimentType("Very Fine Sand",                  13, 1.268f, 1585.2f, 2.217f,  52.3f, 46.20f, 1.0568f,  3.5f,        0.01875f)},
            { 14, new SedimentType("Clayey Sand",                     14, 1.224f, 1554.6f, 2.435f,  65.2f, 46.20f, 1.0364f,  4.0f,        0.02019f)},
            { 15, new SedimentType("Coarse Silt",                     15, 1.195f, 1526.9f, 2.651f,  78.1f, 16.80f, 1.0179f,  4.5f,        0.02158f)},
            { 16, new SedimentType("Gravelly Mud or Sandy Silt",      16, 1.169f, 1499.9f, 1.578f, 659.4f, 16.80f, 0.9999f,  5.0f,        0.01261f)},
            { 17, new SedimentType("Medium Silt or Sand-Silt-Clay",   17, 1.149f, 1482.8f, 0.856f, 351.3f, 46.90f, 0.9885f,  5.5f,        0.00676f)},
            { 18, new SedimentType("Sandy Mud or Silt",               18, 1.149f, 1481.0f, 0.489f, 310.3f, 46.20f, 0.9873f,  6.0f,        0.00386f)},
            { 19, new SedimentType("Fine Silt or Clayey Silt",        19, 1.148f, 1479.2f, 0.388f, 284.8f, 46.20f, 0.9861f,  6.5f,        0.00306f)},
            { 20, new SedimentType("Sandy Clay",                      20, 1.147f, 1477.4f, 0.307f, 259.3f, 60.55f, 0.9849f,  7.0f,        0.00242f)},
            { 21, new SedimentType("Very Fine Silt",                  21, 1.147f, 1474.8f, 0.247f, 203.0f, 60.55f, 0.9832f,  7.5f,        0.00194f)},
            { 22, new SedimentType("Silty Clay",                      22, 1.146f, 1473.6f, 0.207f, 146.7f, 60.55f, 0.9824f,  8.0f,        0.00163f)},
            { 23, new SedimentType("Clay",                            23, 1.145f, 1470.0f, 0.189f, 146.7f, 60.55f, 0.9800f,  9.0f,        0.00148f)},
            {888, new SedimentType("No Data",                        888, 0.000f, 0000.0f, 0.000f, 000.0f, 00.00f, 0.0000f,  0.0f,        0.00000f)},
            {999, new SedimentType("Land",                           999, 0.000f, 0000.0f, 0.000f, 000.0f, 00.00f, 0.0000f,  0.0f,        0.00000f)},
        };
        
        public static readonly Dictionary<int, string> CASSSedimentNames = new Dictionary<int, string>
        {
            {1, "ROUGH ROCK"},
            {2, "ROCK"},
            {3, "GRAVEL"},
            {4, "SANDY GRAVEL"},
            {5, "VERY COARSE SAND"},
            {6, "MUDDY SANDY GRAVEL"},
            {7, "GRAVELLY SAND"},
            {8, "GRAVELLY MUDDY SAND"},
            {9, "SAND"},
            {10,"MUDDY GRAVEL"},
            {11,"FINE SAND"},
            {12,"MUDDY SAND"},
            {13,"VERY FINE SAND"},
            {14,"CLAYEY SAND"},
            {15,"COARSE SILT"},
            {16,"SANDY SILT"},
            {17,"MEDIUM SILT"},
            {18,"SILT"},
            {19,"FINE SILT"},
            {20,"SANDY CLAY"},
            {21,"VERY FINE SILT"},
            {22,"SILTY CLAY"},
            {23,"CLAY"},
            //{888, "No Data"}, // removing this because it will make CASS puke if it ever shows up.
            {999, "SAND"},      // should be LAND
        };
        public static readonly List<ValueToNameMap> CASSMap = new List<ValueToNameMap>
                                                  {
                                                      new ValueToNameMap(1, "ROUGH ROCK"),
                                                      new ValueToNameMap(2, "ROCK"),
                                                      new ValueToNameMap(3, "GRAVEL"),
                                                      new ValueToNameMap(4, "SANDY GRAVEL"),
                                                      new ValueToNameMap(5, "VERY COARSE SAND"),
                                                      new ValueToNameMap(6, "MUDDY SANDY GRAVEL"),
                                                      new ValueToNameMap(7, "GRAVELLY SAND"),
                                                      new ValueToNameMap(8, "GRAVELLY MUDDY SAND"),
                                                      new ValueToNameMap(9, "SAND"),
                                                      new ValueToNameMap(10,"MUDDY GRAVEL"),
                                                      new ValueToNameMap(11,"FINE SAND"),
                                                      new ValueToNameMap(12,"MUDDY SAND"),
                                                      new ValueToNameMap(13,"VERY FINE SAND"),
                                                      new ValueToNameMap(14,"CLAYEY SAND"),
                                                      new ValueToNameMap(15,"COARSE SILT"),
                                                      new ValueToNameMap(16,"SANDY SILT"),
                                                      new ValueToNameMap(17,"MEDIUM SILT"),
                                                      new ValueToNameMap(18,"SILT"),
                                                      new ValueToNameMap(19,"FINE SILT"),
                                                      new ValueToNameMap(20,"SANDY CLAY"),
                                                      new ValueToNameMap(21,"VERY FINE SILT"),
                                                      new ValueToNameMap(22,"SILTY CLAY"),
                                                      new ValueToNameMap(23,"CLAY"),
                                                      //new ValueToNameMap(888, "No Data"), // removing this because it will make CASS puke if it ever shows up.
                                                      new ValueToNameMap(999, "SAND"),      // should be LAND
                                                  };

        static BottomSedimentTypeTable() { Map = CASSMap; }

        public static string Lookup(float value)
        {
            if (float.IsNaN(value)) return "NaN";

            var result = Map.Find(m => m.Value == value);
            return result == null ? "Unknown Value" : result.Name;
        }
    }

    public class ValueToNameMap
    {
        public ValueToNameMap(float value, string name)
        {
            Value = value;
            Name = name;
        }

        public float Value { get; private set; }
        public string Name { get; private set; }
    }
}