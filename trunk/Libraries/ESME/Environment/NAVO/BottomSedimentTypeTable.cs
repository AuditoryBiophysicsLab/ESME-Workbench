using System.Collections.Generic;

namespace ESME.Environment.NAVO
{
    public static class BottomSedimentTypeTable
    {
        static readonly List<ValueToNameMap> Map = new List<ValueToNameMap>();

        static BottomSedimentTypeTable()
        {
            Map.AddRange(new[]
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
                         });
        }

        public static string Lookup(float value)
        {
            if (float.IsNaN(value)) return "NaN";

            var result = Map.Find(m => m.Value == value);
            return result == null ? "Unknown Value" : result.Name;
        }
    }

    internal class ValueToNameMap
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