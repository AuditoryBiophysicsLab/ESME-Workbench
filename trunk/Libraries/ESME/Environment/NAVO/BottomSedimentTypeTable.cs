using System.Collections.Generic;

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
                                                      new ValueToNameMap(888, "No Data"),
                                                      new ValueToNameMap(999, "Land"),
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