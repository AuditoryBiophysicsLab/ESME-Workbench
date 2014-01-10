using System;
using System.IO;

namespace ESME.Environment
{
    public static class EnvironmentFileExtensions
    {
        public static void SaveAsText(this Sediment sediment, string fileName)
        {
            using (var writer = new StreamWriter(new FileStream(fileName, FileMode.Create, FileAccess.Write, FileShare.None)))
            {
                writer.WriteLine("// Sediment file");
                writer.WriteLine("// ");
                writer.WriteLine("// All lines that start with '//' are considered comments and are ignored");
                writer.WriteLine("// All non-comment lines will be interpreted as data. If they are not valid,");
                writer.WriteLine("// an exception will be thrown when reading this file");
                writer.WriteLine("// ");
                writer.WriteLine("// Data format is as follows");
                writer.WriteLine("// <Latitude>, <Longitude>, <SedimentTypeInteger>");
                writer.WriteLine("// <Latitude> and <Longitude> are floating-point values, in degrees");
                writer.WriteLine("// <Latitude> must be between -90 and 90, inclusive");
                writer.WriteLine("// <Longitude> must be between -360 and 360, inclusive");
                writer.WriteLine("// <SedimentTypeInteger> must be an integer, chosen from the following table:");
                writer.WriteLine("// Integer value   Sediment type");
                writer.WriteLine("// =============   =================================");
                writer.WriteLine("//    1            Rough Rock");
                writer.WriteLine("//    2            Rock");
                writer.WriteLine("//    3            Cobble or Gravel or Pebble");
                writer.WriteLine("//    4            Sandy Gravel");
                writer.WriteLine("//    5            Very Coarse Sand");
                writer.WriteLine("//    6            Muddy Sandy Gravel");
                writer.WriteLine("//    7            Coarse Sand or Gravelly Sand");
                writer.WriteLine("//    8            Gravelly Muddy Sand");
                writer.WriteLine("//    9            Medium Sand or Sand");
                writer.WriteLine("//   10            Muddy Gravel");
                writer.WriteLine("//   11            Fine Sand or Silty Sand");
                writer.WriteLine("//   12            Muddy Sand");
                writer.WriteLine("//   13            Very Fine Sand");
                writer.WriteLine("//   14            Clayey Sand");
                writer.WriteLine("//   15            Coarse Silt");
                writer.WriteLine("//   16            Gravelly Mud or Sandy Silt");
                writer.WriteLine("//   17            Medium Silt or Sand-Silt-Clay");
                writer.WriteLine("//   18            Sandy Mud or Silt");
                writer.WriteLine("//   19            Fine Silt or Clayey Silt");
                writer.WriteLine("//   20            Sandy Clay");
                writer.WriteLine("//   21            Very Fine Silt");
                writer.WriteLine("//   22            Silty Clay");
                writer.WriteLine("//   23            Clay");
                writer.WriteLine("//  888            No Data");
                writer.WriteLine("//  999            Land");
                writer.WriteLine("// ");
                writer.WriteLine("// Data begins here");
                foreach (var sample in sediment.Samples)
                    writer.WriteLine(string.Format("{0}, {1}, {2}", sample.Latitude, sample.Longitude, sample.Data.SampleValue));
                //    writer.WriteLine(string.Format("{0}, {1}, {2}", sample.Latitude, sample.Longitude, 9));
            }
        }

        public static void SaveAsText(this Bathymetry bathymetry, string fileName)
        {
            using (var writer = new StreamWriter(new FileStream(fileName, FileMode.Create, FileAccess.Write, FileShare.None)))
            {
                writer.WriteLine("// Bathymetry file");
                writer.WriteLine("// ");
                writer.WriteLine("// All lines that start with '//' are considered comments and are ignored");
                writer.WriteLine("// All non-comment lines will be interpreted as data. If they are not valid,");
                writer.WriteLine("// an exception will be thrown when reading this file");
                writer.WriteLine("// ");
                writer.WriteLine("// Data format is as follows");
                writer.WriteLine("// <Latitude>, <Longitude>, <BottomDepth>");
                writer.WriteLine("// <Latitude> and <Longitude> are floating-point values, in degrees");
                writer.WriteLine("// <Latitude> must be between -90 and 90, inclusive");
                writer.WriteLine("// <Longitude> must be between -360 and 360, inclusive");
                writer.WriteLine("// <BottomDepth> is a floating-point value, in meters");
                writer.WriteLine("//               Depths below sea level are positive, depths above (coastlines, etc) are negative");
                writer.WriteLine("// ");
                writer.WriteLine("// Data begins here");
                foreach (var sample in bathymetry.Samples)
                    writer.WriteLine(string.Format("{0}, {1}, {2}", sample.Latitude, sample.Longitude, sample.Data));
            }
        }

        public static void SaveAsText(this Wind wind, string fileName)
        {
            if (wind.TimePeriods.Count > 0) throw new InvalidOperationException("Cannot save Wind data with multiple time periods to a text file");
            SaveAsText(wind.TimePeriods[0], fileName);
        }

        public static void SaveAsText(this TimePeriodEnvironmentData<WindSample> timePeriod, string fileName)
        {
            using (var writer = new StreamWriter(new FileStream(fileName, FileMode.Create, FileAccess.Write, FileShare.None)))
            {
                writer.WriteLine("// Windspeed file");
                writer.WriteLine("// ");
                writer.WriteLine("// All lines that start with '//' are considered comments and are ignored");
                writer.WriteLine("// All non-comment lines will be interpreted as data. If they are not valid,");
                writer.WriteLine("// an exception will be thrown when reading this file");
                writer.WriteLine("// ");
                writer.WriteLine("// Data format is as follows");
                writer.WriteLine("// <Latitude>, <Longitude>, <Windspeed>");
                writer.WriteLine("// <Latitude> and <Longitude> are floating-point values, in degrees");
                writer.WriteLine("// <Latitude> must be between -90 and 90, inclusive");
                writer.WriteLine("// <Longitude> must be between -360 and 360, inclusive");
                writer.WriteLine("// <Windspeed> is a floating-point value, in meters per second");
                writer.WriteLine("// ");
                writer.WriteLine("// Data begins here");
                foreach (var sample in timePeriod.EnvironmentData)
                    writer.WriteLine(string.Format("{0}, {1}, {2}", sample.Latitude, sample.Longitude, sample.Data));
            }
        }

        public static void SaveAsText(this SoundSpeed soundSpeed, string fileName)
        {
            if (soundSpeed.SoundSpeedFields.Count > 0) throw new InvalidOperationException("Cannot save Soundspeed data with multiple time periods to a text file");
            SaveAsText(soundSpeed.SoundSpeedFields[0], fileName);
        }

        public static void SaveAsText(this SoundSpeedField field, string fileName)
        {
            using (var writer = new StreamWriter(new FileStream(fileName, FileMode.Create, FileAccess.Write, FileShare.None)))
            {
                writer.WriteLine("// Soundspeed file");
                writer.WriteLine("// ");
                writer.WriteLine("// All lines that start with '//' are considered comments and are ignored");
                writer.WriteLine("// All non-comment lines will be interpreted as data. If they are not valid,");
                writer.WriteLine("// an exception will be thrown when reading this file");
                writer.WriteLine("// ");
                writer.WriteLine("// Data format is as follows");
                writer.WriteLine("// <Latitude>, <Longitude>");
                writer.WriteLine("// <Depth>, <Soundspeed>");
                writer.WriteLine("// <Latitude> and <Longitude> are floating-point values, in degrees");
                writer.WriteLine("// <Latitude> must be between -90 and 90, inclusive");
                writer.WriteLine("// <Longitude> must be between -360 and 360, inclusive");
                writer.WriteLine("// <Depth> is a floating-point value, in meters (positive values only)");
                writer.WriteLine("// <Soundspeed> is a floating-point value, in meters per second");
                writer.WriteLine("// ");
                writer.WriteLine("// <Depth>, <Soundspeed> pairs repeat until there are no more data for the current location");
                writer.WriteLine("// A blank line signifies the end of the <Depth>, <Soundspeed> pairs");
                writer.WriteLine("// After a blank line, the next line is expected to contain another <Latitude>, <Longitude>");
                writer.WriteLine("// ");
                writer.WriteLine("// Data begins here");
                foreach (var location in field.EnvironmentData)
                {
                    writer.WriteLine(string.Format("{0}, {1}", location.Latitude, location.Longitude));
                    foreach (var sample in location.Data)
                        writer.WriteLine(string.Format("{0}, {1}", sample.Depth, sample.SoundSpeed));
                    writer.WriteLine();
                }
            }
        }
    }
}
