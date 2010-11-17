using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using HRC.Navigation;

namespace ESME.TransmissionLoss
{
     
    class SoundPressureLevelRadial:TransmissionLossRadial
    {
        public float[,] SoundPressureLevel { get; private set; }

        public void SaveAsCSV(string fileName, SoundPressureLevelField soundPressureLevelField)
        {
            using (var sw = new StreamWriter(fileName))
            {
                // Write the X axis values out first
                sw.WriteLine("Vertical SPL (dB)");
                sw.Write(",Range (m),");

                for (int i = 0; i < Ranges.Length; i++) sw.Write(Ranges[i] + ","); //write out the X axis values.
                sw.WriteLine(); // Terminate the line
                sw.WriteLine("Depth (m)");
                // Write the slice data
                for (int i = 0; i < Depths.Length; i++)
                {
                    // Write out the Y axis value
                    sw.Write(Depths[i] + ",,");
                    for (var j = 0; j < Ranges.Length; j++)
                    {
                        sw.Write(SoundPressureLevel[Depths.Length - i - 1, j] + ","); //todo: verify dimension match.
                    }
                    sw.WriteLine(); // Terminate the line
                } // for i
                sw.WriteLine();
                //sw.Write(",Bottom depth:,");
                //for (var i = 0; i < Ranges.Length; i++)
                //    sw.Write(bottomProfile.Profile[i].ToString() + ",");
                sw.WriteLine();
                sw.WriteLine();
                sw.WriteLine("Sound Source information");
                sw.WriteLine("Source Latitude," + soundPressureLevelField.Latitude);
                sw.WriteLine("Source Longitude," + soundPressureLevelField.Longitude);
                sw.WriteLine("Depth (m)," + soundPressureLevelField.SourceDepth);
                sw.WriteLine("High Frequency (Hz)," + soundPressureLevelField.HighFrequency);
                sw.WriteLine("Low Frequency (Hz)," + soundPressureLevelField.LowFrequency);
                var radialEnd = new EarthCoordinate(soundPressureLevelField.Latitude, soundPressureLevelField.Longitude);
                radialEnd.Move(BearingFromSource, soundPressureLevelField.Radius);
                sw.WriteLine("Receiver Latitude," + radialEnd.Latitude_degrees);
                sw.WriteLine("Receiver Longitude," + radialEnd.Longitude_degrees);
                sw.WriteLine();
            } // using sw
        }
    }


}
