using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ESME.Model;

namespace ESME.TransmissionLoss.Bellhop
{
    public class FieldCalculator
    {
        public  BellhopRunFile RunFile { get; set; }
        public  string OutputFileName { get; set; }
        public  void ComputeField()
        {
            var fieldData = new TransmissionLossField(RunFile)
            {
                Filename = OutputFileName,

            };
            int radialNum = 0;
            foreach (var radial in RunFile.BellhopRadials)
            {
                Console.WriteLine(@"computing radial {0} of {1}", radialNum++, RunFile.BellhopRadials.Count);
                var result = RadialCalculator.ComputeRadial(radial.Configuration, radial.BottomProfile, radial.BearingFromSource_degrees);
                fieldData.AddRadial(result);
            }

            fieldData.DepthsMeters = fieldData.Radials[0].Depths_meters;
            fieldData.RangesMeters = fieldData.Radials[0].Ranges_meters;
            fieldData.Save(true);
        }
    }
}
