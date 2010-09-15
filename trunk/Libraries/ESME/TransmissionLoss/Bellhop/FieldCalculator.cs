using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
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
            Console.WriteLine(@"Starting calculation of {0}", Path.GetFileNameWithoutExtension(OutputFileName));
            Parallel.ForEach(RunFile.BellhopRadials, (r, loopstate) =>
            {
                Console.WriteLine(@"Launching radial {0} of {1} for calculation...", radialNum++, RunFile.BellhopRadials.Count);
                var result = RadialCalculator.ComputeRadial(r.Configuration, r.BottomProfile, r.BearingFromSource_degrees);
                Console.WriteLine(@"   radial complete.");
                fieldData.AddRadial(result);
            });
            Console.WriteLine(@"Calculation complete.");
            fieldData.DepthsMeters = fieldData.Radials[0].Depths_meters;
            fieldData.RangesMeters = fieldData.Radials[0].Ranges_meters;
            fieldData.Save(true);
        }
      }
}
