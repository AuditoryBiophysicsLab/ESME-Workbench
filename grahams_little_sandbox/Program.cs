using System.Collections.Generic;
using System.Diagnostics;
using ESME.Data;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.Bellhop;
using ESME.TransmissionLoss.BellhopNL;
using ESME.TransmissionLoss.REFMS;
using HRC.Navigation;

namespace grahams_little_sandbox
{
    class Program
    {
        static void Main(string[] args) {
#if false
            var cassFile = @"C:\Users\Graham Voysey\Desktop\testoutput.BIN";

            var cassresult = CASSOutput.Load(cassFile, false);

            var tlf = TransmissionLossField.FromCASS(cassresult);

            var cassOut = @"C:\Users\Graham Voysey\Desktop\gvtestoutput.BIN";
            CASSOutput.Write(cassOut, tlf);
            var outresult = CASSOutput.Load(cassOut, false);


            // var tlf = TransmissionLossField.FromCASS(CASSOutput.Load(cassFile));  

            var ddbFile = @"C:\Users\Graham Voysey\Desktop\test2.ddb";
            var output = DDB.Load(ddbFile);
            var foo = output.AnimatStartPoints;
                var mmmbFile =
                    @"C:\Users\Graham Voysey\Desktop\test.3mb";
            var output = MMMB.Load(mmmbFile);
            var foo = output.AnimatStartPoints;
#endif
                                            
            string arrFile = @"C:\tests\score.arr";
            var z = 2500 / 3.2808;
            var w = 300 * 0.45359237;
            var fs     = 88200;
            var T      = 0.25;
            var model = BellhopNLInput.NLModelType.arons;

           const string infilePath = @"C:\tests\nlinput.bin";
           const string outfilePath = @"C:\tests\nloutput.bin";
           const string effectsPath = @"C:\tests\nloutput.effects";

#if true
           var bellhopInput = new BellhopRunFile
           {
               BathymetryName = "",
               BellhopSettings = new BellhopSettings() { },
               TransmissionLossAlgorithm = TransmissionLossAlgorithm.Bellhop,
               EnvironmentName = "",
               Filename = "",
               Metadata = "",
               RangeComplexName = "",
               RangeDistanceIncrement = 0,
               ReferenceLocation = new EarthCoordinate(),
               ScenarioDataDirectory = "",
               TransmissionLossJob = new TransmissionLossJob() { ModeName = ""},
               TransmissionLossRunFileRadials = new List<TransmissionLossRunFileRadial>(),
               
              
           };
           new BellhopNLInput
              {
                  ChargeDepth = z,
                  ChargeMass = w,
                  OutputFreq = fs,
                  OutputTime = T,
                  ModelType = model.ToString(),
                  // Bearing = 180,
                  //BellhopConfiguration = "",
                  //BottomProfile = "",
                  //CalculationRange = 1000,
                  //Depths = "",
                  //EnvFilename = "",
                  //Ranges = "",
                  //TopReflectionCoefficients = "",
                  //WaterDepth = (float)z+500,
                  
                  
              }.Save(infilePath); 
#endif

#if false
           var process = Process.Start(new ProcessStartInfo()
      {
          FileName = @"C:\Projects\ESME Deliverables\Utilities\BellhopNL\bin\Debug\BellhopNL.exe",
          Arguments = string.Format("-dataFile {0} -outFile {1} -isTest true -arrivalsFile {2} ", infilePath, outfilePath, arrFile)
      });
           process.WaitForExit(); 
#endif
            var nlOutput = BellhopNLOutput.Load(outfilePath);
            
            var effectsRecords = BellhopNL.Transform(nlOutput);
        
            EffectsFile.Write(effectsPath,effectsRecords,nlOutput.ModeName,nlOutput.ChargeDepth,nlOutput.OutputTime,nlOutput.TimePeriod);
        }
    }
}
