using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ESME;
using ESME.Animats;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.CASS;
using HRC;
using ESME.Model;
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
            
            string arrFile = @"C:\Projects\ESME Deliverables\Utilities\bellhopNL\score.arr";
            var z = 2500 / 3.2808;
            var w = 300 * 0.45359237;
            var fs     = 88200;
            var T      = 0.25;
            var model = BellhopNLWrapper.ModelType.arons;

            BellhopNLWrapper result = BellhopNLWrapper.Run(arrFile, z, w, fs, T, model);

#endif
                                        }
    }
}
