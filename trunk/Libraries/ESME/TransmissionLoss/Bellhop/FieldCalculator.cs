using System;
using System.IO;
using System.Text;
using System.Threading.Tasks;
using ESME.Model;

namespace ESME.TransmissionLoss.Bellhop
{
    public static class FieldCalculator
    {
        public static TransmissionLossField ComputeField(BellhopRunFile runFile, IHasLog viewModel)
        {
            var stringBuilder = new StringBuilder();
            var fieldData = new TransmissionLossField(runFile);
            var radialNum = 0;
            stringBuilder.Append(String.Format("{0} Starting bellhop calculation\n", DateTime.Now));
            viewModel.Log = stringBuilder.ToString();
            Parallel.ForEach(runFile.BellhopRadials, (r, loopstate) =>
                                                     {
                                                         var localRadialNum = ++radialNum;
                                                         stringBuilder.Append(String.Format("{0} Launching radial {1} of {2} for calculation...\n", DateTime.Now, localRadialNum, runFile.BellhopRadials.Count));
                                                         viewModel.Log = stringBuilder.ToString();
                                                         fieldData.AddRadial(RadialCalculator.ComputeRadial(r.Configuration, r.BottomProfile, r.BearingFromSourceDegrees));
                                                         stringBuilder.Append(String.Format("{0} radial {1} complete.\n", DateTime.Now, localRadialNum));
                                                         viewModel.Log = stringBuilder.ToString();
                                                     });
            stringBuilder.Append(String.Format("{0} Bellhop calculation complete.\n", DateTime.Now));
            viewModel.Log = stringBuilder.ToString();
            fieldData.Depths = fieldData.Radials[0].Depths;
            fieldData.Ranges = fieldData.Radials[0].Ranges;
            return fieldData;
        }
    }
}