using System;
using BellhopNLLib;
using ESME.TransmissionLoss.BellhopNL;
using MathWorks.MATLAB.NET.Arrays;

namespace BellhopNLWrapper
{
    public class BellhopNLWrapper
    {
        public MWCharArray ArrivalsFile { get; set; }
        // ReSharper disable InconsistentNaming
        public enum ModelType
        {
            arons,
            chapman
        }

        // ReSharper restore InconsistentNaming
        public MWCharArray ModelName { get; set; }
        public double ChargeDepth { get; set; }
        public double ChargeMass { get; set; }
        public double OutputSampleRate { get; set; }
        public double OutputWaveformDuration { get; set; }
        public double[,,] Waveforms { get; private set; }
        public double[] Ranges { get; private set; }
        public double[] Depths { get; private set; }
        public double PeakEnergy { get; internal set; }
        public double MaxEnergy { get; internal set; }
        public double[] EFD { get; internal set; }
        public double[] ThirdOctaveCenterFrequencies { get; internal set; }

        public static BellhopNLWrapper Run(string arrivalsFile, double chargeDepth, double chargeMass, double outputRate,
                                           double outputDuration, BellhopNLInput.NLModelType modelType)
        {
            var wrapper = new BellhopNLWrapper()
            {
                    ArrivalsFile = new MWCharArray(arrivalsFile),
                    ChargeDepth = chargeDepth,
                    ChargeMass = chargeMass,
                    OutputSampleRate = outputRate,
                    OutputWaveformDuration = outputDuration,
                    ModelName = new MWCharArray(modelType.ToString()),
            };
            
            var result =new NLDelaySum().NL_delaysum(3,wrapper.ArrivalsFile, wrapper.ChargeDepth, wrapper.ChargeMass,
                                                 wrapper.OutputSampleRate, wrapper.OutputWaveformDuration,
                                                wrapper.ModelName);
            var waveDimensions = result[0].Dimensions;
            wrapper.Waveforms = new double[waveDimensions[1], waveDimensions[2], waveDimensions[0]];
            wrapper.Ranges = new double[result[1].Dimensions[0]];
            wrapper.Depths = new double[ result[2].Dimensions[0]];
            
            for (var i = 0; i < waveDimensions[0]; i++) for (var j = 0; j < waveDimensions[1]; j++) for (var k = 0; k < waveDimensions[2]; k++) wrapper.Waveforms[j, k, i] = (double)((MWNumericArray)result[0])[i + 1, j + 1, k + 1];
            for (var i = 0; i < wrapper.Ranges.Length; i++) wrapper.Ranges[i] = (double)((MWNumericArray)result[1])[i + 1];
            for (var i = 0; i < wrapper.Depths.Length; i++) wrapper.Depths[i] = (double)((MWNumericArray)result[2])[i + 1];
            
            return wrapper;
        }

        public static void ComputeThirdOctaves(BellhopNLWrapper wrapper)
        {
            for (var i = 0; i < wrapper.Waveforms.GetLength(0); i++)
            {
                for (var j = 0; j < wrapper.Waveforms.GetLength(1); j++)
                {
                    var pressures = new double[wrapper.Waveforms.GetLength(2)];
                    for (var k = 0; k < wrapper.Waveforms.GetLength(2); k++)
                    {
                        pressures[k] = wrapper.Waveforms[i, j, k];
                    }
                    var retMat = new ThirdOctaveCalculator().ThirdOctave(4,(MWNumericArray)pressures, wrapper.OutputSampleRate);
                    wrapper.PeakEnergy= (double)((MWNumericArray)retMat[0]);
                    wrapper.MaxEnergy = (double)((MWNumericArray)retMat[1]);
                    wrapper.EFD = new double[retMat[2].Dimensions[0]];
                    wrapper.ThirdOctaveCenterFrequencies = new double[retMat[3].Dimensions[1]];
                    for (var i1 = 0; i1 < wrapper.EFD.Length; i1++) wrapper.EFD[i1] = (double)((MWNumericArray)retMat[2])[i1 + 1];
                    for (var i1 = 0; i1 < wrapper.ThirdOctaveCenterFrequencies.Length; i1++) wrapper.ThirdOctaveCenterFrequencies[i1] = (double)((MWNumericArray)retMat[3])[i1 + 1];
                }
            }
        }
    }
}