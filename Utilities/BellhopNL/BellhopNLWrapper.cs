using MathWorks.MATLAB.NET.Arrays;

namespace BellhopNL
{
    public class BellhopNLWrapper
    {
        public MWCharArray ArrivalsFile { get; set; }
        // ReSharper disable InconsistentNaming
        public enum ModelType{ arons, chapman,}
        // ReSharper restore InconsistentNaming
        public MWCharArray ModelName { get; set; }
        public double ChargeDepth { get; set; }
        public double ChargeMass { get; set; }
        public double OutputSampleRate { get; set; }
        public double OutputWaveformDuration { get; set; }
        public double[, ,] Waveforms { get; private set; }

        public static BellhopNLWrapper Run(string arrivalsFile, double chargeDepth, double chargeMass, double outputRate, double outputDuration, ModelType modelType)
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
            var result = (MWNumericArray)new NLDelaySum().NL_delaysum(wrapper.ArrivalsFile, wrapper.ChargeDepth, wrapper.ChargeMass, wrapper.OutputSampleRate, wrapper.OutputWaveformDuration, wrapper.ModelName);

            var dimensions = result.Dimensions;
            wrapper.Waveforms = new double[dimensions[1],dimensions[2],dimensions[0]];
            for (var i = 0; i < dimensions[0]; i++)
            {
                for (var j = 0; j < dimensions[1]; j++)
                {
                    for (var k = 0; k < dimensions[2]; k++)
                    {
                        wrapper.Waveforms[j, k, i] = (double)result[i + 1, j + 1, k + 1];
                    }
                }
            }
            
            return wrapper;
        }
        
    }
}