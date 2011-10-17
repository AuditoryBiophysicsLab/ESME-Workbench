using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ESME.TransmissionLoss.REFMS;

namespace ESME.TransmissionLoss.BellhopNL
{
    public class BellhopNL
    {
       public static List<EffectsRecord> Transform(BellhopNLOutput output)
        {
            var result = new List<EffectsRecord>();
            for (var i = 0; i < output.Waveforms.GetLength(0); i++)//ranges
            {
                for (var j = 0; j < output.Waveforms.GetLength(1); j++)//depths
                {
                    var effectsRecord = new EffectsRecord();// { Depth = output.Depths[j], Range = output.Ranges[i] };
                    var maxPa = Double.MinValue;
                    var impulseSum =0.0;
                    for (var k = 0; k < output.Waveforms.GetLength(2); k++)//payload waveform.
                    {
                        if (output.Waveforms[i, j, k] > maxPa) maxPa = Math.Abs(output.Waveforms[i, j, k]);
                        impulseSum += output.Waveforms[i, j, k] * output.Waveforms[i, j, k];
                    }
                    
                    effectsRecord.PeakPressurekPa = maxPa*10E-9;  //from uPa to kPa
                    effectsRecord.PeakPressure200dB = 20 * Math.Log10(maxPa);
                    effectsRecord.Energy164dB = 10 * Math.Log10(impulseSum); //?
                    effectsRecord.Impulse = Math.Sqrt(impulseSum);  //?
                    //if (effectsRecord.PeakPressure200dB > 200 && effectsRecord.Energy164dB > 164) break;
                    result.Add(effectsRecord);
                    
                }
            }
            return result;
        }
        public List<double> ThirdOctaves(double lowFrequency, double highFrequency)
        {
            if (lowFrequency <= 0 || highFrequency <= 0 || lowFrequency >= highFrequency) throw new ApplicationException("");
            var result = new List<double>();
            var thislf = lowFrequency;
            var thishf = lowFrequency*Math.Pow(2,(1/3));
            var thiscf = thishf - thislf;
            while (thishf <= highFrequency)
            {
                result.Add(thiscf);
                
            }

            return null;
        }
        
    }
}
