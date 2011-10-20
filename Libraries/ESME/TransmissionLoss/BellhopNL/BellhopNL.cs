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
                    var effectsRecord = new EffectsRecord{ Depth = output.Depths[j], Range = output.Ranges[i] };
                    var maxPa = Double.MinValue;
                    var impulseSum =0.0;
                    for (var k = 0; k < output.Waveforms.GetLength(2); k++)//payload waveform.
                    {
                        var thisPressure = output.Waveforms[i, j, k];
                        if (thisPressure > maxPa) maxPa = Math.Abs(thisPressure);
                        impulseSum += thisPressure * thisPressure;
                    }
                    
                    effectsRecord.PeakPressurekPa = maxPa*10E-9;  //from uPa to kPa
                    effectsRecord.PeakPressure200dB = 20 * Math.Log10(maxPa);
                    effectsRecord.Energy164dB = 10 * Math.Log10(impulseSum); //?
                    effectsRecord.Impulse = 10 * Math.Log10(Math.Sqrt(impulseSum/output.Waveforms.GetLength(2)));  //? RMS.
                    effectsRecord.ThirdOctaveBandPressures = new List<Tuple<double, double>>();
                    //effectsRecord.MidFreq=
                    //effectsRecord.PeakEnergy = 
                    //effectsRecord.EnergyThirdOct = 
                    if (output.EFD.Length != output.ThirdOctaveCenterFrequencies.Length) throw new ApplicationException("");
                    for (var index = 0; index < output.ThirdOctaveCenterFrequencies.Length; index++) effectsRecord.ThirdOctaveBandPressures.Add(new Tuple<double, double>(output.ThirdOctaveCenterFrequencies[index],output.EFD[index]));
                    
                    result.Add(effectsRecord);
                    
                }
            }
            return result;
        }
    }
}
