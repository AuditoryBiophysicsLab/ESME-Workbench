using System;
using System.Collections.Generic;
using System.IO;

namespace ESME.TransmissionLoss.REFMS
{
    public class EffectsFile
    {
        public string Head { get; private set; }
        public string Timestamp { get; private set; }
        public string SystemVersion { get; private set; }
        public string Title { get; private set; }
        public string Mode { get; private set; }
        public string Bin { get; private set; }
        public string Season { get; private set; }
        public string Info { get; private set; }
        public string Location { get; private set; }
        public string SplineLocation { get; private set; }
        public string Units { get; private set; }
        public List<EffectsRecord> EffectsRecords { get; private set; }

        public static EffectsFile Read(string fileName)
        {
            var result = new EffectsFile();
            using (var reader = new StreamReader(fileName))
            {
                var curLine = reader.ReadLine().Trim();
                while (!curLine.StartsWith("Depth(m)"))
                {
                    var fields = curLine.Split(new[] { '=' }, StringSplitOptions.RemoveEmptyEntries);
                    switch (fields[0].Trim().ToLower())
                    {
                        case "#head":
                            result.Head = fields[1];
                            break;
                        case "#tstamp":
                            result.Timestamp = fields[1];
                            break;
                        case "#sysver":
                            result.SystemVersion = fields[1];
                            break;
                        case "#title":
                            result.Title = fields[1];
                            break;
                        case "#mode":
                            result.Mode = fields[1];
                            break;
                        case "#bin":
                            result.Bin = fields[1];
                            break;
                        case "#season":
                            result.Season = fields[1];
                            break;
                        case "#info":
                            result.Info = fields[1];
                            break;
                        case "#location":
                            result.Location = fields[1];
                            break;
                        case "#splineloc":
                            result.SplineLocation = fields[1];
                            break;
                        case "#units":
                            result.Units = fields[1];
                            break;
                        default:
                            throw new FileFormatException("Effects file had unexpected format.  Header field: " + fields[0]);
                    }
                }
                result.EffectsRecords = new List<EffectsRecord>();
                while (!curLine.StartsWith("# SPEC:"))
                {
                    curLine = reader.ReadLine().Trim();
                    result.EffectsRecords.Add(EffectsRecord.Parse(curLine));
                }
            }
            return result;
        }
    }

    public class EffectsRecord
    {
        public double Depth { get; private set; }
        public double Range { get; private set; }
        public double PeakPressurekPa { get; private set; }
        public double PeakPressure200dB { get; private set; }
        public double Impulse { get; private set; }
        public double Energy164dB { get; private set; }
        public double EnergyThirdOct { get; private set; }
        public double MidFreq { get; private set; }
        public double PeakEnergy { get; private set; }

        public static EffectsRecord Parse(string sourceLine)
        {
            var fields = sourceLine.Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
            return new EffectsRecord
            {
                Depth = double.Parse(fields[0]),
                Range = double.Parse(fields[1]),
                PeakPressurekPa = double.Parse(fields[2]),
                PeakPressure200dB = double.Parse(fields[3]),
                Impulse = double.Parse(fields[4]),
                Energy164dB = double.Parse(fields[5]),
                EnergyThirdOct = double.Parse(fields[6]),
                MidFreq = double.Parse(fields[7]),
                PeakEnergy = double.Parse(fields[8]),
            };
        }
    }
}
