using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

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
                    var fields = curLine.Split(new[] {'='}, StringSplitOptions.RemoveEmptyEntries);
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
                            throw new FileFormatException("Effects file had unexpected format.  Header field: " +
                                                          fields[0]);
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

        public static void Write(string fileName, List<EffectsRecord> records)
        {
            var sortedRecords = from record in records
                                orderby record.Depth, record.Range
                                select record;
            var sb = new StringBuilder();
            //header
            sb.AppendLine("#head=");
            sb.AppendLine("#tstamp=" + DateTime.Now.ToString());
            sb.AppendLine("#sysver=");
            sb.AppendLine("#title=");
            sb.AppendLine("#mode=");
            sb.AppendLine("#bin=");
            sb.AppendLine("#season=");
            sb.AppendLine("#info=");
            sb.AppendLine("#location=");
            sb.AppendLine("#splineloc=");
            sb.AppendLine("#units=meters");
            //pressure data
            sb.AppendLine("   Depth(m)     Range(m)   PeakPr(kPa)  PeakPr(200dB)  Impulse(P-s)    TEnergy(164dB) TEnergy(1/3oct,dB) MidFreq(1/3oct,Hz) PEnergy(1/3oct,dB)");
            foreach (var e in sortedRecords)
                sb.AppendLine(string.Format("{0,10}{1,13}{2,13}{3,14}{4,14}{5,17}{6,17}{7,19}{8,19}",
                                            e.Depth, e.Range,
                                            e.PeakPressurekPa, e.PeakPressure200dB, e.Impulse, e.Energy164dB,
                                            e.EnergyThirdOct,
                                            e.MidFreq, e.PeakEnergy));
            //SPEC?
            sb.AppendLine("# SPEC:");
            sb.AppendLine("FOR: LOC_");
            foreach (var e in sortedRecords)
            {
                int numFreqBands=0;
                sb.AppendLine(string.Format("{0,12}{1,12}{2,12}", e.Depth, e.Range, numFreqBands));
            }
        }
    }

    public class EffectsRecord
    {
        public double Depth { get; set; }
        public double Range { get; set; }
        public double PeakPressurekPa { get; set; }
        public double PeakPressure200dB { get; set; }
        public double Impulse { get; set; }
        public double Energy164dB { get; set; }
        public double EnergyThirdOct { get; set; }
        public double MidFreq { get; set; }
        public double PeakEnergy { get; set; }

        public static EffectsRecord Parse(string sourceLine)
        {
            var fields = sourceLine.Split(new[] {' '}, StringSplitOptions.RemoveEmptyEntries);
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