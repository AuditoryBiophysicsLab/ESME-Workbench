using System;
using System.IO;
using System.Text;
using System.Xml.Linq;

namespace ESME.Model
{
    public class SourceRecieverLevelBins
    {
        /// <summary>
        /// Lowest tracked exposure level, in dB re: 1 uPa
        /// </summary>
        public float LowExposureLevel { get; set; }

        /// <summary>
        /// Exposure bin width, in dB
        /// </summary>
        public float BinWidth { get; set; }

        public string ModeName { get; set; }

        /// <summary>
        /// The actual array of recieved level bins
        /// </summary>
        public int[] Bins { get; set; }

        public SourceRecieverLevelBins(float lowExposure, float binWidth, int binCount)
        {
            LowExposureLevel = lowExposure;
            BinWidth = binWidth;
            Bins = new int[binCount + 2];
        }

        public void AddExposure(float exposureLevel, string modeName)
        {
            if (ModeName != modeName) ModeName = modeName;
            int bin;
            if (exposureLevel < LowExposureLevel) bin = 0;
            else bin = (int) Math.Min(((exposureLevel - LowExposureLevel)/BinWidth) + 1, Bins.Length - 1);
            Bins[bin]++;
        }

        public void WriteSummaryHeader(StreamWriter stream)
        {
            stream.WriteLine("Bins widths (dB SPL re: 1 uPa):,{0}", BinWidth);
            stream.WriteLine("Low bin counts all pings less than {0} dB", LowExposureLevel);
            stream.WriteLine("High bin counts all pings greater than {0} dB", LowExposureLevel + ((Bins.Length - 2) * BinWidth));
            stream.WriteLine("Values displayed are bin centers");
        }

        public void WriteBinHeader(StreamWriter stream)
        {
            stream.Write("< {0},", LowExposureLevel);
            for (var bin = 1; bin < Bins.Length - 1; bin++)
                stream.Write("{0},", LowExposureLevel + (BinWidth / 2) + ((bin - 1) * BinWidth));
            stream.Write("> {0},", LowExposureLevel + ((Bins.Length - 2) * BinWidth));
        }

        public void WriteBinValues(StreamWriter stream)
        {
            for (var bin = 1; bin < Bins.Length; bin++)
                stream.Write("{0},", Bins[bin]);
        }

        public override string ToString()
        {
            var sb = new StringBuilder();

            foreach (var b in Bins) sb.Append(b + ", ");

            return sb.Remove(sb.Length - 2, 2).ToString();
        }

        public void AddLevelRangeMetadata(int recieverCount, int sourceCount, StreamWriter sw)
        {
            sw.Write("ReceieverCount, SourceCount, BinCount, ");
            for (var i = 0; i < Bins.Length - 1; i++) sw.Write("Bin{0}Width, ", i);
            sw.WriteLine("Bin{0}Width", Bins.Length - 1);
            sw.Write("{0}, {1}, {2}, ", recieverCount, sourceCount, Bins.Length);
            sw.Write("{0}, ", LowExposureLevel);
            var cur = LowExposureLevel;
            for (var i = 1; i < Bins.Length - 2; i++)
            {
                sw.Write("{0}, ", BinWidth);
                cur += BinWidth;
            }
            sw.WriteLine("{0}", 230 - cur);
        }

        public void AddLevelRangeMetadata(XElement rootElement)
        {
            var levelRanges = new XElement("LevelRanges");
            var lowRange = new XElement("LowValue", 0);
            var highRange = new XElement("HighValue", LowExposureLevel);
            levelRanges.Add(new XElement("LevelRange", lowRange, highRange));
            var curLow = LowExposureLevel;
            for (var i = 0; i < Bins.Length - 2; i++)
            {
                var curHigh = curLow + BinWidth;
                lowRange = new XElement("LowValue", curLow);
                highRange = new XElement("HighValue", curHigh);
                levelRanges.Add(new XElement("LevelRange", lowRange, highRange));
                curLow = curHigh;
            }
            lowRange = new XElement("LowValue", curLow);
            highRange = new XElement("HighValue", 230);
            levelRanges.Add(new XElement("LevelRange", lowRange, highRange));
            rootElement.Add(levelRanges);
        }

        public void AddExposureBins(XElement sourceElement)
        {
            var exposureBins = new XElement("ExposureBins");
            foreach (var bin in Bins) exposureBins.Add(new XElement("ExposureBin", bin));
            sourceElement.Add(exposureBins);
        }
    }
}