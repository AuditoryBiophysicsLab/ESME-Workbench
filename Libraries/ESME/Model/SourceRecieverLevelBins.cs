using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.Linq;

namespace ESME.Model
{
    public class SourceRecieverLevelBins
    {
        public float LowExposure_dBSPL { get; set; }
        public float BinWidth_dBSPL { get; set; }
        public int[] Bins { get; set; }

        public SourceRecieverLevelBins(float LowExposure_dBSPL, float BinWidth_dBSPL, int BinCount)
        {
            this.LowExposure_dBSPL = LowExposure_dBSPL;
            this.BinWidth_dBSPL = BinWidth_dBSPL;
            Bins = new int[BinCount + 2];
        }

        public void AddExposure(float ExposureLevel_dBSPL)
        {
            int bin = 0;
            if (ExposureLevel_dBSPL < LowExposure_dBSPL)
                bin = 0;
            else
                bin = (int)Math.Min(((ExposureLevel_dBSPL - LowExposure_dBSPL) / BinWidth_dBSPL) + 1, Bins.Length - 1);
            Bins[bin]++;
        }

        public override string ToString()
        {
            StringBuilder sb = new StringBuilder();

            foreach (int b in Bins)
                sb.Append(b.ToString()+", ");

            return sb.Remove(sb.Length - 2, 2).ToString();
        }

        public void AddLevelRangeMetadata(int RecieverCount, int SourceCount, System.IO.StreamWriter sw)
        {
            sw.Write("ReceieverCount, SourceCount, BinCount, ");
            for (int i = 0; i < Bins.Length - 1; i++)
                sw.Write("Bin{0}Width, ", i);
            sw.WriteLine("Bin{0}Width", Bins.Length - 1);
            sw.Write("{0}, {1}, {2}, ", RecieverCount, SourceCount, Bins.Length);
            sw.Write("{0}, ", LowExposure_dBSPL);
            float cur = LowExposure_dBSPL;
            for (int i = 1; i < Bins.Length - 2; i++)
            {
                sw.Write("{0}, ", BinWidth_dBSPL);
                cur += BinWidth_dBSPL;
            }
            sw.WriteLine("{0}", 230 - cur);
        }

        public void AddLevelRangeMetadata(XElement RootElement)
        {

            XElement LevelRanges = new XElement("LevelRanges");
            XElement LowRange = new XElement("LowValue", 0);
            XElement HighRange = new XElement("HighValue", LowExposure_dBSPL);
            float CurLow, CurHigh;
            LevelRanges.Add(new XElement("LevelRange", LowRange, HighRange));
            CurLow = LowExposure_dBSPL;
            for (int i = 0; i < Bins.Length - 2; i++)
            {
                CurHigh = CurLow + BinWidth_dBSPL;
                LowRange = new XElement("LowValue", CurLow);
                HighRange = new XElement("HighValue", CurHigh);
                LevelRanges.Add(new XElement("LevelRange", LowRange, HighRange));
                CurLow = CurHigh;
            }
            LowRange = new XElement("LowValue", CurLow);
            HighRange = new XElement("HighValue", 230);
            LevelRanges.Add(new XElement("LevelRange", LowRange, HighRange));
            RootElement.Add(LevelRanges);
        }

        public void AddExposureBins(XElement SourceElement)
        {
            XElement ExposureBins = new XElement("ExposureBins");
            foreach (int bin in Bins)
                ExposureBins.Add(new XElement("ExposureBin", bin));
            SourceElement.Add(ExposureBins);
        }
    }
}
