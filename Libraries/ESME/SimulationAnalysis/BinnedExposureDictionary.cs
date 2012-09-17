using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Xml;
using ESME.Simulator;
using HRC.Collections;

namespace ESME.SimulationAnalysis
{
    public class BinnedExposureDictionary
    {
        public ObservableConcurrentDictionary<int, ObservableConcurrentDictionary<int, List<HistogramBins>>> Exposures { get; private set; }
        public Func<ActorExposureRecord, int?> Filter1 { get; set; }
        public Func<ActorExposureRecord, int?> Filter2 { get; set; }

        public BinnedExposureDictionary() { Exposures = new ObservableConcurrentDictionary<int, ObservableConcurrentDictionary<int, List<HistogramBins>>>(); }

        public void Expose(ActorExposureRecord exposureRecord)
        {
            if (Filter1 == null) throw new ApplicationException("Filter1 cannot be null");
            if (Filter2 == null) throw new ApplicationException("Filter2 cannot be null");
            var key1 = Filter1(exposureRecord);
            if (!key1.HasValue) return;
            var key2 = Filter2(exposureRecord);
            if (!key2.HasValue) return;
            ObservableConcurrentDictionary<int, List<HistogramBins>> level2;
            if (!Exposures.TryGetValue(key1.Value, out level2))
            {
                level2 = new ObservableConcurrentDictionary<int, List<HistogramBins>>();
                if (!Exposures.TryAdd(key1.Value, level2)) if (!Exposures.TryGetValue(key1.Value, out level2)) throw new ApplicationException("Could not add level two dictionary.");
            }
            List<HistogramBins> bins;
            if (!level2.TryGetValue(key2.Value, out bins))
            {
                bins = new List<HistogramBins> { new HistogramBins(), new HistogramBins() }; //peakSPL and Energy
                if (!level2.TryAdd(key2.Value, bins)) if (!level2.TryGetValue(key2.Value, out bins)) throw new ApplicationException("Could not add bins.");
            }
            bins[0].Add(exposureRecord.PeakSPL);
            bins[1].Add(exposureRecord.Energy);
        }

        public void Display(Func<int, string> key1NameFunc, Func<int, string> key2NameFunc)
        {
            foreach (var key1 in Exposures.Keys)
                foreach (var key2 in Exposures[key1].Keys)
                {
                    Debug.WriteLine(string.Format("{0} {1}", key1NameFunc(key1), key2NameFunc(key2)));
                    Debug.WriteLine("PeakSPL bins:");
                    Exposures[key1][key2][0].Display();
                    Debug.WriteLine("");
                }
        }
        public string Write(Func<int, string> key1NameFunc, Func<int, string> key2NameFunc)
        {
            var sb = new StringBuilder();
            foreach (var key1 in Exposures.Keys)
            {
                sb.AppendLine(string.Format("modes: {0}",Exposures[key1].Values.Count));
                foreach (var key2 in Exposures[key1].Keys)
                {
                    sb.AppendLine(string.Format("{0}", key1NameFunc(key1)));
                    sb.AppendLine(string.Format("{0}", key2NameFunc(key2)));
                    sb.AppendLine(Exposures[key1][key2][0].WriteBinWidths());
                    sb.AppendLine(Exposures[key1][key2][0].WriteBinTotals());
                    sb.AppendLine("");
                }
            }
            return sb.ToString();
        }

        public void WriteXML(XmlWriter x, Func<int, string> key1NameFunc, Func<int, string> key2NameFunc)
        {
            x.WriteStartElement("AnimatSpecies");
            foreach (var key1 in Exposures.Keys)
            {
                x.WriteStartElement("Species");
                x.WriteElementString("Name",key1NameFunc(key1));
                x.WriteStartElement("BinWidths");
                Exposures[key1][Exposures[key1].Keys.First()][0].WriteBinWidthsXML(x);
                x.WriteEndElement();
                x.WriteStartElement("Modes");
                foreach (var key2 in Exposures[key1].Keys)
                {
                    x.WriteStartElement("Mode");
                    x.WriteElementString("Name",key2NameFunc(key2));
                    foreach (var bins in Exposures[key1][key2])
                    {
                        bins.WriteBins(x);
                    }
                    x.WriteEndElement();
                }
                x.WriteEndElement();
                x.WriteEndElement();
            }
            x.WriteEndElement();
            
        }
    }
}