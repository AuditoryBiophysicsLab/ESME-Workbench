using System;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Windows;
using System.Xml;
using HRC;
using HRC.Plotting;
using HRC.ViewModels;

namespace ESME.SimulationAnalysis
{
    public class HistogramBins : ViewModelBase
    {
        public BarSeriesViewModel BarSeriesViewModel { get; set; }
        /// <summary>
        /// Lowest tracked exposure level, in dB re: 1 uPa
        /// </summary>
        public double Low { get; private set; }

        /// <summary>
        /// Exposure bin width, in dB
        /// </summary>
        public double Width { get; private set; }

        /// <summary>
        /// The actual array of recieved level bins
        /// </summary>
        public int[] Bins { get; private set; }

        public string[] BinNames { get; set; }

        public string DataSetName { get; set; }
        readonly ObservableCollection<Point> _points;
        [UsedImplicitly] PropertyObserver<HistogramBins> _propertyObserver;
        public HistogramBins(double low=100, double width=10, int count=10)
        {
            Low = low;
            Width = width;
            Bins = new int[count + 2];
            BinNames = new string[count + 2];
            _points = new ObservableCollection<Point>();
            for (var i = 0; i < Bins.Length; i++ )
            {
                _points.Add(new Point(i, 0));
                if (i == 0) BinNames[i] = string.Format("<{0:0.##}", Low);
                else if (i == Bins.Length - 1) BinNames[i] = string.Format(">{0:0.##}", Low + (Width * count));
                else BinNames[i] = string.Format("{0:0.##}", Low + (Width / 2) + (Width * i));
            }
            BarSeriesViewModel = new BarSeriesViewModel
            {
                SeriesData = _points,
                ItemToPoint = i => (Point)i,
                StrokeThickness = 1,
            };
            _propertyObserver = new PropertyObserver<HistogramBins>(this)
                .RegisterHandler(p => p.DataSetName, () => { BarSeriesViewModel.SeriesName = DataSetName; });
        }

        public void Add(double value)
        {
            int bin;
            if (value < Low) bin = 0;
            else bin = (int)Math.Min(((value - Low) / Width) + 1, Bins.Length - 1);
            Bins[bin]++;
            _points[bin] = new Point(bin, Bins[bin]);
        }

        public void Display()
        {
            Debug.Write(string.Format("< {0}, ", Low));
            for (var bin = 1; bin < Bins.Length - 1; bin++)
                Debug.Write(string.Format("{0}, ", Low + (Width / 2 + ((bin - 1) * Width))));
            Debug.WriteLine(string.Format("> {0}", Low + ((Bins.Length - 2) * Width)));
            Debug.WriteLine(string.Format("{0} Total: {1}", string.Join(", ", Bins), Bins.Sum()));
        }

        public string WriteBinWidths()
        {
            var sb = new StringBuilder();
            sb.Append(string.Format("{0}, ", Low));
            for (var bin = 1; bin < Bins.Length - 1; bin++)
                sb.Append(string.Format("{0}, ", Low + (Width / 2 + ((bin - 1) * Width))));
            sb.Append(string.Format("{0}", Low + ((Bins.Length - 2) * Width)));
            sb.AppendLine();
            return sb.ToString();
        }

        public void WriteBinWidthsXML(XmlWriter x)
        {
            x.WriteElementString("Bin", Low.ToString(CultureInfo.InvariantCulture));
            for (var i = 1; i < Bins.Length-1; i++)
            {
                var value = Low + (Width / 2 + ((i - 1) * Width));
                x.WriteElementString("Bin", value.ToString(CultureInfo.InvariantCulture));
            }
            x.WriteElementString("Bin", (Low + ((Bins.Length - 2) * Width)).ToString(CultureInfo.InvariantCulture));
        }

        public string WriteBinTotals()
        {
            var sb = new StringBuilder();
            sb.Append(string.Format("{0}", string.Join(", ",Bins)));
            sb.AppendLine();
            return sb.ToString();
        }

        public XmlWriter WriteBins(XmlWriter x)
        {
            x.WriteStartElement("Exposure");
            foreach (var bin in Bins)
            {
                x.WriteElementString("Bin",bin.ToString(CultureInfo.InvariantCulture));
            }
            x.WriteEndElement();
            return x;
        }

#if false
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
#endif
    }
}