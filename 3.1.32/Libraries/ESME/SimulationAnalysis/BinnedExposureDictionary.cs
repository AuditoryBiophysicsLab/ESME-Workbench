using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Windows.Data;
using System.Windows.Media;
using System.Xml;
using ESME.Simulator;
using HRC.Aspects;
using HRC.Collections;
using HRC.Plotting;
using HRC.ViewModels;

namespace ESME.SimulationAnalysis
{
    public class BinnedExposureDictionary
    {
        public ObservableConcurrentDictionary<int, ObservableConcurrentDictionary<int, ObservableCollection<HistogramBins>>> Exposures { get; private set; }
        public Func<ActorExposureRecord, int?> Filter1 { get; set; }
        public Func<ActorExposureRecord, int?> Filter2 { get; set; }

        public BinnedExposureDictionary(IHistogramSource histogramSource)
        {
            Exposures = new ObservableConcurrentDictionary<int, ObservableConcurrentDictionary<int, ObservableCollection<HistogramBins>>>();
            _histogramSource = histogramSource;
        }

        readonly IHistogramSource _histogramSource;
        public void Expose(ActorExposureRecord exposureRecord)
        {
            if (Filter1 == null) throw new ApplicationException("Filter1 cannot be null");
            if (Filter2 == null) throw new ApplicationException("Filter2 cannot be null");
            var key1 = Filter1(exposureRecord);
            if (!key1.HasValue) return;
            var key2 = Filter2(exposureRecord);
            if (!key2.HasValue) return;
            ObservableConcurrentDictionary<int, ObservableCollection<HistogramBins>> level2;
            if (!Exposures.TryGetValue(key1.Value, out level2))
            {
                level2 = new ObservableConcurrentDictionary<int, ObservableCollection<HistogramBins>>();
                if (!Exposures.TryAdd(key1.Value, level2)) if (!Exposures.TryGetValue(key1.Value, out level2)) throw new ApplicationException("Could not add level two dictionary.");
            }
            ObservableCollection<HistogramBins> bins;
            if (!level2.TryGetValue(key2.Value, out bins))
            {
                bins = new ObservableCollection<HistogramBins> { new HistogramBins(_histogramSource), new HistogramBins(_histogramSource) }; //peakSPL and Energy
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

    public class GroupedExposures : ViewModelBase, IGroupedExposures
    {
        protected readonly int GroupLevel;
        readonly ObservableConcurrentDictionary<int, IGroupedExposures> _groupedExposures = new ObservableConcurrentDictionary<int, IGroupedExposures>();
        protected IHistogramSource HistogramSource;

        public GroupedExposures(IHistogramSource histogramSource, double lowBinValue, double binWidth, int binCount)
        {
            HistogramSource = histogramSource;
            LowBinValue = lowBinValue;
            BinWidth = binWidth;
            BinCount = binCount;
            GroupLevel = 0;
        }
        protected GroupedExposures(IHistogramSource histogramSource, double lowBinValue, double binWidth, int binCount, int groupLevel) : this(histogramSource, lowBinValue, binWidth, binCount) { GroupLevel = groupLevel; }

        [Initialize] public List<ExposureGroupDescription> GroupDescriptions { get; set; }
            
        public string GroupName { get; private set; }
        public double LowBinValue { get; private set; }
        public double BinWidth { get; private set; }
        public int BinCount { get; private set; }
        public virtual void Expose(ActorExposureRecord exposureRecord)
        {
            if (GroupDescriptions == null || GroupDescriptions.Count < GroupLevel) throw new InvalidOperationException("There is no GroupDescription defined for this grouping level");
            var groupDescription = GroupDescriptions[GroupLevel];
            if (groupDescription.RecordToKey == null) throw new InvalidOperationException("RecordToKey cannot be null for the current grouping level");
            if (groupDescription.GroupName == null) throw new InvalidOperationException("GroupName cannot be null for the current grouping level");
            if (groupDescription.RecordFilter != null && (!groupDescription.RecordFilter(exposureRecord))) return;
            var key = groupDescription.RecordToKey(exposureRecord);
            IGroupedExposures value;
            if (!_groupedExposures.TryGetValue(key, out value))
            {
                if (GroupDescriptions.Count > (GroupLevel + 2))
                    value = new GroupedExposures(HistogramSource, LowBinValue, BinWidth, BinCount, GroupLevel + 1)
                    {
                        GroupName = groupDescription.GroupName(exposureRecord),
                        GroupDescriptions = GroupDescriptions
                    };
                else
                    value = new GroupedExposuresHistogram(HistogramSource, LowBinValue, BinWidth, BinCount, GroupLevel + 1)
                    {
                        GroupName = groupDescription.GroupName(exposureRecord),
                        GroupDescriptions = GroupDescriptions
                    };
                if (_groupedExposures.TryAdd(key, value))
                {
                    Debug.WriteLine(string.Format("Adding group {0} at level {1}", groupDescription.GroupName(exposureRecord), GroupLevel));
                    Groups.Add(value);
                }
                else if (!_groupedExposures.TryGetValue(key, out value)) throw new ApplicationException("Could not add new exposure group");
            }
            value.Expose(exposureRecord);
        }

        [Initialize] public virtual ObservableCollection<IGroupedExposures> Groups { get; protected set; }

        public virtual void DebugDisplay()
        {
            foreach (var curGroup in Groups)
            {
                Debug.WriteLine(string.Format("Group: {0}", curGroup.GroupName));
                curGroup.DebugDisplay();
            }
        }
    }

    public class ExposureGroupDescription
    {
        public Func<ActorExposureRecord, bool> RecordFilter { get; set; }
        public Func<ActorExposureRecord, int> RecordToKey { get; set; }
        public Func<ActorExposureRecord, Guid> RecordToGuid { get; set; }
        public Func<ActorExposureRecord, string> GroupName { get; set; }
        public bool UsePressureExposures { get; set; }
    }

    public class GroupedExposuresHistogram : GroupedExposures
    {
        readonly ObservableConcurrentDictionary<int, HistogramBins[]> _groupedExposures = new ObservableConcurrentDictionary<int, HistogramBins[]>();
        internal GroupedExposuresHistogram(IHistogramSource histogramSource, double lowBinValue, double binWidth, int binCount, int groupLevel)
            : base(histogramSource, lowBinValue, binWidth, binCount, groupLevel)
        {
            GroupedBarSeriesViewModels = new GroupedBarSeriesViewModel[2];
            GroupedBarSeriesViewModels[0] = new GroupedBarSeriesViewModel();
            GroupedBarSeriesViewModels[1] = new GroupedBarSeriesViewModel();
            var bins = new HistogramBins(HistogramSource, LowBinValue, BinWidth, BinCount);
            BinNames = new string[bins.BinNames.Length];
            Array.Copy(bins.BinNames, BinNames, bins.BinNames.Length);
            //_cvs = new CollectionViewSource();
            //var speciesPlatformConverter = new GroupingConverter(a => _simulationLog.RecordFromActorID(((ActorExposureRecord)a).ActorID) is SpeciesNameGuid ? "Species" : "Platforms");
            //var actorNameConverter = new GroupingConverter(a => _simulationLog.RecordFromActorID(((ActorExposureRecord)a).ActorID).Name);
            //_cvs.GroupDescriptions.Add(new PropertyGroupDescription(null, speciesPlatformConverter));
            //_cvs.GroupDescriptions.Add(new PropertyGroupDescription(null, actorNameConverter));
            //_cvs.Source = simulationLog;
        }

        public GroupedBarSeriesViewModel[] GroupedBarSeriesViewModels { get; private set; }
        public override void Expose(ActorExposureRecord exposureRecord)
        {
            if (GroupDescriptions == null || GroupDescriptions.Count < GroupLevel) throw new InvalidOperationException("There is no GroupDescription defined for this grouping level");
            var groupDescription = GroupDescriptions[GroupLevel];
            if (groupDescription.RecordToKey == null) throw new InvalidOperationException("RecordToKey cannot be null for the current grouping level");
            if (groupDescription.GroupName == null) throw new InvalidOperationException("GroupName cannot be null for the current grouping level");
            if (groupDescription.RecordFilter != null && (!groupDescription.RecordFilter(exposureRecord))) return;
            var key = groupDescription.RecordToKey(exposureRecord);
            HistogramBins[] bins;
            if (!_groupedExposures.TryGetValue(key, out bins))
            {
                bins = new HistogramBins[2];
                bins[0] = new HistogramBins(HistogramSource, LowBinValue, BinWidth, BinCount)
                {
                    DataSetName = groupDescription.GroupName(exposureRecord),
                    BarSeriesViewModel = { Fill = new SolidColorBrush(HistogramSource.GuidToColorMap[groupDescription.RecordToGuid(exposureRecord)]) },
                };
                GroupedBarSeriesViewModels[0].BarSeriesCollection.Add(bins[0].BarSeriesViewModel);
                bins[1] = new HistogramBins(HistogramSource, LowBinValue, BinWidth, BinCount)
                {
                    DataSetName = groupDescription.GroupName(exposureRecord),
                    BarSeriesViewModel = { Fill = new SolidColorBrush(HistogramSource.GuidToColorMap[groupDescription.RecordToGuid(exposureRecord)]) },
                };
                GroupedBarSeriesViewModels[1].BarSeriesCollection.Add(bins[1].BarSeriesViewModel);
                if (_groupedExposures.TryAdd(key, bins))
                {
                    Debug.WriteLine(string.Format("Adding histograms for {0} at level {1}", groupDescription.GroupName(exposureRecord), GroupLevel));
                }
                else if (!_groupedExposures.TryGetValue(key, out bins)) throw new ApplicationException("Could not add exposure bins to GroupedExposuresHistogram.");
            }
            bins[0].Add(exposureRecord.PeakSPL);
            bins[1].Add(exposureRecord.Energy);
        }

        public string[] BinNames { get; set; }
        public override void DebugDisplay()
        {
            foreach (var curGroup in _groupedExposures.Values)
            {
                Debug.WriteLine(string.Format("{0}", curGroup[0].DataSetName));
                curGroup[0].Display();
                Debug.WriteLine(string.Format("{0}", curGroup[1].DataSetName));
                curGroup[1].Display();
            }
        }
    }

    public interface IGroupedExposures
    {
        void Expose(ActorExposureRecord exposureRecord);
        ObservableCollection<IGroupedExposures> Groups { get; }
        List<ExposureGroupDescription> GroupDescriptions { get; }
        string GroupName { get; }
        void DebugDisplay();
    }

    [ValueConversion(typeof(object), typeof(double))]
    public class GroupingConverter : IValueConverter
    {
        public GroupingConverter(Func<object, object> groupingFunction) { GroupingFunction = groupingFunction; }
        public Func<object, object> GroupingFunction { get; private set; }
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture) { return GroupingFunction(value); }
        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture) { throw new NotImplementedException(); }
    }
}