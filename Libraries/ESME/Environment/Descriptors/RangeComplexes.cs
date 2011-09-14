using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using System.Windows.Threading;
using Cinch;
using HRC;
using HRC.Navigation;
using HRC.Utility;
using HRC.Collections;

namespace ESME.Environment.Descriptors
{
    public class RangeComplexes
    {
        RangeComplexes()
        {
            _dispatcher = Dispatcher.CurrentDispatcher;
            RangeComplexCollection = new ObservableConcurrentDictionary<string, NewRangeComplex>();
        }

        #region Private fields
        readonly object _lockObject = new object();
        readonly Dispatcher _dispatcher;
        #endregion

        [NotNull] public ObservableConcurrentDictionary<string, NewRangeComplex> RangeComplexCollection { get; private set; }

        #region public string SimAreaCSVFile { get; set; }

        public string SimAreaCSVFile
        {
            get { return _simAreaCSVFile; }
            set
            {
                if (_simAreaCSVFile == value) return;
                _simAreaCSVFile = value;
                RangeComplexCollection.Clear();
                if (_simAreaCSVFile != null) InitializeAsync(_simAreaCSVFile);
            }
        }

        string _simAreaCSVFile;

        #endregion

        Task InitializeAsync(string simAreaFile)
        {
            var ranges = new List<NewRangeComplex>();
            if (!File.Exists(simAreaFile)) throw new FileNotFoundException("Error reading sim area file", simAreaFile);
            SimAreaPath = Path.GetDirectoryName(simAreaFile);
            var actionBlock = new ActionBlock<Tuple<string, double, double, double, double, string, string>>(
		        async info =>
		        {
                    var rangeComplex = await NewRangeComplex.ReadAsync(SimAreaPath, info, simArea => _dispatcher.InvokeInBackgroundIfRequired(() => RangeComplexCollection.Add(simArea.Name, simArea)), _dispatcher);     
		        },
		        new ExecutionDataflowBlockOptions
		        {
		            TaskScheduler = TaskScheduler.Default,
		            MaxDegreeOfParallelism = 4,
		        });
            var lines = File.ReadAllLines(simAreaFile);
            foreach (var line in lines)
            {
                if (line == null) throw new ApplicationException("line is null");
                var curLine = line.Trim();
                if ((curLine.Trim() == "") || curLine.StartsWith("!") || curLine.StartsWith("#")) continue;
                var fields = curLine.Split(',');
                var rangeComplexName = fields[0].Trim();
                var latString = fields[1].Trim();
                var lonString = fields[2].Trim();
                var heightString = fields[3].Trim();
                var geoidString = fields[4].Trim();
                var opsLimitFile = fields[5].Trim();
                var simLimitFile = fields[6].Trim();
                double latitude;
                double longitude;
                double height;
                double geoid;
                if (string.IsNullOrEmpty(rangeComplexName)) throw new FormatException(string.Format("RangeComplexes: Error reading sim area file \"{0}\"\nError: Invalid sim area name", simAreaFile));
                if (string.IsNullOrEmpty(opsLimitFile)) throw new FormatException(string.Format("RangeComplexes: Error reading sim area file \"{0}\"\nError: Invalid OpsLimit filename", simAreaFile));
                if (string.IsNullOrEmpty(simLimitFile)) throw new FormatException(string.Format("RangeComplexes: Error reading sim area file \"{0}\"\nError: Invalid SimLimit filename", simAreaFile));
                if (!double.TryParse(latString, out latitude)) throw new FormatException(string.Format("RangeComplexes: Error reading sim area file \"{0}\"\nError: Invalid latitude", simAreaFile));
                if (!double.TryParse(lonString, out longitude)) throw new FormatException(string.Format("RangeComplexes: Error reading sim area file \"{0}\"\nError: Invalid longitude", simAreaFile));
                if (!double.TryParse(heightString, out height)) throw new FormatException(string.Format("RangeComplexes: Error reading sim area file \"{0}\"\nError: Invalid height", simAreaFile));
                if (!double.TryParse(geoidString, out geoid)) throw new FormatException(string.Format("RangeComplexes: Error reading sim area file \"{0}\"\nError: Invalid geoid separation value", simAreaFile));
                actionBlock.Post(Tuple.Create(rangeComplexName, height, latitude, longitude, geoid, opsLimitFile, simLimitFile));
            }
            actionBlock.Complete();
            return actionBlock.Completion;
        }

        public string SimAreaPath { get; private set; }
        static RangeComplexes _instance;
        public static RangeComplexes Singleton { get { return _instance ?? (_instance = new RangeComplexes()); } }

        public NewRangeComplex this[string rangeComplexName]
        {
            get { return RangeComplexCollection[rangeComplexName]; }
        }

        public async Task<NewRangeComplex> CreateAsync(string rangeComplexName, double height, double latitude, double longitude, double geoid, ICollection<Geo> opAreaLimits, List<Geo> simAreaLimits)
        {
            if (opAreaLimits == null) throw new ArgumentNullException("opAreaLimits");
            if (simAreaLimits == null) throw new ArgumentNullException("simAreaLimits");
            if (opAreaLimits.Count < 4) throw new ArgumentException("Must have at least four coordinates", "opAreaLimits");
            if (simAreaLimits.Count < 4) throw new ArgumentException("Must have at least four coordinates", "simAreaLimits");

            var rangeComplexPath = Path.Combine(SimAreaPath, rangeComplexName);

            if (Directory.Exists(rangeComplexPath) || (RangeComplexCollection.Where(complex => complex.Value.Name == rangeComplexName).Count() != 0)) throw new ApplicationException(string.Format("Range complex {0} already exists", rangeComplexName));

            var result = await NewRangeComplex.CreateAsync(SimAreaPath, rangeComplexName, opAreaLimits, simAreaLimits, _dispatcher);

            lock (_lockObject)
            {
                var needsExtraNewline = !File.ReadAllText(SimAreaCSVFile).EndsWith("\n");
                using (var writer = new StreamWriter(SimAreaCSVFile, true))
                {
                    if (needsExtraNewline) writer.WriteLine();
                    writer.WriteLine("{0},{1:0.0###},{2:0.0###},{3:0.0###},{4:0.0###},{5},{6}", rangeComplexName.Trim(),
                                     latitude, longitude, height, geoid,
                                     Path.GetFileName(result.OpArea.Name).Trim(), Path.GetFileName(result.SimArea.Name).Trim());
                }
            }
            RangeComplexCollection.Add(result.Name, result);
            return result;
        }

		//public async Task<RangeComplex> AddAsync(string rangeComplexName, List<Geo> opAreaLimits, List<Geo> simAreaLimits){}
    }
}
