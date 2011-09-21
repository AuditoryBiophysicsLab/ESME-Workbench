using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Windows.Threading;
using Cinch;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.NEMO;
using HRC.Navigation;
using HRC.Utility;
using System.Threading.Tasks;

namespace ESME.TransmissionLoss.REFMS
{
    public class ExplosivePoint : EarthCoordinate, INotifyPropertyChanged
    {
        public ExplosivePoint() { }

        public ExplosivePoint(Geo location, NemoPlatform platform, NemoMode mode, float delta) : base(location)
        {
            ExplosionDepth = Math.Abs(platform.Trackdefs[0].InitialHeight) + mode.DepthOffset;
            Delta = delta;
        }

        #region public double ExplosionDepth { get; set; }

        public double ExplosionDepth
        {
            get { return _explosionDepth; }
            set
            {
                if (_explosionDepth == value) return;
                _explosionDepth = value;
                NotifyPropertyChanged(ExplosionDepthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExplosionDepthChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePoint>(x => x.ExplosionDepth);
        double _explosionDepth;

        #endregion

        #region public string SVPFileName { get; set; }

        public string SVPFileName
        {
            get { return _svpFileName; }
            set
            {
                if (_svpFileName == value) return;
                _svpFileName = value;
                NotifyPropertyChanged(SVPFileNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SVPFileNameChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePoint>(x => x.SVPFileName);
        string _svpFileName;

        #endregion

        #region public bool IsEnabled { get; set; }

        public bool IsEnabled
        {
            get { return _isEnabled; }
            set
            {
                if (_isEnabled == value) return;
                _isEnabled = value;
                NotifyPropertyChanged(IsEnabledChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsEnabledChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePoint>(x => x.IsEnabled);
        bool _isEnabled;

        #endregion

        #region public double Delta { get; set; }

        public double Delta
        {
            get { return _delta; }
            set
            {
                if (_delta == value) return;
                _delta = value;
                NotifyPropertyChanged(DeltaChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DeltaChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePoint>(x => x.Delta);
        double _delta;

        #endregion

        #region public int SourceCount { get; set; }

        public int SourceCount
        {
            get { return _sourceCount; }
            set
            {
                if (_sourceCount == value) return;
                _sourceCount = value;
                NotifyPropertyChanged(SourceCountChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SourceCountChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePoint>(x => x.SourceCount);
        int _sourceCount;

        #endregion

        #region public double DepthLimit { get; set; }

        public double DepthLimit
        {
            get { return _depthLimit; }
            set
            {
                if (_depthLimit == value) return;
                _depthLimit = value;
                NotifyPropertyChanged(DepthLimitChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DepthLimitChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePoint>(x => x.DepthLimit);
        double _depthLimit;

        #endregion

        #region public bool BottomReflectionsEnabled { get; set; }

        public bool BottomReflectionsEnabled
        {
            get { return _bottomReflectionsEnabled; }
            set
            {
                if (_bottomReflectionsEnabled == value) return;
                _bottomReflectionsEnabled = value;
                NotifyPropertyChanged(BottomReflectionsEnabledChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BottomReflectionsEnabledChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePoint>(x => x.BottomReflectionsEnabled);
        bool _bottomReflectionsEnabled;

        #endregion

        #region public bool BottomExponentialEnabled { get; set; }

        public bool BottomExponentialEnabled
        {
            get { return _bottomExponentialEnabled; }
            set
            {
                if (_bottomExponentialEnabled == value) return;
                _bottomExponentialEnabled = value;
                NotifyPropertyChanged(BottomExponentialEnabledChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BottomExponentialEnabledChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePoint>(x => x.BottomExponentialEnabled);
        bool _bottomExponentialEnabled;

        #endregion

        #region INotifyPropertyChanged Members

        public event PropertyChangedEventHandler PropertyChanged;
        protected void NotifyPropertyChanged(PropertyChangedEventArgs e)
        {
            var handlers = PropertyChanged;
            if (handlers == null) return;
            foreach (PropertyChangedEventHandler handler in handlers.GetInvocationList())
            {
                if (handler.Target is DispatcherObject)
                    ((DispatcherObject)handler.Target).Dispatcher.InvokeIfRequired(() => handler(this, e));
                else
                    handler(this, e);
            }
        }

        #endregion
    }

    public class SVPLayer
    {
        public double Thickness { get; set; }
        public double SoundVelocity { get; set; }
        public double Depth { get; set; }
        public double Salinity { get; set; }
        public double Temperature { get; set; }
        public override string ToString() { return string.Format("{0,13:0.0000} {1,13:0.0000} {2,9:0.00000}    0            D= {3,13:0.0000}", Thickness, SoundVelocity, Density, Depth); }
        public static SVPLayer Parse(string layerLine)
        {
            var fields = layerLine.Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
            return new SVPLayer
            {
                Thickness = double.Parse(fields[0]),
                SoundVelocity = double.Parse(fields[1]),
                //Density = double.Parse(fields[2]),
                Depth = double.Parse(fields[5]),
            };
        }

        /// <summary>
        /// Calculate the density of seawater, at a given depth, temperature and salinity
        /// </summary>
        public double Density
        {
            get
            {
                var depthKm = Depth / 1000.0;
                var c = 999.83 + (5.053 * depthKm) - (0.048 * (depthKm * depthKm));
                var beta = 0.808 - (0.0085 * depthKm);
                var alpha = 0.0708 * (1.0 + (0.351 * depthKm) + 0.068 * (1.0 - (0.0683 * depthKm)) * Temperature);
                var gamma = 0.0030 * (1.0 - (0.059 * depthKm) - 0.012 * (1.0 - (0.0640 * depthKm)) * Temperature);

                return (c + (beta * Salinity) - (alpha * Temperature) - (gamma * (35.0 - Salinity)) * Temperature) / 1000.0;
            }
        }
    }

    public class SVPFile : EarthCoordinate
    {
        public List<SVPLayer> Layers { get; private set; }
        public BottomLossData BottomLossData { get; private set; }
        public double Delta;

        double[] _depths, _temps, _salinities, _soundspeeds;

        void MakeLayers(double delta)
        {
            Delta = delta;
            var maxDepth = _depths.Last();
            // this creates the depth increments
            var nodes = (int)Math.Floor(maxDepth);
            var dinc = Math.Floor(maxDepth) / (nodes - 1);
            var depthIncs = new double[nodes];

            for (var m = 0; m < nodes; m++)
            {
                var tmp = Math.Round((m * dinc) * 1000000.0) / 1000000.0;
                depthIncs[m] = tmp;
            }
            // this implements the spline function
            var interp = new SplineInterpolator();
            var temperatureFunction = interp.Interpolate(_depths, _temps);
            var salinityFunction = interp.Interpolate(_depths, _salinities);
            var soundspeedFunction = interp.Interpolate(_depths, _soundspeeds);

            var td = new double[depthIncs.Length];
            var sd = new double[depthIncs.Length];
            var svpd = new double[depthIncs.Length];
            for (var inc = 0; inc < depthIncs.Length; inc++)
            {
                try
                {
                    td[inc] = temperatureFunction.Value(depthIncs[inc]);
                    sd[inc] = salinityFunction.Value(depthIncs[inc]);
                    svpd[inc] = soundspeedFunction.Value(depthIncs[inc]);
                }
                catch {}
            }
            // this is the implementation of the layers function

            nodes = depthIncs.Length;
            Layers = new List<SVPLayer>();
            int start = 0, prev = 0;
            for (var inc = 0; inc < nodes; inc++)
            {
                var depthmid = inc == 0 ? depthIncs[0] * 0.5 : (depthIncs[inc] + depthIncs[inc - 1]) * 0.5;

                var deltaSVP = Math.Abs(svpd[inc] - svpd[start]);

                if (deltaSVP >= Delta || (inc == (nodes - 1)))
                {
                    start = inc;
                    Layers.Add(new SVPLayer
                    {
                        Depth = depthIncs[inc],
                        Thickness = Math.Abs(depthIncs[prev] - depthIncs[start]),
                        SoundVelocity = soundspeedFunction.Value(depthmid),
                        Salinity = sd[inc],
                        Temperature = td[inc],
                    });
                    prev = inc;
                }
            }
        }

        public static async Task<SVPFile> Create(Geo location, float waterDepth, NAVOTimePeriod timePeriod, BackgroundTaskAggregator backgroundTaskAggregator)
        {
#if false
            var assemblyLocation = Assembly.GetCallingAssembly().Location;
            var extractionPath = Path.GetDirectoryName(assemblyLocation);
            if (extractionPath == null) throw new ApplicationException("Extraction path can't be null!");
            var gdemExtractionProgramPath = Path.Combine(extractionPath, "ImportGDEM.exe");
            var gdemRequiredSupportFiles = new List<string>
            {
                Path.Combine(extractionPath, "netcdf.dll"),
                Path.Combine(extractionPath, "NetCDF_Wrapper.dll")
            };

            var requiredMonths = Globals.AppSettings.NAVOConfiguration.MonthsInTimePeriod(timePeriod);
            var uniqueMonths = requiredMonths.Distinct().ToList();
            uniqueMonths.Sort();

            var lat = Math.Round(location.Latitude * 4) / 4;
            var lon = Math.Round(location.Longitude * 4) / 4;
            var extractionArea = new GeoRect(lat, lat, lon, lon);
            var tempPath = Path.GetTempPath().Remove(Path.GetTempPath().Length - 1);
            if (!Directory.Exists(tempPath)) Directory.CreateDirectory(tempPath);
            var maxDepth = new EarthCoordinate<float>(location.Latitude, location.Longitude, waterDepth);
            var monthlyTemperature = new SoundSpeed();
            var monthlySalinity = new SoundSpeed();
            var monthlyExtendedSoundSpeed = new SoundSpeed();
            //var bottomLossData = await BottomLossBackgroundExtractor.ExtractAsync(false, true, true, (float)lat, (float)lat, (float)lon, (float)lon);
            var bottomLossExtractor = new BottomLossBackgroundExtractor
            {
                WorkerSupportsCancellation = false,
                ExtractionArea = extractionArea,
                NAVOConfiguration = Globals.AppSettings.NAVOConfiguration,
                UseExpandedExtractionArea = false,
                TaskName = "Bottom loss extraction",
                PointExtractionMode = true,
            };
            backgroundTaskAggregator.BackgroundTasks.Add(bottomLossExtractor);
            var soundSpeedExtractors = new List<GDEMBackgroundExtractor>();
            foreach (var month in uniqueMonths)
            {
                var soundSpeedExtractor = new GDEMBackgroundExtractor
                {
                    WorkerSupportsCancellation = false,
                    TimePeriod = month,
                    ExtractionArea = extractionArea,
                    NAVOConfiguration = Globals.AppSettings.NAVOConfiguration,
                    DestinationPath = tempPath,
                    UseExpandedExtractionArea = false,
                    ExtractionProgramPath = gdemExtractionProgramPath,
                    MaxDepth = maxDepth,
                    PointExtractionMode = true,
                };
                soundSpeedExtractor.RunWorkerCompleted += (sender, e) =>
                {
                    var extractor = (GDEMBackgroundExtractor)sender;
                    monthlyTemperature.SoundSpeedFields.Add(extractor.TemperatureField);
                    monthlySalinity.SoundSpeedFields.Add(extractor.SalinityField);
                    monthlyExtendedSoundSpeed.SoundSpeedFields.Add(extractor.ExtendedSoundSpeedField);
                    if (soundSpeedExtractors.Any(ssfExtractor => ssfExtractor.IsBusy)) return;
                    if (bottomLossExtractor.IsBusy) return;
                    SoundSpeedField averageTemperature;
                    SoundSpeedField averageSalinity;
                    SoundSpeedField averageSoundspeed;
                    if (uniqueMonths.Count <= 1)
                    {
                        averageTemperature = monthlyTemperature[timePeriod];
                        averageSalinity = monthlySalinity[timePeriod];
                        averageSoundspeed = monthlyExtendedSoundSpeed[timePeriod];
                    }
                    else
                    {
                        averageTemperature = SoundSpeed.Average(monthlyTemperature, timePeriod);
                        averageSalinity = SoundSpeed.Average(monthlySalinity, timePeriod);
                        averageSoundspeed = SoundSpeed.Average(monthlyExtendedSoundSpeed, timePeriod);
                    }
                    // Here is where we create the SVP using the PCHIP algorithm, etc. from the average temp/salinity
                    var result = new SVPFile
                    {
                        //BottomLossData = bottomLossExtractor.BottomLossData[0],
                        _depths = (from temp in averageTemperature.EnvironmentData[0].Data select temp.Depth).Cast<double>().ToArray(),
                        _temps = (from temp in averageTemperature.EnvironmentData[0].Data select temp.Depth).Cast<double>().ToArray(),
                        _salinities = (from temp in averageSalinity.EnvironmentData[0].Data select temp.Depth).Cast<double>().ToArray(),
                        _soundspeeds = (from temp in averageSoundspeed.EnvironmentData[0].Data select temp.Depth).Cast<double>().ToArray(),
                    };
                };
                soundSpeedExtractors.Add(soundSpeedExtractor);
                backgroundTaskAggregator.BackgroundTasks.Add(soundSpeedExtractor);
            }
#endif
            return null;

        }

        public void Write(string fileName)
        {
            using (var writer = new StreamWriter(fileName, false))
            {
                writer.WriteLine("Layers: {0}", Layers.Count);
                foreach (var layer in Layers) writer.WriteLine(layer);
                writer.WriteLine();
                writer.WriteLine();
                writer.WriteLine(" DELTA {0,9:0.0000}", Delta);
                writer.WriteLine("LOCATION: {0,10:##0.000000} {1,10:##0.000000}", Latitude, Longitude);
            }
        }

        public static SVPFile Read(string fileName)
        {
            var result = new SVPFile();
            using (var reader = new StreamReader(fileName))
            {
                var fields = reader.ReadLine().Split(new[] {' '}, StringSplitOptions.RemoveEmptyEntries);
                var fieldCount = int.Parse(fields[1]);
                result.Layers = new List<SVPLayer>();
                for (var i = 0; i < fieldCount; i++)
                    result.Layers.Add(SVPLayer.Parse(reader.ReadLine()));
                var curLine = reader.ReadLine().Trim();
                while (string.IsNullOrEmpty(curLine)) curLine = reader.ReadLine().Trim();
                fields = curLine.Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
                result.Delta = double.Parse(fields[1]);
                fields = reader.ReadLine().Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
                result.Latitude = double.Parse(fields[1]);
                result.Longitude = double.Parse(fields[2]);
            }
            return result;
        }
    }
}
