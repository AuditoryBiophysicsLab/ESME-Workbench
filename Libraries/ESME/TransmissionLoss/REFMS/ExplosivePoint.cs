using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Threading;
using System.Xml.Serialization;
using Cinch;
using ESME.Environment;
using ESME.Environment.Descriptors;
using ESME.Environment.NAVO;
using ESME.Model;
using ESME.NEMO;
using HRC.Collections;
using HRC.Navigation;

namespace ESME.TransmissionLoss.REFMS
{
    public class ExplosivePoint : EarthCoordinate, ISupportValidation
    {
        public ExplosivePoint() { }

        public ExplosivePoint(string outputPath, Geo location, NemoPlatform platform, NemoMode mode, NAVOTimePeriod timePeriod, float delta = 1) : base(location)
        {
            TimePeriod = timePeriod;
            OutputPath = outputPath;
            Directory.CreateDirectory(OutputPath);
            ExplosionDepth = Math.Abs(platform.Trackdefs[0].InitialHeight) + mode.DepthOffset;
            Delta = delta;
            SoundSources = new List<SoundSource> {new SoundSource(location, mode, 1)};
            PSMId = mode.PSMId;
            PSMName = mode.PSMName;
            SourceLevel = mode.SourceLevel;
            ClusterCount = mode.ClusterCount;
            Duration = mode.Duration;
            LowFrequency = mode.LowFrequency;
            HighFrequency = mode.HighFrequency;
            ModeName = mode.Name;
        }

        #region public string OutputPath { get; set; }
        [XmlIgnore]
        public string OutputPath
        {
            get { return _outputPath; }
            set
            {
                if (_outputPath == value) return;
                _outputPath = value;
                NotifyPropertyChanged(OutputPathChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs OutputPathChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePoint>(x => x.OutputPath);
        string _outputPath;

        #endregion

        #region public Geo OldLocation { get; set; }

        public Geo OldLocation
        {
            get { return _oldLocation; }
            set
            {
                if (_oldLocation == value) return;
                _oldLocation = value;
                NotifyPropertyChanged(OldLocationChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs OldLocationChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePoint>(x => x.OldLocation);
        Geo _oldLocation;

        #endregion

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

        #region public SVPFile SVPFile { get; set; }
        [XmlIgnore]
        public SVPFile SVPFile
        {
            get { return _svpFile; }
            set
            {
                if (_svpFile == value) return;
                _svpFile = value;
                NotifyPropertyChanged(SVPFileChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SVPFileChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePoint>(x => x.SVPFile);
        SVPFile _svpFile;

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
        double _delta = 1;

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

        #region public bool DepthLimitEnabled { get; set; }

        public bool DepthLimitEnabled
        {
            get { return _depthLimitEnabled; }
            set
            {
                if (_depthLimitEnabled == value) return;
                _depthLimitEnabled = value;
                NotifyPropertyChanged(DepthLimitEnabledChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DepthLimitEnabledChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePoint>(x => x.DepthLimitEnabled);
        bool _depthLimitEnabled;

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

        #region public NAVOTimePeriod TimePeriod { get; set; }

        public NAVOTimePeriod TimePeriod
        {
            get { return _timePeriod; }
            set
            {
                if (_timePeriod == value) return;
                _timePeriod = value;
                NotifyPropertyChanged(TimePeriodChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TimePeriodChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePoint>(x => x.TimePeriod);
        NAVOTimePeriod _timePeriod;

        #endregion

        #region public ObservableConcurrentDictionary<EnvironmentDataType, Task> EnvironmentData { get; set; }
        [XmlIgnore]
        public ObservableConcurrentDictionary<EnvironmentDataType, Task> EnvironmentData
        {
            get { return _environmentData; }
            set
            {
                if (_environmentData == value) return;
                _environmentData = value;

                var temperatureData = ((Task<SoundSpeed>)EnvironmentData[EnvironmentDataType.Temperature]).Result[TimePeriod].EnvironmentData[this];
                var salinityData = ((Task<SoundSpeed>)EnvironmentData[EnvironmentDataType.Salinity]).Result[TimePeriod].EnvironmentData[this];
                var soundSpeedData = ((Task<SoundSpeed>)EnvironmentData[EnvironmentDataType.SoundSpeed]).Result[TimePeriod].EnvironmentData[this];
                SVPLocation = new Geo(soundSpeedData);
                BottomLossData = ((Task<BottomLoss>)EnvironmentData[EnvironmentDataType.BottomLoss]).Result.Samples[this].Data;
                WaterDepth = ((Task<Bathymetry>)EnvironmentData[EnvironmentDataType.Bathymetry]).Result.Samples[this].Data;
                GeoRect = ((Task<Bathymetry>)EnvironmentData[EnvironmentDataType.Bathymetry]).Result.Samples.GeoRect;

                TemperatureData = new double[temperatureData.Data.Count];
                DepthData = new double[temperatureData.Data.Count];
                SalinityData = new double[temperatureData.Data.Count];
                SoundSpeedData = new double[temperatureData.Data.Count];
                for (var i = 0; i < temperatureData.Data.Count; i++)
                {
                    TemperatureData[i] = temperatureData.Data[i].Value;
                    DepthData[i] = temperatureData.Data[i].Depth;
                    SalinityData[i] = salinityData.Data[i].Value;
                    SoundSpeedData[i] = soundSpeedData.Data[i].Value;
                }

                _svpFile = SVPFile.Create(SVPLocation, DepthData, TemperatureData, SalinityData, SoundSpeedData, BottomLossData, Delta);

                NotifyPropertyChanged(EnvironmentDataChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs EnvironmentDataChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePoint>(x => x.EnvironmentData);
        ObservableConcurrentDictionary<EnvironmentDataType, Task> _environmentData;

        #endregion

        #region public bool IsValid { get; set; }

        [XmlIgnore]
        public bool IsValid
        {
            get
            {
                Validate();
                return _isValid;
            }
            private set
            {
                if (_isValid == value) return;
                _isValid = value;
                NotifyPropertyChanged(IsValidChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsValidChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePoint>(x => x.IsValid);
        bool _isValid;

        #endregion

        #region public string ValidationErrorText { get; set; }
        [XmlIgnore]
        public string ValidationErrorText
        {
            get
            {
                Validate();
                return _validationErrorText;
            }
            private set
            {
                if (_validationErrorText == value) return;
                _validationErrorText = value;
                IsValid = string.IsNullOrEmpty(_validationErrorText);
                NotifyPropertyChanged(ValidationErrorTextChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ValidationErrorTextChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePoint>(x => x.ValidationErrorText);
        string _validationErrorText;

        #endregion

        public double[] TemperatureData { get; set; }
        public double[] SalinityData { get; set; }
        public double[] SoundSpeedData { get; set; }
        public double[] DepthData { get; set; }
        public double WaterDepth { get; set; }
        public BottomLossData BottomLossData { get; set; }
        public Geo SVPLocation { get; set; }

        public string PSMId { get; set; }
        public string PSMName { get; set; }
        public float SourceLevel { get; set; }
        public int ClusterCount { get; set; }
        public TimeSpan Duration { get; set; }
        public double LowFrequency { get; set; }
        public double HighFrequency { get; set; }
        public string ModeName { get; set; }

        [XmlIgnore]
        public GeoRect GeoRect { get; set; }

        public List<SoundSource> SoundSources { get; set; }

        public void Validate()
        {
            if (!GeoRect.Contains(this))
            {
                ValidationErrorText = "Explosive point not contained within bathymetry bounds";
                return;
            }

            var sourcesInError = new List<string>();

            foreach (var source in SoundSources)
            {
                source.Validate();
                if (!source.IsValid) sourcesInError.Add(source.Name); 
            }
            switch (sourcesInError.Count)
            {
                case 0:
                    ValidationErrorText = null;
                    break;
                case 1:
                    ValidationErrorText = string.Format("Source {0} has errors", sourcesInError[0]);
                    break;
                default:
                    var result = new StringBuilder();
                    result.AppendLine("The following sources have errors:");
                    foreach (var sourceName in sourcesInError) result.AppendLine(string.Format("  • {0}", sourceName));
                    ValidationErrorText = result.ToString();
                    break;
            }
        }

        public void Write()
        {
            WriteInputFile();
            WriteBatchFile();
            WriteSVPFile();
        }

        void WriteSVPFile()
        {
            _svpFile.Write(Path.Combine(OutputPath, SVPFilename + ".svp"));
        }

        void WriteInputFile()
        {
            var fileName = Path.Combine(OutputPath, BaseFilename + "refms.in");
            using (var writer = new StreamWriter(fileName, false))
            {
                writer.WriteLine("");
                writer.WriteLine("COMMENT");
                writer.WriteLine("For: {0}[{1}] Build {2} ({3})", SVPFilename, PSMName, BuildInformation.SVNVersion, BuildInformation.BuildDateTime);
                writer.WriteLine("");
                writer.WriteLine("UNITS       1 - Metric Units");
                writer.WriteLine("EXPLOSIVE   1    	Explosive composition (1=TNT)");
                writer.WriteLine("NemoMode.SourceLevel       {0:0.000} 	kg", SourceLevel); // From Mode/sourceLevel
                writer.WriteLine("CLUSTER     {0}   	Charges", ClusterCount); // From Mode/clusterCount
                writer.WriteLine("DEXPLOSION  {0:0.00} 	Depth of explosion in meters", ExplosionDepth); // Platform/trackDef/initialHeight plus Mode/depthOffset
                writer.WriteLine("");
                //writer.WriteLine("RADIUS      0.   	Skip ship response");   
                writer.WriteLine("DURATION    {0:0.0} 	Duration in secs after bottom reflection", Duration.Seconds); // Mode/duration
                //writer.WriteLine("IMULT       0    	Second Order Reflection");
                writer.WriteLine("IRB1        {0}   	Compute bottom reflections", BottomReflectionsEnabled ? "1." : "0.");
                // if water depth > 2000m, uncheck the "Include exponential at bottom" checkbox.  Otherwise check it, but allow the user to override if they wish.
                writer.WriteLine("IRB2        1.   	Compute rays surf to gage"); // Always 1
                writer.WriteLine("IRSC        0    	Compute rays in sound channel"); // Always 0
                writer.WriteLine("IC          1.   	Disables cavitation."); // Always 1
                writer.WriteLine("");
                writer.WriteLine("FILTER     -2    	 "); // Always -2
                writer.WriteLine("");
                writer.WriteLine("DWATER      {0:0.0} 	Depth of water in meters", WaterDepth - 0.1);               // Bathymetry at analysis point - 0.1
                writer.WriteLine("");
                writer.WriteLine("BSSRATIO    {0:0.000} 	Soil Type: Sound Speed Ratio", BottomLossData.RATIOD);  // From LFBL
                writer.WriteLine("BRHO        {0:0.000} 	Soil Type: Density", BottomLossData.RHOSD);             // From LFBL
                writer.WriteLine("BSWSPEED    {0:0.000} 	Soil Type: Shear Wave Velocity", ShearWaveVelocity);    // Computed by property getter
                writer.WriteLine("BMAT        1.0 "); // Always 1.0
                writer.WriteLine("SCALEI      no   "); // Always no
                writer.WriteLine("SVPFILE     loc.svp   "); // Always loc.svp
                writer.WriteLine("");
                writer.WriteLine("SE          1   make 3rd-octave energy spectra"); // Always 1
                var splCutoff = SourceCount == 1 ? Globals.AppSettings.REFMSSettings.SPLCutoffSingleSource : Globals.AppSettings.REFMSSettings.SPLCutoffMultiSource;
                writer.WriteLine("SPLCUTOFF   {0:0.000}  	 from {1} sources", splCutoff, SourceCount); // From scenario - single or multi based on num sources (used below)
                writer.WriteLine("SELCUTOFF   {0}   ", Globals.AppSettings.REFMSSettings.SELCutoffLine); // From REFMS Settings, just copy the string
                writer.WriteLine("SOURCES     {0}   ", SourceCount); // Num sources in scenario
                writer.WriteLine("");
                writer.WriteLine("DEPTHE");
                writer.WriteLine(" {0:0.00} {1:0.00} {2} ", Globals.AppSettings.REFMSSettings.MinimumDepth, WaterDepth, 20); // REFMS min depth config, depth at analysis point, 20
                writer.WriteLine("LOOPE");
                writer.WriteLine(" {0:0.00} {1:0.00} {2} ", LowFrequency, HighFrequency * 0.2, 20); // Mode/Low Freq, Mode/Hi Freq * 0.2, 20
                writer.WriteLine("LOOPL");
                writer.WriteLine(" {0:0.00} {1:0.00} {2} ", HighFrequency * 0.25, HighFrequency, 15); // Mode/High Freq * 0.25, Mode/Hi freq, 15
                writer.WriteLine("STOP");
            }
        }

        void WriteBatchFile()
        {
            var scriptBase = Path.Combine(OutputPath, string.Format("{0}", BaseFilename));
            using (var writer = new StreamWriter(scriptBase + "-refms.bat", false))
            {
                writer.WriteLine("");
                writer.WriteLine("mkdir \"{0}\"", BaseFilename);
                writer.WriteLine("COPY /Y \"{0}refms.in\" \"{0}\\refms.in\" ", BaseFilename);
                writer.WriteLine("COPY /Y \"{0}.svp\" \"{1}\\loc.svp\" ", SVPFilename, BaseFilename);
                writer.WriteLine("rem effects header - ");
                writer.WriteLine("cd \"{0}\"", BaseFilename);
                writer.WriteLine("echo #head=> ref_effect.head");
                writer.WriteLine("echo #tstamp=%DATE% %TIME%>> ref_effect.head");
                writer.WriteLine("echo #sysver=%COMPUTERNAME%^|{0}^|{1}^|{2}^|{3}^|null>> ref_effect.head", System.Environment.UserName, System.Environment.OSVersion.VersionString,
                                 System.Environment.OSVersion.ServicePack, System.Environment.GetEnvironmentVariable("PROCESSOR_ARCHITECTURE"));
                writer.WriteLine("echo #title=Explosives Test, EC_SimArea>> ref_effect.head");
                writer.WriteLine("echo #mode={0}>> ref_effect.head", ModeName);
                writer.WriteLine("echo #bin=E12>> ref_effect.head"); // Where does this come from?
                writer.WriteLine("echo #season={0}>> ref_effect.head", TimePeriod);
                writer.WriteLine("echo #info={0:0.0000}, {1:0.0000}, {2:0.0000}, {3:0.0}, {4:0.0}, {5:0.0}>> ref_effect.head", ExplosionDepth, SourceLevel, Duration.Seconds,
                                 BottomLossData.RATIOD, -20, -15); // where do -20 and -15 come from?
                writer.WriteLine("echo #location={0:0.000000} {1:0.000000}>> ref_effect.head", Latitude, Longitude);
                writer.WriteLine("echo #splineloc={0:0.000000} {1:0.000000}>> ref_effect.head", SVPLocation.Latitude, SVPLocation.Longitude);
                writer.WriteLine("echo #units=meters>> ref_effect.head");
                writer.WriteLine("start \"REFMS\" /wait  \"{0}\"", Globals.AppSettings.REFMSSettings.REFMSExecutablePath);
                writer.WriteLine("COPY /Y ref_effect.head + refms.out refms.effects");
                writer.WriteLine("echo # SPEC:{0}refms.spec >> refms.effects");
                writer.WriteLine("COPY /Y refms.effects + refms.spec refms.effects");
                var specFilename = string.Format("\"..\\{0}refms.spec\"", BaseFilename);
                writer.WriteLine("echo #location={0:0.000000} {1:0.000000}> {2}", Latitude, Longitude, specFilename);
                writer.WriteLine("COPY /Y {0} + refms.spec {0}", specFilename);
                writer.WriteLine("cd ..");
            }
            using (var writer = new StreamWriter(scriptBase + "-refms.sh", false))
            {
                writer.WriteLine("#!/bin/sh");
                writer.WriteLine("mkdir \"{0}\"", BaseFilename);
                writer.WriteLine("cp \"{0}refms.in\" \"{0}\\refms.in\" ", BaseFilename);
                writer.WriteLine("cp \"{0}\" \"{1}\\loc.svp\" ", SVPFilename, BaseFilename);
                writer.WriteLine("# effects header - ");
                writer.WriteLine("cd \"{0}\"", BaseFilename);
                writer.WriteLine("echo -n #head\\n> ref_effect.head");
                writer.WriteLine("echo -n #tstamp=$(date +\"%F %T\")\\n>> ref_effect.head");
                writer.WriteLine("echo -n #sysver=$(hostname)\\|{0}\\|{1}\\|{2}\\|{3}\\|null\\n>> ref_effect.head", System.Environment.UserName, System.Environment.OSVersion.VersionString,
                                 System.Environment.OSVersion.ServicePack, System.Environment.GetEnvironmentVariable("PROCESSOR_ARCHITECTURE"));
                writer.WriteLine("echo -n #title=Explosives Test, EC_SimArea\\n>> ref_effect.head");
                writer.WriteLine("echo -n #mode={0}\\n>> ref_effect.head", ModeName);
                writer.WriteLine("echo -n #bin=E12\\n>> ref_effect.head"); // Where does this come from?
                writer.WriteLine("echo -n #season={0}\\n>> ref_effect.head", TimePeriod);
                writer.WriteLine("echo -n #info={0:0.0000}, {1:0.0000}, {2:0.0000}, {3:0.0}, {4:0.0}, {5:0.0}\\n>> ref_effect.head", ExplosionDepth, SourceLevel, Duration.Seconds,
                                 BottomLossData.RATIOD, -20, -15); // where do -20 and -15 come from?
                writer.WriteLine("echo -n #location={0:0.000000} {1:0.000000}\\n>> ref_effect.head", Latitude, Longitude);
                writer.WriteLine("echo -n #splineloc={0:0.000000} {1:0.000000}\\n>> ref_effect.head", SVPLocation.Latitude, SVPLocation.Longitude);
                writer.WriteLine("echo -n #units=meters\\n>> ref_effect.head");
                writer.WriteLine("refms");
                writer.WriteLine("echo -en ref_effect.head + refms.out refms.effects");
                writer.WriteLine("echo # SPEC:{0}refms.spec >> refms.effects");
                writer.WriteLine("COPY /Y refms.effects + refms.spec refms.effects");
                var specFilename = string.Format("\"..\\{0}refms.spec\"", BaseFilename);
                writer.WriteLine("echo #location={0:0.000000} {1:0.000000}> {2}", Latitude, Longitude, specFilename);
                writer.WriteLine("COPY /Y {0} + refms.spec {0}", specFilename);
                writer.WriteLine("cd ..");
            }
        }

        static readonly Random Random = new Random();

        string BaseFilename
        {
            get
            {
                var step1 = PSMId.Replace(' ', '_').Replace(':', '+').Replace('|', '~');
                return string.Format("{0}_{1}{2:0.#}", step1, SVPFilename, ExplosionDepth);
            }
        }

        string SVPFilename
        {
            get
            {
                var northSouth = SVPLocation.Latitude >= 0 ? "N" : "S";
                var eastWest = SVPLocation.Longitude >= 0 ? "E" : "W";
                var randomInt = Random.Next(99);
                return string.Format("LOC_{0}{1}_{2}{3}_{4}-{5}", DegreesMinutes(SVPLocation.Latitude), northSouth, DegreesMinutes(SVPLocation.Longitude), eastWest, randomInt, TimePeriod);
            }
        }

        static string DegreesMinutes(double itude)
        {
            var degrees = (int)(Math.Abs(itude));
            var fraction = ((int)((Math.Abs(itude) - degrees) * 100)) / 100.0;
            var minutes = (int)(fraction * 60.0);
            return string.Format("{0}{1}", degrees, minutes);
        }

        double ShearWaveVelocity
        {
            get
            {
                double bSwSpeed;
                var vSedKM = (BottomLossData.RATIOD * _svpFile.Layers.Last().SoundVelocity) / 1000.0;

                if (vSedKM < 1.555)
                {
                    //if (vSedKM < 1.512)
                    //    warn("Sediment Compression Velocity outside of algorithm range.");
                    bSwSpeed = (3.884 * vSedKM - 5.757) * 1000.0;
                }
                else if (vSedKM < 1.650)
                    bSwSpeed = (1.137 * vSedKM - 1.485) * 1000.0;
                else if (vSedKM < 2.150)
                    bSwSpeed = (0.991 - 1.136 * vSedKM + 0.47 * vSedKM * vSedKM) * 1000.0;
                else
                    bSwSpeed = (0.78 * vSedKM - 0.962) * 1000.0;
                return bSwSpeed;
            }
        }
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
        public double Delta { get; private set; }

        public static SVPFile Create(Geo geo, double[] depths, double[] temps, double[] salinities, double[] soundspeeds, BottomLossData bottomLossData, double delta)
        {
            var maxDepth = depths.Last();
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
            var temperatureFunction = interp.Interpolate(depths, temps);
            var salinityFunction = interp.Interpolate(depths, salinities);
            var soundspeedFunction = interp.Interpolate(depths, soundspeeds);

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
            var result = new SVPFile {Layers = new List<SVPLayer>(), Latitude = geo.Latitude, Longitude = geo.Longitude, Delta = delta};
            int start = 0, prev = 0;
            for (var inc = 0; inc < nodes; inc++)
            {
                var depthmid = inc == 0 ? depthIncs[0] * 0.5 : (depthIncs[inc] + depthIncs[inc - 1]) * 0.5;

                var deltaSVP = Math.Abs(svpd[inc] - svpd[start]);

                if (deltaSVP >= delta || (inc == (nodes - 1)))
                {
                    start = inc;
                    result.Layers.Add(new SVPLayer
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
            return result;
        }

        public void Write(string fileName)
        {
            using (var writer = new StreamWriter(fileName, false))
            {
                writer.WriteLine("LAYERS {0}", Layers.Count);
                foreach (var layer in Layers) writer.WriteLine(layer);
                writer.WriteLine();
                writer.WriteLine();
                writer.WriteLine(" DELTA {0,9:0.0000}", Delta);
                writer.WriteLine("LOCATION: {0,10:0.000000} {1,10:0.000000}", Latitude, Longitude);
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
