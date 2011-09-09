using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.NEMO;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.TransmissionLoss.REFMS
{
    public class REFMSInputFile
    {
        public double WaterDepth { get; private set; }
        public double BottomShearWaveSpeed { get; set; }
        public int SourceCount { get; private set; }
        public EarthCoordinate SVPLocation { get; private set; }
        public NAVOTimePeriod TimePeriod { get; private set; }
        public NemoMode NemoMode { get; private set; }
        public BottomLossData BottomLossData { get; private set; }
        public string OutputPath { get; private set; }
        public double Delta { get; private set; }
        public bool IsComplete { get; private set; }

        SVPFile _svpFile;

        readonly ExplosivePoint _explosivePoint;
        
        public REFMSInputFile(string outputPath, ExplosivePoint explosivePoint, NemoScenario scenario, NemoMode mode, float waterDepth, Geo svpLocation, double svpDelta, BackgroundTaskAggregator backgroundTaskAggregator)
        {
            _explosivePoint = explosivePoint;
            OutputPath = outputPath;
            Delta = svpDelta;
            NemoMode = mode;
            WaterDepth = waterDepth;
            SVPLocation = new EarthCoordinate(svpLocation);
            NAVOTimePeriod timePeriod;
            if (Enum.TryParse(scenario.TimeFrame, true, out timePeriod)) TimePeriod = timePeriod;
            else throw new ApplicationException("Error converting " + scenario.TimeFrame + " to a known time period");
            SourceCount = 0;
            foreach (var curPlatform in scenario.Platforms) 
                foreach (var curSource in curPlatform.Sources) 
                    if (curSource.Type.ToLower() == "explosive") SourceCount++;
            CreateSVP(backgroundTaskAggregator);
        }

        void WriteInputFile()
        {
            var fileName = Path.Combine(OutputPath, BaseFilename + "refms.in");
            using (var writer = new StreamWriter(fileName, false))
            {
                writer.WriteLine("");
                writer.WriteLine("COMMENT");
                writer.WriteLine("For: {0}[{1}] Build {2} ({3})", SVPFilename, NemoMode.PSMName, BuildInformation.SVNVersion, BuildInformation.BuildDateTime);
                writer.WriteLine("");
                writer.WriteLine("UNITS       1 - Metric Units");
                writer.WriteLine("EXPLOSIVE   1    	Explosive composition (1=TNT)");
                writer.WriteLine("NemoMode.SourceLevel       {0:0.000} 	kg", NemoMode.SourceLevel); // From Mode/sourceLevel
                writer.WriteLine("CLUSTER     {0}   	Charges", NemoMode.ClusterCount); // From Mode/clusterCount
                writer.WriteLine("DEXPLOSION  {0:0.00} 	Depth of explosion in meters", _explosivePoint.ExplosionDepth); // Platform/trackDef/initialHeight plus Mode/depthOffset
                writer.WriteLine("");
                //writer.WriteLine("RADIUS      0.   	Skip ship response");   
                writer.WriteLine("DURATION    {0:0.0} 	Duration in secs after bottom reflection", NemoMode.Duration.Seconds); // Mode/duration
                //writer.WriteLine("IMULT       0    	Second Order Reflection");
                writer.WriteLine("IRB1        {0}   	Compute bottom reflections", _explosivePoint.BottomReflectionsEnabled ? "1." : "0.");
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
                writer.WriteLine(" {0:0.00} {1:0.00} {2} ", NemoMode.LowFrequency, NemoMode.HighFrequency * 0.2, 20); // Mode/Low Freq, Mode/Hi Freq * 0.2, 20
                writer.WriteLine("LOOPL");
                writer.WriteLine(" {0:0.00} {1:0.00} {2} ", NemoMode.HighFrequency * 0.25, NemoMode.HighFrequency, 15); // Mode/High Freq * 0.25, Mode/Hi freq, 15
                writer.WriteLine("STOP");
            }
        }

        void CreateSVP(BackgroundTaskAggregator backgroundTaskAggregator)
        {
            var assemblyLocation = Assembly.GetCallingAssembly().Location;
            var extractionPath = Path.GetDirectoryName(assemblyLocation);
            if (extractionPath == null) throw new ApplicationException("Extraction path can't be null!");
            var gdemExtractionProgramPath = Path.Combine(extractionPath, "ImportGDEM.exe");
            var gdemRequiredSupportFiles = new List<string>
            {
                Path.Combine(extractionPath, "netcdf.dll"),
                Path.Combine(extractionPath, "NetCDF_Wrapper.dll")
            };

            var requiredMonths = Globals.AppSettings.NAVOConfiguration.MonthsInTimePeriod(TimePeriod);
            var uniqueMonths = requiredMonths.Distinct().ToList();
            uniqueMonths.Sort();

            var lat = Math.Round(SVPLocation.Latitude * 4) / 4;
            var lon = Math.Round(SVPLocation.Longitude * 4) / 4;
            var extractionArea = new GeoRect(lat, lat, lon, lon);
            var tempPath = Path.GetTempPath().Remove(Path.GetTempPath().Length - 1);
            if (!Directory.Exists(tempPath)) Directory.CreateDirectory(tempPath);
            var maxDepth = new EarthCoordinate<float>(SVPLocation.Latitude, SVPLocation.Longitude, (float)WaterDepth);
            var monthlyTemperature = new SoundSpeed();
            var monthlySalinity = new SoundSpeed();
            var monthlyExtendedSoundSpeed = new SoundSpeed();
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
                    //BottomLossData = bottomLossExtractor.BottomLossData[0];
                    SoundSpeedField averageTemperature;
                    SoundSpeedField averageSalinity;
                    SoundSpeedField averageSoundspeed;
                    if (uniqueMonths.Count <= 1)
                    {
                        averageTemperature = monthlyTemperature[TimePeriod];
                        averageSalinity = monthlySalinity[TimePeriod];
                        averageSoundspeed = monthlyExtendedSoundSpeed[TimePeriod];
                    }
                    else
                    {
                        averageTemperature = SoundSpeed.Average(monthlyTemperature, TimePeriod);
                        averageSalinity = SoundSpeed.Average(monthlySalinity, TimePeriod);
                        averageSoundspeed = SoundSpeed.Average(monthlyExtendedSoundSpeed, TimePeriod);
                    }
                    // Here is where we create the SVP using the PCHIP algorithm, etc. from the average temp/salinity
                    //_svpFile = new SVPFile(SVPLocation, Delta, averageTemperature, averageSalinity, averageSoundspeed);
                    var svpFilename = Path.Combine(OutputPath, SVPFilename + ".svp");
                    // Write out the SVP file
                    _svpFile.Write(svpFilename);
                    WriteInputFile();
                    WriteBatchFile();
                    IsComplete = true;
                };
                soundSpeedExtractors.Add(soundSpeedExtractor);
                backgroundTaskAggregator.BackgroundTasks.Add(soundSpeedExtractor);
            }
        }

        public void WriteBatchFile()
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
                writer.WriteLine("echo #mode={0}>> ref_effect.head", NemoMode.Name);
                writer.WriteLine("echo #bin=E12>> ref_effect.head"); // Where does this come from?
                writer.WriteLine("echo #season={0}>> ref_effect.head", TimePeriod);
                writer.WriteLine("echo #info={0:0.0000}, {1:0.0000}, {2:0.0000}, {3:0.0}, {4:0.0}, {5:0.0}>> ref_effect.head", _explosivePoint.ExplosionDepth, NemoMode.SourceLevel, NemoMode.Duration.Seconds,
                                 BottomLossData.RATIOD, -20, -15); // where do -20 and -15 come from?
                writer.WriteLine("echo #location={0:0.000000} {1:0.000000}>> ref_effect.head", _explosivePoint.Latitude, _explosivePoint.Longitude);
                writer.WriteLine("echo #splineloc={0:0.000000} {1:0.000000}>> ref_effect.head", SVPLocation.Latitude, SVPLocation.Longitude);
                writer.WriteLine("echo #units=meters>> ref_effect.head");
                writer.WriteLine("start \"REFMS\" /wait  \"{0}\"", Globals.AppSettings.REFMSSettings.REFMSExecutablePath);
                writer.WriteLine("COPY /Y ref_effect.head + refms.out refms.effects");
                writer.WriteLine("echo # SPEC:{0}refms.spec >> refms.effects");
                writer.WriteLine("COPY /Y refms.effects + refms.spec refms.effects");
                var specFilename = string.Format("\"..\\{0}refms.spec\"", BaseFilename);
                writer.WriteLine("echo #location={0:0.000000} {1:0.000000}> {2}", _explosivePoint.Latitude, _explosivePoint.Longitude, specFilename);
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
                writer.WriteLine("echo -n #mode={0}\\n>> ref_effect.head", NemoMode.Name);
                writer.WriteLine("echo -n #bin=E12\\n>> ref_effect.head"); // Where does this come from?
                writer.WriteLine("echo -n #season={0}\\n>> ref_effect.head", TimePeriod);
                writer.WriteLine("echo -n #info={0:0.0000}, {1:0.0000}, {2:0.0000}, {3:0.0}, {4:0.0}, {5:0.0}\\n>> ref_effect.head", _explosivePoint.ExplosionDepth, NemoMode.SourceLevel, NemoMode.Duration.Seconds,
                                 BottomLossData.RATIOD, -20, -15); // where do -20 and -15 come from?
                writer.WriteLine("echo -n #location={0:0.000000} {1:0.000000}\\n>> ref_effect.head", _explosivePoint.Latitude, _explosivePoint.Longitude);
                writer.WriteLine("echo -n #splineloc={0:0.000000} {1:0.000000}\\n>> ref_effect.head", SVPLocation.Latitude, SVPLocation.Longitude);
                writer.WriteLine("echo -n #units=meters\\n>> ref_effect.head");
                writer.WriteLine("refms");
                writer.WriteLine("echo -en ref_effect.head + refms.out refms.effects");
                writer.WriteLine("echo # SPEC:{0}refms.spec >> refms.effects");
                writer.WriteLine("COPY /Y refms.effects + refms.spec refms.effects");
                var specFilename = string.Format("\"..\\{0}refms.spec\"", BaseFilename);
                writer.WriteLine("echo #location={0:0.000000} {1:0.000000}> {2}", _explosivePoint.Latitude, _explosivePoint.Longitude, specFilename);
                writer.WriteLine("COPY /Y {0} + refms.spec {0}", specFilename);
                writer.WriteLine("cd ..");
            }
        }

        static readonly Random Random = new Random();

        string BaseFilename
        {
            get
            {
                var step1 = NemoMode.PSMId.Replace(' ', '_').Replace(':', '+').Replace('|', '~');
                return string.Format("{0}_{1}{2:0.#}", step1, SVPFilename, _explosivePoint.ExplosionDepth);
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
}
