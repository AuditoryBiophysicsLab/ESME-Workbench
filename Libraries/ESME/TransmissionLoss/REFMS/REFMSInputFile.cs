using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using ESME.Data;
using ESME.Environment;
using ESME.Environment.NAVO;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.TransmissionLoss.REFMS
{
    public class REFMSInputFile
    {
        public double Yield { get; set; }
        public double Cluster { get; set; }
        public double ExplosionDepth { get; set; }
        public double Duration { get; set; }
        public double WaterDepth { get; set; }
        public double BottomSoundSpeedRatio { get; set; }
        public double BottomShearWaveSpeed { get; set; }
        public double SPLCutoff { get; set; }
        public double SourceCount { get; set; }

        public void Write(string fileName)
        {
            using (var writer = new StreamWriter(fileName, false))
            {
                writer.WriteLine("");
                writer.WriteLine("COMMENT");
                writer.WriteLine("For: [tbd]");
                writer.WriteLine("");
                writer.WriteLine("UNITS       1 - Metric Units");
                writer.WriteLine("EXPLOSIVE   1    	Explosive composition (1=TNT)");
                writer.WriteLine("YIELD       {0:0.000} 	kg", Yield);
                writer.WriteLine("CLUSTER     {0}   	Charges", Cluster);
                writer.WriteLine("DEXPLOSION  {0:0.00} 	Depth of explosion in meters", ExplosionDepth);
                writer.WriteLine("");
                writer.WriteLine("RADIUS      0.   	Skip ship response");
                writer.WriteLine("DURATION    5.0 	Duration in secs after bottom reflection");
                writer.WriteLine("IMULT       0    	Second Order Reflection");
                writer.WriteLine("IRB1        1.   	Compute bottom reflections");
                writer.WriteLine("IRB2        1.   	Compute rays surf to gage");
                writer.WriteLine("IRSC        0    	Compute rays in sound channel");
                writer.WriteLine("IC          1.   	Disables cavitation.");
                writer.WriteLine("");
                writer.WriteLine("FILTER     -2    	 ");
                writer.WriteLine("");
                writer.WriteLine("DWATER      26.9 	Depth of water in meters");
                writer.WriteLine("");
                writer.WriteLine("BSSRATIO    2.000 	Soil Type: Sound Speed Ratio");
                writer.WriteLine("BRHO        8.000 	Soil Type: Density");
                writer.WriteLine("BSWSPEED    1357.767 	Soil Type: Shear Wave Velocity");
                writer.WriteLine("BMAT        1.0 ");
                writer.WriteLine("SCALEI      no   ");
                writer.WriteLine("SVPFILE     loc.svp   ");
                writer.WriteLine("");
                writer.WriteLine("SE          1   make 3rd-octave energy spectra");
                writer.WriteLine("SPLCUTOFF   200.000  	 from 1 sources");
                writer.WriteLine("SELCUTOFF   164  800  171  1100  183   ");
                writer.WriteLine("SOURCES     1   ");
                writer.WriteLine("");
                writer.WriteLine("DEPTHE");
                writer.WriteLine(" 0.30 27.00 20  ");
                writer.WriteLine("LOOPE");
                writer.WriteLine(" 50.00 549.30 20 ");
                writer.WriteLine("LOOPL");
                writer.WriteLine(" 686.63 2746.51 15  ");
                writer.WriteLine("STOP");
            }
        }
#if false
COMMENT
For: LOC_259-Summer[2000 lb Bomb:1|2000 lb Bomb:1|Explosive:1] Vers: 1.20.0 working

UNITS       1 - Metric Units   
EXPLOSIVE   1    	Explosive composition (1=TNT)
YIELD       453.590 	kg
CLUSTER     1   	Charges
DEXPLOSION  1.00 	Depth of explosion in meters

RADIUS      0.   	Skip ship response
DURATION    5.0 	Duration in secs after bottom reflection
IMULT       0    	Second Order Reflection
IRB1        1.   	Compute bottom reflections
IRB2        1.   	Compute rays surf to gage
IRSC        0    	Compute rays in sound channel
IC          1.   	Disables cavitation.

FILTER     -2    	 

DWATER      24.9 	Depth of water in meters

BSSRATIO    2.000 	Soil Type: Sound Speed Ratio
BRHO        8.000 	Soil Type: Density
BSWSPEED    1394.923 	Soil Type: Shear Wave Velocity
BMAT        1.0 
SCALEI      no   
SVPFILE     LOC_259-Summer.svp   

SE          1   make 3rd-octave energy spectra
SPLCUTOFF   200.000  	 from 1 sources
SELCUTOFF   163  800  171  1100  183   
SOURCES     1   

DEPTHE
 0.30 25.00 16  
LOOPE
 50.00 20000.00 20 
LOOPL
 25000.00 100000.00 15  
STOP
#endif

        public static void CreateSVP(EarthCoordinate location, NAVOTimePeriod timePeriod, float depth, BackgroundTaskAggregator backgroundTaskAggregator)
        {
            var appSettings = Globals.AppSettings;
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
            var maxDepth = new EarthCoordinate<float>(location.Latitude, location.Longitude, depth);
            var monthlyTemperature = new SoundSpeed();
            var monthlySalinity = new SoundSpeed();
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
                    RequiredSupportFiles = gdemRequiredSupportFiles,
                    MaxDepth = maxDepth,
                    PointExtractionMode = true,
                };
                soundSpeedExtractor.RunWorkerCompleted += (sender, e) =>
                {
                    var extractor = (GDEMBackgroundExtractor)sender;
                    monthlyTemperature.SoundSpeedFields.Add(extractor.TemperatureField);
                    monthlySalinity.SoundSpeedFields.Add(extractor.SalinityField);
                    if (soundSpeedExtractors.Any(ssfExtractor => ssfExtractor.IsBusy)) return;
                    SoundSpeed averageTemperature;
                    SoundSpeed averageSalinity;
                    if (uniqueMonths.Count <= 1)
                    {
                        averageTemperature = monthlyTemperature;
                        averageSalinity = monthlySalinity;
                    }
                    else
                    {
                        averageTemperature = SoundSpeed.Average(monthlyTemperature, uniqueMonths);
                        averageSalinity = SoundSpeed.Average(monthlySalinity, uniqueMonths);
                    }
                    // todo: here is where we create the SVP using the PCHIP algorithm, etc. from the average temp/salinity
                };
                soundSpeedExtractors.Add(soundSpeedExtractor);
                backgroundTaskAggregator.BackgroundTasks.Add(soundSpeedExtractor);
            }
        }

        public void WriteBatchFile(string filenameBase, string svpFilename, string modeName, string timeFrame, REFMSInputFile inputFile, EarthCoordinate explosiveLocation, EarthCoordinate svpLocation)
        {
            using (var writer = new StreamWriter(string.Format("{0}-refms.bat", filenameBase), false))
            {
                writer.WriteLine("");
                writer.WriteLine("mkdir \"{0}\"", filenameBase);
                writer.WriteLine("COPY /Y \"{0}refms.in\" \"{0}\\refms.in\" ", filenameBase);
                writer.WriteLine("COPY /Y \"{0}\" \"{1}\\loc.svp\" ", svpFilename, filenameBase);
                writer.WriteLine("rem effects header - ");
                writer.WriteLine("cd \"{0}\"", filenameBase);
                writer.WriteLine("echo #head=> ref_effect.head");
                writer.WriteLine("echo #tstamp=%DATE% %TIME%>> ref_effect.head");
                writer.WriteLine("echo #sysver=%COMPUTERNAME%^|{0}^|{1}^|{2}^|{3}^|null>> ref_effect.head", System.Environment.UserName, System.Environment.OSVersion.VersionString, System.Environment.OSVersion.ServicePack, System.Environment.GetEnvironmentVariable("PROCESSOR_ARCHITECTURE"));
                writer.WriteLine("echo #title=Explosives Test, EC_SimArea>> ref_effect.head");
                writer.WriteLine("echo #mode={0}>> ref_effect.head", modeName);
                writer.WriteLine("echo #bin=E12>> ref_effect.head");    // Where does this come from?
                writer.WriteLine("echo #season={0}>> ref_effect.head", timeFrame);
                writer.WriteLine("echo #info={0:0.0000}, {1:0.0000}, {2:0.0000}, {3:0.0}, {4:0.0}, {5:0.0}>> ref_effect.head", inputFile.ExplosionDepth, inputFile.Yield, inputFile.Duration, inputFile.BottomSoundSpeedRatio, -20, -15); // where do -20 and -15 come from?
                writer.WriteLine("echo #location={0:0.000000} {1:0.000000}>> ref_effect.head", explosiveLocation.Latitude, explosiveLocation.Longitude);
                writer.WriteLine("echo #splineloc={0:0.000000} {1:0.000000}>> ref_effect.head", svpLocation.Latitude, svpLocation.Longitude);
                writer.WriteLine("echo #units=meters>> ref_effect.head");
                writer.WriteLine("start \"REFMS\" /wait  \"{0}\"", Globals.AppSettings.REFMSSettings.REFMSExecutablePath);
                writer.WriteLine("COPY /Y ref_effect.head + refms.out refms.effects");
                writer.WriteLine("echo # SPEC:{0}refms.spec >> refms.effects");
                writer.WriteLine("COPY /Y refms.effects + refms.spec refms.effects");
                var specFilename = string.Format("\"..\\{0}refms.spec\"", filenameBase);
                writer.WriteLine("echo #location={0:0.000000} {1:0.000000}> {2}", explosiveLocation.Latitude, explosiveLocation.Longitude, specFilename);
                writer.WriteLine("COPY /Y {0} + refms.spec {0}", specFilename);
                writer.WriteLine("cd ..");
            }
            using (var writer = new StreamWriter(string.Format("{0}-refms.sh", filenameBase), false))
            {
                writer.WriteLine("#!/bin/sh");
                writer.WriteLine("mkdir \"{0}\"", filenameBase);
                writer.WriteLine("cp \"{0}refms.in\" \"{0}\\refms.in\" ", filenameBase);
                writer.WriteLine("cp \"{0}\" \"{1}\\loc.svp\" ", svpFilename, filenameBase);
                writer.WriteLine("# effects header - ");
                writer.WriteLine("cd \"{0}\"", filenameBase);
                writer.WriteLine("echo -n #head\\n> ref_effect.head");
                writer.WriteLine("echo -n #tstamp=$(date +\"%F %T\")\\n>> ref_effect.head");
                writer.WriteLine("echo -n #sysver=$(hostname)\\|{0}\\|{1}\\|{2}\\|{3}\\|null\\n>> ref_effect.head", System.Environment.UserName, System.Environment.OSVersion.VersionString, System.Environment.OSVersion.ServicePack, System.Environment.GetEnvironmentVariable("PROCESSOR_ARCHITECTURE"));
                writer.WriteLine("echo -n #title=Explosives Test, EC_SimArea\\n>> ref_effect.head");
                writer.WriteLine("echo -n #mode={0}\\n>> ref_effect.head", modeName);
                writer.WriteLine("echo -n #bin=E12\\n>> ref_effect.head");    // Where does this come from?
                writer.WriteLine("echo -n #season={0}\\n>> ref_effect.head", timeFrame);
                writer.WriteLine("echo -n #info={0:0.0000}, {1:0.0000}, {2:0.0000}, {3:0.0}, {4:0.0}, {5:0.0}\\n>> ref_effect.head", inputFile.ExplosionDepth, inputFile.Yield, inputFile.Duration, inputFile.BottomSoundSpeedRatio, -20, -15); // where do -20 and -15 come from?
                writer.WriteLine("echo -n #location={0:0.000000} {1:0.000000}\\n>> ref_effect.head", explosiveLocation.Latitude, explosiveLocation.Longitude);
                writer.WriteLine("echo -n #splineloc={0:0.000000} {1:0.000000}\\n>> ref_effect.head", svpLocation.Latitude, svpLocation.Longitude);
                writer.WriteLine("echo -n #units=meters\\n>> ref_effect.head");
                writer.WriteLine("refms");
                writer.WriteLine("echo -en ref_effect.head + refms.out refms.effects");
                writer.WriteLine("echo # SPEC:{0}refms.spec >> refms.effects");
                writer.WriteLine("COPY /Y refms.effects + refms.spec refms.effects");
                var specFilename = string.Format("\"..\\{0}refms.spec\"", filenameBase);
                writer.WriteLine("echo #location={0:0.000000} {1:0.000000}> {2}", explosiveLocation.Latitude, explosiveLocation.Longitude, specFilename);
                writer.WriteLine("COPY /Y {0} + refms.spec {0}", specFilename);
                writer.WriteLine("cd ..");
            }
        }

        static readonly Random Random = new Random();

        string FilenameFromPSMIdEarthCoordinateTimeFrame(string psmId, string svpFilename)
        {
            //2000 lb Bomb:1|2000 lb Bomb:1|Explosive:1
            //2000_lb_Bomb+1~2000_lb_Bomb+1~Explosive+1_LOC_3715N_7515W_27-Fall0
            var step1 = psmId.Replace(' ', '_').Replace(':', '+').Replace('|', '~');
            return string.Format("{0}_{1}0", step1, svpFilename);
        }

        string SVPFilenameFromEarthCoordinateAndTimeFrame(Geo svpLocation, string timeFrame)
        {
            var northSouth = svpLocation.Latitude >= 0 ? "N" : "S";
            var eastWest = svpLocation.Longitude >= 0 ? "E" : "W";
            var randomInt = Random.Next(99);
            return string.Format("LOC_{0}{1}_{2}{3}_{4}-{5}", DegreesMinutes(svpLocation.Latitude), northSouth, DegreesMinutes(svpLocation.Longitude), eastWest, randomInt, timeFrame);
        }

        static string DegreesMinutes(double itude)
        {
            var degrees = (int)(Math.Abs(itude));
            var fraction = ((int)((Math.Abs(itude) - degrees) * 100)) / 100.0;
            var minutes = (int)(fraction * 60.0);
            return string.Format("{0}{1}", degrees, minutes);
        }
    }
}
