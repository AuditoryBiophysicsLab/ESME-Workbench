using System;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;
using ESME.Data;
using ESME.Environment;
using ESME.Environment.Descriptors;
using ESME.Environment.NAVO;
using ESME.Model;
using ESME.NEMO;
using ESME.TransmissionLoss.Bellhop;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.TransmissionLoss.CASS
{
    public static class CASSFiles
    {
        public static void WriteAcousticSimulatorFiles(AppSettings appSettings, IEnumerable<string> timePeriods, IList<AnalysisPoint> analysisPoints, NemoFile nemoFile, string cassBathymetryFileName, string cassEnvironmentFileName, NemoModeToAcousticModelNameMap modeToAcousticModelNameMap, EnvironmentInformation environmentInformation)
        {
            if ((analysisPoints == null) || (analysisPoints.Count == 0)) return;
            var nemoScenario = nemoFile.Scenario;
            foreach (var timePeriod in timePeriods)
            {
                // These are for CASS and RAM (Restricted NAVY TL models)
                var curScenarioDataPath = Path.GetDirectoryName(nemoFile.FileName);
                var curPropagationPath = Path.Combine(curScenarioDataPath, "Propagation");
                var curTimePeriodPath = Path.Combine(curPropagationPath, timePeriod);
                if (!Directory.Exists(curTimePeriodPath)) Directory.CreateDirectory(curTimePeriodPath);

                foreach (var platform in nemoScenario.Platforms)
                {
                    // Delete any existing CASS files that might have the same name as the file we're about to write.
                    DeleteAcousticSimulatorFiles(curTimePeriodPath, timePeriod, platform);
                    foreach (var source in platform.Sources)
                    {
                        foreach (var mode in source.Modes)
                        {
                            // Find the unique modes for the current platform/source pair
                            var modeSources = (from analysisPoint in analysisPoints
                                               from soundSource in analysisPoint.SoundSources
                                               let psmFields = soundSource.Name.Split('|')
                                               let soundSourcePlatform = psmFields[0]
                                               let soundSourceSource = psmFields[1]
                                               let soundSourceMode = psmFields[2]
                                               where (platform.Name.ToLower() == soundSourcePlatform.ToLower()) && (source.Name.ToLower() == soundSourceSource.ToLower()) && (mode.Name.ToLower() == soundSourceMode.ToLower()) && (soundSource.ShouldBeCalculated)
                                               select soundSource).ToList();
                            if (modeSources.Count > 0)
                            {
                                var thisModel = modeToAcousticModelNameMap[modeSources[0].Name];
                                switch (thisModel)
                                {
                                    case TransmissionLossAlgorithm.CASS:
                                    case TransmissionLossAlgorithm.RAM:
                                    //case TransmissionLossAlgorithm.REFMS:
                                        WriteAcousticSimulatorFiles(curTimePeriodPath, platform, source, mode, modeSources, thisModel, timePeriod, appSettings, nemoFile, cassBathymetryFileName, cassEnvironmentFileName);
                                        break;
                                    case TransmissionLossAlgorithm.Bellhop:
                                    case TransmissionLossAlgorithm.RAMGEO:
                                        foreach (var curSource in modeSources)
                                        {
#if true
                                            var runFile = TransmissionLossRunFile.Create(thisModel, curSource,
                                                                                         nemoFile.Scenario.SimAreaName,
                                                                                         Path.GetFileNameWithoutExtension(cassBathymetryFileName),
                                                                                         Path.GetFileNameWithoutExtension(cassEnvironmentFileName),
                                                                                         platform.Name,
                                                                                         curSource.Name,
                                                                                         mode.Name,
                                                                                         timePeriod);
                                            runFile.Save(Path.Combine(curTimePeriodPath, runFile.Filename));
#else
                                            var lat = curSource.Latitude;
                                            var lon = curSource.Longitude;
                                            var northSouth = lat >= 0 ? "n" : "s";
                                            var eastWest = lon >= 0 ? "e" : "w";
                                            var jobName = string.Format("{0}_{1}{2:0.####}_{3}{4:0.####}",
                                                                        curSource.Name.Replace('|', '_'), northSouth, Math.Abs(lat),
                                                                        eastWest, Math.Abs(lon));
                                            var runfile = TransmissionLossRunFile.Create(thisModel,
                                                                                         new TransmissionLossJob
                                                                                         {
                                                                                             SoundSource = curSource,
                                                                                             Name = jobName,
                                                                                         },
                                                                                         environmentInformation,
                                                                                         Globals.AppSettings);
                                            runfile.Save(curTimePeriodPath);
#endif
                                        }
                                        break;
                                }
                            }
                        }
                    }
                }
            }
        }

        static void DeleteAcousticSimulatorFiles(string curTimePeriodPath, string timePeriod, NemoPSM platform)
        {
            var inputFilePattern = string.Format("*{0}-{1}-{2}.*", platform.Name, platform.Id, timePeriod);
            var matchingFiles = Directory.GetFiles(curTimePeriodPath, inputFilePattern);
            foreach (var matchingFile in matchingFiles) File.Delete(matchingFile);
        }

        public static void WriteAcousticSimulatorFiles(string curTimePeriodPath, NemoPSM platform, NemoPSM source, NemoMode mode, IList<SoundSource> soundSources, TransmissionLossAlgorithm simulatorName, string timePeriod, AppSettings appSettings, NemoFile nemoFile, string cassBathymetryFileName, string cassEnvironmentFileName)
        {
            var nemoScenario = nemoFile.Scenario;
            var simAreaFile = RangeComplexDescriptors.ReadCSV(Path.Combine(appSettings.ScenarioDataDirectory, "SimAreas.csv"), null);
            var rangeComplex = ((RangeComplexDescriptor)simAreaFile[nemoScenario.SimAreaName]).Data;

            var inputFileName = string.Format("base-{0}-{1}-{2}-{3}.inp", simulatorName, platform.Name, platform.Id, timePeriod);
            var batchFileName = "run_" + Path.GetFileNameWithoutExtension(inputFileName) + ".bat";
            var inputFilePath = Path.Combine(curTimePeriodPath, inputFileName);
            var batchFilePath = Path.Combine(curTimePeriodPath, batchFileName);

            // If the file does not yet exist, we need to write the header
            if (!File.Exists(inputFilePath))
            {
                using (var writer = new StreamWriter(inputFilePath))
                {
                    writer.WriteLine("# analystName: {0}", System.Environment.UserName);
                    writer.WriteLine("# creationDate: {0}", DateTime.Now);
                    writer.WriteLine("# bathymetryFile: {0}", cassBathymetryFileName);
                    writer.WriteLine("# environmentFile: {0}", cassEnvironmentFileName);
                    writer.WriteLine();
                    writer.WriteLine("*System Parms");
                    if (simulatorName == TransmissionLossAlgorithm.CASS)
                    {
                        writer.WriteLine("Plot Files,{0}", appSettings.CASSSettings.GeneratePlotFiles ? "y" : "n");
                        writer.WriteLine("Binary Files,{0}", appSettings.CASSSettings.GenerateBinaryFiles ? "y" : "n");
                        writer.WriteLine("Pressure Files,{0}", appSettings.CASSSettings.GeneratePressureFiles ? "y" : "n");
                        //writer.WriteLine("Eigenray Files,{0}", appSettings.CASSSettings.GenerateEigenrayFiles ? "y" : "n");
                    }
                    writer.WriteLine("Data Directory,{0}", appSettings.ScenarioDataDirectory);
                    writer.WriteLine("*End System Parms");
                    writer.WriteLine();
                    if (simulatorName == TransmissionLossAlgorithm.CASS)
                    {
                        writer.WriteLine("*CASS Parms");
                        writer.WriteLine("VOLUME ATTENUATION MODEL                ,FRANCOIS-GARRISON");
                        writer.WriteLine("SURFACE REFLECTION COEFFICIENT MODEL    ,OAML");
                        writer.WriteLine("MAXIMUM SURFACE REFLECTIONS             ,100");
                        writer.WriteLine("MAXIMUM BOTTOM REFLECTIONS              ,100");
                        writer.WriteLine("EIGENRAY MODEL                          ,GRAB-V3");
                        writer.WriteLine("EIGENRAY ADDITION                       ,RANDOM");
                        writer.WriteLine("EIGENRAY TOLERANCE                      ,0.01");
                        writer.WriteLine("VERTICAL ANGLE MINIMUM                  ,-89.9 DEG");
                        writer.WriteLine("VERTICAL ANGLE MAXIMUM                  ,+89.9 DEG");
                        writer.WriteLine("VERTICAL ANGLE INCREMENT                ,0.1 DEG");
                        writer.WriteLine("Reference System                        ,LAT-LON");
                        writer.WriteLine("Source System                           ,LAT-LON");
                        writer.WriteLine("*end Cass Parms");
                    }
                    if (simulatorName == TransmissionLossAlgorithm.RAM)
                    {
                        writer.WriteLine("*RAM Parms");
#if false
                        writer.WriteLine("Speed Dial                              ,{0}", appSettings.RAMSettings.SpeedDial);
                        writer.WriteLine("SSP Units                               ,{0}", appSettings.RAMSettings.SSPUnits);
                        writer.WriteLine("Cass Level                              ,{0}", appSettings.RAMSettings.CASSLevel);
                        writer.WriteLine("Bathymetry Metric                       ,{0}", appSettings.RAMSettings.BathymetryMetric);
#endif
                        writer.WriteLine("*end RAM Parms");
                    }
                    writer.WriteLine();
                    writer.WriteLine();
                }
            }

            using (var writer = new StreamWriter(inputFilePath, true))
            {
                writer.WriteLine("*Loadcase");
                writer.WriteLine("#Sim_Area_ID                             ");
                writer.WriteLine("Range Complex                           ,{0}", nemoScenario.SimAreaName);
                writer.WriteLine("Sim Area                                ,{0}", nemoScenario.SimAreaName);
                writer.WriteLine("Event Name                              ,{0}", nemoScenario.EventName);
                writer.WriteLine("Reference Location                      ,{0:0.000} DEG, {1:0.000} DEG", rangeComplex.Latitude, rangeComplex.Longitude);
                writer.WriteLine("Enviro File                             ,{0}", Path.GetFileName(cassEnvironmentFileName));
                float stepCount;
                float stepSize;
                switch (simulatorName)
                {
                    case TransmissionLossAlgorithm.RAM:
                        // For RAM, range and depth step size in INP files need to be integers, so round up to the next int if the computed value is not an integer.  Max depth cell count is 19
                        stepCount = appSettings.RAMSettings.MaximumDepth / appSettings.RAMSettings.DepthStepSize;
                        if (stepCount > 19) stepSize = appSettings.RAMSettings.MaximumDepth / 19;
                        else stepSize = appSettings.RAMSettings.DepthStepSize;
                        if (Math.Floor(stepSize) != stepSize) stepSize = (float)Math.Ceiling(stepSize);
                        writer.WriteLine("Water Depth                             ,0 M, {0} M, {1} M", appSettings.RAMSettings.MaximumDepth, stepSize);
                        break;
                    //case TransmissionLossAlgorithm.CASS:
                    default:
                        // For CASS, range and depth step size in INP files need to be integers, so round up to the next int if the computed value is not an integer.  Max depth cell count is 1024
                        stepCount = appSettings.CASSSettings.MaximumDepth / appSettings.CASSSettings.DepthStepSize;
                        if (stepCount > 1024) stepSize = appSettings.CASSSettings.MaximumDepth / 1024;
                        else stepSize = appSettings.CASSSettings.DepthStepSize;
                        if (Math.Floor(stepSize) != stepSize) stepSize = (float)Math.Ceiling(stepSize);
                        writer.WriteLine("Water Depth                             ,0 M, {0} M, {1} M", appSettings.CASSSettings.MaximumDepth, stepSize);
                        break;
                }
                writer.WriteLine("Bathy File                              ,{0}", Path.GetFileName(cassBathymetryFileName));
                writer.WriteLine("Season                                  ,{0}", timePeriod);
                writer.Write("Radial Angles                           ");

                foreach (var radial in soundSources[0].RadialBearings) writer.Write(",{0}", radial);
                writer.WriteLine();

                writer.WriteLine("#PSM_ID                                  ");
                writer.WriteLine("Platform Name                           ,{0}", platform.Name);
                writer.WriteLine("Source Name                             ,{0}", source.Name);
                writer.WriteLine("Mode Name                               ,{0}", mode.Name);
                writer.WriteLine("Frequency                               ,{0:0.000} HZ", Math.Sqrt(mode.LowFrequency * mode.HighFrequency));
                writer.WriteLine("DE Angle                                ,{0:0.000} DEG", mode.DepressionElevationAngle);
                writer.WriteLine("Vertical Beam                           ,{0:0.000} DEG", mode.VerticalBeamWidth);
                writer.WriteLine("Source Depth                            ,{0:0.000} M", mode.SourceDepth);
                writer.WriteLine("SOURCE LEVEL                            ,{0:0.000} DB", mode.SourceLevel);

                switch (simulatorName)
                {
                    case TransmissionLossAlgorithm.RAM:
                        // For RAM, range and depth step size in INP files need to be integers, so round up to the next int if the computed value is not an integer.  Max range cell count is 999
                        stepCount = mode.Radius / appSettings.RAMSettings.RangeStepSize;
                        if (stepCount > 999) stepSize = mode.Radius / 999;
                        else stepSize = appSettings.RAMSettings.RangeStepSize;
                        if (Math.Floor(stepSize) != stepSize) stepSize = (float)Math.Ceiling(stepSize);
                        writer.WriteLine("Range Distance                          ,{0} M, {1} M, {0} M", stepSize, mode.Radius);
                        break;
                    //case TransmissionLossAlgorithm.CASS:
                    default:
                        // For CASS, range and depth step size in INP files need to be integers, so round up to the next int if the computed value is not an integer.  Max range cell count is 1024
                        stepCount = mode.Radius / appSettings.CASSSettings.RangeStepSize;
                        if (stepCount > 1024) stepSize = mode.Radius / 1024;
                        else stepSize = appSettings.CASSSettings.RangeStepSize;
                        if (Math.Floor(stepSize) != stepSize) stepSize = (float)Math.Ceiling(stepSize);
                        writer.WriteLine("Range Distance                          ,{0} M, {1} M, {0} M", stepSize, mode.Radius);
                        break;
                }

                foreach (var soundSource in soundSources)
                    writer.WriteLine("Source Location                         ,{0:0.000} DEG, {1:0.000} DEG", soundSource.Latitude, soundSource.Longitude);

                writer.WriteLine("*end loadcase");
                writer.WriteLine();
            }
            switch (simulatorName)
            {
                case TransmissionLossAlgorithm.RAM:
                    if (!string.IsNullOrEmpty(appSettings.NAEMOTools.RAMSupportJarFile) && !string.IsNullOrEmpty(appSettings.NAEMOTools.RAMExecutable)) 
                        using (var writer = new StreamWriter(batchFilePath))
                            writer.WriteLine("java -Dlog4j.configuration=ram-log4j.xml -jar \"{0}\" {1} \"{2}\"", appSettings.NAEMOTools.RAMSupportJarFile, inputFileName, appSettings.NAEMOTools.RAMExecutable);
                    break;
                //case TransmissionLossAlgorithm.CASS:
                default:
                    if (!string.IsNullOrEmpty(appSettings.CASSSettings.PythonExecutablePath) && !string.IsNullOrEmpty(appSettings.CASSSettings.PythonScriptPath) && !string.IsNullOrEmpty(appSettings.CASSSettings.CASSExecutablePath)) 
                        using (var writer = new StreamWriter(batchFilePath)) 
                            writer.WriteLine("start /wait \"{0}\" \"{1}\" \"{2}\" \"{3}\"", appSettings.CASSSettings.PythonExecutablePath, appSettings.CASSSettings.PythonScriptPath, inputFileName, appSettings.CASSSettings.CASSExecutablePath);
                    break;
            }
        }

        public static void WriteEnvironmentFile(string environmentFileName, GeoRect geoRect, Sediment sedimentType, SoundSpeedField soundSpeedField, TimePeriodEnvironmentData<WindSample> wind, BackgroundTask backgroundTask = null, string bathymetryFileName = null, string overlayFileName = null)
        {
            var isFirstPoint = true;
            using (var envFile = new StreamWriter(environmentFileName, false))
            {
                envFile.WriteLine("SOUND SPEED MODEL = TABLE");
                envFile.WriteLine("COMMENT TABLE");
                envFile.WriteLine("Generated by ESME WorkBench (Build date: {0})", BuildInformation.BuildDateTime);
                if (!string.IsNullOrEmpty(overlayFileName)) envFile.WriteLine("limitName={0}", overlayFileName);
                if (!string.IsNullOrEmpty(bathymetryFileName)) envFile.WriteLine("bathName={0}", bathymetryFileName);
                envFile.WriteLine("timeFrame={0}", soundSpeedField.TimePeriod);
                envFile.WriteLine("EOT");
                envFile.WriteLine();

                double lat, lon;
                if (backgroundTask != null) backgroundTask.Maximum += (int)(((geoRect.East - geoRect.West) * 4) * ((geoRect.North - geoRect.South) * 4));
                for (lon = geoRect.West; lon < geoRect.East; lon += 0.25)
                {
                    for (lat = geoRect.South; lat < geoRect.North; lat += 0.25)
                    {
                        WriteEnvironmentFile(envFile, sedimentType, soundSpeedField, wind, new EarthCoordinate(lat, lon),
                                             ref isFirstPoint);
                        if (backgroundTask != null) backgroundTask.Value++;
                    }
                    if ((lat - geoRect.North) < 0.125)
                    {
                        if (backgroundTask != null) backgroundTask.Maximum++;
                        WriteEnvironmentFile(envFile, sedimentType, soundSpeedField, wind,
                                             new EarthCoordinate(geoRect.North, lon), ref isFirstPoint);
                        if (backgroundTask != null) backgroundTask.Value++;
                    }
                }
                if ((lon - geoRect.East) < 0.125)
                {
                    if (backgroundTask != null) backgroundTask.Maximum += (int) ((geoRect.North - geoRect.South)*4);
                    for (lat = geoRect.South; lat < geoRect.North; lat += 0.25)
                    {
                        WriteEnvironmentFile(envFile, sedimentType, soundSpeedField, wind,
                                             new EarthCoordinate(lat, geoRect.East), ref isFirstPoint);
                        if (backgroundTask != null) backgroundTask.Value++;
                    }
                }
            }
        }

        static void WriteEnvironmentFile(TextWriter envFile, Sediment sediment, TimePeriodEnvironmentData<SoundSpeedProfile> soundSpeedField, TimePeriodEnvironmentData<WindSample> wind, EarthCoordinate requestedLocation, ref bool isFirstPoint)
        {
            // ssp is the nearest actual soundspeed profile to the point that's been requested.  The actual profiles are not laid out
            // in a grid, rather they are placed at quarter-degree grid points where there is water.  So grid points on land typically won't
            // have a profile associated with them.  CASS, however, wants a regularly-spaced grid so we tell CASS that every grid point
            // has a profile.  The roundedLat and roundedLon variables is the grid points we report to CASS, regardless of where the actual
            // profile data came from.
            var ssp = soundSpeedField.EnvironmentData[requestedLocation];
            var roundedLat = Math.Round(requestedLocation.Latitude * 4) / 4;
            var roundedLon = Math.Round(requestedLocation.Longitude * 4) / 4;
            if (ssp.Data.Count == 0) return;
            envFile.WriteLine(isFirstPoint ? "RESET ENVIRONMENT NUMBER" : "INCREMENT ENVIRONMENT NUMBER");
            envFile.WriteLine("COMMENT TABLE");
            envFile.WriteLine("Requested Latitude: {0:0.0000} Longitude: {1:0.0000}", requestedLocation.Latitude, requestedLocation.Longitude);
            envFile.WriteLine("Actual Profile Latitude: {0:0.0000} Longitude: {1:0.0000}", ssp.Latitude, ssp.Longitude);
            envFile.WriteLine("Synthesized Latitude: {0:0.0000} Longitude: {1:0.0000}", roundedLat, roundedLon);
            envFile.WriteLine("EOT");
            envFile.WriteLine("ENVIRONMENT LATITUDE  = {0:0.0###} DEG", roundedLat);
            envFile.WriteLine("ENVIRONMENT LONGITUDE = {0:0.0###} DEG", roundedLon);
            envFile.WriteLine("OCEAN SOUND SPEED TABLE");
            envFile.WriteLine("M         M/S       ");
            foreach (var datum in ssp.Data)
                if (!float.IsNaN(datum.Value))
                    envFile.WriteLine("{0,-10:0.000}{1,-10:0.000}", datum.Depth, datum.Value);
                else break;
            envFile.WriteLine("EOT");
            envFile.WriteLine("BOTTOM REFLECTION COEFFICIENT MODEL   = HFEVA");
            //var sedimentSample = sedimentType.Samples[location];
            //var sedimentTypeName = BottomSedimentTypeTable.Lookup(sedimentSample.Data.SampleValue).ToUpper();
            var findResult =
                BottomSedimentTypeTable.CASSMap.Find(mapEntry => mapEntry.Value == sediment.Samples[requestedLocation].Data.SampleValue);
            var sedimentTypeName = findResult == null ? "SAND" : findResult.Name;
            envFile.WriteLine(sedimentTypeName);
            envFile.WriteLine("WIND SPEED                            = {0:0.###} KNOTS", wind.EnvironmentData[requestedLocation].Data * 1.94384449);
            envFile.WriteLine();
            isFirstPoint = false;
        }
    }
}
