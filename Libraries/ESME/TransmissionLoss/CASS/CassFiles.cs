using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using ESME.Data;
using ESME.Environment;
using ESME.Environment.Descriptors;
using ESME.Environment.NAVO;
using ESME.NEMO;
using HRC.Collections;
using HRC.Navigation;
using RangeComplex = ESME.Environment.Descriptors.RangeComplex;

namespace ESME.TransmissionLoss.CASS
{
    public static class CASSFiles
    {
        public static void WriteAcousticSimulatorFiles(AppSettings appSettings, IEnumerable<string> timePeriods, 
            IList<AnalysisPoint> analysisPoints, NemoFile nemoFile, 
            NemoModeToAcousticModelNameMap modeToAcousticModelNameMap,
            ObservableConcurrentDictionary<EnvironmentDataType, Task> environmentTasks, RangeComplexes rangeComplexes)
        {
            if ((analysisPoints == null) || (analysisPoints.Count == 0)) return;
            var rangeComplex = rangeComplexes.SelectedRangeComplex;
            var nemoScenario = nemoFile.Scenario;
            var bathymetry = ((Task<Bathymetry>)environmentTasks[EnvironmentDataType.Bathymetry]).Result;
            var sediment = ((Task<Sediment>)environmentTasks[EnvironmentDataType.Sediment]).Result;
            var bottomLossSamples = ((Task<BottomLoss>)environmentTasks[EnvironmentDataType.BottomLoss]).Result.Samples;
            var cassBathymetryFileName = Path.Combine(rangeComplexes.SelectedRangeComplex.BathymetryPath, string.Format("{0}_{1}_bathy.txt", rangeComplexes.SelectedArea.Name, rangeComplexes.SelectedBathymetry.Name));
            if (!File.Exists(cassBathymetryFileName)) bathymetry.ToYXZ(cassBathymetryFileName, -1);
            foreach (var timePeriod in timePeriods)
            {
                var cassEnvironmentFileName = Path.Combine(rangeComplexes.SelectedRangeComplex.EnvironmentPath, string.Format("{0}_{1}_env_{2}", rangeComplexes.SelectedArea.Name, rangeComplexes.SelectedBathymetry.Name, timePeriod));
                var curTimePeriod = (TimePeriod)Enum.Parse(typeof (TimePeriod), timePeriod, true);
                var soundspeedField = ((Task<SoundSpeed>)environmentTasks[EnvironmentDataType.SoundSpeed]).Result[curTimePeriod];
                var wind = ((Task<Wind>)environmentTasks[EnvironmentDataType.Wind]).Result[curTimePeriod];
                WriteEnvironmentFiles(cassEnvironmentFileName, bathymetry.Samples.GeoRect, sediment, soundspeedField,
                                      wind, cassBathymetryFileName, rangeComplexes.SelectedArea.Name + ".ovr",
                                      bottomLossSamples);
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
                                        WriteAcousticSimulatorFiles(curTimePeriodPath, platform, source, mode, modeSources, thisModel, timePeriod, appSettings, nemoFile, cassBathymetryFileName, cassEnvironmentFileName, rangeComplex);
                                        break;
                                    case TransmissionLossAlgorithm.Bellhop:
                                    case TransmissionLossAlgorithm.RAMGEO:
                                        foreach (var curSource in modeSources)
                                        {
                                            var runFile = TransmissionLossRunFile.Create(thisModel, curSource,
                                                                                         nemoFile.Scenario.SimAreaName,
                                                                                         rangeComplexes,
                                                                                         platform.Name, source.Name, mode.Name,
                                                                                         timePeriod, rangeComplex);
                                            var runFileName = Path.Combine(curTimePeriodPath, runFile.Filename);
                                            // todo: If the file already exists, load it and see if it's the same as the one we just
                                            // created.  If it is, don't write a new one.
                                            if (!File.Exists(runFileName)) runFile.Save(runFileName);
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
            foreach (var matchingFile in matchingFiles)
                try
                {
                    File.Delete(matchingFile);
                }
                catch (Exception)
                {
                    continue;
                }
        }

        public static void WriteAcousticSimulatorFiles(string curTimePeriodPath, NemoPSM platform, NemoPSM source, NemoMode mode, IList<SoundSource> soundSources, TransmissionLossAlgorithm simulatorName, string timePeriod, AppSettings appSettings, NemoFile nemoFile, string cassBathymetryFileName, string cassEnvironmentFileName, RangeComplex rangeComplex)
        {
            var nemoScenario = nemoFile.Scenario;

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

            var frequency = Math.Sqrt(mode.LowFrequency * mode.HighFrequency);
            string environmentFileType;
            if (frequency < 1000) environmentFileType = "-lfbl-pe";
            else if (frequency < 1500) environmentFileType = "-lfbl-hfb";
            else if (frequency < 4000) environmentFileType = "-hfbl";
            else environmentFileType = "";
            using (var writer = new StreamWriter(inputFilePath, true))
            {
                writer.WriteLine("*Loadcase");
                writer.WriteLine("#Sim_Area_ID                             ");
                writer.WriteLine("Range Complex                           ,{0}", nemoScenario.SimAreaName);
                writer.WriteLine("Sim Area                                ,{0}", nemoScenario.SimAreaName);
                writer.WriteLine("Event Name                              ,{0}", nemoScenario.EventName);
                writer.WriteLine("Reference Location                      ,{0:0.000} DEG, {1:0.000} DEG", rangeComplex.RangeComplexMetadata.Latitude, rangeComplex.RangeComplexMetadata.Longitude);
                writer.WriteLine("Enviro File                             ,{0}{1}.dat", Path.GetFileName(cassEnvironmentFileName), environmentFileType);
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
                writer.WriteLine("Frequency                               ,{0:0.000} HZ", frequency);
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
                    else throw new ApplicationException("Could not create RAM batch file because the RAM Support JAR file and/or the RAM Executable file options have not been set.  Please check your configuration and try again.");
                    break;
                case TransmissionLossAlgorithm.CASS:
                default:
                    if (!string.IsNullOrEmpty(appSettings.CASSSettings.PythonExecutablePath) && !string.IsNullOrEmpty(appSettings.CASSSettings.PythonScriptPath) && !string.IsNullOrEmpty(appSettings.CASSSettings.CASSExecutablePath))
                        using (var writer = new StreamWriter(batchFilePath))
                            writer.WriteLine("start /wait \"{0}\" \"{1}\" \"{2}\" \"{3}\"", appSettings.CASSSettings.PythonExecutablePath, appSettings.CASSSettings.PythonScriptPath, inputFileName, appSettings.CASSSettings.CASSExecutablePath);
                    else throw new ApplicationException("Could not create CASS batch file because one or more of the Python Executable Path, the Python Script Path, and/or the CASS Executable path options have not been set.  Please check your configuration and try again.");
                    break;
            }
        }

        public static void WriteEnvironmentFiles(string environmentFileName, GeoRect geoRect, Sediment sedimentType,
                                                 SoundSpeedField soundSpeedField, TimePeriodEnvironmentData<WindSample> wind, string bathymetryFileName,
                                                 string overlayFileName, EnvironmentData<BottomLossSample> bottomLossSamples)
        {
            var soundSpeedProfiles = new List<SoundSpeedProfile>();
            var windSamples = new List<WindSample>();
            var sedimentPoints = new List<SedimentSample>();
            var bottomLossPoints = new List<BottomLossSample>();
            var requestedLocations = new List<Geo>();

            double lat, lon;
            for (lon = geoRect.West; lon < geoRect.East; lon += 0.25)
            {
                for (lat = geoRect.South; lat < geoRect.North; lat += 0.25)
                {
                    var curLocation = new Geo(lat, lon);
                    soundSpeedProfiles.Add(soundSpeedField.EnvironmentData.GetNearestPoint(curLocation));
                    windSamples.Add((wind.EnvironmentData.GetNearestPoint(curLocation)));
                    sedimentPoints.Add(sedimentType.Samples.GetNearestPoint(curLocation));
                    requestedLocations.Add(curLocation);
                    if (bottomLossSamples != null && bottomLossSamples.Count > 0) bottomLossPoints.Add(bottomLossSamples.GetNearestPoint(curLocation));
                }
                if ((lat - geoRect.North) < 0.125)
                {
                    var curLocation = new Geo(lat, lon);
                    soundSpeedProfiles.Add(soundSpeedField.EnvironmentData.GetNearestPoint(curLocation));
                    windSamples.Add((wind.EnvironmentData.GetNearestPoint(curLocation)));
                    sedimentPoints.Add(sedimentType.Samples.GetNearestPoint(curLocation));
                    requestedLocations.Add(curLocation);
                    if (bottomLossSamples != null && bottomLossSamples.Count > 0) bottomLossPoints.Add(bottomLossSamples.GetNearestPoint(curLocation));
                }
            }
            if ((lon - geoRect.East) < 0.125)
            {
                for (lat = geoRect.South; lat < geoRect.North; lat += 0.25)
                {
                    var curLocation = new Geo(lat, lon);
                    soundSpeedProfiles.Add(soundSpeedField.EnvironmentData.GetNearestPoint(curLocation));
                    windSamples.Add((wind.EnvironmentData.GetNearestPoint(curLocation)));
                    sedimentPoints.Add(sedimentType.Samples.GetNearestPoint(curLocation));
                    requestedLocations.Add(curLocation);
                    if (bottomLossSamples != null && bottomLossSamples.Count > 0) bottomLossPoints.Add(bottomLossSamples.GetNearestPoint(curLocation));
                }
            }
            WriteEnvironmentFiles(soundSpeedField.TimePeriod, requestedLocations, environmentFileName, sedimentPoints, soundSpeedProfiles, windSamples, bathymetryFileName, overlayFileName, bottomLossPoints);
        }

        public static void WriteEnvironmentFiles(TimePeriod timePeriod, IList<Geo> requestedLocations, string environmentFileName, IList<SedimentSample> sedimentPoints,
                                                 IList<SoundSpeedProfile> soundSpeedProfiles, IList<WindSample> windSamples, string bathymetryFileName,
                                                 string overlayFileName, IList<BottomLossSample> bottomLossPoints)
        {
            WriteEnvironmentFileHeader(timePeriod, requestedLocations, environmentFileName, sedimentPoints, soundSpeedProfiles, windSamples, bathymetryFileName, overlayFileName, "HFEVA", bottomLossPoints);
            if (bottomLossPoints.Count == 0) return;
            WriteEnvironmentFileHeader(timePeriod, requestedLocations, environmentFileName + "-hfbl", sedimentPoints, soundSpeedProfiles, windSamples, bathymetryFileName, overlayFileName, "HFBL", bottomLossPoints);
            WriteEnvironmentFileHeader(timePeriod, requestedLocations, environmentFileName + "-lfbl-hfb", sedimentPoints, soundSpeedProfiles, windSamples, bathymetryFileName, overlayFileName, "LFBL_HFB", bottomLossPoints);
            WriteEnvironmentFileHeader(timePeriod, requestedLocations, environmentFileName + "-lfbl-pe", sedimentPoints, soundSpeedProfiles, windSamples, bathymetryFileName, overlayFileName, "LFBL_PE", bottomLossPoints);
        }

        static void WriteEnvironmentFileHeader(TimePeriod timePeriod, IList<Geo> requestedLocations, string environmentFileName, IList<SedimentSample> sedimentList,
                                         IList<SoundSpeedProfile> soundSpeedList, IList<WindSample> windList, string bathymetryFileName,
                                         string overlayFileName, string model, IList<BottomLossSample> bottomLossList)
        {
            if (File.Exists(environmentFileName + ".dat")) return;
            var isFirstPoint = true;
            using (var envFile = new StreamWriter(environmentFileName + ".dat", false))
            {
                envFile.WriteLine("SOUND SPEED MODEL = TABLE");
                envFile.WriteLine("COMMENT TABLE");
                envFile.WriteLine("Generated by ESME Workbench (Build date: {0})", BuildInformation.BuildDateTime);
                if (!string.IsNullOrEmpty(overlayFileName)) envFile.WriteLine("limitName={0}", overlayFileName);
                if (!string.IsNullOrEmpty(bathymetryFileName)) envFile.WriteLine("bathName={0}", bathymetryFileName);
                envFile.WriteLine("timeFrame={0}", timePeriod);
                envFile.WriteLine("EOT");
                envFile.WriteLine();
                if (sedimentList.Count != soundSpeedList.Count || soundSpeedList.Count != windList.Count || (bottomLossList.Count != 0 && windList.Count != soundSpeedList.Count ))
                    throw new ApplicationException("Data point count mismatch");
                for (var i = 0; i < sedimentList.Count; i++)
                    WriteEnvironmentFileRecord(envFile, sedimentList[i], soundSpeedList[i], windList[i], requestedLocations[i], ref isFirstPoint, model, bottomLossList.Count > 0 ? bottomLossList[i] : null);
            }
        }

        static void WriteEnvironmentFileRecord(TextWriter envFile, EarthCoordinate<SedimentSampleBase> sedimentSample, EarthCoordinate<DepthValuePairs<float>> soundSpeedProfile, 
                                         EarthCoordinate<float> windSample, Geo requestedLocation, ref bool isFirstPoint,
                                         string model, EarthCoordinate<BottomLossData> bottomLossSample)
        {
            // ssp is the nearest actual soundspeed profile to the point that's been requested.  The actual profiles are not laid out
            // in a grid, rather they are placed at quarter-degree grid points where there is water.  So grid points on land typically won't
            // have a profile associated with them.  CASS, however, wants a regularly-spaced grid so we tell CASS that every grid point
            // has a profile.  The roundedLat and roundedLon variables is the grid points we report to CASS, regardless of where the actual
            // profile data came from.
            var roundedLat = Math.Round(requestedLocation.Latitude * 4) / 4;
            var roundedLon = Math.Round(requestedLocation.Longitude * 4) / 4;

            if (soundSpeedProfile.Data.Count == 0) return;
            envFile.WriteLine(isFirstPoint ? "RESET ENVIRONMENT NUMBER" : "INCREMENT ENVIRONMENT NUMBER");
            envFile.WriteLine("COMMENT TABLE");
            envFile.WriteLine("Requested Latitude: {0:0.0000} Longitude: {1:0.0000}", requestedLocation.Latitude,
                              requestedLocation.Longitude);
            envFile.WriteLine("Actual Profile Latitude: {0:0.0000} Longitude: {1:0.0000}", soundSpeedProfile.Latitude, soundSpeedProfile.Longitude);
            envFile.WriteLine("Synthesized Latitude: {0:0.0000} Longitude: {1:0.0000}", roundedLat, roundedLon);
            envFile.WriteLine("EOT");
            envFile.WriteLine("ENVIRONMENT LATITUDE  = {0:0.0###} DEG", roundedLat);
            envFile.WriteLine("ENVIRONMENT LONGITUDE = {0:0.0###} DEG", roundedLon);
            envFile.WriteLine("OCEAN SOUND SPEED TABLE");
            envFile.WriteLine("M         M/S       ");
            foreach (var datum in soundSpeedProfile.Data)
                if (!float.IsNaN(datum.Value)) envFile.WriteLine("{0,-10:0.000}{1,-10:0.000}", datum.Depth, datum.Value);
                else break;
            envFile.WriteLine("EOT");
            model = model.ToUpper();
            envFile.WriteLine("BOTTOM REFLECTION COEFFICIENT MODEL   = {0}", model);
            if (model == "HFEVA")
            {
                var findResult = BottomSedimentTypeTable.CASSMap.Find(mapEntry => (int)mapEntry.Value == (int)sedimentSample.Data.SampleValue);
                var sedimentTypeName = findResult == null ? "SAND" : findResult.Name;
                envFile.WriteLine(sedimentTypeName);
            }

            if (bottomLossSample != null)
            {
                if (model == "HFBL" || model == "LFBL_HFB") envFile.WriteLine("BOTTOM PROVINCE                       = {0}", bottomLossSample.Data.CurveNumber);
                if (model == "LFBL_HFB" || model == "LFBL_PE")
                {
                    envFile.WriteLine("BOTTOM-TO-WATER SOUND SPEED RATIO        = {0}", bottomLossSample.Data.RATIOD);
                    envFile.WriteLine("THIN LAYER THICKNESS                     = {0}", bottomLossSample.Data.DLD);
                    envFile.WriteLine("THIN LAYER SEDIMENT DENSITY              = {0}", bottomLossSample.Data.RHOLD);
                    envFile.WriteLine("SEDIMENT-SURFACE DENSITY                 = {0}", bottomLossSample.Data.RHOSD);
                    envFile.WriteLine("SOUND-SPEED GRADIENT AT TOP OF SEDIMENT  = {0}", bottomLossSample.Data.GD);
                    envFile.WriteLine("BETA SOUND-SPEED CURVATURE PARAMETER     = {0}", bottomLossSample.Data.BETAD);
                    envFile.WriteLine("ATTENUATION--LOSS AT TOP OF SEDIMENT     = {0}", bottomLossSample.Data.FKZD);
                    envFile.WriteLine("ATTENUATION GRADIENT                     = {0}", bottomLossSample.Data.FKZP);
                    envFile.WriteLine("FREQUENCY EXPONENT                       = {0}", bottomLossSample.Data.FEXP);
                    envFile.WriteLine("BASEMENT REFLECTION LOSS                 = {0}",
                                      -20.0 * Math.Log10(bottomLossSample.Data.BRFLD));
                    envFile.WriteLine("TWO WAY TRAVEL TIME TO GEOLOGIC BASEMENT = {0}", bottomLossSample.Data.SEDTHK_S);
                }
            }

            //var sedimentSample = sedimentType.Samples[location];
            //var sedimentTypeName = BottomSedimentTypeTable.Lookup(sedimentSample.Data.SampleValue).ToUpper();
            envFile.WriteLine("WIND SPEED                            = {0:0.###} KNOTS", windSample.Data * 1.94384449);
            envFile.WriteLine();
            isFirstPoint = false;
        }
    }
}
