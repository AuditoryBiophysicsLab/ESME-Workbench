using System;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;
using ESME.Data;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.NEMO;
using HRC.Navigation;

namespace ESME.TransmissionLoss.CASS
{
    public static class CASSFiles
    {
        public static void WriteAcousticSimulatorFiles(AppSettings appSettings, IEnumerable<string> timePeriods, IList<AnalysisPoint> analysisPoints, NemoFile nemoFile, string cassBathymetryFileName, NemoModeToAcousticModelNameMap modeToAcousticModelNameMap, float maxDepth)
        {
            if ((analysisPoints == null) || (analysisPoints.Count == 0)) return;
            maxDepth = Math.Abs(maxDepth);
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
                                        WriteAcousticSimulatorFiles(curTimePeriodPath, platform, source, mode, modeSources, thisModel, timePeriod, appSettings, nemoFile, "bathymetry.txt", maxDepth);
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

        static void WriteAcousticSimulatorFiles(string curTimePeriodPath, NemoPSM platform, NemoPSM source, NemoMode mode, IList<SoundSource> soundSources, TransmissionLossAlgorithm simulatorName, string timePeriod, AppSettings appSettings, NemoFile nemoFile, string cassBathymetryFileName, float maxDepth)
        {
            var nemoScenario = nemoFile.Scenario;
            var simAreaFile = SimAreaCSV.ReadCSV(Path.Combine(appSettings.ScenarioDataDirectory, "SimAreas.csv"));
            var simAreaData = simAreaFile[nemoScenario.SimAreaName];

            var inputFileName = string.Format("base-{0}-{1}-{2}-{3}.inp", simulatorName, platform.Name, platform.Id, timePeriod);
            var batchFileName = "run_" + Path.GetFileNameWithoutExtension(inputFileName) + ".bat";
            var inputFilePath = Path.Combine(curTimePeriodPath, inputFileName);
            var batchFilePath = Path.Combine(curTimePeriodPath, batchFileName);

            // If the file does not yet exist, we need to write the header
            if (!File.Exists(inputFilePath))
            {
                using (var writer = new StreamWriter(inputFilePath))
                {
                    writer.WriteLine("# analyst name: {0}", System.Environment.UserName);
                    writer.WriteLine("# creation date: {0}", DateTime.Now);
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
                        writer.WriteLine("Speed Dial                              ,{0}", appSettings.RAMSettings.SpeedDial);
                        writer.WriteLine("SSP Units                               ,{0}", appSettings.RAMSettings.SSPUnits);
                        writer.WriteLine("Cass Level                              ,{0}", appSettings.RAMSettings.CASSLevel);
                        writer.WriteLine("Bathymetry Metric                       ,{0}", appSettings.RAMSettings.BathymetryMetric);
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
                writer.WriteLine("Reference Location                      ,{0:0.000} DEG, {1:0.000} DEG", simAreaData.Latitude, simAreaData.Longitude);
                float stepCount;
                float stepSize;
                switch (simulatorName)
                {
                    case TransmissionLossAlgorithm.RAM:
                        writer.WriteLine("Enviro File                             ,env_{0}-lfbl-pe.dat", timePeriod.ToLower());
                        stepCount = appSettings.CASSSettings.MaximumDepth / appSettings.RAMSettings.DepthStepSize;
                        if (stepCount > 21) stepSize = appSettings.CASSSettings.MaximumDepth / 21;
                        else stepSize = appSettings.RAMSettings.DepthStepSize;
                        writer.WriteLine("Water Depth                             ,0 M, {0} M, {1} M", appSettings.RAMSettings.MaximumDepth, stepSize);
                        break;
                    case TransmissionLossAlgorithm.CASS:
                    default:
                        writer.WriteLine("Enviro File                             ,env_{0}.dat", timePeriod.ToLower());
                        stepCount = appSettings.CASSSettings.MaximumDepth / appSettings.CASSSettings.DepthStepSize;
                        if (stepCount > 1024) stepSize = appSettings.CASSSettings.MaximumDepth / 1024;
                        else stepSize = appSettings.CASSSettings.DepthStepSize;
                        writer.WriteLine("Water Depth                             ,0 M, {0} M, {1} M", appSettings.CASSSettings.MaximumDepth, stepSize);
                        break;
                }
                writer.WriteLine("Bathy File                              ,{0}", cassBathymetryFileName);
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
                        stepCount = mode.Radius / appSettings.RAMSettings.RangeStepSize;
                        if (stepCount > 1024) stepSize = mode.Radius / 1024;
                        else stepSize = appSettings.RAMSettings.RangeStepSize;
                        writer.WriteLine("Range Distance                          ,{0} M, {1} M, {0} M", stepSize, mode.Radius);
                        break;
                    case TransmissionLossAlgorithm.CASS:
                    default:
                        stepCount = mode.Radius / appSettings.CASSSettings.RangeStepSize;
                        if (stepCount > 1024) stepSize = mode.Radius / 1024;
                        else stepSize = appSettings.CASSSettings.RangeStepSize;
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
                case TransmissionLossAlgorithm.CASS:
                default:
                    if (!string.IsNullOrEmpty(appSettings.CASSSettings.PythonExecutablePath) && !string.IsNullOrEmpty(appSettings.CASSSettings.PythonScriptPath) && !string.IsNullOrEmpty(appSettings.CASSSettings.CASSExecutablePath)) 
                        using (var writer = new StreamWriter(batchFilePath)) 
                            writer.WriteLine("start /wait \"{0}\" \"{1}\" \"{2}\" \"{3}\"", appSettings.CASSSettings.PythonExecutablePath, appSettings.CASSSettings.PythonScriptPath, inputFileName, appSettings.CASSSettings.CASSExecutablePath);
                    break;
            }
        }

        public static void WriteEnvironmentFile(string environmentFileName, Bathymetry bathymetry, Sediment sedimentType, SoundSpeedField soundSpeedField, TimePeriodEnvironmentData<WindSample> wind)
        {
            var isFirstPoint = true;
            using (var envFile = new StreamWriter(environmentFileName, false))
            {
                envFile.WriteLine("SOUND SPEED MODEL = TABLE");
                envFile.WriteLine("COMMENT TABLE");
                envFile.WriteLine("Generated by ESME WorkBench (Build date: {0})", BuildInformation.BuildDateTime);
                envFile.WriteLine("timeFrame={0}", soundSpeedField.TimePeriod);
                envFile.WriteLine("EOT");
                envFile.WriteLine();

                double lat, lon;

                var selectedArea = bathymetry.Samples.GeoRect;
                for (lon = selectedArea.West; lon < selectedArea.East; lon += 0.25)
                {
                    for (lat = selectedArea.South; lat < selectedArea.North; lat += 0.25)
                        WriteEnvironmentFile(envFile, sedimentType, soundSpeedField, wind, new EarthCoordinate(lat, lon), ref isFirstPoint);
                    if ((lat - selectedArea.North) < 0.125)
                        WriteEnvironmentFile(envFile, sedimentType, soundSpeedField, wind, new EarthCoordinate(selectedArea.North, lon), ref isFirstPoint);
                }
                if ((lon - selectedArea.East) < 0.125)
                    for (lat = selectedArea.South; lat < selectedArea.North; lat += 0.25)
                        WriteEnvironmentFile(envFile, sedimentType, soundSpeedField, wind, new EarthCoordinate(lat, selectedArea.East), ref isFirstPoint);
            }
        }

        static void WriteEnvironmentFile(TextWriter envFile, Sediment sediment, TimePeriodEnvironmentData<SoundSpeedProfile> soundSpeedField, TimePeriodEnvironmentData<WindSample> wind, EarthCoordinate requestedLocation, ref bool isFirstPoint)
        {
            var ssp = soundSpeedField.EnvironmentData[requestedLocation];
            if (ssp.Data.Count == 0) return;
            if (isFirstPoint) envFile.WriteLine("RESET ENVIRONMENT NUMBER");
            else envFile.WriteLine("INCREMENT ENVIRONMENT NUMBER");
            envFile.WriteLine("COMMENT TABLE");
            envFile.WriteLine("Requested Latitude: {0:0.0000} Longitude: {1:0.0000}", requestedLocation.Latitude, requestedLocation.Longitude);
            envFile.WriteLine("Returned Latitude: {0:0.0000} Longitude: {1:0.0000}", ssp.Latitude, ssp.Longitude);
            envFile.WriteLine("EOT");
            envFile.WriteLine("ENVIRONMENT LATITUDE  = {0:0.0###} DEG", ssp.Latitude);
            envFile.WriteLine("ENVIRONMENT LONGITUDE = {0:0.0###} DEG", ssp.Longitude);
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

        public static List<CASSPacket> ReadEnvironmentFile(string environmentFileName)
        {
            var resarray = File.ReadAllLines(environmentFileName).ToList();
            var rawvalues = new List<List<string>>();
            var curLineIndex = 0;
            //make packets
            while (curLineIndex < resarray.Count)
            {
                var thisline = resarray[curLineIndex++].Trim();
                if (curLineIndex >= resarray.Count) break;
                //if the line starts with 'ENVIRONMENT LATITUDE', add it plus everything up to the next blank line to rawvalues[i].
                if (thisline.StartsWith("ENVIRONMENT LATITUDE"))
                {
                    var curGroup = new List<string>
                                   {
                                       thisline.Trim()
                                   };
                    while (!string.IsNullOrEmpty(thisline = resarray[curLineIndex++]))
                    {
                        if (curLineIndex >= resarray.Count) break;
                        curGroup.Add(thisline.Trim());
                    }
                    rawvalues.Add(curGroup);
                }
            }

            var result = new List<CASSPacket>();
            foreach (var packet in rawvalues)
            {
                var retpacket = new CASSPacket
                                {
                                    Filename = environmentFileName
                                };
                double lat,
                       lon;
                if (!double.TryParse(packet[0].Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries)[3], out lat)) throw new DataMisalignedException("");
                if (!double.TryParse(packet[1].Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries)[3], out lon)) throw new DataMisalignedException("");
                retpacket.Location = new EarthCoordinate(lat, lon);
                var curGroupLineIndex = 0;
                while (curGroupLineIndex < packet.Count)
                {
                    var curLine = packet[curGroupLineIndex++].Trim();
                    if (curLine.StartsWith("M"))
                    {
                        var index = curGroupLineIndex;
                        var depths = new List<double>();
                        var speeds = new List<double>();
                        while (!packet[index].Contains("EOT"))
                        {
                            double depth,
                                   speed;
                            if (!double.TryParse(packet[index].Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries)[0], out depth) ||
                                !double.TryParse(packet[index].Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries)[1], out speed)) throw new DataException("");
                            depths.Add(depth);
                            speeds.Add(speed);
                            index++;
                        }
                        retpacket.Depths = depths;
                        retpacket.Soundspeeds = speeds;
                    }
                    if (curLine.StartsWith("BOTTOM REFLECTION"))
                    {
                        var bottom = packet[curGroupLineIndex++];
                        if (bottom.Contains("WIND SPEED"))
                        {
                            double wind;
                            if (!double.TryParse(bottom.Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries)[3], out wind)) throw new DataException("");
                            retpacket.WindSpeed = wind;
                        }
                        else
                        {
                            retpacket.BottomType = bottom;
                            var speed = packet[curGroupLineIndex];
                            double wind;
                            if (!double.TryParse(speed.Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries)[3], out wind)) throw new DataException("");
                            retpacket.WindSpeed = wind;

                        }

                    }
                }
                result.Add(retpacket);
            }
            return result;
        }
    }
}
