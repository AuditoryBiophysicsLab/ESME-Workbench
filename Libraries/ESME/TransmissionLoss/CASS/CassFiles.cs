using System;
using System.Collections;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;
using ESME.Data;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.Model;
using ESME.NEMO;
using HRC.Navigation;

namespace ESME.TransmissionLoss.CASS
{
    public static class CASSFiles
    {
        public static void GenerateSimAreaData(string simAreaPath, string extractedDataPath, string timePeriodName, Environment2DData bathymetry, float north, float south, float east, float west)
        {
            var sedimentFiles = Directory.GetFiles(extractedDataPath, "sediment-*.chb");
            var windFile = Path.Combine(extractedDataPath, string.Format("{0}-wind.txt", timePeriodName));
            var soundspeedFile = Path.Combine(extractedDataPath, string.Format("{0}-soundspeed.xml", timePeriodName));

            Sediment sediment = null;
            var selectedSedimentFile = LargestFileInList(sedimentFiles);
            if (selectedSedimentFile == null) throw new ApplicationException("No sediment files were found, the operation cannot proceed");
            //if (selectedSedimentFile.EndsWith(".eeb")) sediment = Sediment.ReadESMEEnvironmentBinaryFile(selectedSedimentFile, north, south, east, west);
            //else if (selectedSedimentFile.EndsWith(".chb")) sediment = Sediment.ReadChrtrBinaryFile(selectedSedimentFile);
            if (selectedSedimentFile.EndsWith(".chb")) sediment = Sediment.FromSedimentCHB(selectedSedimentFile);

            Environment2DData wind = null;
            if (windFile.EndsWith(".eeb")) wind = Environment2DData.FromEEB(windFile, "windspeed", north, west, south, east);
            else if (windFile.EndsWith(".txt")) wind = SurfaceMarineGriddedClimatologyDatabase.Parse(windFile);
            if (wind == null) throw new ApplicationException("Error reading wind data");

            SoundSpeedField soundSpeedField = null;
            if (soundspeedFile.EndsWith(".eeb")) soundSpeedField = new SoundSpeedField(soundspeedFile, north, west, south, east);
            else if (soundspeedFile.EndsWith(".xml"))
            {
                var rawSoundSpeeds = SerializedOutput.Load(soundspeedFile, null);
                soundSpeedField = new SoundSpeedField(rawSoundSpeeds, timePeriodName);
            }

            var environmentFileName = Path.Combine(Path.Combine(simAreaPath, "Environment"), "env_" + timePeriodName.ToLower() + ".dat");
            WriteEnvironmentFile(environmentFileName, bathymetry, sediment, soundSpeedField, wind);
        }

        static string LargestFileInList(IEnumerable<string> files)
        {
            long largestFileSize = 0;
            string largestFileName = null;
            foreach (var file in files)
            {
                var curFileInfo = new FileInfo(file);
                if (curFileInfo.Length <= largestFileSize) continue;
                largestFileSize = curFileInfo.Length;
                largestFileName = file;
            }
            return largestFileName;
        }

        public static void WriteBathymetryFile(string bathymetryFileName, Environment2DData bathymetry)
        {
            bathymetry.SaveToYXZ(bathymetryFileName, -1.0f);
#if false
            using (var bathyFile = new StreamWriter(bathymetryFileName, false))
            {
                bathyFile.WriteLine("BOTTOM DEPTH TABLE");
                bathyFile.WriteLine("DEG       DEG       M         ");
                for (var lat = _environment.Bathymetry.Latitudes.Length - 1; lat >= 0; lat--) for (var lon = 0; lon < _environment.Bathymetry.Longitudes.Length; lon++) bathyFile.WriteLine("{0,-10:0.0000}{1,-10:0.0000}{2,-10:0.0000}", _environment.Bathymetry.Latitudes[lat], _environment.Bathymetry.Longitudes[lon], -_environment.Bathymetry.Values[lat, lon]);
                bathyFile.WriteLine("EOT");
            }
#endif
        }

        public static void WriteCASSInputFiles(AppSettings appSettings, IList<string> timePeriods, IList<AnalysisPoint> analysisPoints, NemoFile nemoFile, string cassBathymetryFileName)
        {
            
            var nemoScenario = nemoFile.Scenario;
            var simAreaFile = SimAreaCSV.ReadCSV(Path.Combine(appSettings.ScenarioDataDirectory, "SimAreas.csv"));
            var simAreaData = simAreaFile[nemoScenario.SimAreaName];
            foreach (var timePeriod in timePeriods)
            {
                var curScenarioDataPath = Path.GetDirectoryName(nemoFile.FileName);
                var curPropagationPath = Path.Combine(curScenarioDataPath, "Propagation");
                var curTimePeriodPath = Path.Combine(curPropagationPath, timePeriod);
                if (!Directory.Exists(curTimePeriodPath)) Directory.CreateDirectory(curTimePeriodPath);
                foreach (var platform in nemoScenario.Platforms)
                {
                    var inputFileName = string.Format("base-cass-{0}-{1}-{2}.inp", platform.Name, platform.Id, timePeriod);
                    var batchFileName = string.Format("run_base-cass-{0}-{1}-{2}.bat", platform.Name, platform.Id, timePeriod);
                    var inputFilePath = Path.Combine(curTimePeriodPath, inputFileName);
                    var batchFilePath = Path.Combine(curTimePeriodPath, batchFileName);
                    foreach (var source in platform.Sources)
                    {
                        var sourceToModes = new OneToMany<NemoSource, List<OneToMany<NemoMode, List<SoundSource>>>>(source);
                        foreach (var mode in source.Modes)
                        {
                            var modeToSoundSources = new OneToMany<NemoMode, List<SoundSource>>(mode);
                            sourceToModes.Many.Add(modeToSoundSources);
                            foreach (var analysisPoint in analysisPoints)
                            {
                                foreach (var soundSource in analysisPoint.SoundSources)
                                {
                                    var psmFields = soundSource.Name.Split('|');
                                    var soundSourcePlatform = psmFields[0];
                                    var soundSourceSource = psmFields[1];
                                    var soundSourceMode = psmFields[2];
                                    if ((platform.Name.ToLower() == soundSourcePlatform.ToLower()) && 
                                        (source.Name.ToLower() == soundSourceSource.ToLower()) && 
                                        (mode.Name.ToLower() == soundSourceMode.ToLower()))
                                    {
                                        modeToSoundSources.Many.Add(soundSource);
                                    }
                                } // end loop over all sound sources in the current analysis point
                            } // end loop over all analysis points
                        } // end loop over modes in the current source
                        using (var writer = new StreamWriter(inputFilePath))
                        {
                            writer.WriteLine("# analyst name: {0}", System.Environment.UserName);
                            writer.WriteLine("# creation date: {0}", DateTime.Now);
                            writer.WriteLine();
                            writer.WriteLine("*System Parms");
                            writer.WriteLine("Plot Files,{0}", appSettings.CASSSettings.GeneratePlotFiles ? "y" : "n");
                            writer.WriteLine("Binary Files,{0}", appSettings.CASSSettings.GenerateBinaryFiles ? "y" : "n");
                            writer.WriteLine("Pressure Files,{0}", appSettings.CASSSettings.GeneratePressureFiles ? "y" : "n");
                            //writer.WriteLine("Eigenray Files,{0}", appSettings.CASSSettings.GenerateEigenrayFiles ? "y" : "n");
                            writer.WriteLine("Data Directory,{0}", appSettings.ScenarioDataDirectory);
                            writer.WriteLine("*End System Parms");
                            writer.WriteLine();
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
                            writer.WriteLine();
                            writer.WriteLine();

                            foreach (var mode in sourceToModes.Many)
                            {
                                writer.WriteLine("*Loadcase");
                                writer.WriteLine("#Sim_Area_ID                             ");
                                writer.WriteLine("Range Complex                           ,{0}", nemoScenario.SimAreaName);
                                writer.WriteLine("Sim Area                                ,{0}", nemoScenario.SimAreaName);
                                writer.WriteLine("Event Name                              ,{0}", nemoScenario.EventName);
                                writer.WriteLine("Reference Location                      ,{0:0.000} DEG, {1:0.000} DEG", simAreaData.Latitude, simAreaData.Longitude);
                                writer.WriteLine("Enviro File                             ,env_{0}.dat", timePeriod.ToLower());
                                writer.WriteLine("Bathy File                              ,{0}", cassBathymetryFileName);
                                writer.WriteLine("Water Depth                             ,0 M, {0} M, {1} M", appSettings.CASSSettings.MaximumDepth, appSettings.CASSSettings.DepthStepSize);
                                writer.WriteLine("Season                                  ,{0}", timePeriod);
                                writer.Write("Radial Angles                           ");

                                foreach (var radial in mode.Many[0].RadialBearings)
                                    writer.Write(",{0}", radial);
                                writer.WriteLine();

                                writer.WriteLine("#PSM_ID                                  ");
                                writer.WriteLine("Platform Name                           ,{0}", platform.Name);
                                writer.WriteLine("Source Name                             ,{0}", source.Name);
                                writer.WriteLine("Mode Name                               ,{0}", mode.One.Name);
                                writer.WriteLine("Frequency                               ,{0:0.000} HZ", Math.Sqrt(mode.One.LowFrequency * mode.One.HighFrequency));
                                writer.WriteLine("DE Angle                                ,{0:0.000} DEG", mode.One.DepressionElevationAngle);
                                writer.WriteLine("Vertical Beam                           ,{0:0.000} DEG", mode.One.VerticalBeamWidth);
                                writer.WriteLine("Source Depth                            ,{0:0.000} M", mode.One.SourceDepth);
                                writer.WriteLine("SOURCE LEVEL                            ,{0:0.000} DB", mode.One.SourceLevel);
                                writer.WriteLine("Range Distance                          ,{0} M, {1} M, {0} M", appSettings.CASSSettings.RangeStepSize, mode.One.Radius);

                                foreach (var soundSource in mode.Many)
                                    writer.WriteLine("Source Location                         ,{0:0.000} DEG, {1:0.000} DEG", soundSource.Latitude, soundSource.Longitude);
                                writer.WriteLine("*end loadcase");
                                writer.WriteLine();
                            } // end of loop over all modes in the current source
                        } // end of using block that writes the input file
                        // Write the crufty batch file for the NUWC guys
                        if (!string.IsNullOrEmpty(appSettings.CASSSettings.PythonExecutablePath) && !string.IsNullOrEmpty(appSettings.CASSSettings.PythonScriptPath) && !string.IsNullOrEmpty(appSettings.CASSSettings.CASSExecutablePath))
                            using (var writer = new StreamWriter(batchFilePath))
                                writer.WriteLine("start /wait \"{0}\" \"{1}\" \"{2}\" \"{3}\"", appSettings.CASSSettings.PythonExecutablePath, appSettings.CASSSettings.PythonScriptPath, inputFileName, appSettings.CASSSettings.CASSExecutablePath);
                    } // end loop over all sources on the current platform
                } // end loop over all platforms in the scenario
            } // end loop over all time periods we're generating CASS input files for
        } // end of WriteCASSInputFiles

        internal class OneToMany<TOne, TMany> where TMany : IList, new()
        {
            public OneToMany(TOne one)
            {
                One = one;
                Many = new TMany();
            }

            internal TOne One { get; private set; }
            internal TMany Many { get; private set; }
        }

        internal class PlatformSoundSourceCollection
        {
            public PlatformSoundSourceCollection(NemoPlatform nemoPlatform)
            {
                NemoPlatform = nemoPlatform;
                SoundSources = new List<SoundSource>();
            }
            internal NemoPlatform NemoPlatform { get; private set; }
            internal List<SoundSource> SoundSources { get; private set; }
        }

        public static void WriteEnvironmentFile(string environmentFileName, Environment2DData bathymetry, Sediment sedimentType, SoundSpeedField soundSpeedField, Environment2DData windSpeed)
        {
            using (var envFile = new StreamWriter(environmentFileName, false))
            {
                envFile.WriteLine("SOUND SPEED MODEL = TABLE");
                envFile.WriteLine("COMMENT TABLE");
                envFile.WriteLine("EOT");
                envFile.WriteLine("RESET ENVIRONMENT NUMBER");
                envFile.WriteLine("COMMENT TABLE");
                envFile.WriteLine("EOT");
                envFile.WriteLine();
                var latitudes = soundSpeedField.Latitudes.OrderByDescending(x => x);
                var longitudes = soundSpeedField.Longitudes;
                foreach (var longitude in longitudes)
                    foreach (var latitude in latitudes)
                    {
                        var location = new EarthCoordinate(latitude, longitude);
                        var ssp = soundSpeedField[location];
                        if (ssp.SoundSpeeds.Length == 0) continue;

                        envFile.WriteLine("ENVIRONMENT LATITUDE  = {0:0.0###} DEG", latitude);
                        envFile.WriteLine("ENVIRONMENT LONGITUDE = {0:0.0###} DEG", longitude);
                        envFile.WriteLine("OCEAN SOUND SPEED TABLE");
                        envFile.WriteLine("M         M/S       ");
                        for (var dep = 0; dep < ssp.Depths.Length; dep++)
                            if (!float.IsNaN(ssp.SoundSpeeds[dep])) envFile.WriteLine("{0,-10:0.000}{1,-10:0.000}", soundSpeedField.DeepestSSP.Depths[dep], ssp.SoundSpeeds[dep]);
                            else break;
                        envFile.WriteLine("EOT");
                        envFile.WriteLine("BOTTOM REFLECTION COEFFICIENT MODEL   = HFEVA");
                        var sedimentSample = sedimentType[location];
                        var curSedimentValue = sedimentSample.Data;
                        envFile.WriteLine(curSedimentValue.HasValue ? BottomSedimentTypeTable.Lookup(curSedimentValue.Value).ToUpper() : "UNKNOWN");
                        envFile.WriteLine("WIND SPEED                            = {0:0.###} KNOTS", windSpeed[location].Data * 1.94384449);
                        envFile.WriteLine();
                    }
            }
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
