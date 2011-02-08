using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.Model;
using HRC.Navigation;

namespace ESME.TransmissionLoss.CASS
{
    public static class CASSFiles
    {
        public static void GenerateSimAreaData(string simAreaPath, string extractedDataPath, float north, float south, float east, float west)
        {
            var bathyFiles = Directory.GetFiles(extractedDataPath, "bathymetry-*.chb");
            var sedimentFiles = Directory.GetFiles(extractedDataPath, "sediment-*.chb");
            var windFiles = Directory.GetFiles(extractedDataPath, "*-wind.txt");
            var soundspeedFiles = Directory.GetFiles(extractedDataPath, "*-soundspeed.xml");
            //var salinityFiles = Directory.GetFiles(extractedDataPath, "*-salinity.xml");
            //var watertempFiles = Directory.GetFiles(extractedDataPath, "*-water_temp.xml");

            Environment2DData bathymetry = null;
            var selectedBathyFile = LargestFileInList(bathyFiles);
            if (selectedBathyFile == null) throw new ApplicationException("No bathymetry files were found, the operation cannot proceed");
            if (selectedBathyFile.EndsWith(".eeb")) bathymetry = new Environment2DData(selectedBathyFile, "bathymetry", north, west, south, east);
            else if (selectedBathyFile.EndsWith(".chb")) bathymetry = Environment2DData.ReadChrtrBinaryFile(selectedBathyFile, -1);

            Sediment sediment = null;
            var selectedSedimentFile = LargestFileInList(sedimentFiles);
            if (selectedSedimentFile == null) throw new ApplicationException("No sediment files were found, the operation cannot proceed");
            if (selectedSedimentFile.EndsWith(".eeb")) sediment = Sediment.ReadESMEEnvironmentBinaryFile(selectedSedimentFile, north, south, east, west);
            else if (selectedSedimentFile.EndsWith(".chb")) sediment = Sediment.ReadChrtrBinaryFile(selectedSedimentFile);

            if (windFiles.Length == 0) throw new ApplicationException("No wind files were found, the operation cannot proceed");
            var windTimePeriods = (from windFile in windFiles
                                   select Path.GetFileNameWithoutExtension(windFile)
                                   into fileName where fileName != null select fileName.Split(new[]
                                                                                              {
                                                                                                  '-'
                                                                                              })[0].ToLower()).ToList();

            if (soundspeedFiles.Length == 0) throw new ApplicationException("No soundspeed files were found, the operation cannot proceed");
            var soundspeedTimePeriods = (from soundspeedFile in soundspeedFiles
                                         select Path.GetFileNameWithoutExtension(soundspeedFile)
                                         into fileName where fileName != null select fileName.Split(new[]
                                                                                                    {
                                                                                                        '-'
                                                                                                    })[0].ToLower()).ToList();

            var allTimePeriods = new List<string>();
            allTimePeriods.AddRange(windTimePeriods);
            allTimePeriods.AddRange(soundspeedTimePeriods);

            foreach (var timePeriod in allTimePeriods.Distinct())
            {
                var windFileName = Path.Combine(extractedDataPath, timePeriod + "-wind.txt");
                var soundspeedFileName = Path.Combine(extractedDataPath, timePeriod + "-soundspeed.xml");
                var watertempFileName = Path.Combine(extractedDataPath, timePeriod + "-water_temp.xml");
                var salinityFileName = Path.Combine(extractedDataPath, timePeriod + "-salinity.xml");
                var environmentFileName = Path.Combine(Path.Combine(simAreaPath, "Environment"), "env_" + timePeriod.ToLower() + ".dat");
                try
                {
                    CheckWriteEnvironmentFile(timePeriod, environmentFileName, bathymetry, sediment, windFileName, soundspeedFileName, watertempFileName, salinityFileName, north, south, east, west);
                }
                catch (Exception ex)
                {
                    Console.WriteLine(@"Error creating CASS environmental data for " + timePeriod + @":");
                    Console.WriteLine(ex.Message);
                }
            }
        }

        static string LargestFileInList(IEnumerable<string> files)
        {
            long largestFileSize = 0;
            string largestFileName = null;
            foreach (var file in files)
            {
                var curFileInfo = new FileInfo(file);
                if (curFileInfo.Length > largestFileSize)
                {
                    largestFileSize = curFileInfo.Length;
                    largestFileName = file;
                }
            }
            return largestFileName;
        }

        public static void WriteBathymetryFile(string bathymetryFileName, Environment2DData bathymetry)
        {
            bathymetry.SaveToYXZ(bathymetryFileName, 1.0f);
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

        static void CheckWriteEnvironmentFile(string timePeriodName, string environmentFileName, Environment2DData bathymetry, Sediment sediment, string windFileName, string soundspeedFileName, string watertempFileName, string salinityFileName, float north, float south, float east, float west)
        {
            if (string.IsNullOrEmpty(environmentFileName) || string.IsNullOrEmpty(windFileName) || string.IsNullOrEmpty(soundspeedFileName) || string.IsNullOrEmpty(watertempFileName) || string.IsNullOrEmpty(salinityFileName) || (bathymetry == null) || (sediment == null)) 
                throw new ApplicationException("One or more files were not specified.");
            var environmentPath = Path.GetDirectoryName(environmentFileName);
            if (string.IsNullOrEmpty(environmentPath)) throw new ApplicationException("Invalid path provided for environment file");
            if (!Directory.Exists(environmentPath) || !File.Exists(windFileName) || !File.Exists(soundspeedFileName) || !File.Exists(watertempFileName) || !File.Exists(salinityFileName))
                throw new ApplicationException("One or more files were not found");
            Environment2DData wind = null;
            if (windFileName.EndsWith(".eeb")) wind = new Environment2DData(windFileName, "windspeed", north, west, south, east);
            else if (windFileName.EndsWith(".txt")) wind = SurfaceMarineGriddedClimatologyDatabase.Parse(windFileName);
            if (wind == null) throw new ApplicationException("Error reading wind data");

            SoundSpeedField soundSpeedField = null;
            if (soundspeedFileName.EndsWith(".eeb")) soundSpeedField = new SoundSpeedField(soundspeedFileName, north, west, south, east);
            else if (soundspeedFileName.EndsWith(".xml"))
            {
                var rawSoundSpeeds = SerializedOutput.Load(soundspeedFileName, null);
                soundSpeedField = new SoundSpeedField(rawSoundSpeeds, timePeriodName);
                soundSpeedField.ExtendProfilesToDepth(bathymetry.MaxValue, SerializedOutput.Load(watertempFileName, null), SerializedOutput.Load(salinityFileName, null));
            }

            WriteEnvironmentFile(environmentFileName, bathymetry, sediment, soundSpeedField, wind);
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
                foreach (var latitude in latitudes)
                    foreach (var longitude in longitudes)
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
                        float bottomTypeIndex = 0;
                        //envFile.WriteLine(sedimentType.ClosestTo(location, ref bottomTypeIndex) ? BottomSedimentTypeTable.Lookup((int)bottomTypeIndex).ToUpper() : "UNKNOWN");
                        float windSpeedValue = 0;
                        envFile.WriteLine("WIND SPEED                            = {0} KNOTS", windSpeed.ClosestTo(location, ref windSpeedValue) ? windSpeedValue : 0.0f);
                        envFile.WriteLine();
                    }
            }
        }
    }
}