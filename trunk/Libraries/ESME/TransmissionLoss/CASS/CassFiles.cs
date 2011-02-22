using System;
using System.Collections.Generic;
using System.Data;
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
        public static void GenerateSimAreaData(string simAreaPath, string extractedDataPath, string timePeriodName, float north, float south, float east, float west)
        {
            var bathyFiles = Directory.GetFiles(extractedDataPath, "bathymetry-*.chb");
            var sedimentFiles = Directory.GetFiles(extractedDataPath, "sediment-*.chb");
            var windFile = Path.Combine(extractedDataPath, string.Format("{0}-wind.txt", timePeriodName));
            var soundspeedFile = Path.Combine(extractedDataPath, string.Format("{0}-soundspeed.xml", timePeriodName));

            Environment2DData bathymetry = null;
            var selectedBathyFile = LargestFileInList(bathyFiles);
            if (selectedBathyFile == null) throw new ApplicationException("No bathymetry files were found, the operation cannot proceed");
            if (selectedBathyFile.EndsWith(".eeb")) bathymetry = Environment2DData.FromEEB(selectedBathyFile, "bathymetry", north, west, south, east);
            else if (selectedBathyFile.EndsWith(".chb")) bathymetry = Environment2DData.FromCHB(selectedBathyFile, -1);

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