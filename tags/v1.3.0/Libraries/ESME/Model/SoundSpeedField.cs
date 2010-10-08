using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using ESME.Environment;
using HRC.Navigation;

namespace ESME.Model
{
    public class SoundSpeedField
    {
        public string TimePeriod { get; private set; }
        public SoundSpeedProfile[] SoundSpeedProfiles { get; private set; }
        public SoundSpeedProfile DeepestSSP { get; private set; }

        public SoundSpeedField(string environmentFileName)
        {
            var file = DataFile.Open(environmentFileName);

            TimePeriod = null;
            SoundSpeedProfiles = null;
            var layer = file["soundspeed"];
            if (layer == null) throw new System.IO.FileFormatException(string.Format("SoundSpeedField: Specified environment file \"{0}\"does not contain a soundspeed layer", environmentFileName));
            TimePeriod = layer.TimePeriod;
            var profiles = new List<SoundSpeedProfile>();
            profiles.AddRange(from row in layer.Rows
                              from point in row.Points
                              select new SoundSpeedProfile(TimePeriod, point.EarthCoordinate, layer.DepthAxis.Values, point.Data));
            foreach (var profile in profiles) DeepestSSP = (DeepestSSP != null) ? (DeepestSSP.MaxDepth < profile.MaxDepth ? profile : DeepestSSP) : profile;

            SoundSpeedProfiles = profiles.ToArray();
        }

        public SoundSpeedField(string environmentFileName, float north, float west, float south, float east)
        {
            var file = DataFile.Open(environmentFileName);

            TimePeriod = null;
            SoundSpeedProfiles = null;
            var layer = file["soundspeed"];
            if (layer == null) throw new System.IO.FileFormatException(string.Format("SoundSpeedField: Specified environment file \"{0}\"does not contain a soundspeed layer", environmentFileName));
            TimePeriod = layer.TimePeriod;
            var profiles = new List<SoundSpeedProfile>();
            profiles.AddRange(from row in layer.GetRows(south, north)
                              from point in row.GetPoints(west, east)
                              select new SoundSpeedProfile(TimePeriod, point.EarthCoordinate, layer.DepthAxis.Values, point.Data));

            foreach (var profile in profiles) DeepestSSP = (DeepestSSP != null) ? (DeepestSSP.MaxDepth < profile.MaxDepth ? profile : DeepestSSP) : profile;

            SoundSpeedProfiles = profiles.ToArray();
        }

        public SoundSpeedProfile ExtendSSP(SoundSpeedProfile shallowSSP, float requiredDepth)
        {
            var deltaV = float.NaN;
            int i;

            var extendedSSP = new SoundSpeedProfile
                              {
                                  Depths = DeepestSSP.Depths,
                                  SoundSpeeds = new float[DeepestSSP.Depths.Length]
                              };

            for (i = 0; i < DeepestSSP.Depths.Length; i++)
            {
                if (float.IsNaN(shallowSSP.SoundSpeeds[i])) break;
                deltaV = DeepestSSP.SoundSpeeds[i] - shallowSSP.SoundSpeeds[i];
                extendedSSP.SoundSpeeds[i] = shallowSSP.SoundSpeeds[i];
            }

            if (float.IsNaN(deltaV)) throw new ApplicationException("ExtendSSP: ShallowSSP is empty.");

            for (var j = i; j < DeepestSSP.Depths.Length; j++)
            {
                if (DeepestSSP.Depths[j - 1] <= requiredDepth) // need previous depth to be less than req'd, current is slightly greater
                    extendedSSP.SoundSpeeds[j] = DeepestSSP.SoundSpeeds[j] - deltaV;
                else extendedSSP.SoundSpeeds[j] = float.NaN;
            }
            return extendedSSP;
        }

        public SoundSpeedField(string timePeriod, SoundSpeedProfile[] soundSpeedProfiles)
        {
            TimePeriod = timePeriod;
            SoundSpeedProfiles = soundSpeedProfiles;
        }

        public SoundSpeedProfile this[EarthCoordinate location]
        {
            get
            {
                var query = from p in SoundSpeedProfiles
                            orderby p.Location.GetDistanceTo_Meters(location) ascending
                            select p;

                return query.Count() > 0 ? query.First() : SoundSpeedProfile.Empty;
            }
        }
    }

    public class ReaderSSF
    {
        public static bool Validate(string ssfFilePath)
        {
            try
            {
                ReadFile(ssfFilePath, "");
                return true;
            }
            catch
            {
                return false;
            }
        }

        public static SoundSpeedField Read(string ssfPath, out string ssfFullPath)
        {
            if (Directory.Exists(ssfPath))
            {
                ssfFullPath = ssfPath;

                var pathArray = ssfPath.Split('\\');
                var date = pathArray[pathArray.Length - 1];

                var directoryInfo = new DirectoryInfo(ssfPath);
                var ssfFiles = directoryInfo.GetFiles("*_*x*.ssp");

                if (ssfFiles.Length == 0) throw new Exception("No Sound Speed Profiles found that match expected file naming convention.");

                var soundSpeedProfiles = new List<SoundSpeedProfile>();

                string fileName;
                string[] nameArray;
                foreach (var file in ssfFiles)
                {
                    fileName = file.Name.Substring(file.Name.LastIndexOf('_') + 1).Replace(".ssp", "");
                    nameArray = fileName.Split('x');

                    double latitude,
                           longitude;
                    if (file.Length > 0 && nameArray.Length == 2 && double.TryParse(nameArray[0], out latitude) && double.TryParse(nameArray[1], out longitude)) soundSpeedProfiles.Add(SoundSpeedProfile.Read(date, latitude, longitude, ssfPath));
                }

                if (soundSpeedProfiles.Count == 0) throw new Exception("No Sound Speed Profiles found that match expected file naming convention.");

                var soundSpeedFieldProperties = new SoundSpeedField(date, soundSpeedProfiles.ToArray());

                ssfPath = ssfPath + ".txt";
                using (var streamWriter = new StreamWriter(ssfPath, false, Encoding.ASCII))
                {
                    foreach (var profile in soundSpeedFieldProperties.SoundSpeedProfiles)
                    {
                        streamWriter.WriteLine("Lat:  " + profile.Location.Latitude_degrees.ToString("0.00##") + " Lon:  " + profile.Location.Longitude_degrees.ToString("0.00##") + " Valid Days: 1-365");
                        streamWriter.WriteLine("Points in profile: " + profile.Depths.Length + " Min depth: " + profile.Depths[0].ToString("0.0") + " Max depth: " + profile.Depths[profile.Depths.Length - 1] + " Version: ESME 1.0 Distribution Statement A: Approved for public release. Distribution unlimited");

                        for (var i = 0; i < profile.Depths.Length; i++) streamWriter.WriteLine(profile.Depths[i].ToString("###0.0") + "\t00.000\t00.000\t" + profile.SoundSpeeds[i].ToString("###0.000"));
                    }

                    streamWriter.Flush();
                }

                return soundSpeedFieldProperties;
            }
            else
            {
                ssfFullPath = ssfPath = ssfPath + ".txt";

                if (!File.Exists(ssfPath)) throw new Exception("The specified Sound Speed Field file path does not exist.");

                var fileInfo = new FileInfo(ssfPath);
                var date = fileInfo.Name.Replace(fileInfo.Extension, "");

                if (fileInfo.Length == 0) throw new Exception("The specified Sound Speed Field file was empty.");

                var soundSpeedProfile = ReadFile(ssfPath, date);
                return new SoundSpeedField(date, soundSpeedProfile);
            }
        }

        static SoundSpeedProfile[] ReadFile(string ssfPath, string date)
        {
            var profilesList = new List<SoundSpeedProfile>();

            using (var streamReader = new StreamReader(ssfPath, Encoding.ASCII))
            {
                string line;
                string[] lineArray;
                double latitude = -1,
                       longitude = -1;
                var points = 0;

                var depthList = new List<float>();
                var speedList = new List<float>();

                EarthCoordinate earthCoordinate = null;

                while ((line = streamReader.ReadLine()) != null)
                {
                    lineArray = line.Split(new[]
                                           {
                                               ' ', '\t'
                                           }, StringSplitOptions.RemoveEmptyEntries);
                    if (lineArray.Length >= 4 && lineArray[0] == "Lat:" && lineArray[2] == "Lon:" && double.TryParse(lineArray[1], out latitude) && double.TryParse(lineArray[3], out longitude)) {}
                    else if (lineArray.Length >= 9 && lineArray[0] == "Points" && int.TryParse(lineArray[3], out points) && points > 0)
                    {
                        earthCoordinate = new EarthCoordinate(latitude, longitude);
                    }
                    else
                    {
                        float depth;
                        float speed;
                        if (lineArray.Length == 4 && float.TryParse(lineArray[0], out depth) && float.TryParse(lineArray[3], out speed))
                        {
                            depthList.Add(depth);
                            speedList.Add(speed);
                        }
                    }

                    if (earthCoordinate != null && points > 0 && points == depthList.Count && points == speedList.Count)
                    {
                        var depthVector = depthList.ToArray();
                        var speedVector = speedList.ToArray();
                        profilesList.Add(new SoundSpeedProfile(date, earthCoordinate, depthVector, speedVector));

                        earthCoordinate = null;
                        points = 0;
                        depthList.Clear();
                        speedList.Clear();
                    }
                }

                if (profilesList.Count == 0) throw new Exception("There were no valid sound speed profiles found.");

                return profilesList.ToArray();
            }
        }
    }
}