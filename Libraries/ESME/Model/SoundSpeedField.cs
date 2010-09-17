using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using System.IO;
using System.Xml;
using System.Xml.Linq;
using System.Xml.Schema;
using System.Xml.Serialization;
using HRC.Navigation;
using ESME.Environment;

namespace ESME.Model
{
    public class SoundSpeedField
    {
        public string TimePeriod { get; internal set; }
        public SoundSpeedProfile[] SoundSpeedProfiles { get; internal set; }
        private SoundSpeedProfile DeepestSSP;

        public SoundSpeedField(string EnvironmentFileName)
        {
            float[,,] array;
            List<SoundSpeedProfile> profiles = new List<SoundSpeedProfile>();
            DataFile file = DataFile.Open(EnvironmentFileName);
            double[] lats, lons;
            float[] depths, speeds;

            TimePeriod = null;
            SoundSpeedProfiles = null;
            foreach (ESME.Environment.DataLayer layer in file.Layers)
            {
                if (layer.Name == "soundspeed")
                {
                    TimePeriod = layer.TimePeriod;
                    lats = layer.LatitudeAxis.DoubleValues;
                    lons = layer.LongitudeAxis.UnwrappedValues;
                    depths = new float[layer.DepthAxis.DoubleValues.Length];
                    speeds = new float[depths.Length];
                    array = new float[lats.Length, lons.Length, depths.Length];

                    for (int d = 0; d < depths.Length; d++)
                        depths[d] = (float)layer.DepthAxis.DoubleValues[d];

                    foreach (DataRow row in layer.Rows)
                        foreach (DataPoint point in row.Points)
                        {
                            // TODO: Find the deepest SSP here and set it to DeepestSSP
                            profiles.Add(new SoundSpeedProfile(TimePeriod, point.EarthCoordinate, depths, point.Data));
                        }
#if false
                    for (int i = 0; i < lats.Length; i++)
                    {
                        for (int j = 0; j < lons.Length; j++)
                        {
                            for (int d = 0; d < depths.Length; d++)
                                speeds[d] = array[j, i, d];
                            //Dave todo: Find the deepest SSP here and set it to DeepestSSP
                            profiles.Add(new SoundSpeedProfile(TimePeriod, new EarthCoordinate(lats[i], lons[j]), depths, speeds));
                        }
                    }
#endif
                }
            }
            SoundSpeedProfiles = profiles.ToArray();
        }

        public SoundSpeedProfile ExtendSSP(SoundSpeedProfile ShallowSSP, float RequiredDepth_meters)
        {
            float deltaV = float.NaN;
            int i;

            SoundSpeedProfile ExtendedSSP = new SoundSpeedProfile();
            ExtendedSSP.Depths_meters = DeepestSSP.Depths_meters;
            ExtendedSSP.SoundSpeeds_metersSecond = new float[DeepestSSP.Depths_meters.Length];

            for (i = 0; i < DeepestSSP.Depths_meters.Length; i++)
            {
                if (float.IsNaN(ShallowSSP.SoundSpeeds_metersSecond[i]))
                    break;
                else
                {
                    deltaV = DeepestSSP.SoundSpeeds_metersSecond[i] - ShallowSSP.SoundSpeeds_metersSecond[i];
                    ExtendedSSP.SoundSpeeds_metersSecond[i] = ShallowSSP.SoundSpeeds_metersSecond[i];
                }
            }

            if (float.IsNaN(deltaV))
                throw new ApplicationException("ExtendSSP: ShallowSSP is empty.");

            for (int j = i; j < DeepestSSP.Depths_meters.Length; j++)
            {
                if (DeepestSSP.Depths_meters[j-1] <= RequiredDepth_meters) // need previous depth to be less than req'd, current is slightly greater
                    ExtendedSSP.SoundSpeeds_metersSecond[j] = DeepestSSP.SoundSpeeds_metersSecond[j] - deltaV;
                else
                    ExtendedSSP.SoundSpeeds_metersSecond[j] = float.NaN;

            }
            return ExtendedSSP;
        }

        public SoundSpeedField(string TimePeriod, SoundSpeedProfile[] SoundSpeedProfiles)
        {
            this.TimePeriod = TimePeriod;
            this.SoundSpeedProfiles = SoundSpeedProfiles;
        }

        public SoundSpeedProfile this[EarthCoordinate Location]
        {
            get
            {
                var query = from p in SoundSpeedProfiles
                            orderby p.Location.GetDistanceTo_Meters(Location) ascending
                            select p;

                if (query.Count() > 0)
                    return (SoundSpeedProfile)query.First();
                else
                    return SoundSpeedProfile.Empty;
            }
        }

    }

    public class ReaderSSF
    {
        public static bool Validate(string ssfFilePath)
        {
            try
            {
                ReaderSSF.ReadFile(ssfFilePath, "");
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

                string[] pathArray = ssfPath.Split('\\');
                string date = pathArray[pathArray.Length - 1];

                DirectoryInfo directoryInfo = new DirectoryInfo(ssfPath);
                FileInfo[] ssfFiles = directoryInfo.GetFiles("*_*x*.ssp");

                if (ssfFiles.Length == 0)
                    throw new Exception("No Sound Speed Profiles found that match expected file naming convention.");

                List<SoundSpeedProfile> soundSpeedProfiles = new List<SoundSpeedProfile>();

                string fileName;
                string[] nameArray;
                foreach (FileInfo file in ssfFiles)
                {
                    fileName = file.Name.Substring(file.Name.LastIndexOf('_') + 1).Replace(".ssp", "");
                    nameArray = fileName.Split('x');

                    double latitude, longitude;
                    if (file.Length > 0 && nameArray.Length == 2 && double.TryParse(nameArray[0], out latitude) && double.TryParse(nameArray[1], out longitude))
                        soundSpeedProfiles.Add(SoundSpeedProfile.Read(date, latitude, longitude, ssfPath));
                }

                if (soundSpeedProfiles.Count == 0)
                    throw new Exception("No Sound Speed Profiles found that match expected file naming convention.");

                SoundSpeedField soundSpeedFieldProperties = new SoundSpeedField(date, soundSpeedProfiles.ToArray());

                ssfPath = ssfPath + ".txt";
                using (StreamWriter streamWriter = new StreamWriter(ssfPath, false, Encoding.ASCII))
                {
                    foreach (SoundSpeedProfile profile in soundSpeedFieldProperties.SoundSpeedProfiles)
                    {
                        streamWriter.WriteLine("Lat:  " + profile.Location.Latitude_degrees.ToString("0.00##")
                            + " Lon:  " + profile.Location.Longitude_degrees.ToString("0.00##")
                            + " Valid Days: 1-365");
                        streamWriter.WriteLine("Points in profile: " + profile.Depths_meters.Length
                            + " Min depth: " + profile.Depths_meters[0].ToString("0.0")
                            + " Max depth: " + profile.Depths_meters[profile.Depths_meters.Length - 1]
                            + " Version: ESME 1.0 Distribution Statement A: Approved for public release. Distribution unlimited");

                        for (int i = 0; i < profile.Depths_meters.Length; i++)
                            streamWriter.WriteLine(profile.Depths_meters[i].ToString("###0.0") + "\t00.000\t00.000\t" + profile.SoundSpeeds_metersSecond[i].ToString("###0.000"));

                    }

                    streamWriter.Flush();
                }

                return soundSpeedFieldProperties;
            }
            else
            {
                ssfFullPath = ssfPath = ssfPath + ".txt";

                if (!File.Exists(ssfPath))
                    throw new Exception("The specified Sound Speed Field file path does not exist.");

                FileInfo fileInfo = new FileInfo(ssfPath);
                string date = fileInfo.Name.Replace(fileInfo.Extension, "");

                if (fileInfo.Length == 0)
                    throw new Exception("The specified Sound Speed Field file was empty.");

                SoundSpeedProfile[] SoundSpeedProfile = ReaderSSF.ReadFile(ssfPath, date);
                return new SoundSpeedField(date, SoundSpeedProfile);
            }
        }

        private static SoundSpeedProfile[] ReadFile(string ssfPath, string date)
        {
            var profilesList = new List<SoundSpeedProfile>();

            using (StreamReader streamReader = new StreamReader(ssfPath, Encoding.ASCII))
            {
                string line;
                string[] lineArray;
                double latitude = -1, longitude = -1;
                int points = 0;
                float depth, speed;

                var depthList = new List<float>();
                var speedList = new List<float>();

                EarthCoordinate earthCoordinate = null;
                float[] depthVector;
                float[] speedVector;

                while ((line = streamReader.ReadLine()) != null)
                {
                    lineArray = line.Split(new char[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries);
                    if (lineArray.Length >= 4 && lineArray[0] == "Lat:" && lineArray[2] == "Lon:"
                        && double.TryParse(lineArray[1], out latitude) && double.TryParse(lineArray[3], out longitude))
                    {
                    }
                    else if (lineArray.Length >= 9 && lineArray[0] == "Points" && int.TryParse(lineArray[3], out points)
                        && points > 0)
                    {
                        earthCoordinate = new EarthCoordinate(latitude, longitude);
                    }
                    else if (lineArray.Length == 4 && float.TryParse(lineArray[0], out depth) && float.TryParse(lineArray[3], out speed))
                    {
                        depthList.Add(depth);
                        speedList.Add(speed);
                    }

                    if (earthCoordinate != null && points > 0 && points == depthList.Count && points == speedList.Count)
                    {
                        depthVector = depthList.ToArray();
                        speedVector = speedList.ToArray();
                        profilesList.Add(new SoundSpeedProfile(date, earthCoordinate, depthVector, speedVector));

                        earthCoordinate = null;
                        points = 0;
                        depthList.Clear();
                        speedList.Clear();
                    }
                }

                if (profilesList.Count == 0)
                    throw new Exception("There were no valid sound speed profiles found.");

                return profilesList.ToArray();
            }
        }
    }
}
