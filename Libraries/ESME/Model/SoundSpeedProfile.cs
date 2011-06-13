#if false
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using ESME.Environment.NAVO;
using HRC.Navigation;

namespace ESME.Model
{
    public class SoundSpeedProfile : EarthCoordinate
    {
        #region Public Properties

        public float[] Depths { get; set; }
        public float[] SoundSpeeds { get; set; }
        public float MaxDepth { get { return ((Depths == null) || (Depths.Length == 0)) ? 0 : Depths.Last(); } }

        #endregion

        internal static readonly UInt32 Magic = 0xe1074103;

        #region Public Constructors

        public SoundSpeedProfile() { }

        public SoundSpeedProfile(Geo location, IList<float> depths, IList<float> soundSpeeds)
            : base(location)
        {
            var speedList = new List<float>();
            var depthList = new List<float>();
            for (var i = 0; i < soundSpeeds.Count; i++)
            {
                if (float.IsNaN(soundSpeeds[i])) break;
                speedList.Add(soundSpeeds[i]);
                depthList.Add(depths[i]);
            }
            SoundSpeeds = speedList.ToArray();
            Depths = depthList.ToArray();
        }

        public SoundSpeedProfile(EarthCoordinate<List<double>> dataPoint, IEnumerable<double> depthAxis)
            : base(dataPoint)
        {
            SoundSpeeds = (from speed in dataPoint.Data
                           select (float)speed).ToArray();
            Depths = new float[SoundSpeeds.Length];
            if (Depths.Length == 0) return;
            var floatDepths = (from depth in depthAxis
                               select (float) depth).ToArray();
            Array.ConstrainedCopy(floatDepths, 0, Depths, 0, Depths.Length);
        }

        public SoundSpeedProfile(BinaryReader stream)
            : base(stream)
        {
            int i;
            if (stream.ReadUInt32() != Magic) throw new FormatException("Format error reading SoundSpeedProfile from file");
            Depths = new float[stream.ReadInt32()];
            for (i = 0; i < Depths.Length; i++) Depths[i] = stream.ReadSingle();
            SoundSpeeds = new float[stream.ReadInt32()];
            for (i = 0; i < SoundSpeeds.Length; i++) SoundSpeeds[i] = stream.ReadSingle();
        }

        #endregion

        public void Save(BinaryWriter stream)
        
        {
            int i;
            base.Write(stream);
            stream.Write(Magic);
            stream.Write(Depths.Length);
            for (i = 0; i < Depths.Length; i++) stream.Write(Depths[i]);
            stream.Write(SoundSpeeds.Length);
            for (i = 0; i < SoundSpeeds.Length; i++) stream.Write(SoundSpeeds[i]);
        }

        /// <summary>
        /// Extrapolates the current sound speed profile to the given depth, in one step, using the provided temperature and salinity profile
        /// </summary>
        /// <param name="newMaxDepth"></param>
        /// <param name="temperatureProfile"></param>
        /// <param name="salinityProfile"></param>
        public void Extend(float newMaxDepth, SoundSpeedProfile temperatureProfile, SoundSpeedProfile salinityProfile)
        {
            if (newMaxDepth < Depths.Last()) throw new ApplicationException(string.Format("SoundSpeedProfile.Extend: Given depth {0} is less than current maximum depth {1}", newMaxDepth, Depths.Last()));

            //System.Diagnostics.Debug.WriteLine("Extrapolating SSP {0} from data depth of {1}m to bathymetry depth of {2}m", this, MaxDepth, newMaxDepth);
            //System.Diagnostics.Debug.WriteLine("  Initial depth vector length: {0}", Depths.Length);

            var tempD = temperatureProfile.SoundSpeeds.Last();
            var tempD1 = temperatureProfile.SoundSpeeds[SoundSpeeds.Length - 2];
            var salinity = salinityProfile.SoundSpeeds.Last();

            var tempDiff = tempD1 - tempD;
            var newTemp = tempD - tempDiff;
            var soundSpeed = ChenMilleroLi.SoundSpeed(this, newMaxDepth, newTemp, salinity);
            var oldDepths = new List<float>(Depths)
                            {
                                newMaxDepth
                            };
            var oldSSP = new List<float>(SoundSpeeds)
                          {
                              soundSpeed,
                          };
            Depths = oldDepths.ToArray();
            SoundSpeeds = oldSSP.ToArray();
            //System.Diagnostics.Debug.WriteLine("  Final depth vector length: {0}", Depths.Length);
        }

        /// <summary>
        /// Extends the current sound speed profile to the same depth as a given template, adjusting the copied template values to ensure a
        /// smooth curve in the profile
        /// </summary>
        /// <param name="templateSSP"></param>
        public void Extend(SoundSpeedProfile templateSSP)
        {
            //System.Diagnostics.Debug.WriteLine("Extending SSP {0} to new depth {1}", this, templateSSP.MaxDepth);

            if(templateSSP.MaxDepth > MaxDepth)
            {
                if (SoundSpeeds.Length == 0)
                {
                    //System.Diagnostics.Debug.WriteLine("  Original SSP is zero length, copying templateSSP.");
                    Depths = new List<float>(templateSSP.Depths).ToArray();
                    SoundSpeeds = new List<float>(templateSSP.SoundSpeeds).ToArray();
                }
                else
                {
                    //System.Diagnostics.Debug.WriteLine("  Original SSP depth vector length: {0} ({1}m)", Depths.Length, MaxDepth);

                    var shallowSpeed = SoundSpeeds.Last();
                    var deepSpeedAtSameDepth = templateSSP.SoundSpeeds[SoundSpeeds.Length - 1];
                    //System.Diagnostics.Debug.WriteLine("  Original soundspeed at {0}m: {1}", MaxDepth, shallowSpeed);
                    //System.Diagnostics.Debug.WriteLine("  Template soundspeed at {0}m: {1}", MaxDepth, deepSpeedAtSameDepth);

                    var ssDiff = deepSpeedAtSameDepth - shallowSpeed;
                    //System.Diagnostics.Debug.WriteLine("  Delta    soundspeed at {0}m: {1}", MaxDepth, ssDiff);

                    Depths = new List<float>(templateSSP.Depths).ToArray();
                    var speeds = new List<float>(SoundSpeeds);
                    for (var speedIndex = SoundSpeeds.Length; speedIndex < templateSSP.SoundSpeeds.Length; speedIndex++)
                    {
                        var newSpeed = templateSSP.SoundSpeeds[speedIndex] - ssDiff;
                        //System.Diagnostics.Debug.WriteLine("    Template soundspeed at {0}m: Original: {1} Adjusted: {2}", templateSSP.Depths[speedIndex], templateSSP.SoundSpeeds[speedIndex], newSpeed);
                        speeds.Add(newSpeed);
                    }
                    SoundSpeeds = speeds.ToArray();
                }
            }
            //System.Diagnostics.Debug.WriteLine("  New SSP depth vector length: {0}", Depths.Length);

        }

        public static explicit operator EnvironmentalDataPoint(SoundSpeedProfile ssp)
        {
            var result = new EnvironmentalDataPoint
                         {
                             Latitude = ssp.Latitude,
                             Longitude = ssp.Longitude
                         };
            result.Data.AddRange(from speed in ssp.SoundSpeeds select (double)speed);
            return result;
        }

        public static readonly SoundSpeedProfile Empty = new SoundSpeedProfile
                                                         {
                                                             Depths = null,
                                                             SoundSpeeds = null,
                                                         };

        static SoundSpeedProfile Read(string sspPath)
        {
            var depthList = new List<float>();
            var soundspeedList = new List<float>();

            var fileName = sspPath.Substring(sspPath.LastIndexOf('_') + 1).Replace(".ssp", "");
            var nameArray = fileName.Split('x');

            double latitude,
                   longitude;

            if (nameArray.Length != 2 || !double.TryParse(nameArray[0], out latitude) || !double.TryParse(nameArray[1], out longitude)) throw new FileNameFormatException("The Sound Speed Profile file did not have the expected file name format.");

            if (!File.Exists(sspPath)) throw new FileNotFoundException("The requested Sound Speed Profile file does not exist.");

            var fileInfo = new FileInfo(sspPath);
            if (fileInfo.Length == 0) throw new FileIsEmptyException("The requested Sound Speed Profile file is empty.");

            using (var streamReader = new StreamReader(sspPath, Encoding.ASCII))
            {
                string line;
                string[] lineArray;

                while ((line = streamReader.ReadLine()) != null)
                {
                    lineArray = line.Split(new[]
                                           {
                                               ' ', '\t'
                                           }, StringSplitOptions.RemoveEmptyEntries);

                    if (lineArray.Length == 2)
                    {
                        depthList.Add(float.Parse(lineArray[0]));
                        soundspeedList.Add(float.Parse(lineArray[1]));
                    }
                    else if (lineArray.Length != 0) throw new FileFormatException("The Sound Speed Profile file did not have the expected format.");
                }
            }

            return new SoundSpeedProfile(new EarthCoordinate(latitude, longitude), depthList.ToArray(), soundspeedList.ToArray());
        }

        public static SoundSpeedProfile Read(double latitude, double longitude, string ssfPath)
        {
            var directoryInfo = new DirectoryInfo(ssfPath);
            var fileInfo = directoryInfo.GetFiles("*" + latitude.ToString("###.0000") + "x" + longitude.ToString("###.0000") + "*");

            return fileInfo.Length == 1 ? Read(fileInfo[0].FullName) : Empty;
        }
    }
}
#endif
