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
        public float MaxDepth { get; set; }

        #endregion

        internal static readonly UInt32 Magic = 0xe1074103;

        #region Public Constructors

        public SoundSpeedProfile() { }

        public SoundSpeedProfile(EarthCoordinate location, IList<float> depths, IList<float> soundSpeeds)
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
            MaxDepth = Depths.Length == 0 ? 0 : Depths[Depths.Length - 1];
        }

        public SoundSpeedProfile(EnvironmentalDataPoint dataPoint, List<double> depthAxis)
            : base(dataPoint)
        {
            SoundSpeeds = (from speed in dataPoint.Data
                           select (float)speed).ToArray();
            Depths = new float[SoundSpeeds.Length];
            if (Depths.Length == 0) return;
            var floatDepths = (from depth in depthAxis
                               select (float) depth).ToArray();
            Array.ConstrainedCopy(floatDepths, 0, Depths, 0, Depths.Length);
            MaxDepth = Depths.Last();
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
            MaxDepth = Depths[Depths.Length - 1];
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