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

namespace ESME.Model
{
    public class SoundSpeedProfile
    {
        #region Public Properties
        public string TimePeriod { get; internal set; }
        public EarthCoordinate Location { get; internal set; }
        public float[] Depths_meters { get; internal set; }
        public float[] SoundSpeeds_metersSecond { get; internal set; }
        #endregion

        internal readonly static UInt32 Magic = 0xe1071103;

        #region Public Constructors
        public SoundSpeedProfile() { }

        public SoundSpeedProfile(string TimePeriod, EarthCoordinate Location, float[] Depths_meters, float[] SoundSpeeds_metersSecond)
        {
            this.TimePeriod = TimePeriod;
            this.Location = Location;

            var Speeds = new List<float>();
            var Depths = new List<float>();
            for (int i = 0; i < SoundSpeeds_metersSecond.Length; i++)
            {
                if (float.IsNaN(SoundSpeeds_metersSecond[i]))
                    break;
                Speeds.Add(SoundSpeeds_metersSecond[i]);
                Depths.Add(Depths_meters[i]);
            }
            this.SoundSpeeds_metersSecond = Speeds.ToArray();
            this.Depths_meters = Depths.ToArray();
        }

        public SoundSpeedProfile(BinaryReader stream)
        {
            int i;
            if (stream.ReadUInt32() != Magic)
                throw new FormatException("Format error reading SoundSpeedProfile from file");
            TimePeriod = stream.ReadString();
            Location = new EarthCoordinate(stream);
            Depths_meters = new float[stream.ReadInt32()];
            for (i = 0; i < Depths_meters.Length; i++)
                Depths_meters[i] = stream.ReadSingle();
            SoundSpeeds_metersSecond = new float[stream.ReadInt32()];
            for (i = 0; i < SoundSpeeds_metersSecond.Length; i++)
                SoundSpeeds_metersSecond[i] = stream.ReadSingle();
        }
        #endregion

        public void Save(BinaryWriter stream)
        {
            int i;
            stream.Write(Magic);
            stream.Write(TimePeriod);
            Location.Write(stream);
            stream.Write(Depths_meters.Length);
            for (i = 0; i < Depths_meters.Length; i++)
                stream.Write(Depths_meters[i]);
            stream.Write(SoundSpeeds_metersSecond.Length);
            for (i = 0; i < SoundSpeeds_metersSecond.Length; i++)
                stream.Write(SoundSpeeds_metersSecond[i]);
        }

        public readonly static SoundSpeedProfile Empty = new SoundSpeedProfile
        {
            TimePeriod = string.Empty,
            Location = null,
            Depths_meters = null,
            SoundSpeeds_metersSecond = null,
        };

        private static SoundSpeedProfile Read(string date, string sspPath)
        {
            var depthList = new List<float>();
            var soundspeedList = new List<float>();

            string fileName = sspPath.Substring(sspPath.LastIndexOf('_') + 1).Replace(".ssp", "");
            string[] nameArray = fileName.Split('x');

            double latitude, longitude;

            if (nameArray.Length != 2 || !double.TryParse(nameArray[0], out latitude) || !double.TryParse(nameArray[1], out longitude))
                throw new FileNameFormatException("The Sound Speed Profile file did not have the expected file name format.");

            if (!File.Exists(sspPath))
                throw new FileNotFoundException("The requested Sound Speed Profile file does not exist.");

            FileInfo fileInfo = new FileInfo(sspPath);
            if (fileInfo.Length == 0)
                throw new FileIsEmptyException("The requested Sound Speed Profile file is empty.");

            using (StreamReader streamReader = new StreamReader(sspPath, Encoding.ASCII))
            {
                string line;
                string[] lineArray;

                while ((line = streamReader.ReadLine()) != null)
                {
                    lineArray = line.Split(new char[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries);

                    if (lineArray.Length == 2)
                    {
                        depthList.Add(float.Parse(lineArray[0]));
                        soundspeedList.Add(float.Parse(lineArray[1]));
                    }
                    else if (lineArray.Length != 0)
                        throw new FileFormatException("The Sound Speed Profile file did not have the expected format.");
                }
            }

            return new SoundSpeedProfile(date, new EarthCoordinate(latitude, longitude), depthList.ToArray(), soundspeedList.ToArray());
        }

        public static SoundSpeedProfile Read(string date, double latitude, double longitude, string ssfPath)
        {
            DirectoryInfo directoryInfo = new DirectoryInfo(ssfPath);
            FileInfo[] fileInfo = directoryInfo.GetFiles("*" + latitude.ToString("###.0000") + "x" + longitude.ToString("###.0000") + "*");

            if (fileInfo.Length == 1)
                return Read(date, fileInfo[0].FullName);
            else
                return SoundSpeedProfile.Empty;
        }
    }
}
