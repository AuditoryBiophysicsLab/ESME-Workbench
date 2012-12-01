using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Media;
using HRC.Navigation;
using HRC.Utility;
using FileFormatException = ESME.Model.FileFormatException;

namespace ESME.Environment
{
    public class Bathymetry : EnvironmentDataSetBase
    {
        public EnvironmentData<Geo<float>> Samples { get; private set; }

        public Bathymetry() { Samples = new EnvironmentData<Geo<float>>(); }

        public static Task<Bathymetry> LoadAsync(string filename)
        {
            return TaskEx.Run(() => Load(filename));
        }

        public override void Save(string filename)
        {
            using (var stream = new FileStream(filename, FileMode.Create, FileAccess.Write, FileShare.None))
            using (var writer = new BinaryWriter(stream))
            {
                writer.Write(new[]{'b', 't', 'h', 'y'});
                writer.Write(Samples.Count);
                foreach (var sample in Samples)
                {
                    writer.Write(sample.Latitude);
                    writer.Write(sample.Longitude);
                    writer.Write(sample.Data);
                }
            }
        }

        public static Bathymetry Load(string filename)
        {
            using (var stream = new FileStream(filename, FileMode.Open, FileAccess.Read, FileShare.Read))
            using (var reader = new BinaryReader(stream))
            {
                var header = reader.ReadBytes(4);
                if (header[0] != 'b' || header[1] != 't' || header[2] != 'h' || header[3] != 'y') throw new FileFormatException("Bathymetry file header not in the expected format");
                var result = new Bathymetry();
                var count = reader.ReadInt32();
                for (var i = 0; i < count; i++)
                    result.Samples.Add(new Geo<float>(reader.ReadDouble(), reader.ReadDouble(), reader.ReadSingle()));
                return result;
            }
        }

        public static Bathymetry FromYXZ(string fileName, float scaleFactor)
        {
            char[] separators = { ' ' };
            var samples = new List<Geo<float>>();
            //System.Diagnostics.Debug.WriteLine("{0}: FromYXZ: About to read bathymetry file {1}", DateTime.Now, Path.GetFileName(fileName));
            //var lineCount = 0;
            using (var stream = new StreamReader(File.Open(fileName, FileMode.Open, FileAccess.Read, FileShare.Read)))
            {
                var curLine = stream.ReadLine();
                while (curLine != null)
                {
                    //lineCount++;
                    var fields = curLine.Split(separators, StringSplitOptions.RemoveEmptyEntries);
                    var latitude = double.Parse(fields[0],CultureInfo.InvariantCulture);
                    var longitude = double.Parse(fields[1], CultureInfo.InvariantCulture);
                    var depth = float.Parse(fields[2], CultureInfo.InvariantCulture) * scaleFactor;
                    samples.Add(new Geo<float>(latitude, longitude, depth));
                    curLine = stream.ReadLine();
                }
            }
            //System.Diagnostics.Debug.WriteLine("{0}: FromYXZ: Finished readying bathymetry file {1}.  Line count: {2}", DateTime.Now, Path.GetFileName(fileName), lineCount);
            var result = new Bathymetry();
            result.Samples.AddRange(samples);
            return result;
        }

        public void ToYXZ(string fileName, float scaleFactor)
        {
            using (var stream = new StreamWriter(fileName, false, Encoding.ASCII))
            {
                foreach (var sample in Samples)
                    stream.WriteLine(string.Format(CultureInfo.InvariantCulture,"{0:0.0000} {1:0.0000} {2:0.00}", sample.Latitude, sample.Longitude, sample.Data * scaleFactor));
            }
        }

        public Geo<float> Minimum
        {
            get
            {
                if (_minimum != null) return _minimum;
                lock (_minLockObject)
                {
                    if (_minimum != null) return _minimum;
                    var minPoint = Samples[0];
                    foreach (var sample in Samples.Where(sample => sample.Data < minPoint.Data)) minPoint = sample;
                    _minimum = minPoint;
                    return _minimum;
                }
            }
        }
        Geo<float> _minimum;
        readonly object _minLockObject = new object();

        public Geo<float> Maximum
        {
            get
            {
                if (_maximum != null) return _maximum;
                lock (_maxLockObject)
                {
                    if (_maximum != null) return _maximum;
                    var maxPoint = Samples[0];
                    foreach (var sample in Samples.Where(sample => sample.Data > maxPoint.Data)) maxPoint = sample;
                    _maximum = maxPoint;
                    return _maximum;
                }
            }
        }
        Geo<float> _maximum;
        readonly object _maxLockObject = new object();

        public Geo<float> DeepestPoint
        {
            get
            {
                if (_deepestPoint != null) return _deepestPoint;
                lock (_deepestPointLockObject)
                {
                    if (_deepestPoint != null) return _deepestPoint;
                    var minimum = Minimum;
                    _deepestPoint = new Geo<float>(minimum, Math.Abs(minimum.Data));
                    return _deepestPoint;
                }
            }
        }
        Geo<float> _deepestPoint;
        readonly object _deepestPointLockObject = new object();
    }

    public static class BathymetryExtensionMethods
    {
        public static void CreateBitmap(this Bathymetry bathymetry, DualColormap dualColormap, int maxWidth, int maxHeight, string destinationFilename)
        {
            CheckDestinationDirectory(destinationFilename);

            var bathysize = Math.Max(bathymetry.Samples.Longitudes.Count, bathymetry.Samples.Latitudes.Count);
            var rasterSize = Math.Min(maxWidth, maxHeight);
            var displayValues = bathymetry.Samples;
            if (bathysize > rasterSize)
            {
                var scaleFactor = rasterSize / bathysize;
                displayValues = EnvironmentData<Geo<float>>.Decimate(bathymetry.Samples,
                                                                     bathymetry.Samples.Longitudes.Count * scaleFactor,
                                                                     bathymetry.Samples.Latitudes.Count * scaleFactor);
            }

            var imageFilename = Path.GetFileNameWithoutExtension(destinationFilename) + ".bmp";
            var imagePath = Path.GetDirectoryName(destinationFilename);
            var bitmapData = new float[displayValues.Longitudes.Count, displayValues.Latitudes.Count];
            for (var latIndex = 0; latIndex < bitmapData.GetLength(1); latIndex++) for (var lonIndex = 0; lonIndex < bitmapData.GetLength(0); lonIndex++) bitmapData[lonIndex, latIndex] = displayValues[(uint)lonIndex, (uint)latIndex].Data;
            var displayData = dualColormap.ToPixelValues(bitmapData, bathymetry.Minimum.Data, bathymetry.Maximum.Data < 0 ? bathymetry.Maximum.Data : 8000, Colors.Black);
            BitmapWriter.Write(Path.Combine(imagePath, imageFilename), displayData);
        }

        public static void CreateWorldFile(this Bathymetry bathymetry, string destinationFilename)
        {
            CheckDestinationDirectory(destinationFilename);

            var imageFilename = Path.GetFileNameWithoutExtension(destinationFilename) + ".bpw";
            var imagePath = Path.GetDirectoryName(destinationFilename);

            var sb = new StringBuilder();
            sb.AppendLine(bathymetry.Samples.Resolution.ToString(CultureInfo.InvariantCulture));
            sb.AppendLine("0.0");
            sb.AppendLine("0.0");
            sb.AppendLine(bathymetry.Samples.Resolution.ToString(CultureInfo.InvariantCulture));
            sb.AppendLine(bathymetry.Samples.GeoRect.West.ToString(CultureInfo.InvariantCulture));
            sb.AppendLine(bathymetry.Samples.GeoRect.North.ToString(CultureInfo.InvariantCulture));
            using (var writer = new StreamWriter(Path.Combine(imagePath, imageFilename), false)) writer.Write(sb.ToString());
        }

        static void CheckDestinationDirectory(string destinationFilename)
        {
            if (string.IsNullOrEmpty(destinationFilename)) throw new ArgumentNullException("destinationFilename");
            var destinationDirectory = Path.GetDirectoryName(destinationFilename);
            if (string.IsNullOrEmpty(destinationDirectory)) throw new ArgumentException("Destination filename must contain a full path", "destinationFilename");
            if (!Directory.Exists(destinationDirectory)) Directory.CreateDirectory(destinationDirectory);
        }
    }

#if false
    public class AvailableResolution
    {
        public AvailableResolution() { IsAvailable = false; }

        /// <summary>
        /// Resolution of the described dataset, in degrees
        /// </summary>
        public float Resolution { get; set; }

        /// <summary>
        /// True if data is available at this resolution
        /// </summary>
        public bool IsAvailable { get; set; }
    }

    public class SelectedResolution : AvailableResolution
    {
        public SelectedResolution() { IsSelected = false; }

        /// <summary>
        /// True if the this resolution has been selected by the user
        /// </summary>
        public bool IsSelected { get; set; }
    }

    public class Resolutions<T> : List<T> where T: AvailableResolution, new()
    {
        public Resolutions()
        {
            if (Count != 0) return;
            Add(new T {Resolution = 0.05f});
            Add(new T {Resolution = 0.10f});
            Add(new T {Resolution = 0.50f});
            Add(new T {Resolution = 1.00f});
            Add(new T {Resolution = 2.00f});
        }

        public T this[float resolution] { get { return Find(t => Math.Abs(t.Resolution - resolution) < 0.0001f); } }
    }

    public class AvailableResolutions : Resolutions<AvailableResolution> { }
    public class SelectedResolutions : Resolutions<SelectedResolution> { }

    public class AvailableTimePeriod
    {
        public AvailableTimePeriod()
        {
            IsAvailable = false;
        }

        public NAVOTimePeriod TimePeriod { get; set; }
        public bool IsAvailable { get; set; }
    }

    public class SelectedTimePeriod : AvailableTimePeriod
    {
        public SelectedTimePeriod()
        {
            IsSelected = false;
        }

        public bool IsSelected { get; set; }
    }

    public class TimePeriods<T> : List<T> where T: AvailableTimePeriod, new()
    {
        public TimePeriods()
        {
            if (Count != 0) return;
            Add(new T { TimePeriod = NAVOTimePeriod.Invalid });
            Add(new T { TimePeriod = NAVOTimePeriod.January });
            Add(new T { TimePeriod = NAVOTimePeriod.February });
            Add(new T { TimePeriod = NAVOTimePeriod.March });
            Add(new T { TimePeriod = NAVOTimePeriod.April });
            Add(new T { TimePeriod = NAVOTimePeriod.May });
            Add(new T { TimePeriod = NAVOTimePeriod.June});
            Add(new T { TimePeriod = NAVOTimePeriod.July });
            Add(new T { TimePeriod = NAVOTimePeriod.August });
            Add(new T { TimePeriod = NAVOTimePeriod.September });
            Add(new T { TimePeriod = NAVOTimePeriod.October });
            Add(new T { TimePeriod = NAVOTimePeriod.November });
            Add(new T { TimePeriod = NAVOTimePeriod.December });
            Add(new T { TimePeriod = NAVOTimePeriod.Spring });
            Add(new T { TimePeriod = NAVOTimePeriod.Summer });
            Add(new T { TimePeriod = NAVOTimePeriod.Fall });
            Add(new T { TimePeriod = NAVOTimePeriod.Winter });
            Add(new T { TimePeriod = NAVOTimePeriod.Cold });
            Add(new T { TimePeriod = NAVOTimePeriod.Warm });
        }

        public T this[NAVOTimePeriod timePeriod] { get { return Find(t => t.TimePeriod == timePeriod); } }
    }

    public class AvailableTimePeriods : TimePeriods<AvailableTimePeriod> { }
    public class SelectedTimePeriods : TimePeriods<SelectedTimePeriod> { }
#endif
}