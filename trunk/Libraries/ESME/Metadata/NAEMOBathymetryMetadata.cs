using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing.Imaging;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows;
using Cinch;
using ESME.Environment;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Metadata
{
    public class NAEMOBathymetryMetadata : NAEMOMetadataBase
    {
        new internal static readonly List<Type> ReferencedTypes = new List<Type>(NAEMOMetadataBase.ReferencedTypes) { typeof(EarthCoordinate), typeof(GeoRect) };

#if true
        public static NAEMOBathymetryMetadata FromBathymetryFile(string naemoBathymetryFilename, out Bathymetry bathymetry)
        {
            bathymetry = Bathymetry.FromYXZ(naemoBathymetryFilename, -1);

            var metaDataFilename = Path.Combine(Path.GetDirectoryName(naemoBathymetryFilename), Path.GetFileNameWithoutExtension(naemoBathymetryFilename) + ".xml");
            var areasPath = Path.Combine(Path.GetDirectoryName(Path.GetDirectoryName(naemoBathymetryFilename)), "Areas");
            var fields = naemoBathymetryFilename.Split('_');

            var resolution = (float)bathymetry.Samples.Resolution;
            if (float.IsNaN(resolution) && (fields.Length >= 2))
            {
                switch (fields[fields.Length - 2].ToLower())
                {
                    case "0.05min":
                    case "0.05":
                        resolution = 0.05f;
                        break;
                    case "0.1min":
                    case "0.1":
                        resolution = 0.1f;
                        break;
                    case "0.5min":
                    case "0.5":
                        resolution = 0.5f;
                        break;
                    case "1.0min":
                    case "1min":
                    case "1.0":
                    case "1":
                        resolution = 1;
                        break;
                    case "2.0min":
                    case "2min":
                    case "2.0":
                    case "2":
                        resolution = 2;
                        break;
                    default:
                        resolution = float.NaN;
                        break;
                }
            }

            string overlayFilename = null;
            if (fields.Length >= 3)
            {
                var overlayNameEndIndex = metaDataFilename.IndexOf(fields[fields.Length - 2]) - 1;
                overlayFilename = Path.GetFileName(metaDataFilename.Substring(0, overlayNameEndIndex));
                if (!File.Exists(Path.Combine(areasPath, overlayFilename) + ".ovr"))
                    overlayFilename = null;
            }

            var colormap = new DualColormap(Colormap.Summer, Colormap.Jet)
            {
                Threshold = 0,
            };

            var bathysize = Math.Max(bathymetry.Samples.Longitudes.Length, bathymetry.Samples.Latitudes.Length);
            var screenSize = Math.Min(SystemParameters.PrimaryScreenWidth, SystemParameters.PrimaryScreenHeight);
            var displayValues = bathymetry.Samples;
            if (bathysize > screenSize)
            {
                var scaleFactor = screenSize / bathysize;
                displayValues = EnvironmentData<EarthCoordinate<float>>.Decimate(bathymetry.Samples, (int)(bathymetry.Samples.Longitudes.Length * scaleFactor), (int)(bathymetry.Samples.Latitudes.Length * scaleFactor));
            }
            var bitmapData = new float[displayValues.Longitudes.Length, displayValues.Latitudes.Length];
            for (var latIndex = 0; latIndex < bitmapData.GetLength(1); latIndex++)
                for (var lonIndex = 0; lonIndex < bitmapData.GetLength(0); lonIndex++)
                    bitmapData[lonIndex, latIndex] = displayValues[(uint)lonIndex, (uint)latIndex].Data;
            var displayBitmap = colormap.ToBitmap(bitmapData, bathymetry.Minimum.Data, bathymetry.Maximum.Data < 0 ? bathymetry.Maximum.Data : 8000);
            var imagesPath = Path.Combine(Path.GetDirectoryName(Path.GetDirectoryName(naemoBathymetryFilename)), "Images");
            displayBitmap.Save(Path.Combine(imagesPath, Path.GetFileNameWithoutExtension(naemoBathymetryFilename) + ".bmp"), ImageFormat.Bmp);
            var sb = new StringBuilder();
            sb.AppendLine(resolution.ToString());
            sb.AppendLine("0.0");
            sb.AppendLine("0.0");
            sb.AppendLine(resolution.ToString());
            sb.AppendLine(bathymetry.Samples.GeoRect.West.ToString());
            sb.AppendLine(bathymetry.Samples.GeoRect.North.ToString());
            using (var writer = new StreamWriter(Path.Combine(imagesPath, Path.GetFileNameWithoutExtension(naemoBathymetryFilename) + ".bpw"), false))
                writer.Write(sb.ToString());

            return new NAEMOBathymetryMetadata
                       {
                           Resolution = resolution,
                           Bounds = bathymetry.Samples.GeoRect,
                           OverlayFilename = overlayFilename,
                           PointCount = bathymetry.Samples.Count,
                           Filename = metaDataFilename,
                       };
        }

#endif
        public static void CreateMissingBathymetryMetadata(string bathymetryPath)
        {
            var files = Directory.GetFiles(bathymetryPath, "*.txt");
            foreach (var file in files.Where(file => !File.Exists(MetadataFilename(file))).Where(file => !file.ToLower().EndsWith("security_readme.txt")))
            {
                Bathymetry bathymetry;
                var bathyMetadata = FromBathymetryFile(file, out bathymetry);
                bathyMetadata.Save();
            }
        }

        public static NAEMOBathymetryMetadata Load(string metaDataFilename) { return Load<NAEMOBathymetryMetadata>(metaDataFilename); }

        public void Save(string filename = null) { Save(this, ReferencedTypes, filename); }

        public virtual string GeneratedFilename
        {
            get
            {
                var sb = new StringBuilder();
                sb.Append(string.IsNullOrEmpty(OverlayFilename) ? "NoOverlay" : Path.GetFileNameWithoutExtension(OverlayFilename));
                sb.Append(string.Format("_{0:0.0#}min_bathy", Resolution));
                return sb.ToString();
            }
        }

        public string GeneratedBathymetryFilename { get { return GeneratedFilename + ".txt"; } }
        public string GeneratedMetadataFilename { get { return GeneratedFilename + ".txt"; } }

        #region public float Resolution { get; set; }

        public float Resolution
        {
            get { return _resolution; }
            set
            {
                if (_resolution == value) return;
                _resolution = value;
                NotifyPropertyChanged(ResolutionChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ResolutionChangedEventArgs = ObservableHelper.CreateArgs<NAEMOBathymetryMetadata>(x => x.Resolution);
        float _resolution;

        #endregion

        #region public int PointCount { get; set; }

        public int PointCount
        {
            get { return _pointCount; }
            set
            {
                if (_pointCount == value) return;
                _pointCount = value;
                NotifyPropertyChanged(PointCountChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs PointCountChangedEventArgs = ObservableHelper.CreateArgs<NAEMOBathymetryMetadata>(x => x.PointCount);
        private int _pointCount;

        #endregion

    }
}