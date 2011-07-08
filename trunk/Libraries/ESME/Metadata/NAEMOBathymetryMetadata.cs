using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows;
using Cinch;
using ESME.Environment;
using ESME.Overlay;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Metadata
{
    public class NAEMOBathymetryMetadata : NAEMOMetadataBase
    {
        new internal static readonly List<Type> ReferencedTypes = new List<Type>(NAEMOMetadataBase.ReferencedTypes){typeof(EarthCoordinate), typeof(GeoRect)};

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

            var bathy2D = Environment2DData.FromYXZ(naemoBathymetryFilename, -1);
            var bathysize = Math.Max(bathy2D.Longitudes.Count, bathy2D.Latitudes.Count);
            var screenSize = Math.Min(SystemParameters.PrimaryScreenWidth, SystemParameters.PrimaryScreenHeight);
            Bitmap displayBitmap;
            if (bathysize > screenSize)
            {
                var scaleFactor = screenSize / bathysize;
                var decimatedValues = Decimator2D.Decimate(bathy2D.FieldData, (int)(bathy2D.Longitudes.Count * scaleFactor), (int)(bathy2D.Latitudes.Count * scaleFactor));
                displayBitmap = colormap.ToBitmap(decimatedValues, bathy2D.Minimum.Data, bathy2D.Maximum.Data < 0 ? bathy2D.Maximum.Data : 8000);
            }
            else
            {
                displayBitmap = colormap.ToBitmap(bathy2D.FieldData, bathy2D.Minimum.Data, bathy2D.Maximum.Data < 0 ? bathy2D.Maximum.Data : 8000);
            }
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

        public void Save(string filename = null) { Save(this, filename); }

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

        #region public string OverlayFilename { get; set; }

        public string OverlayFilename
        {
            get { return _overlayFilename; }
            set
            {
                if (_overlayFilename == value) return;
                _overlayFilename = value;
                NotifyPropertyChanged(OverlayFilenameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs OverlayFilenameChangedEventArgs = ObservableHelper.CreateArgs<NAEMOBathymetryMetadata>(x => x.OverlayFilename);
        string _overlayFilename;

        #endregion

    }
}