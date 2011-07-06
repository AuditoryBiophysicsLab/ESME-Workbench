using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Text;
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

            var metaDataFilename = naemoBathymetryFilename;
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
                overlayFilename = Path.GetFileName(metaDataFilename.Substring(0, overlayNameEndIndex) + ".ovr");
                if (!File.Exists(Path.Combine(areasPath, overlayFilename)))
                    overlayFilename = null;
            }

            if (overlayFilename == null)
            {
                var overlayFiles = Directory.GetFiles(areasPath, "*.ovr");
                foreach (var overlayFile in overlayFiles)
                {
                    var overlay = new OverlayFile(overlayFile);
                    var geoRect = new GeoRect(overlay.Shapes[0].BoundingBox);
                    if (!geoRect.Equals(bathymetry.Samples.GeoRect)) continue;
                    overlayFilename = Path.GetFileName(overlayFile);
                    break;
                }
            }

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