using System;
using System.Collections.Generic;
using System.Windows.Media;
using System.IO;
using HRC.Navigation;

namespace ESME.NEMO.Overlay
{
    public partial class OverlayFile
    {
        private string _fileName;

        public OverlayFile()
        {
        }

        public OverlayFile(string fileName)
        {
            _fileName = fileName;
            Parse(_fileName);
        }

        public OverlayShape[] Shapes { get; private set; }

        public string FileName
        {
            get { return _fileName; }
            set
            {
                _fileName = value;
                Parse(_fileName);
            }
        }

        public Color Color
        {
            get { return Shapes[0].Color; }
            set
            {
                foreach (var s in Shapes)
                    s.Color = value;
            }
        }

        public string SourceOverlay { get; private set; }

        public string ExpandedRange { get; private set; }

        void Parse(string fileName)
        {
            string sourceOverlay, expandedRange;
            Shapes = Parse(fileName, out sourceOverlay, out expandedRange);
            SourceOverlay = sourceOverlay;
            ExpandedRange = expandedRange;
        }

        public override string ToString()
        {
            return Path.GetFileName(_fileName);
        }

        public static GeoRect ValidateCoordinates(string fieldData, string overlayName, out List<Geo> geos, out string errorText)
        {
            errorText = "";
            var lineSeparators = new[] { '\r', '\n' };
            var lines = fieldData.Split(lineSeparators, StringSplitOptions.RemoveEmptyEntries);
            if (overlayName != null) overlayName += ": ";
            if (lines.Length < 4) errorText += overlayName + "There must be at least four points given to define an area" + System.Environment.NewLine;
            geos = new List<Geo>();
            var lineCount = 0;
            foreach (var line in lines)
            {
                lineCount++;
                var coordSeparators = new[] { ',', ' ' };
                var coords = line.Split(coordSeparators, StringSplitOptions.RemoveEmptyEntries);
                double lat, lon;
                if (coords.Length == 2 && double.TryParse(coords[0], out lat) && (double.TryParse(coords[1], out lon)))
                    geos.Add(new Geo(lat, lon));
                else
                    errorText += string.Format(overlayName + "Invalid latitude/longitude on line {0}. Please use decimal degrees" + System.Environment.NewLine, lineCount);
            }
            if (string.IsNullOrEmpty(errorText))
            {
                if (geos.Count < 4) errorText += overlayName + "There must be at least four points given to define an area" + System.Environment.NewLine;
                else
                {
                    var overlayLineSegments = new OverlayLineSegments(geos.ToArray(), Colors.Black);
                    if (!overlayLineSegments.IsUsableAsPerimeter)
                        errorText += overlayName + "The points provided are not usable as a perimeter.  Line segments are used in the order given, and cannot cross each other.  The resulting polygon must also be closed." + System.Environment.NewLine;
                    else return new GeoRect(overlayLineSegments.BoundingBox);
                }
            }
            geos = null;
            return null;
        }

        public static GeoRect ValidateFile(string overlayFileName, string overlayName, out string errorText)
        {
            errorText = "";
            if (!File.Exists(overlayName))
            {
                errorText += "Specified " + overlayName + " does not exist" + System.Environment.NewLine;
            }
            try
            {
                var myOvr = new OverlayFile(overlayFileName);
                if (myOvr.Shapes.Length != 1 || !myOvr.Shapes[0].IsUsableAsPerimeter)
                    errorText += "Specified " + overlayName + " is invalid" + System.Environment.NewLine;
                else return new GeoRect(myOvr.Shapes[0].BoundingBox);
            }
            catch (Exception e)
            {
                errorText += "Error loading " + overlayFileName + ": " + e.Message + System.Environment.NewLine;
            }
            return null;
        }

        public static void Create(string newOverlayFileName, IEnumerable<Geo> coords, string sourceOverlayFileName = null, double? expandedRangeInKm = null)
        {
            using (var writer = new StreamWriter(newOverlayFileName))
            {
                if (!string.IsNullOrEmpty(sourceOverlayFileName)) writer.WriteLine("# sourceOverlay={0}", sourceOverlayFileName);
                if (expandedRangeInKm.HasValue) writer.WriteLine("# expandedRangeKm={0:0.####}", expandedRangeInKm.Value);
                writer.WriteLine("navigation");
                writer.WriteLine("green");
                writer.WriteLine("solid");
                writer.WriteLine("move");
                var first = true;
                foreach (var coordinate in coords)
                {
                    writer.WriteLine("{0}  {1}", coordinate.Latitude, coordinate.Longitude);
                    if (first) writer.WriteLine("lines");
                    first = false;
                }
            }
        }


    }
}