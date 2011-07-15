using System;
using System.Collections.Generic;
using System.Windows.Media;
using System.IO;
using HRC.Navigation;

namespace ESME.Overlay
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
            Shapes = Parse(fileName);
        }

        public OverlayShape[] Shapes { get; private set; }

        public string FileName
        {
            get { return _fileName; }
            set
            {
                _fileName = value;
                Shapes = Parse(value);
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

        public override string ToString()
        {
            return Path.GetFileName(_fileName);
        }

        public static GeoRect ValidateCoordinates(string fieldData, string overlayName, out List<EarthCoordinate> earthCoordinates, out  string errorText)
        {
            errorText = "";
            var lineSeparators = new[] { '\r', '\n' };
            var lines = fieldData.Split(lineSeparators, StringSplitOptions.RemoveEmptyEntries);
            if (lines.Length < 4) errorText += overlayName + ": There must be at least four points given to define an area\n";
            earthCoordinates = new List<EarthCoordinate>();
            var lineCount = 0;
            foreach (var line in lines)
            {
                lineCount++;
                var coordSeparators = new[] { ',', ' ' };
                var coords = line.Split(coordSeparators, StringSplitOptions.RemoveEmptyEntries);
                double lat, lon;
                if (coords.Length == 2 && double.TryParse(coords[0], out lat) && (double.TryParse(coords[1], out lon)))
                    earthCoordinates.Add(new EarthCoordinate(lat, lon));
                else
                    errorText += string.Format(overlayName + ": Invalid latitude/longitude on line {0}. Please use decimal degrees\n", lineCount);
            }
            if (string.IsNullOrEmpty(errorText))
            {
                if (earthCoordinates.Count < 4) errorText += overlayName + ": There must be at least four points given to define an area\n";
                else
                {
                    var overlayLineSegments = new OverlayLineSegments(earthCoordinates.ToArray(), Colors.Black);
                    if (!overlayLineSegments.IsUsableAsPerimeter)
                        errorText += overlayName + ": The points provided are not usable as a perimeter.  Line segments are used in the order given, and cannot cross each other.  The resulting polygon must also be closed\n";
                    else return new GeoRect(overlayLineSegments.BoundingBox);
                }
            }
            return null;
        }

        public static GeoRect ValidateFile(string overlayFileName, string overlayName, out List<EarthCoordinate> earthCoordinates, out string errorText)
        {
            errorText = "";
            earthCoordinates = null;
            try
            {
                var myOvr = new OverlayFile(overlayFileName);
                if (myOvr.Shapes.Length != 1 || !myOvr.Shapes[0].IsUsableAsPerimeter)
                    errorText += "Specified " + overlayName + " file is invalid\n";
                else
                {
                    earthCoordinates = myOvr.Shapes[0].EarthCoordinates;
                    return new GeoRect(myOvr.Shapes[0].BoundingBox);
                }
            }
            catch (Exception e)
            {
                errorText += "Error loading " + overlayFileName + ": " + e.Message + "\n";
            }
            return null;
        }
    }
}