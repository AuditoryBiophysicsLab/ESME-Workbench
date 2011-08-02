using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using System.Windows.Threading;
using ESME.NEMO.Overlay;
using HRC.Navigation;

namespace ESME.Environment.Descriptors
{
    public class RangeComplexDescriptors : NAEMODescriptors<RangeComplexDescriptor>
    {
        private RangeComplexDescriptors()
        {
        }

        public static RangeComplexDescriptors ReadCSV(string fileName, Dispatcher dispatcher)
        {
            var result = new RangeComplexDescriptors
            {
                             FileName = fileName,
                             Dispatcher = dispatcher,
                         };

            result.Add(new KeyValuePair<string, RangeComplexDescriptor>("[None]", null));

            var lines = File.ReadAllLines(fileName);
            Parallel.ForEach(lines, line =>
            {
                if (line == null) return;
                var curLine = line.Trim();
                if ((curLine.Trim() == "") || curLine.StartsWith("!") || curLine.StartsWith("#")) return;
                var fields = curLine.Split(',');
                var simAreaName = fields[0];
                var latString = fields[1];
                var lonString = fields[2];
                var heightString = fields[3];
                var geoidString = fields[4];
                var opsLimitFile = fields[5];
                var simLimitFile = fields[6];
                double latitude;
                double longitude;
                double height;
                double geoid;
                if (string.IsNullOrEmpty(simAreaName)) throw new FormatException(string.Format("RangeComplexDescriptors: Error reading sim area file \"{0}\"\nError: Invalid sim area name", fileName));
                if (string.IsNullOrEmpty(opsLimitFile)) throw new FormatException(string.Format("RangeComplexDescriptors: Error reading sim area file \"{0}\"\nError: Invalid OpsLimit filename", fileName));
                if (string.IsNullOrEmpty(simLimitFile)) throw new FormatException(string.Format("RangeComplexDescriptors: Error reading sim area file \"{0}\"\nError: Invalid SimLimit filename", fileName));
                if (!double.TryParse(latString, out latitude)) throw new FormatException(string.Format("RangeComplexDescriptors: Error reading sim area file \"{0}\"\nError: Invalid latitude", fileName));
                if (!double.TryParse(lonString, out longitude)) throw new FormatException(string.Format("RangeComplexDescriptors: Error reading sim area file \"{0}\"\nError: Invalid longitude", fileName));
                if (!double.TryParse(heightString, out height)) throw new FormatException(string.Format("RangeComplexDescriptors: Error reading sim area file \"{0}\"\nError: Invalid height", fileName));
                if (!double.TryParse(geoidString, out geoid)) throw new FormatException(string.Format("RangeComplexDescriptors: Error reading sim area file \"{0}\"\nError: Invalid geoid separation value", fileName));
                result.AddRangeComplex(simAreaName, height, latitude, longitude, geoid, opsLimitFile, simLimitFile, dispatcher);
            });
            result.Sort();
            return result;
        }

        public string FileName { get; private set; }

        public RangeComplexDescriptor AddRangeComplex(string rangeComplexName, double height, double latitude, double longitude, double geoid, string opsLimitFile, string simLimitFile, Dispatcher dispatcher)
        {
            NAEMOOverlayDescriptors overlayDescriptors = null;
            NAEMOBathymetryDescriptors bathymetryDescriptors = null;
            NAEMOEnvironmentDescriptors environmentDescriptors = null;
            Parallel.Invoke(() => overlayDescriptors = new NAEMOOverlayDescriptors(rangeComplexName) { Dispatcher = dispatcher },
                            () => bathymetryDescriptors = new NAEMOBathymetryDescriptors(rangeComplexName) { Dispatcher = dispatcher },
                            () => environmentDescriptors = new NAEMOEnvironmentDescriptors(rangeComplexName) { Dispatcher = dispatcher });
            if (overlayDescriptors == null || bathymetryDescriptors == null || environmentDescriptors == null) throw new ApplicationException("Error initializing overlay, bathymetry or environment descriptors");
            CreateRangeComplexDirectories(Path.Combine(Globals.AppSettings.ScenarioDataDirectory, rangeComplexName));
            var rangeComplexDescriptor = new RangeComplexDescriptor
            {
                Data = new RangeComplex(latitude, longitude)
                {
                        Name = rangeComplexName,
                        Height = height,
                        GeoidSeparation = geoid,
                        OpsLimitFile = opsLimitFile,
                        SimLimitFile = simLimitFile,
                },
                NAEMOOverlayDescriptors = overlayDescriptors,
                NAEMOBathymetryDescriptors = bathymetryDescriptors,
                NAEMOEnvironmentDescriptors = environmentDescriptors,
            };
            lock (this)
            {
                Debug.WriteLine("{0}: Adding range complex \"{1}\"", DateTime.Now, rangeComplexName);
                Add(new KeyValuePair<string, RangeComplexDescriptor>(rangeComplexName, rangeComplexDescriptor));
            }
            return rangeComplexDescriptor;
        }

        static void CreateRangeComplexDirectories(string rangeComplexPath)
        {
            Directory.CreateDirectory(rangeComplexPath);
            Directory.CreateDirectory(Path.Combine(rangeComplexPath, "Areas"));
            Directory.CreateDirectory(Path.Combine(rangeComplexPath, "Bathymetry"));
            Directory.CreateDirectory(Path.Combine(rangeComplexPath, "Environment"));
            Directory.CreateDirectory(Path.Combine(rangeComplexPath, "GeographicAreas"));
            Directory.CreateDirectory(Path.Combine(rangeComplexPath, "Images"));
            Directory.CreateDirectory(Path.Combine(rangeComplexPath, "Species"));
        }

        public RangeComplexDescriptor CreateRangeComplex(string rangeComplexName, double height, double latitude, double longitude, double geoid, string opsLimitFile, List<EarthCoordinate> opAreaBoundsCoordinates, string simLimitFile, List<EarthCoordinate> simAreaBoundsCoordinates, Dispatcher dispatcher)
        {
            var rangeComplexPath = Path.Combine(Globals.AppSettings.ScenarioDataDirectory, rangeComplexName);
            if (!Directory.Exists(rangeComplexPath))
            {
                CreateRangeComplexDirectories(rangeComplexPath);
                var areasPath = Path.Combine(rangeComplexPath, "Areas");
                if (string.IsNullOrEmpty(opsLimitFile) && opAreaBoundsCoordinates == null) throw new ApplicationException("Operational area limit file OR operational area bounds MUST be provided, but neither were");
                if (!string.IsNullOrEmpty(opsLimitFile) && opAreaBoundsCoordinates != null) throw new ApplicationException("Operational area limit file OR operational area bounds MUST be provided, but not both");
                var opsOverlayFilename = Path.Combine(areasPath, String.Format("{0}_OpArea.ovr", rangeComplexName));
                if (!string.IsNullOrEmpty(opsLimitFile)) File.Copy(opsLimitFile, opsOverlayFilename);
                if (opAreaBoundsCoordinates != null) OverlayFile.Create(opsOverlayFilename, opAreaBoundsCoordinates);
                opsLimitFile = opsOverlayFilename;

                if (string.IsNullOrEmpty(simLimitFile) && simAreaBoundsCoordinates == null) throw new ApplicationException("Simulation area limit file OR simulation area bounds MUST be provided, but neither were");
                if (!string.IsNullOrEmpty(simLimitFile) && simAreaBoundsCoordinates != null) throw new ApplicationException("Simulation area limit file OR simulation area bounds MUST be provided, but not both");
                var simOverlayFilename = Path.Combine(areasPath, String.Format("{0}_SimArea.ovr", rangeComplexName));
                if (!string.IsNullOrEmpty(simLimitFile)) File.Copy(simLimitFile, simOverlayFilename);
                if (simAreaBoundsCoordinates != null) OverlayFile.Create(simOverlayFilename, simAreaBoundsCoordinates);
                simLimitFile = simOverlayFilename;

                lock (this)
                {
                    var needsExtraNewline = !File.ReadAllText(FileName).EndsWith("\n");
                    using (var writer = new StreamWriter(FileName, true))
                    {
                        if (needsExtraNewline) writer.WriteLine();
                        writer.WriteLine("{0},{1:0.0###},{2:0.0###},{3:0.0###},{4:0.0###},{5},{6}", rangeComplexName,
                                         latitude, longitude, height, geoid,
                                         Path.GetFileName(opsOverlayFilename), Path.GetFileName(simOverlayFilename));
                    }
                }
            }
            return AddRangeComplex(rangeComplexName, height, latitude, longitude, geoid, Path.GetFileName(opsLimitFile), Path.GetFileName(simLimitFile), dispatcher);
        }

        public void DeleteRangeComplex(RangeComplexDescriptor rangeComplexToDelete)
        {
            var rangeComplexName = rangeComplexToDelete.Data.Name;
            var simAreaCSVFileContents = File.ReadAllLines(FileName);
            var oldCSVFileName = FileName;
            var newCSVFileName = FileName + ".new";
            using (var streamWriter = new StreamWriter(newCSVFileName)) foreach (var curLine in simAreaCSVFileContents.Where(curLine => !curLine.StartsWith(rangeComplexName))) streamWriter.WriteLine(curLine);
            File.Delete(oldCSVFileName);
            File.Move(newCSVFileName, oldCSVFileName);

            Task.Factory.StartNew(() =>
            {
                Directory.Delete(Path.Combine(Globals.AppSettings.ScenarioDataDirectory, rangeComplexName), true);
                Remove(rangeComplexToDelete);
            });
        }

    }
}
