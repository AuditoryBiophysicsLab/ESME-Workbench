using System;
using System.Collections.Specialized;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.Locations;
using ESME.Model;
using ESME.Plugins;
using ESME.Scenarios;
using ESME.TransmissionLoss.Bellhop;
using HRC;
using HRC.Aspects;
using HRC.Navigation;
using HRC.Utility;
using MEFedMVVM.ViewModelLocator;

namespace ESME.TransmissionLoss
{
    [PartCreationPolicy(CreationPolicy.Shared)]
    [ExportService(ServiceType.Both, typeof(TransmissionLossCalculatorService))]
    public class TransmissionLossCalculatorService
    {
        public TransmissionLossCalculatorService()
        {
#if false
            WorkQueue.CollectionChanged += (s, e) =>
            {
                switch (e.Action)
                {
                    case NotifyCollectionChangedAction.Add:
                        break;
                    case NotifyCollectionChangedAction.Remove:
                        break;
                    case NotifyCollectionChangedAction.Replace:
                        break;
                    case NotifyCollectionChangedAction.Reset:
                        break;
                    case NotifyCollectionChangedAction.Move:
                        break;
                    default:
                        throw new ApplicationException(string.Format("Unknown CollectionChanged action: {0}", e.Action));
                }
            };
#endif
        }

        public TransmissionLossCalculatorService(MasterDatabaseService databaseService, IPluginManagerService pluginService, EnvironmentalCacheService cacheService, float rangeCellSize, float depthCellSize)
        {
            _databaseService = databaseService;
            _pluginService = pluginService;
            _cacheService = cacheService;
            _rangeCellSize = rangeCellSize;
            _depthCellSize = depthCellSize;
            //var items = from radial in _databaseService.Context.Radials
            //            where radial.IsCalculated == false
            //            select new PercentProgress<Radial>(radial);
            //WorkQueue.AddRange(items);
        }
        [Import] MasterDatabaseService _databaseService;
        [Import] IPluginManagerService _pluginService;
        [Import] EnvironmentalCacheService _cacheService;
        readonly float _rangeCellSize;
        readonly float _depthCellSize;
        StringBuilder _bellhopOutput;
        //[Initialize, UsedImplicitly] public ObservableList<PercentProgress<Radial>> WorkQueue { get; private set; }

        public void TestAdd(Radial radial, string directoryPath)
        {
            Calculate(new PercentProgress<Radial>(radial), directoryPath);
        }

        void Calculate(PercentProgress<Radial> item, string directoryPath)
        {
            var radial = item.ProgressTarget;
            var transmissionLoss = radial.TransmissionLoss;
            var mode = transmissionLoss.Mode;
            var platform = mode.Source.Platform;
            var analysisPoint = transmissionLoss.AnalysisPoint;
            var scenario = analysisPoint.Scenario;
            var timePeriod = scenario.TimePeriod;
            var wind = (Wind)_cacheService[scenario.Wind];
            var soundSpeed = (SoundSpeed)_cacheService[scenario.SoundSpeed];
            var sediment = (Sediment)_cacheService[scenario.Sediment];
            var bathymetry = (Bathymetry)_cacheService[scenario.Bathymetry];
            soundSpeed.Extend(bathymetry.DeepestPoint);
            var centerPoint = ((Geo)analysisPoint.Geo).Offset(Geo.KilometersToRadians(radial.Length / 2000), Geo.DegreesToRadians(radial.Bearing));
            var windSpeed = wind[timePeriod].EnvironmentData.GetNearestPoint(centerPoint);
            var soundSpeedProfile = soundSpeed[timePeriod].EnvironmentData.GetNearestPoint(centerPoint);
            var sedimentType = sediment.Samples.GetNearestPoint(centerPoint);
            var radialSegment = new GeoSegment(analysisPoint.Geo, Geo.KilometersToRadians(radial.Length / 1000f), Geo.DegreesToRadians(radial.Bearing));
            //var bottomProfile = new BottomProfile(100, new Transect("transect", analysisPoint.Geo, radial.Bearing, radial.Length), bathymetry);
            var bottomProfile = new BottomProfile(100, radialSegment, bathymetry);
            var sourceDepth = platform.Depth;
            if (mode.Depth.HasValue) sourceDepth += mode.Depth.Value;
            if (!Directory.Exists(directoryPath)) Directory.CreateDirectory(directoryPath);
            var baseFilename = Path.Combine(directoryPath, Path.GetFileNameWithoutExtension(Path.GetRandomFileName()));
            CreateBellhopEnvironmentFiles(baseFilename, soundSpeedProfile, sedimentType, bottomProfile, windSpeed.Data, (float)Math.Sqrt(mode.HighFrequency * mode.LowFrequency), sourceDepth, (float)radial.Length, mode.VerticalBeamWidth, mode.DepressionElevationAngle, (float)(bottomProfile.MaxDepth * 1.01), _rangeCellSize, _depthCellSize, true, false, 1500);
            var bellhopProcess = new TransmissionLossProcess
            {
                StartInfo = new ProcessStartInfo(@"C:\Projects\ESME Deliverables\Utilities\bellhop\Bellhop.exe", Path.GetFileName(baseFilename))
                {
                    CreateNoWindow = true,
                    UseShellExecute = false,
                    RedirectStandardInput = false,
                    RedirectStandardOutput = true,
                    RedirectStandardError = true,
                    WorkingDirectory = directoryPath
                }
            };
#if false
            // With the 'stock' version of Bellhop, there is nothing written to standard output
            // Rather, it all goes into the .prt file.  At some point, we will want to modify that
            // behavior so we can use the output to drive the progress bar.
            var bellhopOutput = new StringBuilder();
            bellhopProcess.OutputDataReceived += (s, e) =>
            {
                var theProcess = (TransmissionLossProcess)s;
                char[] separators = { ' ', '=' };

                // Collect the sort command output.
                if (!String.IsNullOrEmpty(e.Data))
                {
                    // Add the text to the collected output.
                    bellhopOutput.Append(e.Data);
                    var curLine = e.Data.Trim();
                    string[] fields;
                    if (curLine.StartsWith("Tracing beam"))
                    {
                        fields = curLine.Split(separators, StringSplitOptions.RemoveEmptyEntries);
                        theProcess.CurBeam = int.Parse(fields[2]);
                        //System.Diagnostics.Debug.WriteLine("Currently tracing beam " + theProcess.CurBeam + " of " + theProcess.MaxBeam + " (" + theProcess.ProgressPercent.ToString("0.0") + "%)");
                    }
                    if (curLine.StartsWith("Number of beams"))
                    {
                        fields = curLine.Split(separators);
                        theProcess.BeamCount = int.Parse(fields[fields.Length - 1]);
                    }
                }
            };
#endif
            bellhopProcess.Start();
            radial.CalculationStarted = DateTime.Now;
            bellhopProcess.PriorityClass = ProcessPriorityClass.BelowNormal;
            bellhopProcess.BeginOutputReadLine();
            while (!bellhopProcess.HasExited) Thread.Sleep(100);
            radial.CalculationCompleted = DateTime.Now;
            var output = new TransmissionLossRadial((float)radial.Bearing, new BellhopOutput(baseFilename + ".shd"));
            radial.Filename = Path.GetFileName(baseFilename + ".shd");
            radial.Ranges = output.Ranges.ToArray();
            radial.Depths = output.Depths.ToArray();
            radial.IsCalculated = true;
            _databaseService.Context.SaveChanges();
        }

        public static void CreateBellhopEnvironmentFiles(string baseFilename, SoundSpeedProfile ssp, SedimentType sediment, BottomProfile bottomProfile, float windSpeed, float frequency, float sourceDepth, float radius, float verticalBeamWidth, float depressionElevationAngle, float maxCalculationDepthMeters, float rangeCellSize, float depthCellSize, bool useSurfaceReflection, bool generateArrivalsFile, int beamCount)
        {
            var depthCellCount = (int)Math.Ceiling(bottomProfile.MaxDepth / depthCellSize);
            var rangeCellCount = (int)Math.Ceiling(radius / rangeCellSize);
            using (var envFile = new StreamWriter(baseFilename + ".env", false))
            {
                envFile.WriteLine("'Bellhop'");
                envFile.WriteLine("{0},", frequency);
                envFile.WriteLine("1"); // was NMEDIA in gui_genbellhopenv.m
                envFile.WriteLine(useSurfaceReflection ? "'CFLT'," : "'CVLT',");

                //if (depthCellCount < 5) throw new BathymetryTooShallowException("Error: Maximum depth of transect (" + maxCalculationDepthMeters + " meters) less than minimum required for transmission loss calculations.\nPlease choose a different location for this transect.");

                envFile.WriteLine("0, 0.0, {0}", ssp.Data[ssp.Data.Count - 1].Depth);
                ;
                foreach (var soundSpeedSample in ssp.Data)
                    envFile.WriteLine("{0} {1} 0.0 1.0 0.0 0.0", soundSpeedSample.Depth, soundSpeedSample.SoundSpeed);

                envFile.WriteLine("'A*' 0.0"); // A = Acoustic halfspace, * = read bathymetry file 'BTYFIL', 0.0 = bottom roughness (currently ignored)
                envFile.WriteLine("{0} {1} {2} {3} {4} {5} /", maxCalculationDepthMeters, sediment.CompressionWaveSpeed, sediment.ShearWaveSpeed, sediment.Density, sediment.LossParameter, 0);
                // Source and Receiver Depths and Ranges
                envFile.WriteLine("1"); // Number of Source Depths
                envFile.WriteLine("{0} /", sourceDepth); // source depth
                envFile.WriteLine("{0}", depthCellCount); // Number of Receiver Depths
                envFile.WriteLine("0.0 {0} /", maxCalculationDepthMeters);
                envFile.WriteLine("{0}", rangeCellCount); // Number of receiver ranges
                envFile.WriteLine("0.0 {0} /", radius / 1000.0);

                envFile.WriteLine(generateArrivalsFile ? "'AB'" : "'I'");
                envFile.WriteLine("{0}", beamCount); // Number of beams
                var verticalHalfAngle = verticalBeamWidth / 2;
                var angle1 = depressionElevationAngle - verticalHalfAngle;
                var angle2 = depressionElevationAngle + verticalHalfAngle;
                envFile.WriteLine("{0} {1} /", angle1, angle2); // Beam fan half-angles (negative angles are toward the surface
                envFile.WriteLine("0.0 {0} {1}", ssp.Data[ssp.Data.Count - 1].Depth * 2, (radius / 1000.0) * 1.01); // step zbox(meters) rbox(km)
            }
            using (var trcFile = new StreamWriter(baseFilename + ".trc", false))
            {
                var topReflectionCoefficients = Bellhop.Bellhop.GenerateReflectionCoefficients(windSpeed, frequency);
                trcFile.WriteLine(topReflectionCoefficients.GetLength(0));
                for (var rowIndex = 0; rowIndex < topReflectionCoefficients.GetLength(0); rowIndex++)
                    trcFile.WriteLine("{0} {1} {2} ", topReflectionCoefficients[rowIndex, 0], topReflectionCoefficients[rowIndex, 1], topReflectionCoefficients[rowIndex, 2]);
            }
            using (var writer = new StreamWriter(baseFilename + ".bty")) writer.Write(bottomProfile.ToBellhopString());
        }
    }
}
