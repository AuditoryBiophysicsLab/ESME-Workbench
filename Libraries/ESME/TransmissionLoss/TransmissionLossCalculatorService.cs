﻿using System;
using System.Collections.Specialized;
using System.ComponentModel.Composition;
using System.Data.Entity;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Threading;
using System.Threading.Tasks.Dataflow;
using ESME.Environment;
using ESME.Locations;
using ESME.Model;
using ESME.Plugins;
using ESME.Scenarios;
using ESME.TransmissionLoss.Bellhop;
using HRC.Aspects;
using HRC.Navigation;
using HRC.Utility;
using HRC.ViewModels;
using MEFedMVVM.ViewModelLocator;
using HRC.Collections;

namespace ESME.TransmissionLoss
{
    [PartCreationPolicy(CreationPolicy.Shared)]
    [ExportService(ServiceType.Both, typeof(TransmissionLossCalculatorService))]
    public class TransmissionLossCalculatorService : ViewModelBase, IPartImportsSatisfiedNotification
    {
        public TransmissionLossCalculatorService()
        {
            WorkQueue = new ObservableConcurrentDictionary<Guid, PercentProgress<Radial>>();
            _calculator = new ActionBlock<PercentProgress<Radial>>(job =>
            {
                if (!job.ProgressTarget.IsDeleted) Calculate(job);
                WorkQueue.Remove(job.ProgressTarget.Guid);
            }, new ExecutionDataflowBlockOptions { BoundedCapacity = -1, MaxDegreeOfParallelism = System.Environment.ProcessorCount });
            _queue = new BufferBlock<PercentProgress<Radial>>(new DataflowBlockOptions { BoundedCapacity = -1 });
            _queue.LinkTo(_calculator);
        }

        public TransmissionLossCalculatorService(IMasterDatabaseService databaseService, IPluginManagerService pluginService, EnvironmentalCacheService cacheService) : this()
        {
            _databaseService = databaseService;
            _cacheService = cacheService;
        }
        [Import] IMasterDatabaseService _databaseService;
        [Import] EnvironmentalCacheService _cacheService;
        [Initialize(float.NaN)] public float RangeCellSize { get; set; }
        [Initialize(float.NaN)] public float DepthCellSize { get; set; }
        public ObservableConcurrentDictionary<Guid, PercentProgress<Radial>> WorkQueue { get; private set; }
        readonly ActionBlock<PercentProgress<Radial>> _calculator;
        readonly BufferBlock<PercentProgress<Radial>> _queue;

        public void OnImportsSatisfied()
        {
            if (float.IsNaN(RangeCellSize) || float.IsNaN(DepthCellSize) || _databaseService.MasterDatabaseDirectory == null) return;
            Start();
        }

        static readonly object LockObject = new object();
        bool _isStarted;
        LocationContext _dbContext;
        public void Start()
        {
            if (float.IsNaN(RangeCellSize) || float.IsNaN(DepthCellSize)) return;

            lock (LockObject)
            {
                if (_isStarted) return;
                _isStarted = true;
            }
            _dbContext = _databaseService.Context;
            IQueryable<Radial> radials;
            lock (_dbContext)
                radials = (from radial in _dbContext.Radials
                                .Include(r => r.TransmissionLoss)
                                .Include(r => r.TransmissionLoss.Mode)
                                .Include(r => r.TransmissionLoss.Mode.Source)
                                .Include(r => r.TransmissionLoss.Mode.Source.Platform)
                                .Include(r => r.TransmissionLoss.AnalysisPoint)
                                .Include(r => r.TransmissionLoss.AnalysisPoint.Scenario)
                                .Include(r => r.TransmissionLoss.AnalysisPoint.Scenario.Location)
                            select radial);
            foreach (var radial in radials)
            {
                if (radial.BasePath == null)
                {
                    _dbContext.Radials.Remove(radial);
                    continue;
                }
                if (!File.Exists(radial.BasePath + ".shd")) Add(radial);
            }
            _databaseService.Context.Radials.Local.CollectionChanged += (sender, args) =>
            {
                switch (args.Action)
                {
                    case NotifyCollectionChangedAction.Add:
                        foreach (var radial in from Radial radial in args.NewItems
                                               where !radial.IsCalculated && !File.Exists(radial.BasePath + ".shd")
                                               select radial) Add(radial);
                        break;
                }
            };
        }

        public void Add(Radial radial)
        {
            var geoRect = (GeoRect)radial.TransmissionLoss.AnalysisPoint.Scenario.Location.GeoRect;
            if (!geoRect.Contains(radial.Segment[0]) || !geoRect.Contains(radial.Segment[1]))
            {
                radial.Errors.Add("This radial extends beyond the location boundaries");
                return;
            }
            //Debug.WriteLine("{0}: Queueing calculation of transmission loss for radial bearing {1} degrees, of mode {2} in analysis point {3}", DateTime.Now, radial.Bearing, radial.TransmissionLoss.Mode.ModeName, (Geo)radial.TransmissionLoss.AnalysisPoint.Geo); 
            PercentProgress<Radial> radialProgress;
            if (WorkQueue.TryGetValue(radial.Guid, out radialProgress)) return;
            radialProgress = new PercentProgress<Radial>(radial);
            WorkQueue.Add(radial.Guid, radialProgress);
            _queue.Post(radialProgress);
        }

        public void TestAdd(Radial radial)
        {
            Calculate(new PercentProgress<Radial>(radial));
        }

        void Calculate(PercentProgress<Radial> item)
        {
            var radial = item.ProgressTarget;
            try
            {
                if (radial.IsDeleted) return;
                //Debug.WriteLine("{0}: Starting calculation of transmission loss for radial bearing {1} degrees, of mode {2} in analysis point {3}",
                //                DateTime.Now,
                //                radial.Bearing,
                //                radial.TransmissionLoss.Mode.ModeName,
                //                (Geo)radial.TransmissionLoss.AnalysisPoint.Geo);
                Scenario scenario;
                lock (_dbContext) scenario = (from s in _dbContext.Scenarios
                                                  .Include(s => s.Wind)
                                                  .Include(s => s.SoundSpeed)
                                                  .Include(s => s.Bathymetry)
                                                  .Include(s => s.Sediment)
                                                  where s.Guid == radial.TransmissionLoss.AnalysisPoint.Scenario.Guid
                                              select s).Single();
                var mode = radial.TransmissionLoss.Mode;
                var platform = mode.Source.Platform;
                var timePeriod = platform.Scenario.TimePeriod;
                if (radial.IsDeleted) return;
                var wind = (Wind)_cacheService[scenario.Wind].Result;
                if (radial.IsDeleted) return;
                var soundSpeed = (SoundSpeed)_cacheService[scenario.SoundSpeed].Result;
                if (radial.IsDeleted) return;
                var bathymetry = (Bathymetry)_cacheService[scenario.Bathymetry].Result;
                if (radial.IsDeleted) return;
                var sediment = (Sediment)_cacheService[scenario.Sediment].Result;
                if (radial.IsDeleted) return;
                var deepestPoint = bathymetry.DeepestPoint;
                var deepestProfile = soundSpeed[timePeriod].GetDeepestSSP(deepestPoint).Extend(deepestPoint.Data);

                var windData = wind[timePeriod].EnvironmentData;
                var windSample = windData.IsFast2DLookupAvailable
                                     ? windData.GetNearestPointAsync(radial.Segment.Center).Result
                                     : windData.GetNearestPoint(radial.Segment.Center);

                var soundSpeedData = soundSpeed[timePeriod].EnvironmentData;
                var soundSpeedProfile = soundSpeedData.IsFast2DLookupAvailable
                                            ? soundSpeedData.GetNearestPointAsync(radial.Segment.Center).Result.Extend(deepestProfile)
                                            : soundSpeedData.GetNearestPoint(radial.Segment.Center).Extend(deepestProfile);

                var sedimentSample = sediment.Samples.IsFast2DLookupAvailable
                                         ? sediment.Samples.GetNearestPointAsync(radial.Segment.Center).Result
                                         : sediment.Samples.GetNearestPoint(radial.Segment.Center);
                
                var bottomProfile = new BottomProfile(100, radial.Segment, bathymetry);
                var sourceDepth = platform.Depth;
                if (mode.Depth.HasValue) sourceDepth += mode.Depth.Value;

                var directoryPath = Path.GetDirectoryName(radial.BasePath);
                if (directoryPath == null) return;
                if (!Directory.Exists(directoryPath)) Directory.CreateDirectory(directoryPath);
                CreateBellhopEnvironmentFiles(radial.BasePath,
                                              soundSpeedProfile,
                                              sedimentSample,
                                              bottomProfile,
                                              windSample.Data,
                                              (float)Math.Sqrt(mode.HighFrequency * mode.LowFrequency),
                                              sourceDepth,
                                              (float)radial.Length,
                                              mode.VerticalBeamWidth,
                                              mode.DepressionElevationAngle,
                                              (float)(bottomProfile.MaxDepth * 1.01),
                                              RangeCellSize,
                                              DepthCellSize,
                                              true,
                                              false,
                                              1500);
                var bellhopProcess = new TransmissionLossProcess
                {
                    StartInfo = new ProcessStartInfo(Path.Combine(AssemblyLocation, "bellhop.exe"), radial.Filename)
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
                if (radial.IsDeleted) return;
                bellhopProcess.Start();
                radial.CalculationStarted = DateTime.Now;
                bellhopProcess.PriorityClass = ProcessPriorityClass.BelowNormal;
                bellhopProcess.BeginOutputReadLine();
                while (!bellhopProcess.HasExited)
                {
                    if (radial.IsDeleted)
                    {
                        bellhopProcess.Kill();
                        return;
                    }
                    Thread.Sleep(100);
                }
                radial.CalculationCompleted = DateTime.Now;
                if (radial.IsDeleted) return;
                try
                {
                    radial.ExtractAxisData();
                }
                catch (Exception e)
                {
                    if (radial.IsDeleted) return;
                    Debug.WriteLine(string.Format("{0}: Caught (and discarded) exception in Transmission Loss Calculator: {1}", DateTime.Now, e.Message));
                }
                //Debug.WriteLine("{0}: Finished calculation of transmission loss for radial bearing {1} degrees, of mode {2} in analysis point {3}",
                //                DateTime.Now,
                //                radial.Bearing,
                //                radial.TransmissionLoss.Mode.ModeName,
                //                (Geo)radial.TransmissionLoss.AnalysisPoint.Geo);
            }
            catch (ArgumentOutOfRangeException e)
            {
                Debug.WriteLine("{0}: FAIL: Calculation of transmission loss for radial bearing {1} degrees, of mode {2} in analysis point {3}.  Exception: {4}",
                                DateTime.Now,
                                radial.Bearing,
                                radial.TransmissionLoss.Mode.ModeName,
                                (Geo)radial.TransmissionLoss.AnalysisPoint.Geo, e.Message);
            }
        }
        static readonly string AssemblyLocation = Path.GetDirectoryName(Assembly.GetEntryAssembly().Location);


        public static void CreateBellhopEnvironmentFiles(string baseFilename, SoundSpeedProfile ssp, SedimentType sediment, BottomProfile bottomProfile, float windSpeed, float frequency, float sourceDepth, float radius, float verticalBeamWidth, float depressionElevationAngle, float maxCalculationDepthMeters, float rangeCellSize, float depthCellSize, bool useSurfaceReflection, bool generateArrivalsFile, int beamCount)
        {
            var depthCellCount = (int)Math.Ceiling(bottomProfile.MaxDepth / depthCellSize);
            var rangeCellCount = (int)Math.Ceiling(radius / rangeCellSize);
            using (var envFile = new StreamWriter(baseFilename + ".env", false))
            {
                envFile.WriteLine("'Bellhop'");
                envFile.WriteLine("{0}", frequency);
                envFile.WriteLine("1"); // was NMEDIA in gui_genbellhopenv.m
                envFile.WriteLine(useSurfaceReflection ? "'CFLT'" : "'CVLT'");

                //if (depthCellCount < 5) throw new BathymetryTooShallowException("Error: Maximum depth of transect (" + maxCalculationDepthMeters + " meters) less than minimum required for transmission loss calculations.\nPlease choose a different location for this transect.");

                envFile.WriteLine("0, 0.0, {0}", ssp.Data[ssp.Data.Count - 1].Depth);
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
                envFile.WriteLine("0.0 {0} {1}", maxCalculationDepthMeters, (radius / 1000.0) * 1.01); // step zbox(meters) rbox(km)
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
