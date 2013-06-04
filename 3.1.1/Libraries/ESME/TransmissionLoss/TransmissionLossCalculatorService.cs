﻿using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Data.Entity;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Threading.Tasks.Dataflow;
using System.Windows.Threading;
using ESME.Environment;
using ESME.Locations;
using ESME.Plugins;
using ESME.Scenarios;
using ESME.TransmissionLoss.Bellhop;
using HRC.Navigation;
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
            WorkQueue = new ObservableConcurrentDictionary<Guid, Radial>();
            _calculator = new ActionBlock<Radial>(radial =>
            {
                if (!radial.IsDeleted)
                {
#if false
                    var longestRadiusMode = (from mode in radial.TransmissionLoss.Modes
                                             orderby mode.MaxPropagationRadius descending
                                             select mode).FirstOrDefault();
                    if (longestRadiusMode != null)
                    {
                        var modeName = string.Format("{0}|{1}|{2}", longestRadiusMode.Source.Platform.PlatformName, longestRadiusMode.Source.SourceName, longestRadiusMode.ModeName);
                        var geo = (Geo)radial.TransmissionLoss.AnalysisPoint.Geo;
                        Debug.WriteLine(string.Format("About to calculate radial {0}[{1:0.###},{2:0.###}]/{3:0.#} deg", modeName, geo.Latitude, geo.Longitude, radial.Bearing));
                    }
                    else 
                        Debug.WriteLine("TransmissionLossCalculatorService: longestRadiusMode is null!");
#endif
                    Calculate(radial);
                }
                WorkQueue.Remove(radial.Guid);
                _shadeFileProcessorQueue.Post(radial);
            }, new ExecutionDataflowBlockOptions { BoundedCapacity = -1, MaxDegreeOfParallelism = System.Environment.ProcessorCount });
            _calculatorQueue = new BufferBlock<Radial>(new DataflowBlockOptions { BoundedCapacity = -1 });
            _calculatorQueue.LinkTo(_calculator);
            _shadeFileProcessor = new ActionBlock<Radial>(r => { if (!r.IsDeleted && r.ExtractAxisData()) r.ReleaseAxisData(); },
                                                          new ExecutionDataflowBlockOptions { BoundedCapacity = -1, MaxDegreeOfParallelism = System.Environment.ProcessorCount });
            _shadeFileProcessorQueue = new BufferBlock<Radial>(new DataflowBlockOptions { BoundedCapacity = -1 });
            _shadeFileProcessorQueue.LinkTo(_shadeFileProcessor);
        }

        public TransmissionLossCalculatorService(IMasterDatabaseService databaseService, IPluginManagerService pluginService, EnvironmentalCacheService cacheService) : this()
        {
            _databaseService = databaseService;
            _cacheService = cacheService;
        }

        [Import] IMasterDatabaseService _databaseService;
        [Import] EnvironmentalCacheService _cacheService;
        public Dispatcher Dispatcher { get; set; }
        public ObservableConcurrentDictionary<Guid, Radial> WorkQueue { get; private set; }
        readonly ActionBlock<Radial> _calculator;
        readonly BufferBlock<Radial> _calculatorQueue;
        readonly ActionBlock<Radial> _shadeFileProcessor;
        readonly BufferBlock<Radial> _shadeFileProcessorQueue;

        public void OnImportsSatisfied()
        {
            if (Dispatcher == null || _databaseService.MasterDatabaseDirectory == null) return;
            Start();
        }

        static readonly object LockObject = new object();
        bool _isStarted;
        public void Start()
        {
            if (Dispatcher == null) return;

            lock (LockObject)
            {
                if (_isStarted) return;
                _isStarted = true;
            }
            var radials = (from radial in _databaseService.Context.Radials
                               .Include(r => r.TransmissionLoss)
                               .Include(r => r.TransmissionLoss.Modes)
                               //.Include(r => r.TransmissionLoss.Mode.Source)
                               //.Include(r => r.TransmissionLoss.Mode.Source.Platform)
                               .Include(r => r.TransmissionLoss.AnalysisPoint)
                               .Include(r => r.TransmissionLoss.AnalysisPoint.Scenario)
                               .Include(r => r.TransmissionLoss.AnalysisPoint.Scenario.Location)
                           select radial);
            foreach (var radial in radials)
            {
                if (radial.BasePath == null)
                {
                    _databaseService.Context.Radials.Remove(radial);
                    continue;
                }
                if (radial.TransmissionLoss.AnalysisPoint.Scenario.Wind == null ||
                    radial.TransmissionLoss.AnalysisPoint.Scenario.SoundSpeed == null ||
                    radial.TransmissionLoss.AnalysisPoint.Scenario.Bathymetry == null ||
                    radial.TransmissionLoss.AnalysisPoint.Scenario.Sediment == null)
                {
                    var scenario = (from s in _databaseService.Context.Scenarios
                                        .Include(s => s.Wind)
                                        .Include(s => s.SoundSpeed)
                                        .Include(s => s.Bathymetry)
                                        .Include(s => s.Sediment)
                                    where s.Guid == radial.TransmissionLoss.AnalysisPoint.Scenario.Guid
                                    select s).Single();
                }
                if (!File.Exists(radial.BasePath + ".shd"))
                {
                    _databaseService.Context.Radials.
                    Add(radial);
                }
                else _shadeFileProcessorQueue.Post(radial);
            }
        }

#if false
        public void Add(Radial radial)
        {
            if (radial.HasErrors)
            {
                Debug.WriteLine(string.Format("Radial at bearing {0} has errors. Calculation aborted.", radial.Bearing));
                return;
            }
            Radial outRadial;
            if (WorkQueue.TryGetValue(radial.Guid, out outRadial)) return;
            WorkQueue.Add(radial.Guid, radial);
            _calculatorQueue.Post(radial);
        }
#else
        public void Add(Radial radial)
        {
            var geoRect = (GeoRect)radial.TransmissionLoss.AnalysisPoint.Scenario.Location.GeoRect;
            if (!geoRect.Contains(radial.Segment[0]) || !geoRect.Contains(radial.Segment[1]))
            {
                //radial.Errors.Add("This radial extends beyond the location boundaries");
                return;
            }
            //Debug.WriteLine("{0}: Queueing calculation of transmission loss for radial bearing {1} degrees, of mode {2} in analysis point {3}", DateTime.Now, radial.Bearing, radial.TransmissionLoss.Mode.ModeName, (Geo)radial.TransmissionLoss.AnalysisPoint.Geo); 
            Radial outRadial;
            if (WorkQueue.TryGetValue(radial.Guid, out outRadial)) return;
            WorkQueue.Add(radial.Guid, radial);
            _calculatorQueue.Post(radial);
        }

        public void Remove(Radial radial)
        {
            WorkQueue.Remove(radial.Guid);
        }
#endif

        public void TestAdd(Radial radial)
        {
            Calculate(radial);
        }

        public IPluginManagerService PluginManagerService { get; set; }
        void Calculate(Radial radial)
        {
            try
            {
                var scenario = radial.TransmissionLoss.AnalysisPoint.Scenario;
                var mode = (from m in radial.TransmissionLoss.Modes
                            orderby m.MaxPropagationRadius
                            select m).Last();
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

                var depthAtAnalysisPoint = bathymetry.Samples.IsFast2DLookupAvailable
                                               ? bathymetry.Samples.GetNearestPointAsync(radial.TransmissionLoss.AnalysisPoint.Geo).Result
                                               : bathymetry.Samples.GetNearestPoint(radial.TransmissionLoss.AnalysisPoint.Geo);

                // If there is less than one meter of water at the analysis point, discard this radial
                if (depthAtAnalysisPoint.Data > -1)
                {
                    radial.Delete();
                    return;
                }

                var windData = wind[timePeriod].EnvironmentData;
                var windSample = windData.IsFast2DLookupAvailable
                                     ? windData.GetNearestPointAsync(radial.Segment.Center).Result
                                     : windData.GetNearestPoint(radial.Segment.Center);

                var sedimentSample = sediment.Samples.IsFast2DLookupAvailable
                                         ? sediment.Samples.GetNearestPointAsync(radial.Segment.Center).Result
                                         : sediment.Samples.GetNearestPoint(radial.Segment.Center);
                
                var bottomProfile = new BottomProfile(99, radial.Segment, bathymetry);

                var directoryPath = Path.GetDirectoryName(radial.BasePath);
                if (directoryPath == null) return;
                if (!Directory.Exists(directoryPath)) Directory.CreateDirectory(directoryPath);
                if (PluginManagerService != null && PluginManagerService[PluginType.TransmissionLossCalculator] != null)
                {
                    var profilesAlongRadial = ProfilesAlongRadial(radial.Segment, 0.0, null, null, bottomProfile, soundSpeed[timePeriod].EnvironmentData, deepestProfile).ToList();
                    if (radial.IsDeleted) return;
                    radial.CalculationStarted = DateTime.Now;
                    try
                    {
                        mode.GetTransmissionLossPlugin(PluginManagerService).CalculateTransmissionLoss(platform, mode, radial, bottomProfile, sedimentSample, windSample.Data, profilesAlongRadial);
                    }
                    catch (RadialDeletedByUserException)
                    {
                        radial.CleanupFiles();
                    }
                    radial.CalculationCompleted = DateTime.Now;
                    radial.Length = mode.MaxPropagationRadius;
                    radial.IsCalculated = true;
                }
                else Debug.WriteLine("TransmissionLossCalculatorService: PluginManagerService is not initialized, or there are no transmission loss calculator plugins defined");
            }
            catch (Exception e)
            {
                Debug.WriteLine("{0}: FAIL: Calculation of transmission loss for radial bearing {1} degrees, of mode {2} in analysis point {3}.  Exception: {4}",
                                DateTime.Now,
                                radial == null ? "(null)" : radial.Bearing.ToString(CultureInfo.InvariantCulture),
                                radial == null || radial.TransmissionLoss == null || radial.TransmissionLoss.Modes == null || radial.TransmissionLoss.Modes.Count == 0 ? "(null)" : radial.TransmissionLoss.Modes[0].ToString(),
                                radial == null || radial.TransmissionLoss == null || radial.TransmissionLoss.AnalysisPoint == null || radial.TransmissionLoss.AnalysisPoint.Geo == null ? "(null)" : ((Geo)radial.TransmissionLoss.AnalysisPoint.Geo).ToString(), e.Message);
            }
        }

        /// <summary>
        /// Recursively calculates the nearest sound speed profiles along a given radial using a binary search-like algorithm
        /// 1. If start and end points are provided, use them, otherwise find the nearest SSP to each of those points
        /// 2. If the start point was calculated, add the SSP closest to the calculated start point to the enumerable
        /// 2. If the SSPs closest to the start and end points are within 10m of each other they are considered identical and there are 
        ///    assumed to be no more intervening points
        /// 3. If the SSPs closest to the start and end points are NOT within 10m of each other, calculate the midpoint of the segment 
        ///    and find the nearest SSP to that point.
        /// 4. If the SSP nearest the midpoint is not within 10m of the SSP nearest to the start point, recursively call this function to
        ///    find the new midpoint between the start point and the current midpoint
        /// 5. Return the
        /// </summary>
        /// <param name="segment"></param>
        /// <param name="startDistance"></param>
        /// <param name="startProfile"></param>
        /// <param name="endProfile"></param>
        /// <param name="bottomProfile"></param>
        /// <param name="soundSpeedData"></param>
        /// <param name="deepestProfile"></param>
        /// <returns></returns>
        static IEnumerable<Tuple<double, SoundSpeedProfile>> ProfilesAlongRadial(GeoSegment segment, double startDistance, SoundSpeedProfile startProfile, SoundSpeedProfile endProfile, BottomProfile bottomProfile, EnvironmentData<SoundSpeedProfile> soundSpeedData, SoundSpeedProfile deepestProfile)
        {
            var returnStartProfile = false;
            var returnEndProfile = false;
            if (startProfile == null)
            {
                returnStartProfile = true;
                startProfile = soundSpeedData.IsFast2DLookupAvailable
                                   ? soundSpeedData.GetNearestPointAsync(segment[0]).Result.Extend(deepestProfile)
                                   : soundSpeedData.GetNearestPoint(segment[0]).Extend(deepestProfile);
            }
            if (endProfile == null)
            {
                returnEndProfile = true;
                endProfile = soundSpeedData.IsFast2DLookupAvailable
                                 ? soundSpeedData.GetNearestPointAsync(segment[1]).Result.Extend(deepestProfile)
                                 : soundSpeedData.GetNearestPoint(segment[1]).Extend(deepestProfile);
            }
            if (returnStartProfile) yield return Tuple.Create(NearestBottomProfileDistanceTo(bottomProfile, startDistance), startProfile);
            // If the start and end profiles are the same, we're done
            if (startProfile.DistanceKilometers(endProfile) <= 0.01) yield break;

            // If not, create a middle profile
            var middleProfile = soundSpeedData.IsFast2DLookupAvailable
                                    ? soundSpeedData.GetNearestPointAsync(segment.Center).Result.Extend(deepestProfile)
                                    : soundSpeedData.GetNearestPoint(segment.Center).Extend(deepestProfile);
            // If the center profile is different from BOTH endpoints
            if (startProfile.DistanceKilometers(middleProfile) > 0.01 && middleProfile.DistanceKilometers(endProfile) > 0.01)
            {
                // Recursively create and return any new sound speed profiles between the start and the center
                var firstHalfSegment = new GeoSegment(segment[0], segment.Center);
                foreach (var tuple in ProfilesAlongRadial(firstHalfSegment, startDistance, startProfile, middleProfile, bottomProfile, soundSpeedData, deepestProfile)) yield return tuple;

                var centerDistance = startDistance + Geo.RadiansToKilometers(segment[0].DistanceRadians(segment.Center));
                // return the center profile
                yield return Tuple.Create(NearestBottomProfileDistanceTo(bottomProfile, centerDistance), middleProfile);

                // Recursively create and return any new sound speed profiles between the center and the end
                var secondHalfSegment = new GeoSegment(segment.Center, segment[1]);
                foreach (var tuple in ProfilesAlongRadial(secondHalfSegment, centerDistance, middleProfile, endProfile, bottomProfile, soundSpeedData, deepestProfile)) yield return tuple;
            }
            var endDistance = startDistance + Geo.RadiansToKilometers(segment.LengthRadians);
            // return the end profile
            if (returnEndProfile) yield return Tuple.Create(NearestBottomProfileDistanceTo(bottomProfile, endDistance), endProfile);
        }

        static double NearestBottomProfileDistanceTo(BottomProfile bottomProfile, double desiredDistance)
        {
            var profilePoints = bottomProfile.Profile;
            for (var i = 0; i < profilePoints.Count - 1; i++)
            {
                if (desiredDistance > profilePoints[i + 1].Range) continue;
                var distanceToNearerPoint = desiredDistance - profilePoints[i].Range;
                var distanceToFartherPoint = profilePoints[i + 1].Range - desiredDistance;
                return distanceToNearerPoint <= distanceToFartherPoint ? profilePoints[i].Range : profilePoints[i + 1].Range;
            }
            return profilePoints.Last().Range;
        }
    }
}