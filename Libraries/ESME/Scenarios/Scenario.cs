using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.DataAnnotations;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows.Data;
using System.Windows.Threading;
using ESME.Database;
using ESME.Environment;
using ESME.Locations;
using ESME.TransmissionLoss;
using HRC.Aspects;
using HRC.Navigation;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Scenarios
{
    [NotifyPropertyChanged]
    public class Scenario : IHaveGuid, IMouseOverAware
    {
        public Scenario()
        {
            StorageDirectory = Path.Combine("scenarios", MasterDatabaseService.RandomFilenameWithoutExension);
            Duration = new TimeSpan(0, 1, 0, 0);
        }

        public Scenario(Scenario scenario) : this()
        {
            Copy(scenario);
        }

        void Copy(Scenario scenario)
        {
            Name = scenario.Name;
            Comments = scenario.Comments;
            ShowAllAnalysisPoints = scenario.ShowAllAnalysisPoints;
            ShowAllPerimeters = scenario.ShowAllPerimeters;
            ShowAllSpecies = scenario.ShowAllSpecies;
            StartTime = new TimeSpan(scenario.StartTime.Ticks);
            Duration = new TimeSpan(scenario.Duration.Ticks);
            TimePeriod = (TimePeriod)scenario.TimePeriod;
            Location = scenario.Location;
            Wind = scenario.Wind;
            SoundSpeed = scenario.SoundSpeed;
            Sediment = scenario.Sediment;
            Bathymetry = scenario.Bathymetry;
            // Here we map the old perimeter to the new perimeter so that the copied platform gets the proper perimeter
            var perimeterMap = new Dictionary<Guid, Guid>();
            foreach (var perimeter in scenario.Perimeters)
            {
                var newPerimeter = new Perimeter(perimeter) { Scenario = this };
                perimeterMap.Add(perimeter.Guid, newPerimeter.Guid);
                Perimeters.Add(newPerimeter);
            }
            var modeMap = new Dictionary<Guid, Guid>();
            var allModes = new List<Mode>();
            foreach (var platform in scenario.Platforms)
            {
                var newPlatform = new Platform(platform) { Scenario = this };
                // Make sure the new perimeter gets the proper copied perimeter from the original scenario
                if (platform.Perimeter != null) newPlatform.Perimeter = Perimeters.Find(p => p.Guid == perimeterMap[platform.Perimeter.Guid]);
                Platforms.Add(newPlatform);
                foreach (var source in platform.Sources)
                {
                    var newSource = new Source(source) { Platform = newPlatform };
                    newPlatform.Sources.Add(newSource);
                    foreach (var mode in source.Modes)
                    {
                        var newMode = new Mode(mode) { Source = newSource };
                        modeMap.Add(mode.Guid, newMode.Guid);
                        newSource.Modes.Add(newMode);
                        allModes.Add(newMode);
                    }
                }
            }
            foreach (var analysisPoint in scenario.AnalysisPoints)
            {
                var newAnalysisPoint = new AnalysisPoint(analysisPoint) { Scenario = this };
                AnalysisPoints.Add(newAnalysisPoint);
                foreach (var transmissionLoss in analysisPoint.TransmissionLosses)
                {
                    var newTransmissionLoss = new TransmissionLoss { AnalysisPoint = newAnalysisPoint, LayerSettings = new LayerSettings(transmissionLoss.LayerSettings) };
                    foreach (var mode in transmissionLoss.Modes) 
                        newTransmissionLoss.Modes.Add(allModes.Find(m => m.Guid == modeMap[mode.Guid]));
                    newAnalysisPoint.TransmissionLosses.Add(newTransmissionLoss);
                    foreach (var radial in transmissionLoss.Radials)
                    {
                        var newRadial = new Radial(radial) { TransmissionLoss = newTransmissionLoss };
                        newTransmissionLoss.Radials.Add(newRadial);
                        newRadial.CopyFiles(radial);
                    }
                }
            }
            foreach (var species in scenario.ScenarioSpecies)
            {
                var newSpecies = new ScenarioSpecies(species) { Scenario = this };
                ScenarioSpecies.Add(newSpecies);
                newSpecies.CopyFiles(species);
            }
        }

        [Key, Initialize]
        public Guid Guid { get; set; }

        public string Name { get; set; }
        public string Comments { get; set; }
        public string StorageDirectory { get; set; }
        public bool ShowAllAnalysisPoints
        {
            get { return _showAllAnalysisPoints; }
            set
            {
                _showAllAnalysisPoints = value;
                foreach (var analysisPoint in AnalysisPoints) analysisPoint.LayerSettings.IsChecked = _showAllAnalysisPoints;
            }
        }
        bool _showAllAnalysisPoints;

        public bool ShowAllPerimeters
        {
            get { return _showAllPerimeters; }
            set
            {
                _showAllPerimeters = value;
                foreach (var perimeter in Perimeters) perimeter.LayerSettings.IsChecked = _showAllPerimeters;
            }
        }
        bool _showAllPerimeters;

        public bool ShowAllSpecies
        {
            get { return _showAllSpecies; }
            set
            {
                _showAllSpecies = value;
                foreach (var species in ScenarioSpecies) species.LayerSettings.IsChecked = _showAllSpecies;
            }
        }
        bool _showAllSpecies;

        [Initialize]
        public DbTimeSpan StartTime { get; set; }

        DbTimeSpan _duration;

        [Initialize] public DbTimeSpan Duration
        {
            get { return _duration; }
            set
            {
                _duration = value;
                foreach (var platform in Platforms) platform.Refresh();
            }
        }

        [Initialize]
        public DbTimePeriod TimePeriod { get; set; }

        public virtual Location Location { get; set; }

        public virtual EnvironmentalDataSet Wind { get; set; }
        public virtual EnvironmentalDataSet SoundSpeed { get; set; }
        public virtual EnvironmentalDataSet Sediment { get; set; }
        public virtual EnvironmentalDataSet Bathymetry { get; set; }

        [Initialize]
        public virtual ObservableList<Platform> Platforms { get; set; }
        [Initialize]
        public virtual ObservableList<ScenarioSpecies> ScenarioSpecies { get; set; }
        [Initialize]
        public virtual ObservableList<AnalysisPoint> AnalysisPoints { get; set; }
        [Initialize]
        public virtual ObservableList<Perimeter> Perimeters { get; set; }
        [Initialize]
        public virtual ObservableList<LogEntry> Logs { get; set; }

        public void Add(Platform platform) { Platforms.Add(platform); }

        [NotMapped] public Wind WindData { get { return ((Wind)Cache[Wind].Result); } }
        [NotMapped] public SoundSpeed SoundSpeedData { get { return ((SoundSpeed)Cache[SoundSpeed].Result); } }
        [NotMapped] public Bathymetry BathymetryData { get { return ((Bathymetry)Cache[Bathymetry].Result); } }
        [NotMapped] public Sediment SedimentData { get { return ((Sediment)Cache[Sediment].Result); } }
        [NotMapped]
        public string StorageDirectoryPath
        {
            get
            {
                if (_storageDirectoryPath != null) return _storageDirectoryPath;
                if (Database == null || Database.MasterDatabaseDirectory == null) throw new ApplicationException("Database or Database.MasterDatabaseDirectory is null");
                _storageDirectoryPath = Path.Combine(Database.MasterDatabaseDirectory, StorageDirectory);
                if (!Directory.Exists(_storageDirectoryPath)) Directory.CreateDirectory(_storageDirectoryPath);
                return _storageDirectoryPath;
            }
        }
        string _storageDirectoryPath;

        [NotMapped]
        public static IMasterDatabaseService Database { get; set; }
        [NotMapped]
        public static IEnvironmentalCacheService Cache { get; set; }
        [NotMapped]
        public bool IsNew { get; set; }
        [NotMapped]
        public object LayerControl
        {
            get { return _layerControl; }
            set
            {
                _layerControl = value;
                MediatorMessage.Send(MediatorMessage.ScenarioBoundToLayer, this);
            }
        }
        object _layerControl;


        public void CreateMapLayers()
        {
            if (Platforms != null) foreach (var platform in Platforms) platform.CreateMapLayers();
            foreach (var analysisPoint in AnalysisPoints.ToList()) analysisPoint.CreateMapLayers();
            foreach (var species in ScenarioSpecies.ToList()) species.CreateMapLayers();
            foreach (var perimeter in Perimeters.ToList()) perimeter.CreateMapLayers();
        }

        public void RemoveMapLayers()
        {
            if (Wind != null) Wind.RemoveMapLayers();
            if (SoundSpeed != null) SoundSpeed.RemoveMapLayers();
            if (Bathymetry != null) Bathymetry.RemoveMapLayers();
            if (Sediment != null) Sediment.RemoveMapLayers();
            if (Platforms != null) foreach (var platform in Platforms) platform.RemoveMapLayers();
            foreach (var analysisPoint in AnalysisPoints) analysisPoint.RemoveMapLayers();
            foreach (var species in ScenarioSpecies.ToList()) species.RemoveMapLayers();
            foreach (var perimeter in Perimeters.ToList()) perimeter.RemoveMapLayers();
        }

        public event PropertyChangedEventHandler PropertyChanged;
        protected void OnPropertyChanged(string propertyName)
        {
            var handlers = PropertyChanged;
            if (handlers == null) return;
            foreach (PropertyChangedEventHandler handler in handlers.GetInvocationList())
            {
                if (handler.Target is DispatcherObject)
                {
                    var localHandler = handler;
                    ((DispatcherObject)handler.Target).Dispatcher.InvokeIfRequired(() => localHandler(this, new PropertyChangedEventArgs(propertyName)));
                }
                else
                    handler(this, new PropertyChangedEventArgs(propertyName));
            }
        }

        [NotMapped]
        public bool IsMouseOver { get; set; }
        [NotMapped]
        public GeoRect GeoRect { get { return Location.GeoRect; } }
        #region AddPlatformCommand
        public SimpleCommand<object, EventToCommandArgs> AddPlatformCommand { get { return _addPlatform ?? (_addPlatform = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.AddPlatform, this))); } }
        SimpleCommand<object, EventToCommandArgs> _addPlatform;
        #endregion

        #region ViewScenarioPropertiesCommand
        public SimpleCommand<object, EventToCommandArgs> ViewScenarioPropertiesCommand { get { return _viewScenarioProperties ?? (_viewScenarioProperties = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.ViewScenarioProperties, this))); } }
        SimpleCommand<object, EventToCommandArgs> _viewScenarioProperties;
        #endregion
        public void Delete()
        {
            RemoveMapLayers();
            Location.Scenarios.Remove(this);
            foreach (var platform in Platforms.ToList()) platform.Delete();
            foreach (var analysisPoint in AnalysisPoints.ToList()) analysisPoint.Delete();
            foreach (var species in ScenarioSpecies.ToList()) species.Delete();
            foreach (var perimeter in Perimeters.ToList()) perimeter.Delete();
            Database.Context.Scenarios.Remove(this);
            Directory.Delete(StorageDirectoryPath, true);
        }
        #region LoadScenarioCommand
        public SimpleCommand<object, EventToCommandArgs> LoadScenarioCommand { get { return _loadScenario ?? (_loadScenario = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.LoadScenario, this))); } }
        SimpleCommand<object, EventToCommandArgs> _loadScenario;
        #endregion

        #region DeleteScenarioCommand
        public SimpleCommand<object, EventToCommandArgs> DeleteScenarioCommand { get { return _deleteScenario ?? (_deleteScenario = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.DeleteScenario, this))); } }
        SimpleCommand<object, EventToCommandArgs> _deleteScenario;
        #endregion
        #region ScenarioPropertiesCommand
        public SimpleCommand<object, EventToCommandArgs> ScenarioPropertiesCommand { get { return _scenarioProperties ?? (_scenarioProperties = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.ScenarioProperties, this))); } }
        SimpleCommand<object, EventToCommandArgs> _scenarioProperties;
        #endregion
        #region SaveACopyCommand
        public SimpleCommand<object, EventToCommandArgs> SaveACopyCommand { get { return _saveACopy ?? (_saveACopy = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.SaveScenarioCopy, this))); } }
        SimpleCommand<object, EventToCommandArgs> _saveACopy;
        #endregion
    }

    public static class ScenarioExensions
    {
        public static TransmissionLossCalculatorService TransmissionLossCalculator;
        public static Dispatcher Dispatcher;

        /// <summary>
        /// Adds a new analysis point to the scenario, creating TLs for all acoustically distinct modes
        /// </summary>
        /// <param name="scenario"> </param>
        /// <param name="analysisPoint"></param>
        public static void Add(this Scenario scenario, AnalysisPoint analysisPoint)
        {
            var bathymetry = scenario.BathymetryData;
            var depthAtAnalysisPoint = bathymetry.Samples.IsFast2DLookupAvailable
                                           ? -bathymetry.Samples.GetNearestPointAsync(analysisPoint.Geo).Result.Data
                                           : -bathymetry.Samples.GetNearestPoint(analysisPoint.Geo).Data;
            analysisPoint.Scenario = scenario;
            scenario.AnalysisPoints.Add(analysisPoint);
            var groupedModes = scenario.GroupEquivalentModes();
            if (groupedModes == null || groupedModes.Groups == null) throw new ApplicationException("groupedModes or groupedModes.Groups is null");
            foreach (CollectionViewGroup collectionViewGroup in groupedModes.Groups)
            {
                var firstMode = (Mode)(collectionViewGroup.Items[0]);
                var sourceDepth = firstMode.Source.Platform.Depth;
                if (firstMode.Depth.HasValue) sourceDepth += firstMode.Depth.Value;
                // If the current mode's depth is below the bottom at the given point, skip it
                if (sourceDepth >= depthAtAnalysisPoint) continue;
                analysisPoint.Add((from item in collectionViewGroup.Items select (Mode)item).ToList());
            }
            //Log(analysisPoint, "Added new analysis point at {0} to scenario {1} in location {2}", (Geo)analysisPoint.Geo, analysisPoint.Scenario.Name, analysisPoint.Scenario.Location.Name);
        }

        /// <summary>
        /// Adds a new transmission loss with the specified list of acoustically-identical modes 
        /// to the current analysis point, and queues the resulting radials for calculation
        /// </summary>
        /// <param name="analysisPoint"></param>
        /// <param name="modes"> </param>
        public static void Add(this AnalysisPoint analysisPoint, IList<Mode> modes)
        {
            var modeMaxRadius = modes.Max(m => m.MaxPropagationRadius);
            var matchingTL = (from tl in analysisPoint.TransmissionLosses
                              where tl.Modes.Count > 0 && modes[0].IsAcousticallyEquivalentTo(tl.Modes[0])
                              select tl).FirstOrDefault();
            if (matchingTL != null)
            {
                matchingTL.Modes.AddRange(modes);
                if (modeMaxRadius <= matchingTL.Radials[0].Length)
                {
                    Debug.WriteLine(string.Format("Added mode(s) matching an existing TL to analysis point at ({0:0.###}, {1:0.###}). No recalculation required.", matchingTL.AnalysisPoint.Geo.Latitude, matchingTL.AnalysisPoint.Geo.Longitude));
                    return;
                }
                var mode = matchingTL.Modes[0];
                var depth = mode.Source.Platform.Depth;
                if (mode.Depth.HasValue) depth += mode.Depth.Value;
                Debug.WriteLine(string.Format("Recalculating TL: {0}Hz, {1}Hz, {2}m, {3}deg, {4}deg in analysis point at ({5:0.###}, {6:0.###}) after adding mode(s) with longer radius. Old radius: {7}m, new radius {8}m", mode.HighFrequency, mode.LowFrequency, depth, mode.VerticalBeamWidth, mode.DepressionElevationAngle, matchingTL.AnalysisPoint.Geo.Latitude, matchingTL.AnalysisPoint.Geo.Longitude, matchingTL.Radials[0].Length, modeMaxRadius));
                matchingTL.Recalculate();
            }
            else
            {
                var transmisionLoss = new TransmissionLoss { AnalysisPoint = analysisPoint };
                analysisPoint.TransmissionLosses.Add(transmisionLoss);
                transmisionLoss.Modes.AddRange(modes);
                var radialCount = modeMaxRadius <= 10000 ? 8 : 16;
                var mode = transmisionLoss.Modes[0];
                var depth = mode.Source.Platform.Depth;
                if (mode.Depth.HasValue) depth += mode.Depth.Value;
                Debug.WriteLine(string.Format("Adding TL: {0}Hz, {1}Hz, {2}m, {3}deg, {4}deg to analysis point at ({5:0.###}, {6:0.###})", mode.HighFrequency, mode.LowFrequency, depth, mode.VerticalBeamWidth, mode.DepressionElevationAngle, transmisionLoss.AnalysisPoint.Geo.Latitude, transmisionLoss.AnalysisPoint.Geo.Longitude));
                for (var radialIndex = 0; radialIndex < radialCount; radialIndex++)
                {
                    var radial = new Radial
                    {
                        TransmissionLoss = transmisionLoss,
                        CalculationCompleted = DateTime.MaxValue,
                        CalculationStarted = DateTime.MaxValue,
                        Bearing = (360.0 / radialCount) * radialIndex,
                        Length = modeMaxRadius,
                        IsCalculated = false,
                    };
                    transmisionLoss.Radials.Add(radial);
                    TransmissionLossCalculator.Add(radial);
                }
                Dispatcher.InvokeIfRequired(transmisionLoss.CreateMapLayers);
            }
        }
        /// <summary>
        /// Adds a new mode to the scenario.  This also adds the mode to all analysis points where the mode is not beneath the bottom
        /// </summary>
        /// <param name="scenario"></param>
        /// <param name="mode"></param>
        public static void Add(this Scenario scenario, Mode mode)
        {
            var apCount = 0;
            foreach (var ap in scenario.AnalysisPoints)
            {
                var foundMatch = false;
                foreach (var tl in ap.TransmissionLosses.Where(tl => mode.IsAcousticallyEquivalentTo(tl.Modes[0]))) 
                {
                    if (mode.MaxPropagationRadius > tl.Modes.Max(m => m.MaxPropagationRadius))
                        tl.Recalculate();
                    tl.Modes.Add(mode);
                    foundMatch = true;
                }
                if (foundMatch) continue;
                Debug.WriteLine(string.Format("Adding mode {0}:{1}:{2} to analysis point at ({3:0.###}, {4:0.###})", mode.Source.Platform.PlatformName, mode.Source.SourceName, mode.ModeName, ap.Geo.Latitude, ap.Geo.Longitude));
                ap.Add(new List<Mode> { mode });
                apCount++;
            }
            Debug.WriteLine(string.Format("Mode {0}:{1}:{2} was added to {3} analysis points", mode.Source.Platform.PlatformName, mode.Source.SourceName, mode.ModeName, apCount));
        }


        /// <summary>
        /// Notifies the scenario that the radius on an existing mode has changed.
        /// If it is now the mode with the longest propagation radius, all TLs containing this mode will be recalculated.
        /// </summary>
        /// <param name="scenario"> </param>
        /// <param name="mode"></param>
        public static void NotifyRadiusChanged(this Scenario scenario, Mode mode)
        {
            foreach (var transmissionLoss in from tl in mode.TransmissionLosses
                                             let maxRadius = tl.Modes.Max(m => m.MaxPropagationRadius)
                                             where tl.Radials != null && tl.Radials.Count > 0 && tl.Radials[0].Length < maxRadius
                                             select tl)
            {
                var depth = mode.Source.Platform.Depth;
                if (mode.Depth.HasValue) depth += mode.Depth.Value;
                Debug.WriteLine(string.Format("Recalculating TL: {0}Hz, {1}Hz, {2}m, {3}deg, {4}deg in analysis point at ({5:0.###}, {6:0.###}) due to radius change. Old radius: {7}m, new radius {8}m", mode.HighFrequency, mode.LowFrequency, depth, mode.VerticalBeamWidth, mode.DepressionElevationAngle, transmissionLoss.AnalysisPoint.Geo.Latitude, transmissionLoss.AnalysisPoint.Geo.Longitude, transmissionLoss.Radials[0].Length, transmissionLoss.Modes.Max(m => m.MaxPropagationRadius)));
                transmissionLoss.Recalculate();
            }
        }

        public static void NotifyAcousticsChanged(this Scenario scenario, Mode mode)
        {
            foreach (var transmissionLoss in mode.TransmissionLosses.ToList())
            {
                var analysisPoint = transmissionLoss.AnalysisPoint;
                transmissionLoss.Modes.Remove(mode);
                if (transmissionLoss.Modes.Count == 0) transmissionLoss.Delete();
                analysisPoint.Add(new List<Mode> { mode });
            }
        }

        private static ListCollectionView GroupEquivalentModes(this Scenario scenario)
        {
            var allModes = (from platform in scenario.Platforms
                            from source in platform.Sources
                            from mode in source.Modes
                            select mode).ToList();
            var cv = new ListCollectionView(allModes);
            if (cv == null || cv.GroupDescriptions == null) throw new ApplicationException("ListCollectionView is null or GroupDescriptions is null");
            cv.GroupDescriptions.Add(new PropertyGroupDescription(null, new ModeGroupingConverter()));
            return cv;
        }

        public static IEnumerable<Mode> AcousticallyDistinct(this IEnumerable<Mode> source)
        {
            var distinctModes = new ConcurrentBag<Mode>();
            foreach (var mode in source.Where(mode => !distinctModes.Any(mode.IsAcousticallyEquivalentTo)))
            {
                distinctModes.Add(mode);
                yield return mode;
            }
        }

        public static IEnumerable<Mode> AcousticallyDistinctModes(this Scenario scenario)
        {
            return (from p in scenario.Platforms 
                    from s in p.Sources 
                    from m in s.Modes 
                    select m).AcousticallyDistinct();
        }

        public static IEnumerable<Mode> GetDistinctAnalysisPointModes(this Scenario scenario)
        {
            return (from analysisPoint in scenario.AnalysisPoints
                    from transmissionLoss in analysisPoint.TransmissionLosses
                    from mode in transmissionLoss.Modes
                    select mode).AcousticallyDistinct();
        }

        public static string MissingSpeciesText(this Scenario scenario)
        {
            if (!scenario.ScenarioSpecies.Any()) return "There are no species specified in this scenario.";
            var sb = new StringBuilder();
            foreach (var species in scenario.ScenarioSpecies.Where(species => !species.Animat.Locations.Any())) sb.AppendLine(string.Format("There are no animats seeded for species {0}", species.LatinName));
            return sb.ToString();
        }

        public static TransmissionLoss ClosestTransmissionLoss(this Scenario scenario, Geo geo, Mode mode)
        {
            var closest = (from ap in scenario.AnalysisPoints
                           from tl in ap.TransmissionLosses
                           where tl.Modes[0].IsAcousticallyEquivalentTo(mode)
                           let d = geo.DistanceKilometers(ap.Geo)
                           orderby d
                           select new { d, tl }).FirstOrDefault();
            return closest != null ? closest.tl : null;
        }

        public static string Validate(this Scenario scenario)
        {
            if (scenario == null) return "Scenario is null";
            if (scenario.Platforms == null || scenario.Platforms.Count == 0) return "No platforms have been defined";

            var distinctScenarioModes = AcousticallyDistinctModes(scenario).ToList();
            if (distinctScenarioModes.Count == 0) return "No modes have been defined";

            if (scenario.AnalysisPoints.Count == 0) return "No analysis points have been defined";

            var missingScenarioModes = distinctScenarioModes.Except(GetDistinctAnalysisPointModes(scenario).ToList()).ToList();
            if (missingScenarioModes.Count != 0) return "The following modes do not appear in any currently defined analysis points: " + string.Join(", ", missingScenarioModes.Select(m => m.ModeName));

            var radialsNotCalculated = (from analysisPoint in scenario.AnalysisPoints
                                        from transmissionLoss in analysisPoint.TransmissionLosses
                                        from radial in transmissionLoss.Radials
                                        where !File.Exists(radial.BasePath + ".shd")
                                        select radial).Count();
            if (radialsNotCalculated != 0) return string.Format("There are still {0} radials awaiting calculation in this scenario.", radialsNotCalculated);

            var missingSpecies = MissingSpeciesText(scenario);
            if (missingSpecies.Length != 0) return missingSpecies;

            //todo warn if there are ghost platforms
            return null;
        }
    }
}
