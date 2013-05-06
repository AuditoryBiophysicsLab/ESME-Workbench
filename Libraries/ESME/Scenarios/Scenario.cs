using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.DataAnnotations;
using System.Diagnostics;
using System.Globalization;
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
            ((INotifyPropertyChanged)this).PropertyChanged += NotifyPropertyChanged;
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

        void NotifyPropertyChanged(object sender, PropertyChangedEventArgs args)
        {
            switch (args.PropertyName)
            {
                case "IsLoaded":
                    if (IsLoaded) CreateMapLayers();
                    else RemoveMapLayers();
                    break;
            }
        }

        [NotMapped] public bool IsLoaded { get; set; }
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

        public void Log()
        {
            var log = log4net.LogManager.GetLogger(GetType());
            var sb = new StringBuilder();
            sb.AppendLine(string.Format("Scenario :{0}",Name ));
            sb.AppendLine(string.Format("Location :{0}", Location.Name));
            sb.AppendLine(string.Format("Duration :{0}",(TimeSpan)Duration ));
            sb.AppendLine(string.Format("Time Period :{0}", (TimePeriod)TimePeriod));
            sb.AppendLine(string.Format("Start Time :{0}", StartTime.Ticks.ToString(CultureInfo.InvariantCulture)));
            sb.AppendLine(string.Format("Species Count :{0}", ScenarioSpecies.Count));
            sb.AppendLine(string.Format("Platform Count  :{0}",Platforms.Count ));
            sb.AppendLine(string.Format("Source Count :{0}", (from p in Platforms from s in p.Sources select s).Count()));
            sb.AppendLine(string.Format("Mode Count :{0}", (from p in Platforms from s in p.Sources from m in s.Modes select m).Count()));
            sb.AppendLine(string.Format("Perimeter Count :{0}", Perimeters.Count ));
            sb.AppendLine(string.Format("Analysis Point Count :{0}", AnalysisPoints.Count ));
            sb.AppendLine(string.Format("Transmission Loss Count :{0}", (from a in AnalysisPoints from t in a.TransmissionLosses select t).Count() ));
            sb.AppendLine(string.Format("Radial Count :{0}",(from a in AnalysisPoints from t in a.TransmissionLosses from r in t.Radials select r).Count() ));
            sb.AppendLine(string.Format("Storage Directory :{0}",StorageDirectoryPath ));
            log.Info(sb.ToString());
        }

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
        /// Update all the AnalysisPoints contained in a specified Scenario, ensuring that all modes are properly represented across
        /// all AnalysisPoints
        /// </summary>
        /// <param name="scenario"></param>
        public static void UpdateAnalysisPoints(this Scenario scenario)
        {
            // If there are no analysis points, there's nothing to update
            if (scenario.AnalysisPoints == null) return;
            // Group acoustically equivalent modes
            var groupedModes = scenario.GroupEquivalentModes();
            if (groupedModes == null || groupedModes.Groups == null) throw new ApplicationException("groupedModes or groupedModes.Groups is null");
            // Get the grouped modes into a mode convenient form - a List of Lists of equivalent modes
            var unmatchedModeGroups = (from CollectionViewGroup collectionViewGroup in groupedModes.Groups
                                       select (from item in collectionViewGroup.Items
                                               select (Mode)item).ToList()).ToList();
            // Internal variable used to track which groups of modes have been matched to existing TransmissionLosses
            var matchedModeGroups = new List<List<Mode>>();
            // Examine all the AnalysisPoints in the Scenario
            foreach (var analysisPoint in scenario.AnalysisPoints.ToList())
            {
                // Examine each TransmissionLoss in the current AnalysisPoint
                foreach (var transmissionLoss in analysisPoint.TransmissionLosses.ToList())
                {
                    // If the current TransmissionLoss has no modes
                    if (transmissionLoss.Modes == null || transmissionLoss.Modes.Count == 0)
                    {
                        Debug.WriteLine(string.Format("Deleting empty TL at {0}", (Geo)transmissionLoss.AnalysisPoint.Geo));
                        // Delete the current TransmissionLoss
                        transmissionLoss.Delete();
                        // And look at the next one
                        continue;
                    }
                    // Get a representative Mode from the current TransmissionLoss.
                    var originalMode = transmissionLoss.Modes.First();
                    // Find a group of modes that matches the representative Mode
                    var matchingModeGroup = (from modeGroup in unmatchedModeGroups
                                             let curMode = modeGroup.First()
                                             where curMode.IsAcousticallyEquivalentTo(originalMode)
                                             select modeGroup).FirstOrDefault();
                    // If no current mode group matches the representative Mode
                    if (matchingModeGroup == null)
                    {
                        Debug.WriteLine(string.Format("Deleting TL [{0}:{1}] at {2}", transmissionLoss.Modes.First().ModeName, transmissionLoss.Modes.First().MaxPropagationRadius, (Geo)transmissionLoss.AnalysisPoint.Geo));
                        // Delete the current TransmissionLoss
                        transmissionLoss.Delete();
                        // And look at the next one
                        continue;
                    }
                    // Get the list of the original modes in the current TransmissionLoss that don't match the new mode group.
                    // Match criteria (provided by GuidComparer<Mode>) are that the Guids in each mode must be identical
                    var unmatchedOriginalModes = transmissionLoss.Modes.Except(matchingModeGroup, new GuidComparer<Mode>()).ToList();
                    // Remove the unmatched original modes in the current TransmissionLoss
                    unmatchedOriginalModes.ForEach(m =>
                    {
                        Debug.WriteLine(string.Format("Removing mode [{0}:{1}] from [{2}:{3}] at {4}", m.ModeName, m.MaxPropagationRadius, transmissionLoss.Modes.First().ModeName, transmissionLoss.Modes.First().MaxPropagationRadius, (Geo)transmissionLoss.AnalysisPoint.Geo));
                        transmissionLoss.Modes.Remove(m);
                        m.TransmissionLosses.Remove(transmissionLoss);
                    });
                    // Get the list of the modes in the new mode group that don't match modes already in the current TransmissionLoss.
                    // Match criteria (provided by GuidComparer<Mode>) are that the Guids in each mode must be identical
                    var unmatchedNewModes = matchingModeGroup.Except(transmissionLoss.Modes, new GuidComparer<Mode>()).ToList();
                    // Add those new modes to the current TransmissionLoss
                    unmatchedNewModes.ForEach(m =>
                    {
                        Debug.WriteLine(string.Format("Adding mode [{0}:{1}] to [{2}:{3}] at {4}", m.ModeName, m.MaxPropagationRadius, transmissionLoss.Modes.First().ModeName, transmissionLoss.Modes.First().MaxPropagationRadius, (Geo)transmissionLoss.AnalysisPoint.Geo));
                        m.TransmissionLosses.Add(transmissionLoss);
                        transmissionLoss.Modes.Add(m);
                    });
                    // Add the matching mode group to the internal holding list so we can re-use it for the next analysis point
                    matchedModeGroups.Add(matchingModeGroup);
                    // Remove the matching mode group from the list of unmatched mode groups so it can't match another TransmissionLoss
                    // in the current AnalysisPoint
                    unmatchedModeGroups.Remove(matchingModeGroup);
                    // Update the current TransmissionLoss
                    transmissionLoss.Update();
                }
                // If any mode groups remain unmatched in the current AnalysisPoint, add new TransmissionLosses to hold them
                foreach (var newModeGroup in unmatchedModeGroups)
                {
                    // Get the bathymetry for the scenario that the AnalysisPoint belongs to
                    var bathymetry = analysisPoint.Scenario.BathymetryData;
                    // Look up the depth at the AnalysisPoint
                    var depthAtAnalysisPoint = bathymetry.Samples.IsFast2DLookupAvailable
                                                   ? -bathymetry.Samples.GetNearestPointAsync(analysisPoint.Geo).Result.Data
                                                   : -bathymetry.Samples.GetNearestPoint(analysisPoint.Geo).Data;
                    // Get a representative mode from the list of modes
                    var firstMode = newModeGroup.First();
                    // Calculate the actual source depth of the mode
                    var sourceDepth = firstMode.Source.Platform.Depth;
                    if (firstMode.Depth.HasValue) sourceDepth += firstMode.Depth.Value;
                    // If the source depth is greater than the depth at this AnalysisPoint, 
                    // then don't add a TransmissionLoss for this list of modes to the current AnalysisPoint
                    if (sourceDepth >= depthAtAnalysisPoint)
                    {
                        Debug.WriteLine(string.Format("Skipping TL mode [{0}:{1}] at {2} because depth is too shallow ", firstMode.ModeName, firstMode.MaxPropagationRadius, (Geo)analysisPoint.Geo));
                        continue;
                    }
                    // The depth check is OK, so create a new TransmissionLoss
                    Debug.WriteLine(string.Format("Creating new TL for mode [{0}:{1}] at {2}", firstMode.ModeName, firstMode.MaxPropagationRadius, (Geo)analysisPoint.Geo));
                    var transmissionLoss = new TransmissionLoss { AnalysisPoint = analysisPoint };
                    // Add the list of modes to the TransmissionLoss
                    newModeGroup.ForEach(m =>
                    {
                        Debug.WriteLine(string.Format("Adding mode [{0}:{1}] to new TL at {2}", m.ModeName, m.MaxPropagationRadius, (Geo)transmissionLoss.AnalysisPoint.Geo));
                        transmissionLoss.Modes.Add(m);
                        m.TransmissionLosses.Add(transmissionLoss);
                    });
                    // Add the TransmissionLoss to the current AnalysisPoint
                    analysisPoint.TransmissionLosses.Add(transmissionLoss);
                    // Verify the TransmissionLoss, which will create the radials and queue them for calculation
                    transmissionLoss.Update();
                }
                // Add the matched mode groups from the last pass back into the list of unmatched mode groups for the next iteration
                unmatchedModeGroups.AddRange(matchedModeGroups);
                // Clear the matched mode group list for the next iteration
                matchedModeGroups.Clear();
            }
        }

        /// <summary>
        /// Update a TransmissionLoss to reflect the modes that it currently contains.
        /// If there are no modes associated with the TransmissionLoss, delete it.
        /// If the max number of radials among the associated modes has changed, 
        /// or if the max calculation radius of the associated modes is larger than the existing radials, 
        /// recalculate the TransmissionLoss
        /// </summary>
        /// <param name="transmissionLoss"></param>
        static void Update(this TransmissionLoss transmissionLoss)
        {
            if (transmissionLoss.Modes == null || transmissionLoss.Modes.Count == 0)
            {
                // If there are no modes, the TransmissionLoss is not needed
                transmissionLoss.Delete();
                return;
            }

            // Get the maximum radial count called for by the associated modes in the TransmissionLoss
            var modeRadialCount = transmissionLoss.Modes.Max(m => m.RadialCount);
            // Get the maximum propagation radius called for by the associated modes in the TransmissionLoss
            var modeMaxRadius = transmissionLoss.Modes.Max(m => m.MaxPropagationRadius);

            // Check to see if none of the modes have defined a radial count.  If not, use the default values
            modeRadialCount = modeRadialCount > 0 ? modeRadialCount : modeMaxRadius <= 10000 ? 8 : 16;

            // If there are any radials already defined, and if the count of those radials is the same as the max radial count
            // and the length of all radials is greater than or equal to the required max radius, this TransmissionLoss is valid
            if (transmissionLoss.Radials != null && transmissionLoss.Radials.Count == modeRadialCount && transmissionLoss.Radials.All(radial => radial.Length >= modeMaxRadius))
            {
                // Redraw the map layers to display the new TransmissionLoss as it should be
                Dispatcher.InvokeIfRequired(transmissionLoss.CreateMapLayers);
                return;
            }

            // If the list of radials is null, create a list
            if (transmissionLoss.Radials == null) transmissionLoss.Radials = new ObservableList<Radial>();

            // Create a list of required bearings so we can delete any existing radials that are no longer required
            var requiredBearings = new List<double>();
            // Loop through all the radials we will need to satisfy the current mode requirements
            for (var radialIndex = 0; radialIndex < modeRadialCount; radialIndex++)
            {
                // Calculate the bearing of the next radial we will need to have
                var radialBearing = (360.0 / modeRadialCount) * radialIndex;
                // Add the current bearing to the list of required bearings
                requiredBearings.Add(radialBearing);
                // Find the radial in the current set that matches the bearing, if there is one
                var currentRadial = transmissionLoss.Radials.FirstOrDefault(r => Math.Abs(r.Bearing - radialBearing) < double.Epsilon);
                // If there is a radial that matches the bearing AND that radial is too short to meet the current length requirements
                if (currentRadial != null && currentRadial.Length < modeMaxRadius)
                {
                    //Debug.WriteLine(string.Format("Deleting too-short radial [{0}] from mode [{1}] at {2}", currentRadial, transmissionLoss.Modes.First(), (Geo)transmissionLoss.AnalysisPoint.Geo));
                    // Delete the radial
                    currentRadial.Delete();
                    // Pretend that we didn't find a matching radial, so we will create one below
                    currentRadial = null;
                }
                // If we found a radial that matches the bearing and length requirements, check the next one
                if (currentRadial != null) continue;

                // We didn't find a radial that matches the bearing and length requirements, so create one now
                var radial = new Radial
                {
                    TransmissionLoss = transmissionLoss,
                    CalculationCompleted = DateTime.MaxValue,
                    CalculationStarted = DateTime.MaxValue,
                    Bearing = radialBearing,
                    Length = modeMaxRadius,
                    IsCalculated = false,
                };
                //Debug.WriteLine(string.Format("Adding new radial [{0}] to mode [{1}] at {2}", radial, transmissionLoss.Modes.First(), (Geo)transmissionLoss.AnalysisPoint.Geo));
                // Add the new Radial to the TransmissionLoss
                transmissionLoss.Radials.Add(radial);
                // Queue the new Radial for calculation
                TransmissionLossCalculator.Add(radial);
            }
            // Delete any radials in the current TransmissionLoss that are no longer required
            //  Debug.WriteLine(string.Format("Deleting now-unused radial [{0}] from mode [{1}] at {2}", r.ToString(), transmissionLoss.Modes.First(), (Geo)transmissionLoss.AnalysisPoint.Geo));
            transmissionLoss.Radials.FindAll(r => !requiredBearings.Contains(r.Bearing)).ForEach(r => r.Delete());
            // Redraw the map layers to display the new TransmissionLoss as it should be
            Dispatcher.InvokeIfRequired(transmissionLoss.CreateMapLayers);
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
        public static bool CanBeSimulated(this Scenario scenario) { return scenario.GenerateCanBeSimulatedErrorString() == null; }
        public static bool CanPlaceAnalysisPoints(this Scenario scenario) { return scenario.GenerateCanPlaceAnalysisPointsErrorString() == null; }

        public static string GenerateCanPlaceAnalysisPointsErrorString(this Scenario scenario)
        {
            var sb = new StringBuilder();

            if (scenario == null) return "Scenario is null";
            if (scenario.Platforms == null || scenario.Platforms.Count == 0) sb.AppendLine("  • No platforms have been defined");
            var scenarioModeCount = (from platform in scenario.Platforms
                                     from source in platform.Sources
                                     from mode in source.Modes
                                     select mode).Count();
            if (scenarioModeCount == 0) sb.AppendLine("  • No modes have been defined");

            return sb.Length == 0 ? null : sb.ToString();
        }

        public static string GenerateCanBeSimulatedErrorString(this Scenario scenario)
        {
            var sb = new StringBuilder();

            if (scenario == null) return "Scenario is null";
            var result = scenario.GenerateCanPlaceAnalysisPointsErrorString();
            if (result != null) sb.Append(result);
            if (scenario.AnalysisPoints.Count == 0) sb.AppendLine("  • There are no analysis points specified.");
            else
            {
                var radialsNotCalculated = (from analysisPoint in scenario.AnalysisPoints
                                            from transmissionLoss in analysisPoint.TransmissionLosses
                                            from radial in transmissionLoss.Radials
                                            where !File.Exists(radial.BasePath + ".shd")
                                            select radial).ToList();
                if (radialsNotCalculated.Count != 0)
                {
                    //var radialList = (from analysisPoint in scenario.AnalysisPoints
                    //                  from transmissionLoss in analysisPoint.TransmissionLosses
                    //                  from radial in transmissionLoss.Radials
                    //                  where !File.Exists(radial.BasePath + ".shd")
                    //                  select radial).ToList();

                    var radialsOutsideScenarioBounds = (from radial in radialsNotCalculated
                                                        where !((GeoRect)scenario.Location.GeoRect).Contains(radial.Segment[1])
                                                        select radial).ToList();
                    var analysisPointsInError = (from radial in radialsOutsideScenarioBounds
                                                 where !((GeoRect)scenario.Location.GeoRect).Contains(radial.Segment[1])
                                                 select radial.TransmissionLoss.AnalysisPoint).Distinct().ToList();
                    var radialsNotYetCalculatedCount = radialsNotCalculated.Except(radialsOutsideScenarioBounds).Count();
                    if (radialsNotYetCalculatedCount > 0) sb.AppendLine(string.Format("  • There are still {0} radials awaiting calculation in this scenario.", radialsNotYetCalculatedCount));
                    if (analysisPointsInError.Count > 0)
                        sb.AppendLine(analysisPointsInError.Count == 1
                                          ? string.Format("  • An analysis point has one or more radials that extend outside the location boundaries.")
                                          : string.Format("  • {0} analysis points have radials that extend outside the location boundaries.", analysisPointsInError.Count));
                }
            }

            if (!scenario.ScenarioSpecies.Any()) sb.AppendLine("  • There are no species specified in this scenario.");
            foreach (var species in scenario.ScenarioSpecies.Where(species => !species.Animat.Locations.Any())) sb.AppendLine(string.Format("  • There are no animats seeded for species {0}", species.LatinName));

            return sb.Length == 0 ? null : sb.ToString();
        }
    }
}
