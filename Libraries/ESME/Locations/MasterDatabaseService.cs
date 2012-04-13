using System;
using System.Collections.ObjectModel;
using System.ComponentModel.Composition;
using System.Data;
using System.Data.Common;
using System.Data.Entity;
using System.Data.Entity.Infrastructure;
using System.Data.Entity.Validation;
using System.IO;
using System.Linq;
using Devart.Data.SQLite;
using Devart.Data.SQLite.Entity.Configuration;
using ESME.Database;
using ESME.Environment;
using ESME.NEMO;
using ESME.NEMO.Overlay;
using ESME.Plugins;
using ESME.Scenarios;
using HRC.Navigation;
using HRC.Utility;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Locations
{
    [PartCreationPolicy(CreationPolicy.Shared)]
    [ExportService(ServiceType.Both, typeof(MasterDatabaseService))]
    public class MasterDatabaseService : PropertyChangedBase, IDisposable
    {
        #region Public methods and properties
        string _masterDatabaseDirectory;
        public string MasterDatabaseDirectory
        {
            get { return _masterDatabaseDirectory; }
            set
            {
                _masterDatabaseDirectory = value;
                Initialize();
                Location.Database = this;
                Scenario.Database = this;
            }
        }

        public ObservableCollection<Location> Locations { get { return Context == null ? null : Context.Locations.Local; } }
        public Location FindLocation(string locationName) { return Locations == null ? null : Locations.FirstOrDefault(l => l.Name == locationName); }
        public bool LocationExists(string locationName) { return FindLocation(locationName) != null; }

        public ObservableCollection<Scenario> Scenarios { get { return Context == null ? null : Context.Scenarios.Local; } }
        public Scenario FindScenario(string scenarioName) { return Scenarios == null ? null : Scenarios.FirstOrDefault(l => l.Name == scenarioName); }
        public bool ScenarioExists(string scenarioName) { return FindScenario(scenarioName) != null; }
        #region Add operations
        public void Add(Location location, bool saveChanges = false)
        {
            if (LocationExists(location.Name)) throw new DuplicateNameException(String.Format("A location named {0} already exists, choose another name", location.Name));
            if (location.StorageDirectory == null)
                location.StorageDirectory = Path.Combine("locations", Path.GetFileNameWithoutExtension(Path.GetRandomFileName()));
            var storageDirectoryPath = Path.Combine(MasterDatabaseDirectory, location.StorageDirectory);
            if (!Directory.Exists(storageDirectoryPath)) Directory.CreateDirectory(storageDirectoryPath);
            Context.Locations.Add(location);
            Log(location, "Added location {0}", location.Name);
            if (saveChanges) SaveChanges();
        }
        public void Add(EnvironmentalDataSet dataSet, bool saveChanges = false)
        {
            if (dataSet.LayerSettings == null) dataSet.LayerSettings = new LayerSettings();
            Context.LayerSettings.Add(dataSet.LayerSettings);
            Context.EnvironmentalDataSets.Add(dataSet);
            Log(dataSet, "Added new data set to location {0}. Data type: {1}, resolution: {2}{3}", dataSet.Location.Name, dataSet.SourcePlugin.PluginSubtype, dataSet.Resolution, (TimePeriod)dataSet.TimePeriod != TimePeriod.Invalid ? String.Format("  TimePeriod: {0}", (TimePeriod)dataSet.TimePeriod) : "");
            if (saveChanges) SaveChanges();
        }
        public void Add(Scenario scenario, bool saveChanges = false)
        {
            var existing = (from s in Context.Scenarios.Local
                            where s.Name == scenario.Name && s.Location == scenario.Location
                            select s).FirstOrDefault();
            if (existing != null) throw new DuplicateNameException(String.Format("A scenario named {0} already exists in location {1}, choose another name", scenario.Name, scenario.Location.Name));
            if (scenario.LayerSettings == null) scenario.LayerSettings = new LayerSettings();
            Context.LayerSettings.Add(scenario.LayerSettings);
            if (scenario.StorageDirectory == null)
                scenario.StorageDirectory = Path.Combine("scenarios", Path.GetFileNameWithoutExtension(Path.GetRandomFileName()));
            var storageDirectoryPath = Path.Combine(MasterDatabaseDirectory, scenario.StorageDirectory);
            if (!Directory.Exists(storageDirectoryPath)) Directory.CreateDirectory(storageDirectoryPath);

            Context.Scenarios.Add(scenario);
            Log(scenario, "Added new scenario {0} to data set to location {1}", scenario.Name, scenario.Location.Name);
            if (saveChanges) SaveChanges();
        }
        public void Add(Platform platform, bool saveChanges = false)
        {
            Context.Platforms.Add(platform);
            if (platform.LayerSettings == null) platform.LayerSettings = new LayerSettings();
            Context.LayerSettings.Add(platform.LayerSettings);
            Log(platform, "Added new platform {0} to scenario {1} in location {2}", platform.Description, platform.Scenario.Name, platform.Scenario.Location.Name);
            if (saveChanges) SaveChanges();
        }
        public void Add(Source source, bool saveChanges = false)
        {
            Context.Sources.Add(source);
            if (source.LayerSettings == null) source.LayerSettings = new LayerSettings();
            Context.LayerSettings.Add(source.LayerSettings);
            Log(source, "Added new source {0} to platform {1} in scenario {2} in location {3}", source.SourceName, source.Platform.Description, source.Platform.Scenario.Name, source.Platform.Scenario.Location.Name);
            if (saveChanges) SaveChanges();
        }
        public void Add(Mode mode, bool saveChanges = false)
        {
            Context.Modes.Add(mode);
            if (mode.LayerSettings == null) mode.LayerSettings = new LayerSettings();
            Context.LayerSettings.Add(mode.LayerSettings);
            Log(mode, "Added new mode {0} to source {1} of platform {2} in scenario {3} in location {4}", mode.ModeName, mode.Source.SourceName, mode.Source.Platform.Description, mode.Source.Platform.Scenario.Name, mode.Source.Platform.Scenario.Location.Name);
            if (saveChanges) SaveChanges();
        }
        public void Add(Scenario scenario, EnvironmentalDataSet dataSet, bool replaceExisting = false, bool saveChanges = false)
        {
            EnvironmentalDataSet oldData = null;
            // todo: Check to see if replacing any of these datasets might invalidate any transmission losses we have previously calculated
            switch ((PluginSubtype)dataSet.SourcePlugin.PluginSubtype)
            {
                case PluginSubtype.Wind:
                    if (scenario.Wind != null && !replaceExisting) throw new ArgumentException(string.Format("Scenario {0} already has a wind dataset.  Did you intend to replace it?", scenario.Name), "dataSet");
                    oldData = scenario.Wind;
                    scenario.Wind = dataSet;
                    break;
                case PluginSubtype.SoundSpeed:
                    if (scenario.SoundSpeed != null && !replaceExisting) throw new ArgumentException(string.Format("Scenario {0} already has a sound speed dataset.  Did you intend to replace it?", scenario.Name), "dataSet");
                    oldData = scenario.SoundSpeed;
                    scenario.SoundSpeed = dataSet;
                    break;
                case PluginSubtype.Sediment:
                    if (scenario.Sediment != null && !replaceExisting) throw new ArgumentException(string.Format("Scenario {0} already has a sediment dataset.  Did you intend to replace it?", scenario.Name), "dataSet");
                    oldData = scenario.Sediment;
                    scenario.Sediment = dataSet;
                    break;
                case PluginSubtype.Bathymetry:
                    if (scenario.Bathymetry != null && !replaceExisting) throw new ArgumentException(string.Format("Scenario {0} already has a bathymetry dataset.  Did you intend to replace it?", scenario.Name), "dataSet");
                    oldData = scenario.Bathymetry;
                    scenario.Bathymetry = dataSet;
                    break;
            }
            // todo: enhance these messages to include resolution and time period
            if (oldData == null)
                Log(scenario, dataSet, "Added new {0} data set to scenario {1} (source {2})", dataSet.SourcePlugin.PluginSubtype, scenario.Name, dataSet.SourcePlugin.Type);
            else
                Log(scenario, dataSet, "Replaced old {0} data set in scenario {1} (old source {2}) with data from source {3}", oldData.SourcePlugin.PluginSubtype, scenario.Name, oldData.SourcePlugin.Type, dataSet.SourcePlugin.Type);
            if (saveChanges) SaveChanges();
        }
        public void Add(Perimeter perimeter, bool saveChanges = false)
        {
            var existing = (from p in Context.Perimeters
                            where p.Name == perimeter.Name && p.Scenario.Guid == perimeter.Scenario.Guid
                            select p).FirstOrDefault();
            if (existing != null) throw new DuplicateNameException(String.Format("A perimeter named {0} already exists in scenario {1}, choose another name", perimeter.Name, perimeter.Scenario.Name));
            if (perimeter.LayerSettings == null) perimeter.LayerSettings = new LayerSettings();
            Context.LayerSettings.Add(perimeter.LayerSettings);
            Context.Perimeters.Add(perimeter);
            Log(perimeter, "Added new perimeter {0} to scenario {1} in location {2}", perimeter.Name, perimeter.Scenario.Name, perimeter.Scenario.Location.Name);
            if (saveChanges) SaveChanges();
        }
        public void Add(PerimeterCoordinate coordinate, bool replaceExisting = false, bool saveChanges = false)
        {
            var existing = (from c in Context.PerimeterCoordinates.Local
                            where c.Perimeter == coordinate.Perimeter && c.Order == coordinate.Order
                            select c).FirstOrDefault();
            if (existing != null && !replaceExisting) throw new ArgumentException(string.Format("Perimeter {0} already has a point at index {1}.  Did you intend to replace it?", coordinate.Perimeter.Name, coordinate.Order), "coordinate");
            if (existing != null) Context.PerimeterCoordinates.Remove(existing);
            Context.PerimeterCoordinates.Add(coordinate);
            if (saveChanges) SaveChanges();
        }
        public void Add(ScenarioSpecies species, bool saveChanges = false)
        {
            var existing = (from s in Context.ScenarioSpecies.Local
                            where s.LatinName == species.LatinName && s.Scenario == species.Scenario
                            select s).FirstOrDefault();
            if (existing != null) throw new DuplicateNameException(String.Format("A species named {0} already exists in scenario {1}, choose another name", species.LatinName, species.Scenario.Name));
            if (species.LayerSettings == null) species.LayerSettings = new LayerSettings();
            Context.LayerSettings.Add(species.LayerSettings);
            Context.ScenarioSpecies.Add(species);
            Log(species, "Added new species {0} to scenario {1} in location {2}", species.LatinName, species.Scenario.Name, species.Scenario.Location.Name);
            if (saveChanges) SaveChanges();
        }

        public void Add(AnalysisPoint analysisPoint, Bathymetry bathymetry, bool saveChanges = false)
        {
            var existing = (from a in Context.AnalysisPoints.Local
                            where a.Geo == analysisPoint.Geo
                            select a).FirstOrDefault();
            if (existing != null) throw new DuplicateNameException(String.Format("An analysis point already exists at {0}, choose another location or edit the existing point", (Geo)analysisPoint.Geo));
            if (analysisPoint.LayerSettings == null) analysisPoint.LayerSettings = new LayerSettings();
            Context.LayerSettings.Add(analysisPoint.LayerSettings);
            if (analysisPoint.Scenario == null) throw new ScenarioException(string.Format("Scenario for analysis point at {0} was not specified", analysisPoint.Geo));
            var depthAtAnalysisPoint = -bathymetry.Samples.GetNearestPoint(analysisPoint.Geo).Data;
            foreach (var mode in analysisPoint.Scenario.GetAllModes())
            {
                var sourceDepth = mode.Source.Platform.Depth;
                if (mode.Depth.HasValue) sourceDepth += mode.Depth.Value;
                if (sourceDepth >= depthAtAnalysisPoint)
                {
                    Console.WriteLine("Skipping {0}:{1}:{2}, because the depth is below the bottom for this analysis point", mode.Source.Platform.PlatformName, mode.Source.SourceName, mode.ModeName);
                    continue;
                }
                var transmissionLoss = new Scenarios.TransmissionLoss
                {
                    AnalysisPoint = analysisPoint,
                    IsReadyToCalculate = false,
                    Mode = mode,
                };
                var radialCount = mode.MaxPropagationRadius <= 10000 ? 8 : 16;
                for (var radialIndex = 0; radialIndex < radialCount; radialIndex++)
                {
                    var radial = new Radial
                    {
                        TransmissionLoss = transmissionLoss,
                        CalculationCompleted = DateTime.MaxValue,
                        CalculationStarted = DateTime.MaxValue,
                        Bearing = (360.0 / radialCount) * radialIndex,
                        Length = transmissionLoss.Mode.MaxPropagationRadius,
                        IsCalculated = false,
                    };
                    Add(radial);
                    transmissionLoss.Radials.Add(radial);
                }
                Add(transmissionLoss);
                analysisPoint.TransmissionLosses.Add(transmissionLoss);
            }
            Context.AnalysisPoints.Add(analysisPoint);
            Log(analysisPoint, "Added new analysis point at {0} to scenario {1} in location {2}", (Geo)analysisPoint.Geo, analysisPoint.Scenario, analysisPoint.Scenario.Location);
            if (saveChanges) SaveChanges();
        }
        public void Add(Scenarios.TransmissionLoss transmissionLoss, bool saveChanges = false)
        {
            var existing = (from t in Context.TransmissionLosses
                            where t.Mode.Guid == transmissionLoss.Mode.Guid && t.AnalysisPoint.Guid == transmissionLoss.AnalysisPoint.Guid
                            select t).FirstOrDefault();
            if (existing != null) throw new DuplicateNameException(String.Format("A transmission loss for mode {0} already exists for the analysis point at {1}", transmissionLoss.Mode.ModeName, (Geo)transmissionLoss.AnalysisPoint.Geo));
            if (transmissionLoss.LayerSettings == null) transmissionLoss.LayerSettings = new LayerSettings();
            Context.LayerSettings.Add(transmissionLoss.LayerSettings);
            Context.TransmissionLosses.Add(transmissionLoss);
            Log(transmissionLoss, "Added new transmission loss for mode {0} to analysis point at {1} to scenario {2} in location {3}", transmissionLoss.Mode.ModeName, (Geo)transmissionLoss.AnalysisPoint.Geo, transmissionLoss.AnalysisPoint.Scenario, transmissionLoss.AnalysisPoint.Scenario.Location);
            if (saveChanges) SaveChanges();
        }
        public void Add(Radial radial, bool saveChanges = false)
        {
            var existing = (from r in Context.Radials
                            where r.Bearing == radial.Bearing && r.TransmissionLoss.Guid == radial.TransmissionLoss.Guid
                            select r).FirstOrDefault();
            if (existing != null) throw new DuplicateNameException(String.Format("A radial with bearing {0} already exists in the transmission loss for mode {1} in analysis point at {2}", radial.Bearing, radial.TransmissionLoss.Mode.ModeName, (Geo)radial.TransmissionLoss.AnalysisPoint.Geo));
            Context.Radials.Add(radial);
            Log(radial, "Added new radial with bearing {0} and length {1} to transmission loss for mode {2} in analysis point at {3} to scenario {4} in location {5}", radial.Bearing, radial.Length, radial.TransmissionLoss.Mode.ModeName, (Geo)radial.TransmissionLoss.AnalysisPoint.Geo, radial.TransmissionLoss.AnalysisPoint.Scenario, radial.TransmissionLoss.AnalysisPoint.Scenario.Location);
            if (saveChanges) SaveChanges();
        }
        public void Add(LevelRadius levelRadius, bool saveChanges = false)
        {
            var existing = (from lr in Context.LevelRadii
                            where lr.Level == levelRadius.Level && lr.Radial.Guid == levelRadius.Radial.Guid
                            select lr).FirstOrDefault();
            if (existing != null) throw new DuplicateNameException(String.Format("A LevelRadius with level {0} already exists for radial with bearing {1} already exists in the transmission loss for mode {2} in analysis point at {3}", levelRadius.Level, levelRadius.Radial.Bearing, levelRadius.Radial.TransmissionLoss.Mode.ModeName, (Geo)levelRadius.Radial.TransmissionLoss.AnalysisPoint.Geo));
            Context.LevelRadii.Add(levelRadius);
            Log(levelRadius, "Added new radius at level {0} on radial with bearing {1} and length {2} in transmission loss for mode {3} in analysis point at {4} to scenario {5} in location {6}", levelRadius.Level, levelRadius.Radial.Bearing, levelRadius.Radial.Length, levelRadius.Radial.TransmissionLoss.Mode.ModeName, (Geo)levelRadius.Radial.TransmissionLoss.AnalysisPoint.Geo, levelRadius.Radial.TransmissionLoss.AnalysisPoint.Scenario, levelRadius.Radial.TransmissionLoss.AnalysisPoint.Scenario.Location);
            if (saveChanges) SaveChanges();
        }
        #endregion

        #region Create operations for Locations
        public Location CreateLocation(string locationName, string comments, double north, double south, double east, double west)
        {
            if (LocationExists(locationName)) throw new DuplicateNameException(String.Format("A location named {0} already exists, choose another name", locationName));
            var result = new Location
                             {
                                 Name = locationName,
                                 Comments = comments,
                                 GeoRect = new GeoRect(north, south, east, west),
                                 StorageDirectory = Path.Combine("locations", Path.GetFileNameWithoutExtension(Path.GetRandomFileName())),
                             };
            Context.Locations.Add(result);
            Log(result, "Created");
            Directory.CreateDirectory(Path.Combine(MasterDatabaseDirectory, result.StorageDirectory));
            SaveChanges();
            return result;
        }

        public Location ImportLocationFromOverlayFile(string overlayFilename, string locationName)
        {
            var geoRect = new OverlayFile(overlayFilename).Shapes[0].GeoRect;
            return FindLocation(locationName) ?? CreateLocation(locationName,
                                                                String.Format("Imported from {0} on {1} by {2} on {3}", overlayFilename, System.Environment.UserName, DateTime.Now, System.Environment.MachineName),
                                                                geoRect.North,
                                                                geoRect.South,
                                                                geoRect.East,
                                                                geoRect.West);
        }

        public EnvironmentalDataSet CreateEnvironmentalDataSet(Location location, float resolution, TimePeriod timePeriod, PluginIdentifier sourcePlugin)
        {
            var environmentalDataSet = new EnvironmentalDataSet
            {
                FileName = Path.GetFileNameWithoutExtension(Path.GetRandomFileName()) + "." + sourcePlugin.PluginSubtype.ToString().ToLower(),
                Resolution = resolution,
                TimePeriod = timePeriod,
                Location = location,
                SourcePlugin = sourcePlugin,
            };
            if (environmentalDataSet.LayerSettings == null) environmentalDataSet.LayerSettings = new LayerSettings();
            Context.LayerSettings.Add(environmentalDataSet.LayerSettings);
            Context.EnvironmentalDataSets.Add(environmentalDataSet);
            Log(environmentalDataSet, "Added new data set to {0}. Data type: {1}, resolution: {2}{3}", location.Name, sourcePlugin.PluginSubtype, resolution, timePeriod != TimePeriod.Invalid ? String.Format("  TimePeriod: {0}", timePeriod) : "");
            SaveChanges();
            return environmentalDataSet;
        }
        #endregion

        #region Create operations for Scenarios
        public Scenario CreateScenario(string scenarioName, string comments, TimeSpan startTime, TimeSpan duration, TimePeriod timePeriod, Location location)
        {
            if (ScenarioExists(scenarioName)) throw new DuplicateNameException(String.Format("A scenario named {0} already exists, choose another name", scenarioName));
            var scenario = Context.Scenarios.Create();
            scenario.Name = scenarioName;
            scenario.Comments = comments;
            scenario.StartTime = startTime;
            scenario.Duration = duration;
            scenario.TimePeriod = timePeriod;
            scenario.Location = location;
            scenario.StorageDirectory = Path.Combine("scenarios", Path.GetFileNameWithoutExtension(Path.GetRandomFileName()));
            Directory.CreateDirectory(Path.Combine(MasterDatabaseDirectory, scenario.StorageDirectory));
            if (scenario.LayerSettings == null) scenario.LayerSettings = new LayerSettings();
            Context.LayerSettings.Add(scenario.LayerSettings);
            Context.Scenarios.Add(scenario);
            Log(scenario, "Created");
            SaveChanges();
            return scenario;
        }

        public void SetEnvironmentalData(Scenario scenario, EnvironmentalDataSet data)
        {
            switch ((PluginSubtype)data.SourcePlugin.PluginSubtype)
            {
                case PluginSubtype.Wind:
                    scenario.Wind = data;
                    break;
                case PluginSubtype.SoundSpeed:
                    scenario.SoundSpeed = data;
                    break;
                case PluginSubtype.Sediment:
                    scenario.Sediment = data;
                    break;
                case PluginSubtype.Bathymetry:
                    scenario.Bathymetry = data;
                    break;
            }
            SaveChanges();
        }
        #endregion

        #region Delete operations
        public void DeleteLocation(Location location) { }
        protected void DeleteLocation(Location location, bool saveChanges)
        {
            // todo: Handle the case where this location is used by one or more scenarios
                foreach (var dataSet in location.EnvironmentalDataSets)
                    Context.EnvironmentalDataSets.Remove(dataSet);
            Context.Locations.Remove(location);
            if (saveChanges) SaveChanges();
        }
        public void DeleteEnvironmentalDataSet(EnvironmentalDataSet dataSet) { DeleteEnvironmentalDataSet(dataSet, true); }
        protected void DeleteEnvironmentalDataSet(EnvironmentalDataSet dataSet, bool saveChanges)
        {
            // todo: Handle the case where this data set is used by one or more scenarios
            var fileName = Path.Combine(MasterDatabaseDirectory, dataSet.Location.StorageDirectory, dataSet.FileName);
            var filesToDelete = Directory.EnumerateFiles(Path.GetDirectoryName(fileName), Path.GetFileNameWithoutExtension(fileName) + ".*");
            foreach (var file in filesToDelete) File.Delete(file);
            Context.EnvironmentalDataSets.Remove(dataSet);
            if (saveChanges) SaveChanges();
        }
        #endregion

        #endregion
        #region Private helper methods, properties and fields

        public LocationContext Context { get; private set; }

        void Initialize()
        {
            if (String.IsNullOrEmpty(MasterDatabaseDirectory)) throw new ApplicationException("MasterDatabaseDirectory cannot be null or empty");
            if (!Directory.Exists(MasterDatabaseDirectory))
            {
                Directory.CreateDirectory(MasterDatabaseDirectory);
                Directory.CreateDirectory(Path.Combine(MasterDatabaseDirectory, "locations"));
                Directory.CreateDirectory(Path.Combine(MasterDatabaseDirectory, "scenarios"));
            }
            SQLiteEntityProviderConfig.Instance.Workarounds.IgnoreSchemaName = true;
            SQLiteEntityProviderConfig.Instance.DmlOptions.BatchUpdates.Enabled = true;
            SQLiteEntityProviderConfig.Instance.DmlOptions.BatchUpdates.BatchSize = 30;
            SQLiteEntityProviderConfig.Instance.DmlOptions.BatchUpdates.AsynchronousBatch = true;
            var connectionStringBuilder = new SQLiteConnectionStringBuilder
            {
                FailIfMissing = false,
                DataSource = Path.Combine(MasterDatabaseDirectory, "esme.db"),
                BinaryGUID = true,
            };
            DbConnection connection = new SQLiteConnection(connectionStringBuilder.ToString());
            Context = new LocationContext(connection, true);
            Context.Locations.Load();
            Context.Scenarios.Load();
            OnPropertyChanged("Locations");
            OnPropertyChanged("Scenarios");
        }

        void SaveChanges()
        {
            lock (Context)
            {
                try
                {
                    Context.SaveChanges();
                }
                catch (DbEntityValidationException dbEntityValidationException)
                {
                    Console.WriteLine("SaveChanges caught DbEntityValidationException");
                    foreach (var innerError in dbEntityValidationException.EntityValidationErrors.SelectMany(validationError => validationError.ValidationErrors))
                        Console.WriteLine("  {0}: {1}", innerError.PropertyName, innerError.ErrorMessage);
                    throw;
                }
                catch (DbUpdateException dbUpdateException)
                {
                    Console.WriteLine("SaveChanges caught DbUpdateException");
                    Console.WriteLine("  {0}", dbUpdateException.InnerException.Message);
                    if (dbUpdateException.InnerException.InnerException != null)
                        Console.WriteLine("    {0}", dbUpdateException.InnerException.InnerException.Message);
                    throw;
                }
                catch (Exception exception)
                {
                    Console.WriteLine("SaveChanges caught Exception: {0}", exception.Message);
                    throw;
                }
            }
        }
#if true
        void Log(Location location, string message, params object[] args) { LogBase(new LogEntry(location) { Location = location }, message, args); }
        void Log(EnvironmentalDataSet dataSet, string message, params object[] args) { LogBase(new LogEntry(dataSet) { Location = dataSet.Location, EnvironmentalDataSet = dataSet }, message, args); }
        void Log(Scenario scenario, string message, params object[] args) { LogBase(new LogEntry(scenario) { Location = scenario.Location, Scenario = scenario }, message, args); }
        void Log(Scenario scenario, EnvironmentalDataSet dataSet, string message, params object[] args) { LogBase(new LogEntry(dataSet) { Location = scenario.Location, EnvironmentalDataSet = dataSet, Scenario = scenario }, message, args); }
        void Log(Platform platform, string message, params object[] args) { LogBase(new LogEntry(platform) { Location = platform.Scenario.Location, Scenario = platform.Scenario, Platform = platform }, message, args); }
        void Log(Source source, string message, params object[] args) { LogBase(new LogEntry(source) { Location = source.Platform.Scenario.Location, Scenario = source.Platform.Scenario, Platform = source.Platform, Source = source }, message, args); }
        void Log(Mode mode, string message, params object[] args) { LogBase(new LogEntry(mode) { Location = mode.Source.Platform.Scenario.Location, Scenario = mode.Source.Platform.Scenario, Platform = mode.Source.Platform, Source = mode.Source, Mode = mode }, message, args); }
        void Log(Perimeter perimeter, string message, params object[] args) { LogBase(new LogEntry(perimeter) { Location = perimeter.Scenario.Location, Scenario = perimeter.Scenario, Perimeter = perimeter }, message, args); }
        void Log(ScenarioSpecies species, string message, params object[] args) { LogBase(new LogEntry(species) { Location = species.Scenario.Location, Scenario = species.Scenario, ScenarioSpecies = species }, message, args); }
        void Log(AnalysisPoint analysisPoint, string message, params object[] args) { LogBase(new LogEntry(analysisPoint) { Location = analysisPoint.Scenario.Location, Scenario = analysisPoint.Scenario, AnalysisPoint = analysisPoint }, message, args); }
        void Log(Scenarios.TransmissionLoss transmissionLoss, string message, params object[] args) { LogBase(new LogEntry(transmissionLoss) { Location = transmissionLoss.AnalysisPoint.Scenario.Location, Scenario = transmissionLoss.AnalysisPoint.Scenario, TransmissionLoss = transmissionLoss }, message, args); }
        void Log(Radial radial, string message, params object[] args) { LogBase(new LogEntry(radial) { Location = radial.TransmissionLoss.AnalysisPoint.Scenario.Location, Scenario = radial.TransmissionLoss.AnalysisPoint.Scenario, Radial = radial }, message, args); }
        void Log(LevelRadius levelRadius, string message, params object[] args) { LogBase(new LogEntry(levelRadius) { Location = levelRadius.Radial.TransmissionLoss.AnalysisPoint.Scenario.Location, Scenario = levelRadius.Radial.TransmissionLoss.AnalysisPoint.Scenario, LevelRadius = levelRadius }, message, args); }
#else
        internal void Log(Location location, string message) { LogBase(new LogEntry(location), message); }
        internal void Log(EnvironmentalDataSet dataSet, string message) { LogBase(new LogEntry(dataSet), message); }
        internal void Log(Scenario scenario, string message) { LogBase(new LogEntry(scenario), message); }
        internal void Log(Platform platform, string message) { LogBase(new LogEntry(platform), message); }
        internal void Log(Source source, string message) { LogBase(new LogEntry(source), message); }
        internal void Log(Mode mode, string message) { LogBase(new LogEntry(mode), message); }
        internal void Log(TrackDefinition trackDefinition, string message) { LogBase(new LogEntry(trackDefinition), message); }
        internal void Log(Perimeter perimeter, string message) { LogBase(new LogEntry(perimeter), message); }
#endif

        void LogBase(LogEntry logEntry, string message, params object[] args)
        {
            logEntry.Message = string.Format(message, args);
            logEntry.MessageSource = new DbWhoWhenWhere(true);
            Context.Log.Add(logEntry);
        }

        #endregion
        #region IDisposable implementation
        public void Dispose() 
        { 
            Dispose(true);
            GC.SuppressFinalize(this);
        }
        bool _disposed;
        // Dispose(bool disposing) executes in two distinct scenarios.
        // If disposing equals true, the method has been called directly
        // or indirectly by a user's code. Managed and unmanaged resources
        // can be disposed.
        // If disposing equals false, the method has been called by the
        // runtime from inside the finalizer and you should not reference
        // other objects. Only unmanaged resources can be disposed.
        protected virtual void Dispose(bool disposing)
        {
            // Check to see if Dispose has already been called.
            if (_disposed) return;
            // If disposing equals true, dispose all managed
            // and unmanaged resources.
            if (disposing)
            {
                // Dispose managed resources.
                Context.Dispose();
            }

            // Note disposing has been done.
            _disposed = true;
        }
        ~MasterDatabaseService() { Dispose(false); }
        #endregion
    }
}
