using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Data;
using System.Data.Common;
using System.Data.Entity.Infrastructure;
using System.Data.Entity.Validation;
using System.IO;
using System.Linq;
using Devart.Data.SQLite;
using ESME.Database;
using ESME.Environment;
using ESME.Plugins;
using HRC.Aspects;
using HRC.Navigation;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Locations
{
    [PartCreationPolicy(CreationPolicy.Shared)]
    [ExportService(ServiceType.Both, typeof(MasterDatabaseService))]
    [NotifyPropertyChanged]
    public class MasterDatabaseService : IDisposable
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
            }
        }

        public IEnumerable<Location> Locations { get { return _context.Locations; } }
        public Location FindLocation(string locationName) { return Locations.FirstOrDefault(l => l.Name == locationName); }
        public bool LocationExists(string locationName) { return FindLocation(locationName) != null; }

        public IEnumerable<Scenario> Scenarios { get { return _context.Scenarios; } }
        public Scenario FindScenario(string scenarioName) { return Scenarios.FirstOrDefault(l => l.Name == scenarioName); }
        public bool ScenarioExists(string scenarioName) { return FindScenario(scenarioName) != null; }

        #region Create operations for Locations
        public Location CreateLocation(string locationName, string comments, double north, double south, double east, double west)
        {
            if (LocationExists(locationName)) throw new DuplicateNameException(string.Format("A location named {0} already exists, choose another name", locationName));
            var result = new Location
                             {
                                 Name = locationName,
                                 Comments = comments,
                                 GeoRect = new GeoRect(north, south, east, west),
                                 StorageDirectory = Path.GetFileNameWithoutExtension(Path.GetRandomFileName()),
                             };
            _context.Locations.Add(result);
            Log(result, "Created");
            Directory.CreateDirectory(Path.Combine(MasterDatabaseDirectory, result.StorageDirectory));
            SaveChanges();
            return result;
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
            _context.EnvironmentalDataSets.Add(environmentalDataSet);
            Log(environmentalDataSet, string.Format("Added new data set. Resolution: {0}{1}", resolution, timePeriod != TimePeriod.Invalid ? string.Format("  TimePeriod: {0}", timePeriod) : ""));
            SaveChanges();
            return environmentalDataSet;
        }
        #endregion

        #region Create operations for Scenarios
        public Scenario CreateScenario(string scenarioName, string comments, TimeSpan startTime, TimeSpan duration, TimePeriod timePeriod, Location location)
        {
            if (ScenarioExists(scenarioName)) throw new DuplicateNameException(string.Format("A scenario named {0} already exists, choose another name", scenarioName));
            var result = new Scenario
            {
                Name = scenarioName,
                Comments = comments,
                StartTime = startTime,
                Duration = duration,
                TimePeriod = timePeriod,
                //Location = location,
            };
            _context.Scenarios.Add(result);
            Log(result, "Created");
            return result;
        }

        public Platform AddPlatform(Scenario scenario, PSMPlatform psmPlatform, string description)
        {
            if (scenario.Platforms != null && scenario.Platforms.FirstOrDefault(p => p.Description == description) != null) throw new DuplicateNameException(string.Format("A platform with the description \"{0}\" already exists in this scenario, choose another name", description));
            var platform = new Platform
            {
                Description = description,
                Launches = false,
                Tows = false,
                RepeatCount = 1,
                PSMPlatformGuid = psmPlatform.Guid,
                PlatformName = psmPlatform.PlatformName,
                PlatformType = psmPlatform.PlatformType,
                Scenario = scenario,
            };
            _context.Platforms.Add(platform);
            Log(platform, "Created");
            foreach (var psmSource in psmPlatform.PSMSources)
            {
                var source = new Source
                {
                    PSMSourceGuid = psmSource.Guid,
                    SourceName = psmSource.SourceName,
                    SourceType = psmSource.SourceType,
                    Platform = platform,
                };
                _context.Sources.Add(source);
                Log(source, "Created");
                foreach (var psmMode in psmSource.PSMModes)
                {
                    var mode = new Mode
                    {
                        PSMModeGuid = psmMode.Guid,
                        ModeName = psmMode.ModeName,
                        ModeType = psmMode.ModeType,
                        ActiveTime = psmMode.ActiveTime,
                        Depth = psmMode.Depth,
                        SourceLevel = psmMode.SourceLevel,
                        LowFrequency = psmMode.LowFrequency,
                        HighFrequency = psmMode.HighFrequency,
                        PulseInterval = psmMode.PulseInterval,
                        PulseLength = psmMode.PulseLength,
                        HorizontalBeamWidth = psmMode.HorizontalBeamWidth,
                        VerticalBeamWidth = psmMode.VerticalBeamWidth,
                        RelativeBeamAngle = psmMode.RelativeBeamAngle,
                        MaxPropagationRadius = psmMode.MaxPropagationRadius,
                        Source = source,
                    };
                    _context.Modes.Add(mode);
                    Log(mode, "Created");
                }
            }
            SaveChanges();
            return platform;
        }

        public void SetTrackDefinition(Platform platform, TrackDefinition trackDefinition)
        {
            trackDefinition.Platform = platform;
            _context.TrackDefinitions.Add(trackDefinition);
            Log(trackDefinition, "Added");
            platform.TrackDefinition = trackDefinition;
            Log(platform, string.Format("Set TrackDefinition to {0}", trackDefinition.Guid));
            SaveChanges();
        }

        public Perimeter AddOrGetPerimeter(Scenario scenario, string perimeterName, IEnumerable<PerimeterCoordinate> coordinates)
        {
            var existing = _context.Perimeters.FirstOrDefault(p => p.Scenario.Guid == scenario.Guid && p.Name == perimeterName);
            if (existing != null) return existing;
            var perimeter = new Perimeter {Scenario = scenario};
            _context.Perimeters.Add(perimeter);
            SaveChanges();
            Log(perimeter, "Added");
            SaveChanges();
            Log(scenario, string.Format("Added perimeter {0}", perimeter.Guid));
            SaveChanges();
            SetPerimeterCoordinates(perimeter, coordinates);
            return perimeter;
        }

        public void SetPerimeterCoordinates(Perimeter perimeter, IEnumerable<PerimeterCoordinate> coordinates)
        {
            // Delete all the old coordinates, if any
            if (perimeter.PerimeterCoordinates != null) foreach (var coordinate in perimeter.PerimeterCoordinates) _context.PerimeterCoordinates.Remove(coordinate);
            // Add the new ones
            foreach (var coordinate in coordinates)
            {
                coordinate.Perimeter = perimeter;
                _context.PerimeterCoordinates.Add(coordinate);
                SaveChanges();
            }
            // Save to the database
            Log(perimeter, string.Format("Changed coordinates of perimeter {0}", perimeter.Guid));
            SaveChanges();
        }

        public void SetPerimeter(TrackDefinition trackDefinition, Perimeter perimeter)
        {
            trackDefinition.Perimeter = perimeter;
            Log(trackDefinition, string.Format("Set perimeter for trackdef {0} to {1}", trackDefinition.Guid, perimeter.Guid));
            SaveChanges();
        }

        public ScenarioSpecies AddOrReplaceSpecies(Scenario scenario, ScenarioSpecies newSpecies, IEnumerable<AnimatLocation> animatLocations)
        {
            ScenarioSpecies existing = null;
            if (scenario.Species != null) existing = scenario.Species.FirstOrDefault(s => s.LatinName == newSpecies.LatinName);
            if (existing != null)
            {
                foreach (var animat in existing.AnimatLocations) _context.AnimatLocations.Remove(animat);
                _context.ScenarioSpecies.Remove(existing);
                //Log(newSpecies, string.Format("Replaced species {0}", newSpecies.LatinName));
            }
            _context.ScenarioSpecies.Add(newSpecies);
            foreach (var animat in animatLocations) _context.AnimatLocations.Add(animat);
            SaveChanges();
            return null;
        }

        #endregion

        #region Delete operations
        public void DeleteLocation(Location location) { }
        protected void DeleteLocation(Location location, bool saveChanges)
        {
            // todo: Handle the case where this location is used by one or more scenarios
                foreach (var dataSet in location.EnvironmentalDataSets)
                    _context.EnvironmentalDataSets.Remove(dataSet);
            _context.Locations.Remove(location);
            if (saveChanges) SaveChanges();
        }
        public void DeleteEnvironmentalDataSet(EnvironmentalDataSet dataSet) { DeleteEnvironmentalDataSet(dataSet, true); }
        protected void DeleteEnvironmentalDataSet(EnvironmentalDataSet dataSet, bool saveChanges)
        {
            // todo: Handle the case where this data set is used by one or more scenarios
            var fileName = Path.Combine(MasterDatabaseDirectory, dataSet.Location.StorageDirectory, dataSet.FileName);
            var filesToDelete = Directory.EnumerateFiles(Path.GetDirectoryName(fileName), Path.GetFileNameWithoutExtension(fileName) + ".*");
            foreach (var file in filesToDelete) File.Delete(file);
            _context.EnvironmentalDataSets.Remove(dataSet);
            if (saveChanges) SaveChanges();
        }
        #endregion

        #endregion
        #region Private helper methods, properties and fields

        private LocationContext _context;
        
        void Initialize()
        {
            if (string.IsNullOrEmpty(MasterDatabaseDirectory)) throw new ApplicationException("MasterDatabaseDirectory cannot be null or empty");
            if (!Directory.Exists(MasterDatabaseDirectory)) Directory.CreateDirectory(MasterDatabaseDirectory);
            Devart.Data.SQLite.Entity.Configuration.SQLiteEntityProviderConfig.Instance.Workarounds.IgnoreSchemaName = true;
            Devart.Data.SQLite.Entity.Configuration.SQLiteEntityProviderConfig.Instance.DmlOptions.BatchUpdates.Enabled = true;
            Devart.Data.SQLite.Entity.Configuration.SQLiteEntityProviderConfig.Instance.DmlOptions.BatchUpdates.BatchSize = 30;
            Devart.Data.SQLite.Entity.Configuration.SQLiteEntityProviderConfig.Instance.DmlOptions.BatchUpdates.AsynchronousBatch = true;
            var connectionStringBuilder = new SQLiteConnectionStringBuilder
            {
                FailIfMissing = false,
                DataSource = Path.Combine(MasterDatabaseDirectory, "esme.db"),
                BinaryGUID = true,
            };
            DbConnection connection = new SQLiteConnection(connectionStringBuilder.ToString());
            _context = new LocationContext(connection, true);
        }

        void SaveChanges()
        {
            lock (_context)
            {
                try
                {
                    _context.SaveChanges();
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

        void Log(Location location, string message) { LogBase(new LogEntry(location) { Location = location }, message); }
        void Log(EnvironmentalDataSet dataSet, string message) { LogBase(new LogEntry(dataSet) { EnvironmentalDataSet = dataSet }, message); }
        void Log(Scenario scenario, string message) { LogBase(new LogEntry(scenario) { Scenario = scenario }, message); }
        void Log(Platform platform, string message) { LogBase(new LogEntry(platform) { Platform = platform }, message); }
        void Log(Source source, string message) { LogBase(new LogEntry(source) { Source = source }, message); }
        void Log(Mode mode, string message) { LogBase(new LogEntry(mode) { Mode = mode }, message); }
        void Log(TrackDefinition trackDefinition, string message) { LogBase(new LogEntry(trackDefinition) { TrackDefinition = trackDefinition }, message); }
        void Log(Perimeter perimeter, string message) { LogBase(new LogEntry(perimeter) { Perimeter = perimeter }, message); }

        void LogBase(LogEntry logEntry, string message)
        {
            logEntry.Message = message;
            logEntry.MessageSource = new DbWhoWhenWhere(true);
            _context.Log.Add(logEntry);
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
                _context.Dispose();
            }

            // Note disposing has been done.
            _disposed = true;
        }
        ~MasterDatabaseService() { Dispose(false); }
        #endregion
    }
}
