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
using Devart.Data.SQLite.Entity.Configuration;
using ESME.Database;
using ESME.Environment;
using ESME.Database.Importers;
using ESME.NEMO.Overlay;
using ESME.Plugins;
using ESME.Scenarios;
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

        public IEnumerable<Location> Locations { get { return Context.Locations; } }
        public Location FindLocation(string locationName) { return Locations.FirstOrDefault(l => l.Name == locationName); }
        public bool LocationExists(string locationName) { return FindLocation(locationName) != null; }

        public IEnumerable<Scenario> Scenarios { get { return Context.Scenarios; } }
        public Scenario FindScenario(string scenarioName) { return Scenarios.FirstOrDefault(l => l.Name == scenarioName); }
        public bool ScenarioExists(string scenarioName) { return FindScenario(scenarioName) != null; }

        #region Create operations for Locations
        public Location CreateLocation(string locationName, string comments, double north, double south, double east, double west)
        {
            if (LocationExists(locationName)) throw new DuplicateNameException(String.Format("A location named {0} already exists, choose another name", locationName));
            var result = new Location
                             {
                                 Name = locationName,
                                 Comments = comments,
                                 GeoRect = new GeoRect(north, south, east, west),
                                 StorageDirectory = Path.GetFileNameWithoutExtension(Path.GetRandomFileName()),
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
            return CreateLocation(locationName,
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
            Context.EnvironmentalDataSets.Add(environmentalDataSet);
            Log(environmentalDataSet, String.Format("Added new data set. Resolution: {0}{1}", resolution, timePeriod != TimePeriod.Invalid ? String.Format("  TimePeriod: {0}", timePeriod) : ""));
            SaveChanges();
            return environmentalDataSet;
        }
        #endregion

        #region Create operations for Scenarios
        public Scenario CreateScenario(string scenarioName, string comments, TimeSpan startTime, TimeSpan duration, TimePeriod timePeriod, Location location)
        {
            if (ScenarioExists(scenarioName)) throw new DuplicateNameException(String.Format("A scenario named {0} already exists, choose another name", scenarioName));
            var result = new Scenario
            {
                Name = scenarioName,
                Comments = comments,
                StartTime = startTime,
                Duration = duration,
                TimePeriod = timePeriod,
                Location = location,
            };
            Context.Scenarios.Add(result);
            Log(result, "Created");
            SaveChanges();
            return result;
        }

        public void SetEnvironmentalData(Scenario scenario, EnvironmentalDataSet data)
        {
            switch (data.SourcePlugin.PluginSubtype)
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

        internal LocationContext Context { get; private set; }

        void Initialize()
        {
            if (String.IsNullOrEmpty(MasterDatabaseDirectory)) throw new ApplicationException("MasterDatabaseDirectory cannot be null or empty");
            if (!Directory.Exists(MasterDatabaseDirectory)) Directory.CreateDirectory(MasterDatabaseDirectory);
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

        internal void Log(Location location, string message) { LogBase(new LogEntry(location) { Location = location }, message); }
        internal void Log(EnvironmentalDataSet dataSet, string message) { LogBase(new LogEntry(dataSet) { EnvironmentalDataSet = dataSet }, message); }
        internal void Log(Scenario scenario, string message) { LogBase(new LogEntry(scenario) { Scenario = scenario }, message); }
        internal void Log(Platform platform, string message) { LogBase(new LogEntry(platform) { Platform = platform }, message); }
        internal void Log(Source source, string message) { LogBase(new LogEntry(source) { Source = source }, message); }
        internal void Log(Mode mode, string message) { LogBase(new LogEntry(mode) { Mode = mode }, message); }
        internal void Log(TrackDefinition trackDefinition, string message) { LogBase(new LogEntry(trackDefinition) { TrackDefinition = trackDefinition }, message); }
        internal void Log(Perimeter perimeter, string message) { LogBase(new LogEntry(perimeter) { Perimeter = perimeter }, message); }

        void LogBase(LogEntry logEntry, string message)
        {
            logEntry.Message = message;
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
