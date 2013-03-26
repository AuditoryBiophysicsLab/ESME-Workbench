using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Data;
using System.Data.Common;
using System.Data.Entity;
using System.Data.Entity.Infrastructure;
using System.Data.Entity.Validation;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading;
using System.Windows;
using System.Windows.Data;
using Devart.Data.SQLite;
using Devart.Data.SQLite.Entity.Configuration;
using ESME.Database;
using ESME.Environment;
using ESME.Plugins;
using ESME.Scenarios;
using ESME.TransmissionLoss;
using HRC;
using HRC.Aspects;
using HRC.Services;
using HRC.Utility;
using MEFedMVVM.ViewModelLocator;   

namespace ESME.Locations
{
    public interface IMasterDatabaseService : IDisposable
    {
        string MasterDatabaseDirectory { get; set; }
        LocationContext Context { get; }
        void Refresh();
        void Add(Perimeter perimeter);
        void Add(PerimeterCoordinate coordinate, bool replaceExisting = false);
        void Add(ScenarioSpecies species);
        EnvironmentalDataSet LoadOrCreateEnvironmentalDataSet(Location location, float resolution, TimePeriod timePeriod, PluginIdentifier sourcePlugin);
        void SaveChanges();
    }

    [PartCreationPolicy(CreationPolicy.Shared)]
    [ExportService(ServiceType.Both, typeof(IMasterDatabaseService))]
    [NotifyPropertyChanged]
    public class MasterDatabaseService : PropertyChangedBase, IMasterDatabaseService
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

        public void Refresh()
        {
            Context.Platforms.Load();
            Context.Sources.Load();
            Context.Modes.Load();
            Context.Locations.Load();
            Context.Scenarios.Load();
            Context.LayerSettings.Load();
            Context.PerimeterCoordinates.Load();
            Context.Perimeters.Load();
            Context.Waypoints.Load();
            Context.ShipTracks.Load();
#if false
            Context.EnvironmentalDataSets.Load();
            Context.ScenarioSpecies.Load();
            Context.AnalysisPoints.Load();
            Context.TransmissionLosses.Load();
            Context.Radials.Load();
#endif
        }

        #region Add operations
        public void Add(Perimeter perimeter)
        {
            var existing = (from p in Context.Perimeters
                            where p.Name == perimeter.Name && p.Scenario.Guid == perimeter.Scenario.Guid
                            select p).FirstOrDefault();
            if (existing != null) throw new DuplicateNameException(String.Format("A perimeter named {0} already exists in scenario {1}, choose another name", perimeter.Name, perimeter.Scenario.Name));
            Log(perimeter, "Added new perimeter {0} to scenario {1} in location {2}", perimeter.Name, perimeter.Scenario.Name, perimeter.Scenario.Location.Name);
        }
        public void Add(PerimeterCoordinate coordinate, bool replaceExisting = false)
        {
            var existing = (from c in Context.PerimeterCoordinates.Local
                            where c.Perimeter == coordinate.Perimeter && c.Order == coordinate.Order
                            select c).FirstOrDefault();
            if (existing != null && !replaceExisting) throw new ArgumentException(string.Format("Perimeter {0} already has a point at index {1}.  Did you intend to replace it?", coordinate.Perimeter.Name, coordinate.Order), "coordinate");
            if (existing != null) Context.PerimeterCoordinates.Remove(existing);
            Context.PerimeterCoordinates.Add(coordinate);
        }
        public void Add(ScenarioSpecies species)
        {
            var existing = (from s in Context.ScenarioSpecies.Local
                            where s.LatinName == species.LatinName && s.Scenario == species.Scenario
                            select s).FirstOrDefault();
            if (existing != null) throw new DuplicateNameException(String.Format("A species named {0} already exists in scenario {1}, choose another name", species.LatinName, species.Scenario.Name));
            Log(species, "Added new species {0} to scenario {1} in location {2}", species.LatinName, species.Scenario.Name, species.Scenario.Location.Name);
        }
        #endregion

        #region Create operations for Locations
        public EnvironmentalDataSet LoadOrCreateEnvironmentalDataSet(Location location, float resolution, TimePeriod timePeriod, PluginIdentifier sourcePlugin)
        {
            var timePeriodAsByte = ((DbTimePeriod)timePeriod).TimePeriodAsByte;
            var existing = (from e in Context.EnvironmentalDataSets
                            where e.Location.Guid == location.Guid && 
                            e.SourcePlugin.Type == sourcePlugin.Type && 
                            e.TimePeriod.TimePeriodAsByte == timePeriodAsByte &&
                            e.Resolution == resolution
                            select e).FirstOrDefault();
            if (existing != null) return existing;

            var environmentalDataSet = new EnvironmentalDataSet
            {
                FileName = Path.GetFileNameWithoutExtension(Path.GetRandomFileName()) + "." + sourcePlugin.PluginSubtype.ToString().ToLower(),
                Resolution = resolution,
                TimePeriod = timePeriod,
                Location = location,
                SourcePlugin = sourcePlugin,
            };
            location.EnvironmentalDataSets.Add(environmentalDataSet);
            Context.EnvironmentalDataSets.Add(environmentalDataSet);
            Log(environmentalDataSet, "Added new data set to {0}. Data type: {1}, resolution: {2}{3}", location.Name, sourcePlugin.PluginSubtype, resolution, timePeriod != TimePeriod.Invalid ? String.Format("  TimePeriod: {0}", timePeriod) : "");
            return environmentalDataSet;
        }
        #endregion

        #endregion
        #region Private helper methods, properties and fields

        public LocationContext Context { get; private set; }

        [NotNull] 
        public static string RandomFilenameWithoutExension
        {
            get
            {
                var result = Path.GetFileNameWithoutExtension(Path.GetRandomFileName());
                if (result == null) throw new NullReferenceException("RandomFileName returned null!  Inconceivable!");
                return result;
            }
        }

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
            Refresh();
            OnPropertyChanged("Locations");
            OnPropertyChanged("Scenarios");
        }

        public void SaveChanges()
        {
            var retry = 20;
            while (retry > 0)
            {
                lock (Context)
                {
                    try
                    {
                        Context.SaveChanges();
                        return;
                    }
                    catch (DbEntityValidationException dbEntityValidationException)
                    {
                        Console.WriteLine("SaveChanges caught DbEntityValidationException");
                        foreach (var innerError in dbEntityValidationException.EntityValidationErrors.SelectMany(validationError => validationError.ValidationErrors)) Console.WriteLine("  {0}: {1}", innerError.PropertyName, innerError.ErrorMessage);
                        if (retry <= 0) throw;
                    }
                    catch (DbUpdateException dbUpdateException)
                    {
                        Console.WriteLine("SaveChanges caught DbUpdateException");
                        Console.WriteLine("  {0}", dbUpdateException.InnerException.Message);
                        if (dbUpdateException.InnerException.InnerException != null) Console.WriteLine("    {0}", dbUpdateException.InnerException.InnerException.Message);
                        if (retry <= 0) throw;
                    }
                    catch (Exception exception)
                    {
                        Console.WriteLine("SaveChanges caught Exception: {0}", exception.Message);
                        if (retry <= 0) throw;
                    }
                }
                Thread.Sleep(50);
                retry--;
            }
        }
#if true
        //void Log(Location location, string message, params object[] args) { LogBase(new LogEntry(location) { Location = location }, message, args); }
        void Log(EnvironmentalDataSet dataSet, string message, params object[] args) { LogBase(new LogEntry(dataSet) { Location = dataSet.Location, EnvironmentalDataSet = dataSet }, message, args); }
        //void Log(Scenario scenario, string message, params object[] args) { LogBase(new LogEntry(scenario) { Location = scenario.Location, Scenario = scenario }, message, args); }
        //void Log(Scenario scenario, EnvironmentalDataSet dataSet, string message, params object[] args) { LogBase(new LogEntry(dataSet) { Location = scenario.Location, EnvironmentalDataSet = dataSet, Scenario = scenario }, message, args); }
        //void Log(Platform platform, string message, params object[] args) { LogBase(new LogEntry(platform) { Location = platform.Scenario.Location, Scenario = platform.Scenario, Platform = platform }, message, args); }
        //void Log(Source source, string message, params object[] args) { LogBase(new LogEntry(source) { Location = source.Platform.Scenario.Location, Scenario = source.Platform.Scenario, Platform = source.Platform, Source = source }, message, args); }
        //void Log(Mode mode, string message, params object[] args) { LogBase(new LogEntry(mode) { Location = mode.Source.Platform.Scenario.Location, Scenario = mode.Source.Platform.Scenario, Platform = mode.Source.Platform, Source = mode.Source, Mode = mode }, message, args); }
        void Log(Perimeter perimeter, string message, params object[] args) { LogBase(new LogEntry(perimeter) { Location = perimeter.Scenario.Location, Scenario = perimeter.Scenario, Perimeter = perimeter }, message, args); }
        void Log(ScenarioSpecies species, string message, params object[] args) { LogBase(new LogEntry(species) { Location = species.Scenario.Location, Scenario = species.Scenario, ScenarioSpecies = species }, message, args); }
        //void Log(AnalysisPoint analysisPoint, string message, params object[] args) { LogBase(new LogEntry(analysisPoint) { Location = analysisPoint.Scenario.Location, Scenario = analysisPoint.Scenario, AnalysisPoint = analysisPoint }, message, args); }
        //void Log(Scenarios.TransmissionLoss transmissionLoss, string message, params object[] args) { LogBase(new LogEntry(transmissionLoss) { Location = transmissionLoss.AnalysisPoint.Scenario.Location, Scenario = transmissionLoss.AnalysisPoint.Scenario, TransmissionLoss = transmissionLoss }, message, args); }
        //void Log(Radial radial, string message, params object[] args) { LogBase(new LogEntry(radial) { Location = radial.TransmissionLoss.AnalysisPoint.Scenario.Location, Scenario = radial.TransmissionLoss.AnalysisPoint.Scenario, Radial = radial }, message, args); }
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
            try
            {
                Context.Log.Local.Add(logEntry);
            }
            catch (Exception e)
            {
                Debug.WriteLine(string.Format("{0}: Caught (and discarded) exception from LogBase: {1}", DateTime.Now, e.Message));
                Debug.WriteLine(string.Format("{0}:   Log message causing exception: {1}", DateTime.Now, logEntry.Message));
            }
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
