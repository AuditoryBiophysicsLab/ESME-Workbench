using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Data;
using System.Data.Common;
using System.Data.Entity;
using System.Data.Entity.Validation;
using System.IO;
using System.Linq;
using ESME.Database;
using ESME.Environment;
using ESME.Plugins;
using HRC.Aspects;
using HRC.Navigation;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Locations
{
    [PartCreationPolicy(CreationPolicy.Shared)]
    [ExportService(ServiceType.Both, typeof(LocationManagerService))]
    [NotifyPropertyChanged]
    public class LocationManagerService : IDisposable
    {
        #region Public methods and properties
        string _locationRootDirectory;
        public string LocationRootDirectory
        {
            get { return _locationRootDirectory; }
            set
            {
                _locationRootDirectory = value;
                Initialize();
            }
        }

        public IEnumerable<Location> Locations { get { return _context.Locations; } }
        public bool LocationExists(string locationName) { return Locations.FirstOrDefault(l => l.Name == locationName) != null; }
        public Location this[string locationName] { get { return Locations.Single(l => l.Name == locationName); } }

        #region Create operations
        public Location CreateLocation(string locationName, string comments, double north, double south, double east, double west)
        {
            if (LocationExists(locationName)) throw new DuplicateNameException(string.Format("A location named {0} already exists, choose another name", locationName));
            var result = new Location
                             {
                                 Name = locationName,
                                 Comments = comments,
                                 GeoRect = new GeoRect(north, south, east, west),
                                 StorageDirectory = Path.GetFileNameWithoutExtension(Path.GetRandomFileName()),
                                 CreationInfo = new DbWhoWhenWhere(true),
                             };
            _context.Locations.Add(result);
            SaveChanges();
            AddLocationLogEntry(result, "Created");
            Directory.CreateDirectory(Path.Combine(LocationRootDirectory, result.StorageDirectory));
            return result;
        }

        public EnvironmentalDataSetCollection CreateEnvironmentalDataSetCollection(Location location, PluginIdentifier sourcePlugin)
        {
            var environmentalDataSetCollection = new EnvironmentalDataSetCollection
            {
                Location = location,
                SourcePlugin = sourcePlugin,
                CreationInfo = new DbWhoWhenWhere(true),
            };
            AddLocationLogEntry(location, string.Format("Added new {0} data set collection. Source plugin: {1}", sourcePlugin.PluginSubtype, sourcePlugin.Type));
            _context.EnvironmentalDataSetCollections.Add(environmentalDataSetCollection);
            SaveChanges();
            return environmentalDataSetCollection;
        }

        public EnvironmentalDataSet CreateEnvironmentalDataSet(EnvironmentalDataSetCollection collection, float resolution, TimePeriod timePeriod)
        {
            var environmentalDataSet = new EnvironmentalDataSet
            {
                CreationInfo = new DbWhoWhenWhere(true),
                FileName = Path.GetFileNameWithoutExtension(Path.GetRandomFileName()) + "." + collection.SourcePlugin.PluginSubtype.ToString().ToLower(),
                Resolution = resolution,
                TimePeriod = timePeriod,
                EnvironmentalDataSetCollection = collection,
            };
            AddEnvironmentDataSetCollectionLogEntry(collection, string.Format("Added new data set. Resolution: {0}{1}", resolution, timePeriod != TimePeriod.Invalid ? string.Format("  TimePeriod: {0}", timePeriod) : ""));
            _context.EnvironmentalDataSets.Add(environmentalDataSet);
            SaveChanges();
            return environmentalDataSet;
        }
        #endregion

        #region Delete operations
        public void DeleteLocation(Location location) { }
        protected void DeleteLocation(Location location, bool saveChanges)
        {
            // todo: Handle the case where this location is used by one or more scenarios
            foreach (var collection in location.EnvironmentalDataSetCollections)
            {
                foreach (var dataSet in collection.EnvironmentalDataSets)
                    _context.EnvironmentalDataSets.Remove(dataSet);
                _context.EnvironmentalDataSetCollections.Remove(collection);
            }
            _context.Locations.Remove(location);
            if (saveChanges) _context.SaveChanges();
        }
        public void DeleteEnvironmentalDataSetCollection(EnvironmentalDataSetCollection collection) { DeleteEnvironmentalDataSetCollection(collection, true); }
        protected void DeleteEnvironmentalDataSetCollection(EnvironmentalDataSetCollection collection, bool saveChanges)
        {
            // todo: Handle the case where this collection is used by one or more scenarios
            foreach (var dataSet in collection.EnvironmentalDataSets)
                DeleteEnvironmentalDataSet(dataSet, false);
            if (saveChanges) _context.SaveChanges();
        }
        public void DeleteEnvironmentalDataSet(EnvironmentalDataSet dataSet) { DeleteEnvironmentalDataSet(dataSet, true); }
        protected void DeleteEnvironmentalDataSet(EnvironmentalDataSet dataSet, bool saveChanges)
        {
            // todo: Handle the case where this data set is used by one or more scenarios
            var fileName = Path.Combine(LocationRootDirectory, dataSet.EnvironmentalDataSetCollection.Location.StorageDirectory, dataSet.FileName);
            var filesToDelete = Directory.EnumerateFiles(Path.GetDirectoryName(fileName), Path.GetFileNameWithoutExtension(fileName) + ".*");
            foreach (var file in filesToDelete) File.Delete(file);
            _context.EnvironmentalDataSets.Remove(dataSet);
            if (saveChanges) _context.SaveChanges();
        }
        #endregion

        public void UpdateEnvironmentDataSetPercentCached(EnvironmentalDataSet dataSet, int percentCached)
        {
            if (percentCached < 0 || percentCached > 100) throw new ArgumentOutOfRangeException("percentCached", "Must be between 0 and 100, inclusive");
            if (dataSet.PercentCached >= percentCached) throw new ArgumentOutOfRangeException("percentCached", "Cannot reduce the PercentCached value with this method");
            dataSet.PercentCached = percentCached;
            SaveChanges();
        }

        #endregion
        #region Private helper methods, properties and fields

        private LocationContext _context;
        
        void Initialize()
        {
            if (string.IsNullOrEmpty(LocationRootDirectory)) throw new ApplicationException("LocationRootDirectory cannot be null or empty");
            if (!Directory.Exists(LocationRootDirectory)) Directory.CreateDirectory(LocationRootDirectory);
            Devart.Data.SQLite.Entity.Configuration.SQLiteEntityProviderConfig.Instance.Workarounds.IgnoreSchemaName = true;
            DbConnection connection = new Devart.Data.SQLite.SQLiteConnection(string.Format("Data Source='{0}';FailIfMissing=False", Path.Combine(LocationRootDirectory, "locations.db")));
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
                catch (DbEntityValidationException ex)
                {
                    Console.WriteLine("SaveChanges caught DbEntityValidationException");
                    foreach (var innerError in ex.EntityValidationErrors.SelectMany(validationError => validationError.ValidationErrors))
                        Console.WriteLine("  {0}: {1}", innerError.PropertyName, innerError.ErrorMessage);
                    throw;
                }
            }
        }

        void AddLocationLogEntry(Location location, string message)
        {
            var logEntry = new LocationLogEntry
            {
                Location = location,
                LogEntry = new LogEntry
                {
                    Message = message,
                    MessageSource = new DbWhoWhenWhere(true),
                },
            };
            _context.LocationLogEntries.Add(logEntry);
            SaveChanges();
        }

        void AddEnvironmentDataSetCollectionLogEntry(EnvironmentalDataSetCollection collection, string message)
        {
            var logEntry = new EnvironmentalDataSetCollectionLogEntry
            {
                EnvironmentalDataSetCollection = collection,
                LogEntry = new LogEntry
                {
                    Message = message,
                    MessageSource = new DbWhoWhenWhere(true),
                },
            };
            _context.EnvironmentalDataSetCollectionLogEntries.Add(logEntry);
            SaveChanges();
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
        ~LocationManagerService() { Dispose(false); }
        #endregion
    }
}
