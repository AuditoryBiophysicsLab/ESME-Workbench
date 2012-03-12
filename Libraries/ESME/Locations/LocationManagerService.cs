using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Data;
using System.Data.Common;
using System.Data.Entity;
using System.IO;
using System.Linq;
using Cinch;
using ESME.Database;
using ESME.Environment;
using HRC.Navigation;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Locations
{
    public interface ILocationManagerService
    {
        string LocationDirectory { get; set; }
        DbSet<Location> Locations { get; }
        bool LocationExists(string locationName);
        Location this[string locationName] { get; }
        Location CreateLocation(string locationName, string comments, double north, double south, double east, double west);
        void SaveChanges();
    }

    [PartCreationPolicy(CreationPolicy.Shared)]
    [ExportService(ServiceType.Both, typeof(ILocationManagerService))]
    public class LocationManagerService : ViewModelBase, ILocationManagerService
    {
        #region public string LocationRootDirectory { get; set; }

        public string LocationDirectory
        {
            get { return _locationDirectory; }
            set
            {
                if (_locationDirectory == value) return;
                _locationDirectory = value;
                if (!string.IsNullOrEmpty(_locationDirectory) && !Directory.Exists(_locationDirectory))
                {
                    if (_messageBoxService.ShowYesNo(string.Format("The Location Directory '{0}' does not exist.  Create it?", _locationDirectory), CustomDialogIcons.Question) == CustomDialogResults.No)
                        return;
                    Directory.CreateDirectory(_locationDirectory);
                }
                Initialize();
                NotifyPropertyChanged(LocationRootDirectoryChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs LocationRootDirectoryChangedEventArgs = ObservableHelper.CreateArgs<LocationManagerService>(x => x.LocationDirectory);
        string _locationDirectory;

        #endregion

        [Import] IMessageBoxService _messageBoxService;
        private LocationContext _context;
        void Initialize()
        {
            Devart.Data.SQLite.Entity.Configuration.SQLiteEntityProviderConfig.Instance.Workarounds.IgnoreSchemaName = true;
            DbConnection connection = new Devart.Data.SQLite.SQLiteConnection(string.Format("Data Source='{0}';FailIfMissing=False", Path.Combine(LocationDirectory, "locations.db")));
            _context = new LocationContext(connection, false, new CreateDatabaseIfNotExists<LocationContext>());
            Locations = _context.Locations;
        }

        public DbSet<Location> Locations { get; private set; }

        public bool LocationExists(string locationName) { return Locations.FirstOrDefault(l => l.Name == locationName) != null; }
        public Location this[string locationName] { get { return Locations.Single(l => l.Name == locationName); } }
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
            Directory.CreateDirectory(Path.Combine(LocationDirectory, result.StorageDirectory));
            return result;
        }

        public void SaveChanges() { lock(_context) _context.SaveChanges(); }

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
            location.LogEntries.Add(logEntry);
            _context.LocationLogEntries.Add(logEntry);
            SaveChanges();
        }

        public EnvironmentalDataSetCollection AddEnvironmentDataSetCollection(Location location, DbPluginIdentifier sourcePlugin)
        {
            var environmentalDataSetCollection = new EnvironmentalDataSetCollection
            {
                Location = location,
                SourcePlugin = sourcePlugin,
                CreationInfo = new DbWhoWhenWhere(true),
            };
            AddLocationLogEntry(location, string.Format("Added new data set collection. Source plugin: {0} ", sourcePlugin));
            location.EnvironmentalDataSetCollections.Add(environmentalDataSetCollection);
            _context.EnvironmentalDataSetCollections.Add(environmentalDataSetCollection);
            SaveChanges();
            return environmentalDataSetCollection;
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
            collection.LogEntries.Add(logEntry);
            _context.EnvironmentalDataSetCollectionLogEntries.Add(logEntry);
            SaveChanges();
        }

        public EnvironmentalDataSet AddEnvironmentDataSet(EnvironmentalDataSetCollection collection, float resolution, TimePeriod timePeriod)
        {
            var environmentalDataSet = new EnvironmentalDataSet
            {
                CreationInfo = new DbWhoWhenWhere(true),
                FileName = Path.GetRandomFileName(),
                Resolution = resolution,
                TimePeriod = timePeriod,
                EnvironmentalDataSetCollection = collection,
            };
            AddEnvironmentDataSetCollectionLogEntry(collection, string.Format("Added new data set. Resolution: {0}{1}", resolution, timePeriod != TimePeriod.Invalid ? string.Format("  TimePeriod: {0}", timePeriod) : ""));
            collection.EnvironmentalDataSets.Add(environmentalDataSet);
            _context.EnvironmentalDataSets.Add(environmentalDataSet);
            SaveChanges();
            return environmentalDataSet;
        }
    }
}
