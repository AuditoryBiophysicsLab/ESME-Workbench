using System;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Data;
using System.Data.Common;
using System.Data.Entity;
using System.IO;
using System.Linq;
using Cinch;
using ESME.Database;
using HRC.Navigation;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Locations
{
    public interface ILocationManagerService
    {
        string LocationDirectory { get; set; }
        DbSet<Location> Locations { get; }
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
            DbConnection connection = new Devart.Data.SQLite.SQLiteConnection(string.Format("Data Source='{0}';FailIfMissing=False", "locations.db"));
            _context = new LocationContext(connection, false, new DropCreateDatabaseAlways<LocationContext>());
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
                                 Creator = new DbWhoWhenWhere
                                               {
                                                   When = DateTime.Now,
                                                   Where = System.Environment.MachineName,
                                                   Who = System.Environment.UserName,
                                               },
                             };
            Locations.Add(result);
            _context.SaveChanges();
            return result;
        }
    }

}
