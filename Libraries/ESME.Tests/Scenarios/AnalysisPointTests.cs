//#define Dave
using System;
using System.IO;
using System.Linq;
using System.Threading;
using ESME.Behaviors;
using ESME.Database;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.Locations;
using ESME.Plugins;
using ESME.Scenarios;
using ESME.Tests.Common;
using HRC.Navigation;
using NUnit.Framework;

namespace ESME.Tests.Scenarios
{
    public class AnalysisPointTests
    {
#if Dave
        const string OverlayFile = @"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Sim Areas\Jacksonville\Areas\Jax_Sim_Area.ovr";
        const string SimAreaDirectory = @"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Sim Areas";
        const string NemoFile = @"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Jacksonville\BU Test Sample.nemo";
#else
        const string OverlayFile = @"C:\Users\Graham Voysey\Documents\NAEMO\NAEMO demos\BU Test Sample2\Sim Areas\Jacksonville\Areas\Jax_Sim_Area.ovr";
        const string SimAreaDirectory = @"C:\Users\Graham Voysey\Documents\NAEMO\NAEMO demos\BU Test Sample2\Sim Areas";
        const string NemoFile = @"C:\Users\Graham Voysey\Documents\NAEMO\NAEMO demos\BU Test Sample2\Jacksonville\BU Test Sampleda.nemo";
#endif
        readonly string _databaseDirectory = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData), @"ESME Workbench\Database");
        const string PluginDirectory = @"C:\Projects\ESME Deliverables\Libraries\ESME.Tests\bin\Debug";
        [Test, RequiresSTA]
        public void CreateTestDatabase()
        {
            if (Directory.Exists(_databaseDirectory))
            {
                Console.WriteLine("Deleting database directory {0}", _databaseDirectory);
                foreach (var file in Directory.EnumerateFiles(_databaseDirectory, "*.*", SearchOption.AllDirectories))
                    File.Delete(file);
                Directory.Delete(_databaseDirectory, true);
                var retry = 10;
                while (Directory.Exists(_databaseDirectory) && retry > 0)
                {
                    Thread.Sleep(100);
                    retry--;
                }
            }
            IMasterDatabaseService database;
            EnvironmentalCacheService cache;
            PluginManagerService plugins;
            var location = TestLocation.LoadOrCreate("Jacksonville", OverlayFile, _databaseDirectory, PluginDirectory, out database, out cache, out plugins);
            var scenario = TestScenario.LoadOrCreate(database, location, SimAreaDirectory, NemoFile);
            var center = new Geo((location.GeoRect.North + location.GeoRect.South) / 2, (location.GeoRect.East + location.GeoRect.West) / 2);
        }

        [Test, RequiresSTA]
        public void CreateAnalysisPoints()
        {
            IMasterDatabaseService database;
            EnvironmentalCacheService cache;
            PluginManagerService plugins;
            var location = TestLocation.LoadOrCreate("Jacksonville", OverlayFile, _databaseDirectory, PluginDirectory, out database, out cache, out plugins);
            var scenario = TestScenario.LoadOrCreate(database, location, SimAreaDirectory, NemoFile);
            var center = new Geo((location.GeoRect.North + location.GeoRect.South) / 2, (location.GeoRect.East + location.GeoRect.West) / 2);
        }

        [RequiresSTA]
        [TestCase("Carolina Coast", "Carolina Coast Scenario", 36, 33, -75, -78)]
        [TestCase("Massachusetts Bay", "Mass Bay Scenario", 44, 40, -68, -72)]
        public void CreateScenario(string locationName, string scenarioName, double north, double south, double east, double west)
        {
            Console.WriteLine("Creating database service...");
            var database = new MasterDatabaseService { MasterDatabaseDirectory = _databaseDirectory };
            Console.WriteLine("Loading plugins...");
            var plugins = new PluginManagerService { PluginDirectory = PluginDirectory };
            var cache = new EnvironmentalCacheService(plugins, database);
            Console.WriteLine(string.Format("Looking for test location '{0}'...", locationName));
            var location = database.FindLocation(locationName);
            if (location != null)
            {
                Console.WriteLine(string.Format("Test location '{0}' already exists.  Deleting the existing location.", locationName));
                database.DeleteLocation(location, true);
            }
            Console.WriteLine(string.Format("Creating test location '{0}'...", locationName));
            var geoRect = new GeoRect(north, south, east, west);
            location = new Location
            {
                Name = locationName,
                Comments = null,
                GeoRect = geoRect
            };
            database.Add(location, true);

            foreach (var month in NAVOConfiguration.AllMonths)
            {
                // SoundSpeed dataset for each month
                Console.WriteLine(string.Format("Importing soundspeed for {0}", month));
                cache.ImportDatasetTest(database.LoadOrCreateEnvironmentalDataSet(location, 15, month, plugins[PluginType.EnvironmentalDataSource, PluginSubtype.SoundSpeed].PluginIdentifier));

                // Wind dataset for each month
                Console.WriteLine(string.Format("Importing wind for {0}", month));
                cache.ImportDatasetTest(database.LoadOrCreateEnvironmentalDataSet(location, 60, month, plugins[PluginType.EnvironmentalDataSource, PluginSubtype.Wind].PluginIdentifier));
            }
            
            // Sediment dataset
            Console.WriteLine("Importing 5min sediment");
            cache.ImportDatasetTest(database.LoadOrCreateEnvironmentalDataSet(location, 5f, TimePeriod.Invalid, plugins[PluginType.EnvironmentalDataSource, PluginSubtype.Sediment].PluginIdentifier));
            Console.WriteLine("Importing 0.1min sediment");
            cache.ImportDatasetTest(database.LoadOrCreateEnvironmentalDataSet(location, 0.1f, TimePeriod.Invalid, plugins[PluginType.EnvironmentalDataSource, PluginSubtype.Sediment].PluginIdentifier));

            // Bathymetry dataset at 2min resolution
            Console.WriteLine("Importing 2min bathymetry");
            cache.ImportDatasetTest(database.LoadOrCreateEnvironmentalDataSet(location, 2f, TimePeriod.Invalid, plugins[PluginType.EnvironmentalDataSource, PluginSubtype.Bathymetry].PluginIdentifier));
            // Bathymetry dataset at 1min resolution
            Console.WriteLine("Importing 1min bathymetry");
            cache.ImportDatasetTest(database.LoadOrCreateEnvironmentalDataSet(location, 1f, TimePeriod.Invalid, plugins[PluginType.EnvironmentalDataSource, PluginSubtype.Bathymetry].PluginIdentifier));
            // Bathymetry dataset at 0.5min resolution
            Console.WriteLine("Importing 0.5min bathymetry");
            cache.ImportDatasetTest(database.LoadOrCreateEnvironmentalDataSet(location, 0.5f, TimePeriod.Invalid, plugins[PluginType.EnvironmentalDataSource, PluginSubtype.Bathymetry].PluginIdentifier));
            Scenario scenario;
            database.Add(scenario = new Scenario
            {
                Location = location,
                Name = scenarioName,
                Comments = string.Format("Some comments for {0}", scenarioName),
                StartTime = new DbTimeSpan(new TimeSpan(0, 12, 0, 0)),
                Duration = new DbTimeSpan(new TimeSpan(0, 1, 0, 0)),
                TimePeriod = TimePeriod.April,
            });
            Platform platform;
            database.Add(platform = new Platform
            {
                Description = "Platform description",
                PlatformName = "PlatformName",
                PlatformType = "PlatformType",
                RepeatCount = 0,
                Scenario = scenario,
                Course = 45,
                Depth = 0,
                Geo = geoRect.Center,
                Speed = 0,
                IsRandom = false,
                TrackType = TrackType.Stationary,
                Perimeter = null,
            });
            Source source;
            database.Add(source = new Source
            {
                Platform = platform,
                SourceName = "SourceName",
                SourceType = "SourceType",
            });
            database.Add(new Mode
            {
                ActiveTime = 500,
                DepressionElevationAngle = 0,
                Depth = 5,
                HighFrequency = 3000,
                HorizontalBeamWidth = 360,
                LowFrequency = 3000,
                MaxPropagationRadius = 25000,
                ModeName = "ModeName",
                ModeType = "ModeType",
                //PSMModeGuid = 
                PulseInterval = new TimeSpan(0, 0, 0, 30),
                PulseLength = new TimeSpan(0, 0, 0, 0, 500),
                RelativeBeamAngle = 0,
                Source = source,
                SourceLevel = 200,
                VerticalBeamWidth = 180,
            });
            database.SetEnvironmentalData(scenario,
                                          (from data in location.EnvironmentalDataSets
                                           where data.SourcePlugin.PluginSubtype == PluginSubtype.Wind && ((TimePeriod)scenario.TimePeriod == (TimePeriod)data.TimePeriod)
                                           select data).FirstOrDefault());

            database.SetEnvironmentalData(scenario,
                                          (from data in location.EnvironmentalDataSets
                                           where data.SourcePlugin.PluginSubtype == PluginSubtype.SoundSpeed && ((TimePeriod)scenario.TimePeriod == (TimePeriod)data.TimePeriod)
                                           select data).FirstOrDefault());

            database.SetEnvironmentalData(scenario,
                                          (from data in location.EnvironmentalDataSets
                                           where data.SourcePlugin.PluginSubtype == PluginSubtype.Sediment
                                           orderby data.Resolution
                                           select data).FirstOrDefault());

            database.SetEnvironmentalData(scenario,
                                          (from data in location.EnvironmentalDataSets
                                           where data.SourcePlugin.PluginSubtype == PluginSubtype.Bathymetry
                                           orderby data.Resolution
                                           select data).FirstOrDefault());
            database.Context.SaveChanges();
        }
#if false
        [Test, RequiresSTA]
        public void DomainCollection_AddDomainObjectFromWorkerThread()
        {
            var dispatcher = Dispatcher.CurrentDispatcher;
            var frame = new DispatcherFrame();
            var workerTask = new Task<bool>(() =>
            {
                
            });
            Dispatcher.PushFrame(frame);
            Assert.IsTrue(raisedCollectionChanged, "CollectionChanged event not raised.");
        }

        public void Calculate(ICollection<AnalysisPoint> analysisPoints, MasterDatabaseService database, TransmissionLossCalculatorService calculator)
        {
            foreach (var analysisPoint in analysisPoints)
            {
                Console.WriteLine("  Analysis point at {0} contains {1} transmission loss jobs", (Geo)analysisPoint.Geo, analysisPoint.TransmissionLosses.Count);
                foreach (var transmissionLoss in analysisPoint.TransmissionLosses)
                {
                    Console.WriteLine("    Transmission loss job for platform {0}, source {1}, mode {2} has {3} radials", transmissionLoss.Mode.Source.Platform.PlatformName, transmissionLoss.Mode.Source.SourceName, transmissionLoss.Mode.ModeName, transmissionLoss.Radials.Count);
                    var radials = from r in transmissionLoss.Radials
                                  orderby r.Bearing
                                  select r;
                    foreach (var radial in radials)
                    {
                        Console.WriteLine("      Computing radial at bearing {0} and length {1}", radial.Bearing, radial.Length);
                        calculator.Add(radial);
                    }
                }
            }
        }
#endif
    }
}
