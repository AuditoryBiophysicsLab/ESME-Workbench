#define Dave
using System;
using System.IO;
using System.Linq;
using System.Threading;
using ESME.Environment;
using ESME.Locations;
using ESME.Plugins;
using ESME.Tests.Common;
using ESME.TransmissionLoss;
using HRC.Navigation;
using NUnit.Framework;
using AnalysisPoint = ESME.Scenarios.AnalysisPoint;

namespace ESME.Tests.Scenarios
{
    public class AnalysisPointTests
    {
#if Dave
        const string OverlayFile = @"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Sim Areas\Jacksonville\Areas\Jax_Ops_Area.ovr";
        const string SimAreaDirectory = @"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Sim Areas";
        const string NemoFile = @"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Jacksonville\BU Test Sample.nemo";
#else
        const string OverlayFile = @"C:\Users\Graham Voysey\Documents\NAEMO\NAEMO demos\BU Test Sample2\Sim Areas\Jacksonville\Areas\Jax_Ops_Area.ovr";
        const string SimAreaDirectory = @"C:\Users\Graham Voysey\Documents\NAEMO\NAEMO demos\BU Test Sample2\Sim Areas";
        const string NemoFile = @"C:\Users\Graham Voysey\Documents\NAEMO\NAEMO demos\BU Test Sample2\Jacksonville\BU Test Sample.nemo";
#endif
        readonly string _databaseDirectory = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData), @"ESME.AnalysisPoint Tests\Database");
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
            MasterDatabaseService database;
            EnvironmentalCacheService cache;
            PluginManagerService plugins;
            var location = TestLocation.LoadOrCreate("Jacksonville", OverlayFile, _databaseDirectory, PluginDirectory, out database, out cache, out plugins);
            var scenario = TestScenario.LoadOrCreate(database, location, SimAreaDirectory, NemoFile);
            var center = new Geo((location.GeoRect.North + location.GeoRect.South) / 2, (location.GeoRect.East + location.GeoRect.West) / 2);
            database.Add(new AnalysisPoint { Geo = center, Scenario = scenario }, (Bathymetry)cache[scenario.Bathymetry], 16, 25000, true);
            var northEast = center.Offset(Geo.KilometersToRadians(25), Geo.DegreesToRadians(45));
            database.Add(new AnalysisPoint { Geo = northEast, Scenario = scenario }, (Bathymetry)cache[scenario.Bathymetry], 16, 25000, true);
            var southEast = center.Offset(Geo.KilometersToRadians(25), Geo.DegreesToRadians(135));
            database.Add(new AnalysisPoint { Geo = southEast, Scenario = scenario }, (Bathymetry)cache[scenario.Bathymetry], 16, 25000, true);
            var southWest = center.Offset(Geo.KilometersToRadians(25), Geo.DegreesToRadians(225));
            database.Add(new AnalysisPoint { Geo = southWest, Scenario = scenario }, (Bathymetry)cache[scenario.Bathymetry], 16, 25000, true);
            var northWest = center.Offset(Geo.KilometersToRadians(25), Geo.DegreesToRadians(315));
            database.Add(new AnalysisPoint { Geo = northWest, Scenario = scenario }, (Bathymetry)cache[scenario.Bathymetry], 16, 25000, true);
        }

        [Test, RequiresSTA]
        public void CalculateAnalysisPoint()
        {
            MasterDatabaseService database;
            EnvironmentalCacheService cache;
            PluginManagerService plugins;
            var location = TestLocation.LoadOrCreate("Jacksonville", OverlayFile, _databaseDirectory, PluginDirectory, out database, out cache, out plugins);
            var scenario = TestScenario.LoadOrCreate(database, location, SimAreaDirectory, NemoFile);
            var calculator = new TransmissionLossCalculatorService(database, plugins, cache, 10, 1);
            Console.WriteLine("Found {0} analysis points", scenario.AnalysisPoints.Count);
            foreach (var analysisPoint in scenario.AnalysisPoints)
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
                        calculator.TestAdd(radial, Path.Combine(database.MasterDatabaseDirectory, scenario.StorageDirectory));
                    }
                } 
            }
        }
    }
}
