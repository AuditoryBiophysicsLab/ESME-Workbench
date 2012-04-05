using System;
using System.IO;
using System.Linq;
using System.Threading;
using ESME.Locations;
using ESME.Plugins;
using ESME.Scenarios;
using ESME.Simulator;
using ESME.Tests.Common;
using ESME.TransmissionLoss;
using HRC.Navigation;
using NUnit.Framework;
using AnalysisPoint = ESME.Scenarios.AnalysisPoint;

namespace ESME.Tests.Scenarios
{
    public class AnalysisPointTests
    {
        readonly string _databaseDirectory = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData), @"ESME.AnalysisPoint Tests\Database");
        readonly string _simulationDirectory = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData), @"ESME.AnalysisPoint Tests\Simulation Output");
        const string PluginDirectory = @"C:\Projects\ESME Deliverables\Libraries\ESME.Tests\bin\Debug";
        [Test, RequiresSTA]
        public void AnalysisPointTest()
        {
            MasterDatabaseService database;
            EnvironmentalCacheService cache;
            PluginManagerService plugins;
            var location = TestLocation.LoadOrCreate("Jacksonville", @"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Sim Areas\Jacksonville\Areas\Jax_Ops_Area.ovr", _databaseDirectory, PluginDirectory, out database, out cache, out plugins);
            var scenario = TestScenario.LoadOrCreate(database, location, @"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Sim Areas", @"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Jacksonville\BU Test Sample.nemo");
            var center = new Geo((location.GeoRect.North + location.GeoRect.South) / 2, (location.GeoRect.East + location.GeoRect.West) / 2);
            var analysisPoint = new AnalysisPoint
            {
                Geo = center,
                Scenario = scenario,
            };
            foreach (var mode in scenario.GetAllModes())
            {
                var transmissionLoss = new ESME.Scenarios.TransmissionLoss
                {
                    AnalysisPoint = analysisPoint,
                    IsReadyToCalculate = false,
                    Mode = mode,
                };
                const int radialCount = 32;
                const double radialLength = 100000;
                for (var radialIndex = 0; radialIndex < radialCount; radialIndex++)
                {
                    var radial = new Radial
                    {
                        TransmissionLoss = transmissionLoss,
                        CalculationCompleted = DateTime.MaxValue,
                        CalculationStarted = DateTime.MaxValue,
                        Bearing = (360.0 / radialCount) * radialIndex,
                        Length = radialLength,
                        IsCalculated = false,
                    };
                    database.Add(radial);
                    transmissionLoss.Radials.Add(radial);
                    database.Add(transmissionLoss);
                }
                analysisPoint.TransmissionLosses.Add(transmissionLoss);
            }
            database.Add(analysisPoint, true);
        }

        [Test, RequiresSTA]
        public void CalculationTest()
        {
            MasterDatabaseService database;
            EnvironmentalCacheService cache;
            PluginManagerService plugins;
            var location = TestLocation.LoadOrCreate("Jacksonville", @"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Sim Areas\Jacksonville\Areas\Jax_Ops_Area.ovr", _databaseDirectory, PluginDirectory, out database, out cache, out plugins);
            var scenario = TestScenario.LoadOrCreate(database, location, @"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Sim Areas", @"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Jacksonville\BU Test Sample.nemo");
            var calculator = new TransmissionLossCalculatorService(database, plugins, cache, 50, 25);
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
