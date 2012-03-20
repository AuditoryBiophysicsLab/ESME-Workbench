using System;
using System.IO;
using System.Linq;
using System.Threading;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.Locations;
using ESME.Plugins;
using ESME.Simulator;
using NUnit.Framework;

namespace ESME.Tests.Simulator
{
    public class SimulationTests
    {
        readonly string _databaseDirectory = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData), @"ESME.Simulation Tests\Database");
        readonly string _simulationDirectory = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData), @"ESME.Simulation Tests\Simulation Output");
        const string PluginDirectory = @"C:\Projects\ESME Deliverables\Libraries\ESME.Tests\bin\Debug";

        [Test, RequiresSTA]
        public void CreateNewSimulation()
        {
            Console.WriteLine("Creating database service...");
            var database = new MasterDatabaseService { MasterDatabaseDirectory = _databaseDirectory };
            Console.WriteLine("Loading plugins...");
            var plugins = new PluginManagerService { PluginDirectory = PluginDirectory };
            Console.WriteLine("Looking for test location 'Jacksonville'...");
            var location = database.FindLocation("Jacksonville");
            if (location == null)
            {
                Console.WriteLine("Creating test location 'Jacksonville'...");
                location = database.ImportLocationFromOverlayFile(@"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Sim Areas\Jacksonville\Areas\Jax_Ops_Area_300km.ovr", "Jacksonville");
                foreach (var month in NAVOConfiguration.AllMonths)
                {
                    // SoundSpeed dataset for each month
                    database.CreateEnvironmentalDataSet(location, 15, month, plugins[PluginType.EnvironmentalDataSource, PluginSubtype.SoundSpeed].PluginIdentifier);
                    // Wind dataset for each month
                    database.CreateEnvironmentalDataSet(location, 60, month, plugins[PluginType.EnvironmentalDataSource, PluginSubtype.Wind].PluginIdentifier);
                }
                // Sediment dataset
                database.CreateEnvironmentalDataSet(location, 5f, TimePeriod.Invalid, plugins[PluginType.EnvironmentalDataSource, PluginSubtype.Sediment].PluginIdentifier);
                // Bathymetry dataset at 2min resolution
                database.CreateEnvironmentalDataSet(location, 2f, TimePeriod.Invalid, plugins[PluginType.EnvironmentalDataSource, PluginSubtype.Bathymetry].PluginIdentifier);
                // Bathymetry dataset at 1min resolution
                database.CreateEnvironmentalDataSet(location, 1f, TimePeriod.Invalid, plugins[PluginType.EnvironmentalDataSource, PluginSubtype.Bathymetry].PluginIdentifier);
                // Bathymetry dataset at 0.5min resolution
                database.CreateEnvironmentalDataSet(location, 0.5f, TimePeriod.Invalid, plugins[PluginType.EnvironmentalDataSource, PluginSubtype.Bathymetry].PluginIdentifier);
            }
            Console.WriteLine("Looking for test scenario 'BU Test Sample'...");
            var scenario = database.FindScenario("BU Test Sample");
            if (scenario == null)
            {
                Console.WriteLine("Importing test scenario 'BU Test Sample'...");
                scenario = database.ImportScenarioFromNemoFile(location,
                                                               @"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Jacksonville\BU Test Sample.nemo",
                                                               @"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Sim Areas");
                if (scenario.Wind != null)
                {
                    database.SetEnvironmentalData(scenario, (from data in location.EnvironmentalDataSets
                                                             where data.SourcePlugin.PluginSubtype == PluginSubtype.Wind
                                                             select data).FirstOrDefault());
                    var sourceFile = Path.Combine(database.MasterDatabaseDirectory, location.StorageDirectory, scenario.Wind.FileName);
                    var destFile = Path.Combine(_databaseDirectory, scenario.Wind.FileName);
                    if (File.Exists(sourceFile)) File.Copy(sourceFile, destFile);
                }
                if (scenario.SoundSpeed != null)
                {
                    var sourceFile = Path.Combine(database.MasterDatabaseDirectory, location.StorageDirectory, scenario.SoundSpeed.FileName);
                    database.SetEnvironmentalData(scenario, (from data in location.EnvironmentalDataSets
                                                             where data.SourcePlugin.PluginSubtype == PluginSubtype.SoundSpeed
                                                             select data).FirstOrDefault());
                    var destFile = Path.Combine(_databaseDirectory, scenario.SoundSpeed.FileName);
                    if (File.Exists(sourceFile)) File.Copy(sourceFile, destFile);
                }
                if (scenario.Sediment != null)
                {
                    database.SetEnvironmentalData(scenario, (from data in location.EnvironmentalDataSets
                                                             where data.SourcePlugin.PluginSubtype == PluginSubtype.Sediment
                                                             select data).FirstOrDefault());
                    var sourceFile = Path.Combine(database.MasterDatabaseDirectory, location.StorageDirectory, scenario.Sediment.FileName);
                    var destFile = Path.Combine(_databaseDirectory, scenario.Sediment.FileName);
                    if (File.Exists(sourceFile)) File.Copy(sourceFile, destFile);
                }
                if (scenario.Bathymetry != null)
                {
                    database.SetEnvironmentalData(scenario, (from data in location.EnvironmentalDataSets
                                                             where data.SourcePlugin.PluginSubtype == PluginSubtype.Bathymetry
                                                             select data).FirstOrDefault());
                    var sourceFile = Path.Combine(database.MasterDatabaseDirectory, location.StorageDirectory, scenario.Bathymetry.FileName);
                    var destFile = Path.Combine(_databaseDirectory, scenario.Bathymetry.FileName);
                    if (File.Exists(sourceFile)) File.Copy(sourceFile, destFile);
                }

            }
            Console.WriteLine("Deleting existing simulation directory...");
            if (Directory.Exists(_simulationDirectory)) Directory.Delete(_simulationDirectory, true);
            for (var i = 0; i < 10; i++) if (Directory.Exists(_simulationDirectory)) Thread.Sleep(100); else break;
            Assert.IsFalse(Directory.Exists(_simulationDirectory));

            Console.WriteLine("Starting simulation...");
            var foo = Simulation.Create(scenario, _simulationDirectory);
            Console.WriteLine("Test complete");
        }
    }
}
