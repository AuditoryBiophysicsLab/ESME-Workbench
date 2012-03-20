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
        readonly string _masterDatabaseDirectory = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData), "ESME.Simulation Tests");
        const string PluginDirectory = @"C:\Projects\ESME Deliverables\Libraries\ESME.Tests\bin\Debug";

        [Test, RequiresSTA]
        public void CreateNewSimulation()
        {
            if (Directory.Exists(_masterDatabaseDirectory)) Directory.Delete(_masterDatabaseDirectory, true);
            for (var i = 0; i < 10; i++) if (Directory.Exists(_masterDatabaseDirectory)) Thread.Sleep(100); else break;
            Assert.IsFalse(Directory.Exists(_masterDatabaseDirectory));
            
            var database = new MasterDatabaseService { MasterDatabaseDirectory = _masterDatabaseDirectory };
            Assert.IsTrue(Directory.Exists(_masterDatabaseDirectory));
            Assert.AreEqual(0, database.Locations.Count());

            var plugins = new PluginManagerService { PluginDirectory = PluginDirectory };
            
            var location = database.FindLocation("Jacksonville");
            if (location == null)
            {
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
            var scenario = database.FindScenario("BU Test Sample") ?? database.ImportScenarioFromNemoFile(location,
                                                                                                          @"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Jacksonville\BU Test Sample.nemo",
                                                                                                          @"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Sim Areas");
            var foo = Simulation.Run(scenario, @"S:\SimulationTests");
        }
    }
}
