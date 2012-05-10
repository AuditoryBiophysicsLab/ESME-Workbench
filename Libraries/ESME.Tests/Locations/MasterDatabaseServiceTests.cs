using System;
using System.Data;
using System.IO;
using System.Linq;
using System.Threading;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.Locations;
using ESME.Plugins;
using ESME.Tests.Common;
using HRC.Navigation;
using NUnit.Framework;

namespace ESME.Tests.Locations
{
    public class MasterDatabaseServiceTests
    {
        readonly string _masterDatabaseDirectory = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData), "ESME.MasterDatabaseService Tests");
        const string PluginDirectory = @"C:\Projects\ESME Deliverables\Libraries\ESME.Tests\bin\Debug";

        [Test, RequiresSTA]
        public void EmptyAndFillDatabase()
        {
            if (Directory.Exists(_masterDatabaseDirectory)) Directory.Delete(_masterDatabaseDirectory, true);
            for (var i = 0; i < 10; i++) if (Directory.Exists(_masterDatabaseDirectory)) Thread.Sleep(100); else break;
            Assert.IsFalse(Directory.Exists(_masterDatabaseDirectory));
            var database = new MasterDatabaseService { MasterDatabaseDirectory = _masterDatabaseDirectory };
            Assert.IsTrue(Directory.Exists(_masterDatabaseDirectory));
            Assert.AreEqual(0, database.Context.Locations.Local.Count());

            var plugins = new PluginManagerService { PluginDirectory = PluginDirectory };

            var location = new Location
            {
                Name = "Mass Bay",
                Comments = "These are some comments",
                GeoRect = new GeoRect(44, 41, -69, -72),
            };
            database.Add(location);

            Assert.AreEqual(1, database.Context.Locations.Local.Count());
            Assert.AreEqual("Mass Bay", location.Name);
            Assert.Throws(typeof(DuplicateNameException), () => database.Add(new Location
            {
                Name = "Mass Bay",
                Comments = "These are some comments",
                GeoRect = new GeoRect(44, 41, -69, -72),
            }));
            Assert.AreEqual(1, database.Context.Locations.Local.Count());
            foreach (var month in NAVOConfiguration.AllMonths)
            {
                // SoundSpeed dataset for each month
                database.LoadOrCreateEnvironmentalDataSet(location, 15, month, plugins[PluginType.EnvironmentalDataSource, PluginSubtype.SoundSpeed].PluginIdentifier);
                // Wind dataset for each month
                database.LoadOrCreateEnvironmentalDataSet(location, 60, month, plugins[PluginType.EnvironmentalDataSource, PluginSubtype.Wind].PluginIdentifier);
            }
            // Sediment dataset
            database.LoadOrCreateEnvironmentalDataSet(location, 5f, TimePeriod.Invalid, plugins[PluginType.EnvironmentalDataSource, PluginSubtype.Sediment].PluginIdentifier);
            // Bathymetry dataset at 2min resolution
            database.LoadOrCreateEnvironmentalDataSet(location, 2f, TimePeriod.Invalid, plugins[PluginType.EnvironmentalDataSource, PluginSubtype.Bathymetry].PluginIdentifier);
            // Bathymetry dataset at 1min resolution
            database.LoadOrCreateEnvironmentalDataSet(location, 1f, TimePeriod.Invalid, plugins[PluginType.EnvironmentalDataSource, PluginSubtype.Bathymetry].PluginIdentifier);
            // Bathymetry dataset at 0.5min resolution
            database.LoadOrCreateEnvironmentalDataSet(location, 0.5f, TimePeriod.Invalid, plugins[PluginType.EnvironmentalDataSource, PluginSubtype.Bathymetry].PluginIdentifier);
            //NemoFile.Import(@"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Jacksonville\BU Test Sample.nemo", @"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Sim Areas", location, database);
            TestLocation.Dump(database);
        }
    }

    public class TestProgress : IProgress<float>
    {
        public TestProgress() { TimePeriod = TimePeriod.Invalid; }
        public string DataType { get; set; }
        public TimePeriod TimePeriod { get; set; }
        public float Resolution { get; set; }
        public float PercentComplete { get; private set; }

        public void Report(float value) 
        { 
            Console.WriteLine("{0,10}{1,12}[{2,3} min]: {3,3}%", DataType, TimePeriod == TimePeriod.Invalid ? "" : string.Format("({0})", TimePeriod), Resolution, value);
            PercentComplete = value;
        }
    }
}
