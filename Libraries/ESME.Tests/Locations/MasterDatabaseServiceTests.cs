using System;
using System.Data;
using System.IO;
using System.Linq;
using System.Threading;
using ESME.Database.Importers;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.Locations;
using ESME.Plugins;
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
            Assert.AreEqual(0, database.Locations.Count());

            var plugins = new PluginManagerService { PluginDirectory = PluginDirectory };

            var location = database.CreateLocation("Mass Bay", "These are some comments", 44, 41, -69, -72);
            Assert.AreEqual(1, database.Locations.Count());
            Assert.AreEqual("Mass Bay", location.Name);
            Assert.Throws(typeof(DuplicateNameException), () => database.CreateLocation("Mass Bay", "These are some comments", 44, 41, -69, -72));
            Assert.AreEqual(1, database.Locations.Count());
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
            //NemoFile.Import(@"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Jacksonville\BU Test Sample.nemo", @"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Sim Areas", location, database);
            DumpLocationDatabase(database);
        }

        public void DumpLocationDatabase(MasterDatabaseService locationService, bool dumpLogs = false)
        {
            foreach (var location in locationService.Locations)
            {
                DumpLocation(location);
                if (dumpLogs) foreach (var logEntry in location.Logs) DumpLogEntry(logEntry);
                if (location.EnvironmentalDataSets != null)
                    foreach (var dataSet in location.EnvironmentalDataSets)
                        DumpEnvironmentalDataSet(dataSet);
            }
        }

        public void DumpLocation(Location location)
        {
            Console.WriteLine("       Location name: {0}", location.Name);
            //Console.WriteLine("  Location ID: {0}", location.LocationID);
            Console.WriteLine("            Comments: {0}", location.Comments);
            Console.WriteLine("               North: {0}", location.GeoRect.North);
            Console.WriteLine("               South: {0}", location.GeoRect.South);
            Console.WriteLine("                East: {0}", location.GeoRect.East);
            Console.WriteLine("                West: {0}", location.GeoRect.West);
            Console.WriteLine("           Directory: {0}", location.StorageDirectory);
            if (location.EnvironmentalDataSets == null)
                Console.WriteLine("           Data sets: (none)");
            else
                Console.WriteLine("           Data sets: {0}", location.EnvironmentalDataSets.Count);
        }

        public void DumpEnvironmentalDataSet(EnvironmentalDataSet dataSet)
        {
            Console.WriteLine("            Data set file: {0} ({1} bytes)", dataSet.FileName, dataSet.FileSize);
            Console.WriteLine("               Resolution: {0} ({1} samples)", dataSet.Resolution, dataSet.SampleCount);
            if (dataSet.TimePeriod != TimePeriod.Invalid)
                Console.WriteLine("              Time period: {0}", dataSet.TimePeriod);
        }

        public void DumpLogEntry(LogEntry logEntry)
        {
            Console.WriteLine("           Log message: {0}", logEntry.Message);
            Console.WriteLine("             Logged by: {0}", logEntry.MessageSource);
            Console.WriteLine("           Source Guid: {0}", logEntry.SourceGuid);
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
