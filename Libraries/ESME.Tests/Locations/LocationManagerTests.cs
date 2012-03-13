using System;
using System.Data;
using System.IO;
using System.Linq;
using System.Threading;
using System.Windows.Threading;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.Locations;
using ESME.Plugins;
using NUnit.Framework;

namespace ESME.Tests.Locations
{
    public class LocationManagerTests
    {
        readonly string _locationRootDirectory = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData), "ESME.LocationService Tests");
        const string PluginDirectory = @"C:\Projects\ESME Deliverables\Libraries\ESME.Tests\bin\Debug";

        [Test, RequiresSTA]
        public void DispatcherTest()
        {
            if (Directory.Exists(_locationRootDirectory)) Directory.Delete(_locationRootDirectory, true);
            for (var i = 0; i < 10; i++ ) if (Directory.Exists(_locationRootDirectory)) Thread.Sleep(100);
            Assert.IsFalse(Directory.Exists(_locationRootDirectory));
            var locationManager = new LocationManagerService { LocationRootDirectory = _locationRootDirectory };
            Assert.IsTrue(Directory.Exists(_locationRootDirectory));

            var dispatcher = Dispatcher.CurrentDispatcher;
            Assert.NotNull(dispatcher);
            dispatcher.BeginInvoke(new Action(() =>
            {
                var importManager = new EnvironmentalDatabaseImportService(new PluginManagerService {PluginDirectory = PluginDirectory},
                                                                           locationManager);
                dispatcher.BeginInvoke(new Action(() => EmptyAndFillDatabase(locationManager, importManager)), DispatcherPriority.Normal);
            }), DispatcherPriority.Background);
            dispatcher.ShutdownStarted += (s, e) => Console.WriteLine("Dispatcher shutdown started");
            dispatcher.ShutdownFinished += (s, e) => Console.WriteLine("Dispatcher shutdown finished");
            dispatcher.Hooks.DispatcherInactive += (s, e) =>
            {
                Console.WriteLine("Dispatcher is now inactive");
                dispatcher.BeginInvokeShutdown(DispatcherPriority.Background);
            };
            while (!dispatcher.HasShutdownFinished) Thread.Sleep(1000);
        }

        public void EmptyAndFillDatabase(LocationManagerService locationManager, EnvironmentalDatabaseImportService importManager)
        {
            Assert.AreEqual(0, locationManager.Locations.Count());
            locationManager.CreateLocation("Mass Bay", "These are some comments", 44, 41, -69, -72);
            Assert.AreEqual(1, locationManager.Locations.Count());
            var location = locationManager.Locations.First();
            Assert.AreEqual("Mass Bay", location.Name);
            Assert.AreEqual(1, location.LocationLogEntries.Count);
            Assert.AreEqual("Created", location.LocationLogEntries.First().LogEntry.Message);
            Assert.Throws(typeof(DuplicateNameException), () => locationManager.CreateLocation("Mass Bay", "These are some comments", 44, 41, -69, -72));
            Assert.AreEqual(1, locationManager.Locations.Count());
            Assert.NotNull(importManager);
            var soundSpeedCollection = locationManager.CreateEnvironmentalDataSetCollection(location, new PluginIdentifier
            {
                PluginType = PluginType.EnvironmentalDataSource,
                PluginSubtype = PluginSubtype.SoundSpeed,
                Type = typeof(InstallableNAVOPlugin.GDEM3ForESME).ToString(),
            });
            foreach (var month in NAVOConfiguration.AllMonths) importManager.BeginImport(locationManager.CreateEnvironmentalDataSet(soundSpeedCollection, 15, month), new TestProgress { DataType = "SoundSpeed", TimePeriod = month, Resolution = 15 });
            var sedimentCollection = locationManager.CreateEnvironmentalDataSetCollection(location, new PluginIdentifier
            {
                PluginType = PluginType.EnvironmentalDataSource,
                PluginSubtype = PluginSubtype.Sediment,
                Type = typeof(InstallableNAVOPlugin.BST20ForESME).ToString(),
            });
            importManager.BeginImport(locationManager.CreateEnvironmentalDataSet(sedimentCollection, 5f, TimePeriod.Invalid), new TestProgress { DataType = "Sediment", Resolution = 5 });
            var bathymetryCollection = locationManager.CreateEnvironmentalDataSetCollection(location, new PluginIdentifier
            {
                PluginType = PluginType.EnvironmentalDataSource,
                PluginSubtype = PluginSubtype.Bathymetry,
                Type = typeof(InstallableNAVOPlugin.DBDB54ForESME).ToString(),
            });
            importManager.BeginImport(locationManager.CreateEnvironmentalDataSet(bathymetryCollection, 2f, TimePeriod.Invalid), new TestProgress { DataType = "Bathymetry", Resolution = 2 });
            importManager.BeginImport(locationManager.CreateEnvironmentalDataSet(bathymetryCollection, 1f, TimePeriod.Invalid), new TestProgress { DataType = "Bathymetry", Resolution = 1 });
            importManager.BeginImport(locationManager.CreateEnvironmentalDataSet(bathymetryCollection, 0.5f, TimePeriod.Invalid), new TestProgress { DataType = "Bathymetry", Resolution = 0.5f });
            var windCollection = locationManager.CreateEnvironmentalDataSetCollection(location, new PluginIdentifier
            {
                PluginType = PluginType.EnvironmentalDataSource,
                PluginSubtype = PluginSubtype.Wind,
                Type = typeof(InstallableNAVOPlugin.SMGC20ForESME).ToString(),
            });
            foreach (var month in NAVOConfiguration.AllMonths)
                importManager.BeginImport(locationManager.CreateEnvironmentalDataSet(windCollection, 60, month), new TestProgress { DataType = "Wind", TimePeriod = month, Resolution = 60 });
            while (importManager.BusyCount > 0)
            {
                Console.WriteLine("Waiting for import manager.  Busy count = {0}", importManager.BusyCount);
                Thread.Sleep(1000);
            }
            DumpLocationDatabase(locationManager);
        }

        [Test]
        public void DumpDatabase()
        {
            using(var locationManager = new LocationManagerService { LocationRootDirectory = _locationRootDirectory })
                DumpLocationDatabase(locationManager);
        }

        public void DumpLocationDatabase(LocationManagerService locationService, bool dumpLogs = false)
        {
            foreach (var location in locationService.Locations)
            {
                DumpLocation(location);
                if (dumpLogs) foreach (var locationLogEntry in location.LocationLogEntries) DumpLocationLogEntry(locationLogEntry);
                foreach (var collection in location.EnvironmentalDataSetCollections)
                {
                    Console.WriteLine();
                    DumpEnvironmentalDataSetCollection(collection);
                    if (collection.EnvironmentalDataSets != null)
                        foreach (var dataSet in collection.EnvironmentalDataSets)
                            DumpEnvironmentalDataSet(dataSet);
                }
            }
        }

        public void DumpLocation(Location location)
        {
            Console.WriteLine("       Location name: {0}", location.Name);
            //Console.WriteLine("  Location ID: {0}", location.LocationID);
            Console.WriteLine("            Comments: {0}", location.Comments);
            Console.WriteLine("          Created by: {0}", location.CreationInfo);
            Console.WriteLine("               North: {0}", location.GeoRect.North);
            Console.WriteLine("               South: {0}", location.GeoRect.South);
            Console.WriteLine("                East: {0}", location.GeoRect.East);
            Console.WriteLine("                West: {0}", location.GeoRect.West);
            Console.WriteLine("           Directory: {0}", location.StorageDirectory);
            if (location.EnvironmentalDataSetCollections == null)
                Console.WriteLine("Data set collections: (none)");
            else 
                Console.WriteLine("Data set collections: {0}", location.EnvironmentalDataSetCollections.Count);
        }

        public void DumpEnvironmentalDataSetCollection(EnvironmentalDataSetCollection collection)
        {
            Console.WriteLine("    Data set collection: {0} from {1}", collection.SourcePlugin.PluginSubtype, collection.SourcePlugin.Type);
            Console.WriteLine("             Created by: {0}", collection.CreationInfo);
            if (collection.EnvironmentalDataSets == null)
                Console.WriteLine("              Data sets: (none)");
            else
                Console.WriteLine("              Data sets: {0}", collection.EnvironmentalDataSets.Count);
        }

        public void DumpEnvironmentalDataSet(EnvironmentalDataSet dataSet)
        {
            Console.WriteLine("            Data set file: {0} ({1} bytes)", dataSet.FileName, dataSet.FileSize);
            Console.WriteLine("               Resolution: {0} ({1} samples)", dataSet.Resolution, dataSet.SampleCount);
            if (dataSet.TimePeriod != TimePeriod.Invalid)
                Console.WriteLine("              Time period: {0}", (TimePeriod)dataSet.TimePeriod);
            Console.WriteLine("               Created by: {0}", dataSet.CreationInfo);
            Console.WriteLine("                   Cached: {0}%", dataSet.PercentCached);
        }

        public void DumpLocationLogEntry(LocationLogEntry logEntry)
        {
            Console.WriteLine("           Log message: {0}", logEntry.LogEntry.Message);
            //Console.WriteLine("     Message ID: {0}", logEntry.LocationLogEntryID);
            Console.WriteLine("             Logged by: {0}", logEntry.LogEntry.MessageSource);
            if (logEntry.LogEntry.OldSourceID.HasValue)
                Console.WriteLine("         Old source ID: {0}", logEntry.LogEntry.OldSourceID);
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
