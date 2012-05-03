using System;
using System.IO;
using System.Linq;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.Locations;
using ESME.Plugins;
using ESME.Scenarios;

namespace ESME.Tests.Common
{
    public static class TestLocation
    {
        public static Location LoadOrCreate(string locationName, string overlayFile, string databaseDirectory, string pluginDirectory, out IMasterDatabaseService databaseService, out EnvironmentalCacheService cacheService, out PluginManagerService pluginService)
        {
            Console.WriteLine("Creating database service...");
            databaseService = new MasterDatabaseService {MasterDatabaseDirectory = databaseDirectory};
            Console.WriteLine("Loading plugins...");
            pluginService = new PluginManagerService {PluginDirectory = pluginDirectory};
            cacheService = new EnvironmentalCacheService(pluginService, databaseService);
            Console.WriteLine(string.Format("Looking for test location '{0}'...", locationName));
            var location = databaseService.FindLocation(locationName);
            if (location != null)
            {
                Console.WriteLine(string.Format("Test location '{0}' already exists.  Deleting the existing location.", locationName));
                databaseService.DeleteLocation(location, true);
            }
            Console.WriteLine(string.Format("Creating test location '{0}'...", locationName));
            location = databaseService.ImportLocationFromOverlayFile(overlayFile, locationName);
            foreach (var month in NAVOConfiguration.AllMonths)
            {
                // SoundSpeed dataset for each month
                Console.WriteLine(string.Format("Importing soundspeed for {0}", month));
                cacheService.ImportDatasetTest(databaseService.CreateEnvironmentalDataSet(location, 15, month, pluginService[PluginType.EnvironmentalDataSource, PluginSubtype.SoundSpeed].PluginIdentifier));

                // Wind dataset for each month
                Console.WriteLine(string.Format("Importing wind for {0}", month));
                cacheService.ImportDatasetTest(databaseService.CreateEnvironmentalDataSet(location, 60, month, pluginService[PluginType.EnvironmentalDataSource, PluginSubtype.Wind].PluginIdentifier));
            }
            // Sediment dataset
            Console.WriteLine("Importing sediment");
            cacheService.ImportDatasetTest(databaseService.CreateEnvironmentalDataSet(location, 5f, TimePeriod.Invalid, pluginService[PluginType.EnvironmentalDataSource, PluginSubtype.Sediment].PluginIdentifier));

            // Bathymetry dataset at 2min resolution
            Console.WriteLine("Importing 2min bathymetry");
            cacheService.ImportDatasetTest(databaseService.CreateEnvironmentalDataSet(location, 2f, TimePeriod.Invalid, pluginService[PluginType.EnvironmentalDataSource, PluginSubtype.Bathymetry].PluginIdentifier));
            // Bathymetry dataset at 1min resolution
            Console.WriteLine("Importing 1min bathymetry");
            cacheService.ImportDatasetTest(databaseService.CreateEnvironmentalDataSet(location, 1f, TimePeriod.Invalid, pluginService[PluginType.EnvironmentalDataSource, PluginSubtype.Bathymetry].PluginIdentifier));
            // Bathymetry dataset at 0.5min resolution
            Console.WriteLine("Importing 0.5min bathymetry");
            cacheService.ImportDatasetTest(databaseService.CreateEnvironmentalDataSet(location, 0.5f, TimePeriod.Invalid, pluginService[PluginType.EnvironmentalDataSource, PluginSubtype.Bathymetry].PluginIdentifier));
            return location;
        }

        public static void Dump(IMasterDatabaseService locationService, bool dumpLogs = false)
        {
            foreach (var location in locationService.Context.Locations)
            {
                DumpLocation(location);
                if (dumpLogs) foreach (var logEntry in location.Logs) DumpLogEntry(logEntry);
                if (location.EnvironmentalDataSets != null)
                    foreach (var dataSet in location.EnvironmentalDataSets)
                        DumpEnvironmentalDataSet(dataSet);
            }
        }

        public static void DumpLocation(Location location)
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

        public static void DumpEnvironmentalDataSet(EnvironmentalDataSet dataSet)
        {
            Console.WriteLine("            Data set file: {0} ({1} bytes)", dataSet.FileName, dataSet.FileSize);
            Console.WriteLine("               Resolution: {0} ({1} samples)", dataSet.Resolution, dataSet.SampleCount);
            if (dataSet.TimePeriod != TimePeriod.Invalid)
                Console.WriteLine("              Time period: {0}", dataSet.TimePeriod);
        }

        public static void DumpLogEntry(LogEntry logEntry)
        {
            Console.WriteLine("           Log message: {0}", logEntry.Message);
            Console.WriteLine("             Logged by: {0}", logEntry.MessageSource);
            Console.WriteLine("           Source Guid: {0}", logEntry.SourceGuid);
        }
    }

    public static class TestScenario
    {
        public static Scenario LoadOrCreate(IMasterDatabaseService databaseService, Location location, string simAreaFolder, string scenarioFile)
        {
            var scenarioName = Path.GetFileNameWithoutExtension(scenarioFile);
            Console.WriteLine(string.Format("Looking for test scenario '{0}'...", scenarioName));
            var scenario = databaseService.FindScenario(scenarioName);
            if (scenario != null) return scenario;
            Console.WriteLine(string.Format("Importing test scenario '{0}'...", scenarioName));
            scenario = Scenario.FromNemoFile(databaseService, location, scenarioFile, simAreaFolder);

            databaseService.SetEnvironmentalData(scenario, (from data in location.EnvironmentalDataSets
                                                            where data.SourcePlugin.PluginSubtype == PluginSubtype.Wind && ((TimePeriod)scenario.TimePeriod == (TimePeriod)data.TimePeriod)
                                                            select data).FirstOrDefault());

            databaseService.SetEnvironmentalData(scenario, (from data in location.EnvironmentalDataSets
                                                            where data.SourcePlugin.PluginSubtype == PluginSubtype.SoundSpeed && ((TimePeriod)scenario.TimePeriod == (TimePeriod)data.TimePeriod)
                                                            select data).FirstOrDefault());

            databaseService.SetEnvironmentalData(scenario, (from data in location.EnvironmentalDataSets
                                                            where data.SourcePlugin.PluginSubtype == PluginSubtype.Sediment
                                                            select data).FirstOrDefault());

            databaseService.SetEnvironmentalData(scenario, (from data in location.EnvironmentalDataSets
                                                            where data.SourcePlugin.PluginSubtype == PluginSubtype.Bathymetry
                                                            select data).FirstOrDefault());
            return scenario;
        }
    }
}