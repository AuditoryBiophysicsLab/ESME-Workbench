using System.Data;
using System.IO;
using System.Linq;
using ESME.Locations;
using ESME.Plugins;
using NUnit.Framework;

namespace ESME.Tests.Locations
{
    public class LocationManagerTests
    {
        [Test]
        public void EmptyDatabase()
        {
            var locationRootDirectory = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData), "ESME.LocationService Tests");
            if (Directory.Exists(locationRootDirectory)) Directory.Delete(locationRootDirectory, true);
            Assert.IsFalse(Directory.Exists(locationRootDirectory));
            var locationService = new LocationManagerService { LocationRootDirectory = locationRootDirectory };
            Assert.IsTrue(Directory.Exists(locationRootDirectory));
            Assert.AreEqual(0, locationService.Locations.Count());
            locationService.AddLocation("Location1", "These are some comments", 45, 44, -81, -80);
            Assert.AreEqual(1, locationService.Locations.Count());
            var location = locationService.Locations.First();
            Assert.AreEqual("Location1", location.Name);
            Assert.AreEqual(1, location.LocationLogEntries.Count);
            Assert.AreEqual("Created", location.LocationLogEntries.First().LogEntry.Message);
            Assert.Throws(typeof(DuplicateNameException), () => locationService.AddLocation("Location1", "These are some comments", 45, 44, -81, -80));
            Assert.AreEqual(1, locationService.Locations.Count());
            const string pluginDirectory = @"C:\Projects\ESME Deliverables\Libraries\ESME.Tests\bin\Debug";
            var pluginManager = new PluginManagerService {PluginDirectory = pluginDirectory};
        }
    }
}
