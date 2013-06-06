using System;
using System.IO;
using ESME.Plugins;
using InstallableNAVOPlugin;
using NUnit.Framework;

namespace ESME.Tests.Plugins
{
    public class PluginManagerTests
    {
        [Test, RequiresSTA]
        public void PluginManager()
        {
            const string debugDirectory = @"C:\Projects\ESME Deliverables\Libraries\ESME.Tests\bin\Debug";
            const string pluginDirectory = debugDirectory + @"\Plugins";
            if (Directory.Exists(pluginDirectory)) Directory.Delete(pluginDirectory, true);
            Assert.IsFalse(Directory.Exists(pluginDirectory));
            Directory.CreateDirectory(pluginDirectory);
            Assert.IsTrue(Directory.Exists(pluginDirectory));
            File.Copy(Path.Combine(debugDirectory, "InstallableNAVOPlugin.dll"), Path.Combine(pluginDirectory, "InstallableNAVOPlugin.dll"));
            File.Copy(Path.Combine(debugDirectory, "NAVODatabaseAdapter.dll"), Path.Combine(pluginDirectory, "NAVODatabaseAdapter.dll"));
            var pluginManager = new PluginManagerService();
            Assert.IsNull(pluginManager[PluginType.EnvironmentalDataSource]);
            pluginManager.PluginDirectory = pluginDirectory;
            Assert.IsNotNull(pluginManager[PluginType.EnvironmentalDataSource]);
            var pluginIdentifier = new PluginIdentifier
            {
                PluginType = PluginType.EnvironmentalDataSource,
                PluginSubtype = PluginSubtype.SoundSpeed,
                Type = typeof(GDEM3ForESME).ToString(),
            };
            Assert.IsTrue(pluginManager[pluginIdentifier] is GDEM3ForESME);
        }
    }
}
