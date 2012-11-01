using System;
using System.Collections.Generic;
using System.IO;
using ESME.Settings;
using HRC;
using HRC.Aspects;
using HRC.Navigation;
using HRC.Utility;
using NUnit.Framework;

namespace ESME.Tests.Settings
{
    public class SettingsServiceTests
    {
        [Test]
        public void CreateNewSettings()
        {
            var settingsService = new SettingsService { SettingsRootDirectory = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData), "ESME.SettingsService Tests") };
            var testSettings = new ServiceTestSettings();
            settingsService[typeof(ServiceTestSettings), "ServiceTestSettings.xml"] = testSettings;
        }

        [Test]
        public void ReadExistingSettings()
        {
            var settingsService = new SettingsService { SettingsRootDirectory = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData), "ESME.SettingsService Tests") };
            var testSettings = (ServiceTestSettings)settingsService[typeof(ServiceTestSettings), "ServiceTestSettings.xml"];
            Assert.AreEqual("ServiceTestSettings", testSettings.StringProperty);
        }
    }

    [Serializable]
    public class ServiceTestSettings : SettingsBase
    {
        [ReferencedTypes, UsedImplicitly]
        static readonly List<Type> ReferencedTypes = new List<Type> { typeof(Geo) };

        [Initialize("ServiceTestSettings")]
        public string StringProperty { get; set; }

        [Initialize]
        public Geo Geo { get; set; }

        public int UninitializedInt { get; set; }
        
        [Initialize(42)]
        public int InitializedInt { get; set; }

        public override void Save(string fileName = null) { SaveBase(this, fileName); }
    }
}
