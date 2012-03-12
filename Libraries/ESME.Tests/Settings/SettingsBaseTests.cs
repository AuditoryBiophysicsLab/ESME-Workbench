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
    public class SettingsBaseTests
    {
        [Test]
        public void SavingAndLoading()
        {
            TestSaving();
            TestLoading();
        }
        
        public void TestSaving()
        {
            var testSettings = new TestSettings();
            Assert.AreEqual("Initial String Property Value", testSettings.StringProperty);
            Assert.AreEqual(0.0, testSettings.Geo.Latitude);
            Assert.AreEqual(0.0, testSettings.Geo.Longitude);
            File.Delete("TestSettings.xml");
            Assert.IsFalse(File.Exists("TestSettings.xml"));
            testSettings.Save("TestSettings.xml");
            Assert.IsTrue(File.Exists("TestSettings.xml"));
        }

        public void TestLoading()
        {
            var testSettings = (TestSettings)StaticXmlSerializer.Load("TestSettings.xml", typeof(TestSettings));
            Assert.NotNull(testSettings);
            Assert.AreEqual("Initial String Property Value", testSettings.StringProperty);
            Assert.AreEqual(0.0, testSettings.Geo.Latitude);
            Assert.AreEqual(0.0, testSettings.Geo.Longitude);
        }

        //[Import] ISettingsService _settingsService;
    }

    [Serializable]
    public class TestSettings : SettingsBase
    {
        [ReferencedTypes, UsedImplicitly]
        static readonly List<Type> ReferencedTypes = new List<Type> { typeof(Geo) };

        [Initialize("Initial String Property Value")]
        public string StringProperty { get; set; }

        [Initialize]
        public Geo Geo { get; set; }

        public override void Save(string fileName)
        {
            //XmlSerializer<TestSettings>.SaveStatic(this, fileName, ReferencedTypes);
            StaticXmlSerializer.Save(fileName, this);
        }
    }
}
