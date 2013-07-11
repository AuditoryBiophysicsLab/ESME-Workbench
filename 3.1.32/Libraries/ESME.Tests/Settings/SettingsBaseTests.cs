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
        public void DefaultSavingAndLoading()
        {
            File.Delete("DefaultTestSettings.xml");
            Assert.IsFalse(File.Exists("DefaultTestSettings.xml"));
            TestDefaultSaving();
            TestDefaultLoading();
            Console.WriteLine(File.ReadAllText("DefaultTestSettings.xml"));
        }

        public void TestDefaultSaving()
        {
            var testSettings = new BaseTestSettings();
            Assert.AreEqual("Initial String Property Value", testSettings.StringProperty);
            Assert.AreEqual(0.0, testSettings.Geo.Latitude);
            Assert.AreEqual(0.0, testSettings.Geo.Longitude);
            testSettings.Save("DefaultTestSettings.xml");
            Assert.IsTrue(File.Exists("DefaultTestSettings.xml"));
        }

        public void TestDefaultLoading()
        {
            var testSettings = (BaseTestSettings)StaticXmlSerializer.Load("DefaultTestSettings.xml", typeof(BaseTestSettings));
            Assert.NotNull(testSettings);
            Assert.AreEqual("Initial String Property Value", testSettings.StringProperty);
            Assert.AreEqual(0.0, testSettings.Geo.Latitude);
            Assert.AreEqual(0.0, testSettings.Geo.Longitude);
        }

        [Test]
        public void ModifiedSavingAndLoading()
        {
            if (File.Exists("ModifiedTestSettings.xml")) File.Delete("ModifiedTestSettings.xml");
            Assert.IsFalse(File.Exists("ModifiedTestSettings.xml"));
            TestModifiedSaving();
            TestModifiedLoading();
            Console.WriteLine(File.ReadAllText("ModifiedTestSettings.xml"));
        }

        public void TestModifiedSaving()
        {
            var testSettings = new BaseTestSettings();
            testSettings.StringProperty = "Modified string property value";
            Assert.AreEqual("Modified string property value", testSettings.StringProperty);
            testSettings.Geo = new Geo(43, -71);
            Assert.AreEqual(43f, testSettings.Geo.Latitude);
            Assert.AreEqual(-71f, testSettings.Geo.Longitude);
            testSettings.Save("ModifiedTestSettings.xml");
            Assert.IsTrue(File.Exists("ModifiedTestSettings.xml"));
        }

        public void TestModifiedLoading()
        {
            var testSettings = (BaseTestSettings)StaticXmlSerializer.Load("ModifiedTestSettings.xml", typeof(BaseTestSettings));
            Assert.NotNull(testSettings);
            Assert.AreEqual("Modified string property value", testSettings.StringProperty);
            Assert.AreEqual(43f, testSettings.Geo.Latitude);
            Assert.AreEqual(-71f, testSettings.Geo.Longitude);
        }

        //[Import] ISettingsService _settingsService;
    }

    [Serializable]
    public class BaseTestSettings : SettingsBase
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
