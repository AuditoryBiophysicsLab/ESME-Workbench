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
            File.Delete("BaseTestSettings.xml");
            Assert.IsFalse(File.Exists("BaseTestSettings.xml"));
            TestSaving();
            TestLoading();
            Console.WriteLine(File.ReadAllText("BaseTestSettings.xml"));
        }
        
        public void TestSaving()
        {
            var testSettings = new BaseTestSettings();
            Assert.AreEqual("Initial String Property Value", testSettings.StringProperty);
            Assert.AreEqual(0.0, testSettings.Geo.Latitude);
            Assert.AreEqual(0.0, testSettings.Geo.Longitude);
            testSettings.Save("BaseTestSettings.xml");
            Assert.IsTrue(File.Exists("BaseTestSettings.xml"));
        }

        public void TestLoading()
        {
            var testSettings = (BaseTestSettings)StaticXmlSerializer.Load("BaseTestSettings.xml", typeof(BaseTestSettings));
            Assert.NotNull(testSettings);
            Assert.AreEqual("Initial String Property Value", testSettings.StringProperty);
            Assert.AreEqual(0.0, testSettings.Geo.Latitude);
            Assert.AreEqual(0.0, testSettings.Geo.Longitude);
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
