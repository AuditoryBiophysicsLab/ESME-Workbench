using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Reflection;
using System.Xml.Serialization;
using ESME.Settings;
using HRC.Aspects;
using HRC.Navigation;
using HRC.Utility;
using HRC.Validation;
using NUnit.Framework;
using PostSharp;

namespace HRC.Tests.Aspects
{
    public class TestNotifyPropertyChangedAttribute
    {
        [Test]
        public void TestNotifyPropertyChangedAndAffectsAspects()
        {
            var test1 = new TestPropertyContainer();
            var test1PropertyChanged = new List<string>();
            Post.Cast<TestPropertyContainer, INotifyPropertyChanged>(test1).PropertyChanged += (s, e) =>
            {
                Console.WriteLine("PropertyChanged: {0}", e.PropertyName);
                test1PropertyChanged.Add(e.PropertyName);
            };
            var test2 = new TestPropertyContainer2();
            var test2PropertyChanged = new List<string>();
            Post.Cast<TestPropertyContainer2, INotifyPropertyChanged>(test2).PropertyChanged += (s, e) =>
            {
                Console.WriteLine("PropertyChanged: {0}", e.PropertyName);
                test2PropertyChanged.Add(e.PropertyName);
            };
            test1.Property1 = "Property1";
            Assert.AreEqual(test1PropertyChanged.Count, 1);
            Assert.AreEqual("Property1", test1PropertyChanged[0]);
            test1PropertyChanged.Clear();
            test1.Property2 = "Property2";
            Assert.AreEqual(test1PropertyChanged.Count, 1);
            Assert.AreEqual("Property2", test1PropertyChanged[0]);
            test1PropertyChanged.Clear();
            test1.Property3 = "Property3";
            Assert.AreEqual(test1PropertyChanged.Count, 2);
            Assert.IsTrue(test1PropertyChanged.Contains("Property3"));
            Assert.IsTrue(test1PropertyChanged.Contains("YYY"));
            test1PropertyChanged.Clear();
            test2.Property1 = "Property1";
            Assert.AreEqual(test2PropertyChanged.Count, 1);
            Assert.AreEqual("Property1", test2PropertyChanged[0]);
            test2PropertyChanged.Clear();
            test2.Property2 = "Property2";
            Assert.AreEqual(test2PropertyChanged.Count, 1);
            Assert.AreEqual("Property2", test2PropertyChanged[0]);
            test2PropertyChanged.Clear();
            test2.Property3 = "Property3";
            Assert.AreEqual(test2PropertyChanged.Count, 2);
            Assert.IsTrue(test2PropertyChanged.Contains("Property3"));
            Assert.IsTrue(test2PropertyChanged.Contains("YYY"));
            test2PropertyChanged.Clear();
            test2.Property4 = "Property4";
            Assert.AreEqual(test2PropertyChanged.Count, 1);
            Assert.AreEqual("Property4", test2PropertyChanged[0]);
            test2PropertyChanged.Clear();
            test2.Property5 = "Property5";
            Assert.AreEqual(test2PropertyChanged.Count, 1);
            Assert.AreEqual("Property5", test2PropertyChanged[0]);
            test2PropertyChanged.Clear();
            test2.Property6 = "Property6";
            Assert.AreEqual(test2PropertyChanged.Count, 3);
            Assert.IsTrue(test2PropertyChanged.Contains("Property6"));
            Assert.IsTrue(test2PropertyChanged.Contains("ZZZ"));
            Assert.IsTrue(test2PropertyChanged.Contains("abc"));
            test2PropertyChanged.Clear();
        }

        [NotifyPropertyChanged]
        public class TestPropertyContainer
        {
            public string Property1 { get; set; }

            public string Property2 { get; set; }

            [Affects("YYY")]
            public string Property3 { get; set; }
        }

        public class TestPropertyContainer2 : TestPropertyContainer
        {
            public string Property4 { get; set; }

            public string Property5 { get; set; }

            [Affects("ZZZ", "abc")]
            public string Property6 { get; set; }
        }

        [Test]
        public void TestInitializeAspect()
        {
            var testSettings = new InitializeAspectTests();
            Assert.AreEqual("Initial String Property Value", testSettings.StringProperty);
#if true
            Assert.AreEqual(testSettings.ZeroIntProperty++, 0);
            Assert.AreEqual(testSettings.ZeroIntProperty++, 1);
            Assert.IsNull(testSettings.NullStringProperty);
            Assert.AreEqual("Default", testSettings.DefaultedStringProperty);
            Assert.Throws(typeof (InvalidOperationException), () => Console.WriteLine(testSettings.ExceptionStringProperty));
            Console.WriteLine("GuidStringProperty = {0}", testSettings.GuidStringProperty);
            Console.WriteLine("GuidProperty = {0}", testSettings.GuidProperty);
            Assert.AreEqual(testSettings.DefaultedIntProperty++, 5);
            Assert.AreEqual(testSettings.DefaultedIntProperty++, 6);
            Assert.IsNull(testSettings.NullStringList);
            Assert.IsEmpty(testSettings.DefaultedStringList);
#endif
        }

        [Test]
        public void TestSettings()
        {
            var testSettings = new SomeOtherClassName();
            Assert.AreEqual("Initial String Property Value", testSettings.StringProperty);
            testSettings.Save("TestSettings.xml");
        }

        [Test]
        public void TestSettingsOnNearlyIdenticalClass()
        {
            var testSettings = new InitializeAspectTests();
            Assert.AreEqual("Initial String Property Value", testSettings.StringProperty);
        }
    }

    public class InitializeAspectTests : SettingsBase
    {
        [ReferencedTypes, UsedImplicitly]
        static readonly List<Type> ReferencedTypes = new List<Type> { typeof(Geo) };

        [Initialize("Initial String Property Value")]
        public string StringProperty { get; set; }

        [Initialize]
        public Geo Geo { get; set; }

        public override void Save(string fileName)
        {
            XmlSerializer<InitializeAspectTests>.SaveStatic(this, fileName, ReferencedTypes);
            //SaveBase(this, fileName);
        }

#if true
        public int ZeroIntProperty { get; set; }
        public string NullStringProperty { get; set; }

        [Initialize("Default")]
        public string DefaultedStringProperty { get; set; }

        [Initialize]
        public string ExceptionStringProperty { get; set; }

        [Initialize(IsGuid = true)]
        public string GuidStringProperty { get; set; }

        [Initialize]
        public Guid GuidProperty { get; set; }

        [Initialize(5)]
        public int DefaultedIntProperty { get; set; }

        public List<string> NullStringList { get; set; }
        [Initialize]
        public List<string> DefaultedStringList { get; set; }
#endif
    }

    public class SomeOtherClassName : SettingsBase
    {
        [ReferencedTypes, UsedImplicitly]
        static readonly List<Type> ReferencedTypes = new List<Type> { typeof(Geo) };

        [Initialize("Initial String Property Value")]
        public string StringProperty { get; set; }

        [Initialize]
        public Geo Geo { get; set; }

        public override void Save(string fileName)
        {
            XmlSerializer<SomeOtherClassName>.SaveStatic(this, fileName, ReferencedTypes);
            //SaveBase(this, fileName);
        }
    }

    [Serializable, NotifyPropertyChanged]
    public abstract class SettingsBase : ValidatingViewModel, ISettingsBase
    {
        public abstract void Save(string fileName);

        protected void SaveBase<T>(T source, string fileName) where T : class, ISettingsBase, new()
        {
            //var serializerType = typeof(StaticXmlSerializer);
            //var methodInfo = serializerType.GetMethod("SaveStatic", new[] { typeof(T), typeof(string), typeof(List<Type>) });
            Console.WriteLine("Looking for [ReferencedTypes] attribute on static fields and properties...");
            var memberInfo = new List<MemberInfo>();
            memberInfo.AddRange(typeof(T).GetFields(BindingFlags.Static | BindingFlags.FlattenHierarchy | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance));
            memberInfo.AddRange(typeof(T).GetProperties(BindingFlags.Static | BindingFlags.FlattenHierarchy | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance));
            var referencedTypes = new List<Type>();
            foreach (var member in memberInfo)
            {
                if (member.GetCustomAttributes(typeof(ReferencedTypesAttribute), true).Any())
                {
                    var fieldInfo = member as FieldInfo;
                    var propertyInfo = member as PropertyInfo;
                    if (propertyInfo != null)
                    {
                        if (!typeof(IList<Type>).IsAssignableFrom(propertyInfo.PropertyType)) throw new InvalidReferencedTypesAttributeException("Properties with the [ReferencedTypes] attribute must be assignable to type IList<Type>");
                        referencedTypes.AddRange((List<Type>)(propertyInfo).GetValue(null, BindingFlags.Static, null, null, null));
                    }
                    else if (fieldInfo != null)
                    {
                        if (!typeof(IList<Type>).IsAssignableFrom(fieldInfo.FieldType)) throw new InvalidReferencedTypesAttributeException("Properties with the [ReferencedTypes] attribute must be assignable to type IList<Type>");
                        referencedTypes.AddRange((List<Type>)(fieldInfo).GetValue(null));
                    }
                    Console.WriteLine("referencedTypes = {0}, count = {1}", referencedTypes, referencedTypes.Count);
                }
                else
                {
                    Console.WriteLine("{0}.{1} does not have [ReferencedTypes] attribute", member.DeclaringType, member.Name);
                }
            }
            //methodInfo.Invoke(null, new object[] { source, fileName, referencedTypes });
        }

        //List<Type> _allReferencedTypes = new List<Type>();
        //MethodInfo _saveMethod;

        //protected void SaveBase<T>(T source, string settingsRootDirectory) where T : SettingsBase, new()
        //{
        //    var serializer = new XmlSerializer<T> { Data = source };
        //    serializer.Save(Path.Combine(settingsRootDirectory, typeof(T).ToString(), source.SettingsFilename), source.ReferencedTypes);
        //}
        [XmlIgnore]
        public string SettingsFilename { get; protected set; }
    }
}
