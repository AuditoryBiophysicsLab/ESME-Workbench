using System;
using System.Xml.Serialization;

namespace ESMEWorkBench.Data
{
    public class OldExperimentData : PersistentApplicationData<OldExperimentData>, IApplicationData
    {
        public OldExperimentData(EventHandler valueChangedHandler) : this() { if (valueChangedHandler != null) ValueChanged += valueChangedHandler; }

        public OldExperimentData()
        {
            Initialize();
        }

        #region Private fields

        readonly WatchableReferenceDatum<string> _testString = new WatchableReferenceDatum<string>();
        WatchableList<string> _layerFileNames = new WatchableList<string>();
        
        #endregion

        #region Public properties
        
        [XmlIgnore]
        public bool IsModified
        {
            get { return LastModified > LastSaved; }
        }

        [XmlElement]
        public DateTime LastModified { get; set; }

        [XmlElement]
        public DateTime LastSaved { get; set; }

        [XmlElement]
        public string TestString
        {
            get { return _testString.Value; }
            set { _testString.Value = value; }
        }

        [XmlArray]
        public WatchableList<string> LayerFileNames
        {
            get { return _layerFileNames; }
            set { _layerFileNames = value; }
        }
        #endregion

        #region Event Handlers

        void ValueChangedHandler(object sender, EventArgs e)
        {
            LastModified = DateTime.Now;
            OnValueChanged();
        }

        #endregion

        #region IApplicationData Members

        public void Initialize()
        {
            _testString.ValueChanged += ValueChangedHandler;
            _layerFileNames.ValueChanged += ValueChangedHandler;
        }

        #endregion

        protected override void OnSave()
        {
            LastSaved = DateTime.Now;
            OnValueChanged();
        }

        public static void Test()
        {
            const string layerFileName1 = "LayerFileName1";
            const string layerFileName2 = "LayerFileName2";

            Console.WriteLine(@"Creating new ExperimentData object");
            var myData = new OldExperimentData((s, e) => Console.WriteLine(@"Value Changed!"));
            Console.WriteLine(@"IsModified = " + (myData.IsModified ? "True" : "False"));
            Console.WriteLine(@"Setting TestString");
            myData.TestString = "Test String";
            Console.WriteLine(@"Adding a LayerFileName");
            myData.LayerFileNames.Add(layerFileName1);
            Console.WriteLine(@"Adding a LayerFileName");
            myData.LayerFileNames.Add(layerFileName2);
            Console.WriteLine(@"Changing a LayerFileName to the same value");
            myData.LayerFileNames[0] = layerFileName1;
            Console.WriteLine(@"IsModified = " + (myData.IsModified ? "True" : "False"));
            myData.Save("myData.xml");
        }
    }
}