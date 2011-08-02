using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Xml.Serialization;
using Cinch;
using ESME.Mapping;
using ESME.NEMO;
using ESME.TransmissionLoss;

namespace ESME.Metadata
{
    public class NAEMOScenarioMetadata : NAEMOMetadataBase
    {
        new internal static readonly List<Type> ReferencedTypes = new List<Type>(NAEMOMetadataBase.ReferencedTypes) {typeof(NemoModeToAcousticModelNameMap)};

        public static NAEMOScenarioMetadata Load(string metaDataFilename) { return Load<NAEMOScenarioMetadata>(metaDataFilename); }

        public void Save(string filename = null) { Save(this, ReferencedTypes, filename); }

        public NAEMOScenarioMetadata() {  }

        public NAEMOScenarioMetadata(string scenarioFilename)
        {
            ScenarioFilename = scenarioFilename;
            Filename = MetadataFilename(scenarioFilename);
            NemoModeToAcousticModelNameMap = new NemoModeToAcousticModelNameMap(_nemoFile.Scenario.DistinctModePSMNames, TransmissionLossAlgorithm.CASS);
        }

        #region public NemoFile NemoFile { get; set; }
        [XmlIgnore]
        public NemoFile NemoFile
        {
            get { return _nemoFile; }
            set
            {
                if (_nemoFile == value) return;
                _nemoFile = value;
                NotifyPropertyChanged(NemoFileChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs NemoFileChangedEventArgs = ObservableHelper.CreateArgs<NAEMOScenarioMetadata>(x => x.NemoFile);
        NemoFile _nemoFile;

        #endregion

        #region public string ScenarioFilename { get; set; }

        public string ScenarioFilename
        {
            get { return _scenarioFilename; }
            set
            {
                if (_scenarioFilename == value) return;
                _scenarioFilename = value;
                NotifyPropertyChanged(ScenarioFilenameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ScenarioFilenameChangedEventArgs = ObservableHelper.CreateArgs<NAEMOScenarioMetadata>(x => x.ScenarioFilename);
        string _scenarioFilename;

        #endregion


        #region public string EnvironmentName { get; set; }

        public string EnvironmentName
        {
            get { return _environmentName; }
            set
            {
                if (_environmentName == value) return;
                _environmentName = value;
                NotifyPropertyChanged(EnvironmentNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs EnvironmentNameChangedEventArgs = ObservableHelper.CreateArgs<NAEMOScenarioMetadata>(x => x.EnvironmentName);
        string _environmentName;

        #endregion

        #region public MapLayerCollection MapLayerCollection { get; set; }

        public MapLayerCollection MapLayerCollection
        {
            get { return _mapLayerCollection; }
            set
            {
                if (_mapLayerCollection == value) return;
                _mapLayerCollection = value;
                NotifyPropertyChanged(MapLayerCollectionChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MapLayerCollectionChangedEventArgs = ObservableHelper.CreateArgs<NAEMOScenarioMetadata>(x => x.MapLayerCollection);
        MapLayerCollection _mapLayerCollection;

        #endregion

        #region public NemoModeToAcousticModelNameMap NemoModeToAcousticModelNameMap { get; set; }

        public NemoModeToAcousticModelNameMap NemoModeToAcousticModelNameMap
        {
            get { return _nemoModeToAcousticModelNameMap; }
            set
            {
                if (_nemoModeToAcousticModelNameMap == value) return;
                _nemoModeToAcousticModelNameMap = value;
                NotifyPropertyChanged(NemoModeToAcousticModelNameMapChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs NemoModeToAcousticModelNameMapChangedEventArgs = ObservableHelper.CreateArgs<NAEMOScenarioMetadata>(x => x.NemoModeToAcousticModelNameMap);
        NemoModeToAcousticModelNameMap _nemoModeToAcousticModelNameMap;

        #endregion

    }
}
