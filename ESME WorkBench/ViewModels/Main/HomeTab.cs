using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using Cinch;
using ESME;
using ESME.Mapping;
using ESME.NEMO;
using ESMEWorkBench.ViewModels.Map;

namespace ESMEWorkBench.ViewModels.Main
{
    public partial class MainViewModel
    {
        public MapLayerCollection HomeTabMapLayers { get; set; }

        #region public NemoFile Scenario { get; set; }

        public NemoFile Scenario
        {
            get { return _scenario; }
            set
            {
                if (_scenario == value) return;
                _scenario = value;
                NotifyPropertyChanged(ScenarioChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ScenarioChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.Scenario);
        NemoFile _scenario;

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

        static readonly PropertyChangedEventArgs ScenarioFilenameChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.ScenarioFilename);
        string _scenarioFilename;

        #endregion



    }
}
