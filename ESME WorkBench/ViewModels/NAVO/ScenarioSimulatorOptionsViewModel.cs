using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Text;
using Cinch;
using ESME.Data;
using ESME.NEMO;

namespace ESMEWorkBench.ViewModels.NAVO
{
    public class ScenarioSimulatorOptionsViewModel : ViewModelBase
    {
        #region public ScenarioSimulatorSettings ScenarioSimulatorSettings { get; set; }

        static readonly PropertyChangedEventArgs ScenarioSimulatorSettingsChangedEventArgs = ObservableHelper.CreateArgs<ScenarioSimulatorOptionsViewModel>(x => x.ScenarioSimulatorSettings);
        ScenarioSimulatorSettings _scenarioSimulatorSettings;

        public ScenarioSimulatorSettings ScenarioSimulatorSettings
        {
            get { return _scenarioSimulatorSettings; }
            set
            {
                if (_scenarioSimulatorSettings == value) return;

                _scenarioSimulatorSettings = value;

                NotifyPropertyChanged(ScenarioSimulatorSettingsChangedEventArgs);
            }
        }

        #endregion

        #region public NemoFile NemoFile { get; set; }

        static readonly PropertyChangedEventArgs NemoFileChangedEventArgs = ObservableHelper.CreateArgs<ScenarioSimulatorOptionsViewModel>(x => x.NemoFile);
        NemoFile _nemoFile;

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

        #endregion

        #region RunCommand

        SimpleCommand<object, object> _run;

        public SimpleCommand<object, object> RunCommand
        {
            get
            {
                return _run ?? (_run = new SimpleCommand<object, object>(delegate
                {
                    return ((Globals.AppSettings.ScenarioSimulatorSettings.ExecutablePath != null)
                        && File.Exists(Globals.AppSettings.ScenarioSimulatorSettings.ExecutablePath) && NemoFile != null);
                },
                delegate
                {
                    string commandArgs = string.Format("-Dlog4j.configuration=\"{3}\" -Dsim.output.level={4} {2} -r -n {0} -s \"{1}\"", ScenarioSimulatorSettings.Iterations, NemoFile.FileName, ScenarioSimulatorSettings.IsRandomized ? "-b" : "", ScenarioSimulatorSettings.JavaConfigurationFile,ScenarioSimulatorSettings.LogLevel);

                    new Process
                    {
                        StartInfo =
                            {
                                FileName = Globals.AppSettings.ScenarioSimulatorSettings.ExecutablePath,
                                Arguments = commandArgs,
                            },
                    }.Start();

                    CloseActivePopUpCommand.Execute(true);
                }));
            }
        }

        #endregion

        string CommandArgs()
        {
            var sb = new StringBuilder();
            
            return sb.ToString();
        }
    }
}