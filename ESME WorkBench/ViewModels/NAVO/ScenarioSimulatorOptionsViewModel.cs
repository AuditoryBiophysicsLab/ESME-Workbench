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
                    return ((Globals.AppSettings.ScenarioSimulatorSettings.ScenarioExecutablePath != null)
                        && File.Exists(Globals.AppSettings.ScenarioSimulatorSettings.ScenarioExecutablePath) 
                        && (Globals.AppSettings.JavaExecutablePath !=null) 
                        && File.Exists(Globals.AppSettings.JavaExecutablePath) 
                        && NemoFile != null);
                },
                delegate
                {
                    // string commandArgs = string.Format("-Dlog4j.configuration=\"{3}\" -Dsim.output.level={4} {2} -r -n {0} -s \"{1}\"", ScenarioSimulatorSettings.Iterations, NemoFile.FileName, ScenarioSimulatorSettings.IsRandomized ? "-b" : "", ScenarioSimulatorSettings.JavaConfigurationFile,ScenarioSimulatorSettings.SimOutputLevel);

                    new Process
                    {
                        StartInfo =
                            {
                                FileName = Globals.AppSettings.JavaExecutablePath,
                                Arguments = CommandArgs(),
                                CreateNoWindow = true,

                            },
                    }.Start();

                    CloseActivePopUpCommand.Execute(true);
                }));
            }
        }

        #endregion

        string CommandArgs()
        {
            //compute max physical memory
            var info = new Microsoft.VisualBasic.Devices.ComputerInfo();
            var bytes = info.AvailablePhysicalMemory;
            var megs = bytes >> 20;

            var sb = new StringBuilder();
            
            sb.Append(string.Format(" -Xmx{0}m", megs)); // maximum memory on computer.
            if (!string.IsNullOrEmpty(ScenarioSimulatorSettings.JavaConfigurationFile)) sb.Append(string.Format(" -Dlog4j.configuration=\"{0}\"", ScenarioSimulatorSettings.JavaConfigurationFile)); // location of xml config file
            if (ScenarioSimulatorSettings.SimOutputLevel > 0) sb.Append(string.Format(" -Dsim.output.level={0}", ScenarioSimulatorSettings.SimOutputLevel)); // log level
            if (ScenarioSimulatorSettings.DecibelCutoff > 0) sb.Append(string.Format(" -Dsim.receive.cutoff={0}", ScenarioSimulatorSettings.DecibelCutoff)); //db cutoff
            sb.Append(string.Format(" -Dsim.species.cached={0} -Dsim.area.clip={1}", ScenarioSimulatorSettings.ReadAllMammals.ToString().ToLower(), ScenarioSimulatorSettings.ClipOutsideFootprint.ToString().ToLower()));// cache all species, clip? 
            sb.Append(string.Format(" -Dsim.output.filebuffer={0}", ScenarioSimulatorSettings.OutputBufferSize));  //output buffer options
            if (ScenarioSimulatorSettings.ParallelSimulations > 0) sb.Append(string.Format(" -Dnemo.sim.count={0}", ScenarioSimulatorSettings.ParallelSimulations)); // parallel sims
            if(ScenarioSimulatorSettings.OptimizeBuffer) sb.Append(string.Format(" -Dsim.filebuffer.adapt=true"));
            if (ScenarioSimulatorSettings.CASSFileSize > 0 && !ScenarioSimulatorSettings.OptimizeBuffer) sb.Append(string.Format(" -Dsim.cass.filebuffer={0}", ScenarioSimulatorSettings.CASSFileSize)); // cass file buffer.
            if (ScenarioSimulatorSettings.SpeciesFileSize > 0 && !ScenarioSimulatorSettings.OptimizeBuffer) sb.Append(string.Format(" -Dsim.species.filebuffer={0}", ScenarioSimulatorSettings.SpeciesFileSize)); // species file size
            sb.Append(string.Format(" -jar \"{0}\"", ScenarioSimulatorSettings.ScenarioExecutablePath));
            sb.Append(string.Format(" -b{2} -n {0} -s \"{1}\"", ScenarioSimulatorSettings.Iterations, NemoFile.FileName, ScenarioSimulatorSettings.IsRandomized ? " -r" : ""));
            return sb.ToString();
        }
    }
}