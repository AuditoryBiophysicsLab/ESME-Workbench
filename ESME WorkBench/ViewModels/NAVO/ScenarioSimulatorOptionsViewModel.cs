using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using Cinch;
using ESME.NEMO;

namespace ESMEWorkBench.ViewModels.NAVO
{
    public class ScenarioSimulatorOptionsViewModel : ViewModelBase
    {
        #region public bool IsRandomized { get; set; }

        public bool IsRandomized
        {
            get { return _isRandomized; }
            set
            {
                if (_isRandomized == value) return;
                _isRandomized = value;
                NotifyPropertyChanged(IsRandomizedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsRandomizedChangedEventArgs = ObservableHelper.CreateArgs<ScenarioSimulatorOptionsViewModel>(x => x.IsRandomized);
        bool _isRandomized;

        #endregion

        #region public int Iterations { get; set; }

        public int Iterations
        {
            get { return _iterations; }
            set
            {
                if (_iterations == value) return;
                _iterations = value;
                NotifyPropertyChanged(IterationsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IterationsChangedEventArgs = ObservableHelper.CreateArgs<ScenarioSimulatorOptionsViewModel>(x => x.Iterations);
        int _iterations;

        #endregion

        #region public NemoFile NemoFile { get; set; }

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

        static readonly PropertyChangedEventArgs NemoFileChangedEventArgs = ObservableHelper.CreateArgs<ScenarioSimulatorOptionsViewModel>(x => x.NemoFile);
        NemoFile _nemoFile;

        #endregion


        #region RunCommand

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
                        var numIterations = Iterations;
                        var isRandomized = IsRandomized;
                        var commandArgs = string.Format(isRandomized ? "-b -r -n {0} -s \"{1}\"" : "-b -n {0} -s \"{1}\"", numIterations, NemoFile.FileName);

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

        SimpleCommand<object, object> _run;

        #endregion

    }
}
