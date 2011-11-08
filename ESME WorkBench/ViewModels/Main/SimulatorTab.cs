using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using Cinch;
using ESME;
using ESME.Environment;
using ESME.Environment.Descriptors;
using OneNavyModel.Properties;

namespace OneNavyModel.ViewModels.Main
{
    public partial class MainViewModel
    {

        #region AddAnimalPopulationFileCommand
        public SimpleCommand<object, object> AddAnimalPopulationFileCommand
        {
            get { return _addAnimalPopulationFile ?? (_addAnimalPopulationFile = new SimpleCommand<object, object>(obj => MediatorMessage.Send(MediatorMessage.AddAnimatPopulationFileCommand))); }
        }

        SimpleCommand<object, object> _addAnimalPopulationFile;
        #endregion

        #region LaunchMMMBSCommand
        public SimpleCommand<object, object> LaunchMMMBSCommand
        {
            get
            {
                return _launchMMMBS ??
                       (_launchMMMBS =
                        new SimpleCommand<object, object>(delegate { return IsLaunchMMMBSCommandEnabled; },
                                                          delegate { LaunchMMMBSHandler(); }));
            }
        }

        SimpleCommand<object, object> _launchMMMBS;

        static bool IsLaunchMMMBSCommandEnabled
        {
            get { return File.Exists(Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "3MB.exe")); }
        }

        static void LaunchMMMBSHandler()
        {
            var mbsPath = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "3MB.exe");

            new Process
            {
                StartInfo =
                {
                    FileName = mbsPath,
                    WorkingDirectory = Path.GetDirectoryName(mbsPath),
                }
            }.Start();
        }
        #endregion

        #region LaunchMMMBSpeciesBuilderCommand
        public SimpleCommand<object, object> LaunchMMMBSpeciesBuilderCommand
        {
            get
            {
                return _launchMMMBSpeciesBuilder ?? (_launchMMMBSpeciesBuilder = new SimpleCommand<object, object>(obj =>
                {
                    var mbsPath = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "3mbSpeciesBuilder.exe");

                    new Process
                    {
                        StartInfo =
                        {
                            FileName = mbsPath,
                            WorkingDirectory = Path.GetDirectoryName(mbsPath),
                        }
                    }.Start();
                }));
            }
        }

        SimpleCommand<object, object> _launchMMMBSpeciesBuilder;
        #endregion

        #region CreateMMMBSBathymetryFileCommand
        public SimpleCommand<object, object> CreateMMMBSBathymetryFileCommand
        {
            get
            {
                return _createMMMBSBathymetryFile ??
                       (_createMMMBSBathymetryFile =
                        new SimpleCommand<object, object>(delegate { return IsCreateMMMBSBathymetryFileCommandEnabled; },
                                                          delegate { CreateMMMBSBathymetryFileHandler(); }));
            }
        }

        SimpleCommand<object, object> _createMMMBSBathymetryFile;

        bool IsCreateMMMBSBathymetryFileCommandEnabled
        {
            get { return (_rangeComplexes != null) && (_rangeComplexes.IsEnvironmentFullySpecified); }
        }

        void CreateMMMBSBathymetryFileHandler()
        {
            _saveFileService.Filter = "MMMBS bathymetry files (*.bth)|*.bth|All files (*.*)|*.*";
            _saveFileService.OverwritePrompt = true;
            _saveFileService.InitialDirectory = Settings.Default.LastBathymetryFileDirectory;
            _saveFileService.FileName = _rangeComplexes.SelectedArea.Name +".bth";
            var result = _saveFileService.ShowDialog((Window)_viewAwareStatus.View);
            if ((!result.HasValue) || (!result.Value)) return;
            Settings.Default.LastBathymetryFileDirectory = Path.GetDirectoryName(_saveFileService.FileName);
            if (_rangeComplexes.IsEnvironmentLoaded)
            {
                ((Task<Bathymetry>)_rangeComplexes.EnvironmentData[EnvironmentDataType.Bathymetry]).Result.ToYXZ(_saveFileService.FileName,-1);
            }
            else
            {
                ((Task<Bathymetry>)_rangeComplexes.EnvironmentData[EnvironmentDataType.Bathymetry]).ContinueWith(task=>task.Result.ToYXZ(_saveFileService.FileName, -1));
            }
        }
        #endregion
    }
}
