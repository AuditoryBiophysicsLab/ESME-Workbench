using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Reflection;
using Cinch;
using ESME.Data;
using HRC.Validation;

namespace ESME.Views.InstallationWizard
{
    public sealed class WizardViewModel : ViewModelBase
    {
        #region private constructor and associated methods
        WizardViewModel()
        {
            AppSettings = Globals.AppSettings;
            CreatePanels();
            SelectedPanelIndex = 0;
            SelectedPanel = Panels[SelectedPanelIndex];
        }

        void CreatePanels()
        {
            Panels = new List<WizardPanelInfo>
            {
                    new WizardPanelInfo
                    {
                            DescriptiveText =
                                    "Welcome to the One Navy Model Configuration Wizard.\n\n" +
                                    "The next several panes of this wizard will help configure the One Navy Model.\n\n" +
                                    "Please press Next to continue, or Cancel to exit without saving changes.",
                            IsFileBrowerEnabled = false,
                            UserResponse = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
                            //will always be a valid location; "next" button always lit.
                            PropertyName = "welcomeScreen",
                    },
            };

            if (string.IsNullOrEmpty(AppSettings.NAEMOTools.JavaExecutablePath) ||
                !File.Exists(AppSettings.NAEMOTools.JavaExecutablePath))
                Panels.Add(
                           new WizardPanelInfo
                           {
                                   DescriptiveText =
                                   "Some components of the One Navy Model require Java to run.\n\n" +
                                   "Please select the installed javaw.exe executable.\n\n" +
                                   "If Java is not installed, the latest Java Runtime Environment (JRE) can be downloaded from http://www.oracle.com/technetwork/java/javase/downloads/index.html",
                                   Hyperlink =
                                   new Uri("http://www.oracle.com/technetwork/java/javase/downloads/index.html"),
                                   HyperlinkText = "the Oracle Java download page.",
                                   FieldName = "Java Executable (javaw.exe)",
                                   DialogTitle = "Locate the Java executable",
                                   UserResponse =
                                   System.Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFilesX86) +
                                   @"\Java\",
                                   FileNameFilter = "Java executable (javaw.exe)|javaw.exe|All files (*.*)|*.*",
                                   PropertyName = "javaw",
                           });

            if (string.IsNullOrEmpty(AppSettings.NAEMOTools.NAEMOToolsDirectory) ||
                !Directory.Exists(AppSettings.NAEMOTools.NAEMOToolsDirectory))
                Panels.Add(
                           new WizardPanelInfo
                           {
                                   DescriptiveText =
                                   "The Naval Undersea Warfare Center (NUWC) provides a collection of java applications necessary for the proper operation of the One Navy Model.\n\n" +
                                   "Please select the NUWC Scenario Builder (scenario-builder.jar) that is stored in a directory containing the complete NUWC application collection.",
                                   FieldName = "NUWC Application (*.jar)",
                                   DialogTitle = "Locate the Scenario Builder (scenario-builder.jar)",
                                   FileNameFilter =
                                   "NUWC Scenario Builder (scenario-builder.jar)|scenario-builder.jar|Java executables(*.jar)|*.jar|All files(*.*)|(*.*)",
                                   PropertyName = "NUWCToolsDirectory",
                           });

            if (string.IsNullOrEmpty(AppSettings.ScenarioDataDirectory) ||
                !Directory.Exists(AppSettings.ScenarioDataDirectory))
                Panels.Add(
                           new WizardPanelInfo
                           {
                                   DescriptiveText =
                                   "The Scenario Data Directory is the default location for all experimental data.  It is typically named Sim Areas, and must contain " +
                                   "the files SimAreas.csv, PSM.csv, and Species.csv.",
                                   FieldName = "Scenario Data Directory",
                                   DialogTitle = "Locate the SimAreas.csv file",
                                   FileNameFilter =
                                   "Simulation Areas File (SimAreas.csv)|SimAreas.csv|All files (*.*)|*.*",
                                   PropertyName = "ScenarioDataDirectory",
                           });

            if (string.IsNullOrEmpty(AppSettings.NAVOConfiguration.GDEMDirectory) ||
                !Directory.Exists(AppSettings.NAVOConfiguration.GDEMDirectory))
                Panels.Add(
                           new WizardPanelInfo
                           {
                                   DescriptiveText =
                                   "The Oceanographic and Atmospheric Master Library (OAML) provides key environmental information necessary for operation.  " +
                                   "The Generalized Digital Environmental Model (GDEM) is a required OAML database organized as a collection of NetCDF files.\n\n" +
                                   "Please select any .nc file in the directory containing all of the GDEM data.\n\n" +
                                   "If you do not have this database, a copy can be downloaded from http://esme.bu.edu",
                                   Hyperlink = new Uri("http://esme.bu.edu"),
                                   HyperlinkText = "the One Navy Model webpage.",
                                   FieldName = "GDEM-V",
                                   DialogTitle = "Choose one GDEM database file",
                                   FileNameFilter = "NetCDF files (*.nc)|*.nc|All files (*.*)|*.*",
                                   PropertyName = "GDEM-V",
                           });
            if (string.IsNullOrEmpty(AppSettings.NAVOConfiguration.SMGCDirectory) ||
                !Directory.Exists(AppSettings.NAVOConfiguration.SMGCDirectory))
                Panels.Add(
                           new WizardPanelInfo
                           {
                                   DescriptiveText =
                                   "The The Oceanographic and Atmospheric Master Library (OAML) provides key environmental information necessary for operation.  " +
                                   "The Surface Marine Gridded Climatological (SMGC) database is a required OAML database organized as a collection of 64,800 .stt files\n\n " +
                                   "Please select any .stt file a directory tree containing all of the SMGC data.\n\n" +
                                   "If you do not have this database, a copy can be downloaded from http://esme.bu.edu",
                                   Hyperlink = new Uri("http://esme.bu.edu"),
                                   HyperlinkText = "the One Navy Model webpage.",
                                   FieldName = "SMGC",
                                   DialogTitle = "Choose one SMGC database file (*.stt)",
                                   FileNameFilter = "SMGC files (*.stt)|*.stt|All files (*.*)|*.*",
                                   PropertyName = "SMGC",
                           });
            if (string.IsNullOrEmpty(AppSettings.NAVOConfiguration.BSTDirectory) ||
                !File.Exists(AppSettings.NAVOConfiguration.BSTDirectory))
                Panels.Add(
                           new WizardPanelInfo
                           {
                                   DescriptiveText =
                                   "The Oceanographic and Atmospheric Master Library (OAML) provides key environmental information necessary for operation.  " +
                                   "The High-Frequency Environment Acoustic (HFEVA) Bottom Sediment Type (BST) database is a required OAML database packaged as a standalone HDF5 file.\n\n" +
                                   "Please select the .h5 file that contains the HFEVA BST database.\n\n" +
                                   "If you do not have this database, a copy can be downloaded from http://esme.bu.edu",
                                   Hyperlink = new Uri("http://esme.bu.edu"),
                                   HyperlinkText = "the One Navy Model webpage.",
                                   DialogTitle = "Locate the BST HFEVA database file",
                                   FieldName = "BST",
                                   FileNameFilter =
                                   "BST HFEVA database (hfevav2.h5)|hfevav2.h5|All HDF5 files (*.h5)|*.h5|All files (*.*)|*.*",
                                   PropertyName = "BST",
                           });
            if (string.IsNullOrEmpty(AppSettings.NAVOConfiguration.DBDBDirectory) ||
                !File.Exists(AppSettings.NAVOConfiguration.DBDBDirectory))
                Panels.Add(
                           new WizardPanelInfo
                           {
                                   DescriptiveText =
                                   "The Oceanographic and Atmospheric Master Library (OAML) provides key environmental information necessary for operation.  " +
                                   "The Digital Bathymetric DataBase (DBDB) is a required OAML database packaged as a standalone HDF5 file.\n\n" +
                                   "Please select the .h5 file that contains the DBDB.\n\n" +
                                   "If you do not have this database, a copy can be downloaded from http://esme.bu.edu",
                                   Hyperlink = new Uri("http://esme.bu.edu"),
                                   HyperlinkText = "the One Navy Model webpage.",
                                   FieldName = "DBDB",
                                   DialogTitle = "Locate the DBDB database file (dbdbv5_level0c_0.h5)",
                                   FileNameFilter =
                                   "DBDB database (dbdbv5_level0c_0.h5)|dbdbv5_level0c_0.h5|All HDF5 files (*.h5)|*.h5|All files (*.*)|*.*",
                                   PropertyName = "DBDB",
                           });
            if (string.IsNullOrEmpty(AppSettings.NAVOConfiguration.DBDBEXEPath) ||
                !File.Exists(AppSettings.NAVOConfiguration.DBDBEXEPath))
                Panels.Add(
                           new WizardPanelInfo
                           {
                                   DescriptiveText =
                                   "The Digital Bathymetric DataBase (DBDB) requires a seperate extraction tool for use.\n\n" +
                                   "Please select the DBDB extractor.\n\n" +
                                   "If you do not have this tool, a copy can be downloaded from http://esme.bu.edu",
                                   Hyperlink = new Uri("http://esme.bu.edu"),
                                   HyperlinkText = "the One Navy Model webpage.",
                                   FieldName = "DBDB Extractor (dbv5_command.exe)",
                                   DialogTitle = "Locate the DBDB Extractor program (dbv5_command.exe)",
                                   FileNameFilter =
                                   "DBDB Extractor (dbv5_command.exe)|dbv5_command.exe|Executable files (*.exe)|*.exe|Batch files (*.bat)|*.bat|All files (*.*)|*.*",
                                   PropertyName = "DBDBExtractor",
                           });
            #region classified model parameters
            if (Configuration.IsClassifiedModel)
            {

                if (string.IsNullOrEmpty(AppSettings.NAVOConfiguration.HFBLEXEPath) ||
                    !File.Exists(AppSettings.NAVOConfiguration.HFBLEXEPath))
                    Panels.Add(
                               new WizardPanelInfo
                               {
                                       DescriptiveText =
                                       "The High Frequency Bottom Loss (HFBL) database requires a seperate extraction tool for use.\n\n" +
                                       "Please select the HFBL database extractor.\n\n",
                                       DialogTitle = "Locate the High Frequency Bottom Loss Database Extractor program",
                                       FieldName = "HFBL Extractor",
                                       FileNameFilter =
                                       "Executable files (*.exe)|*.exe|Batch files (*.bat)|*.bat|All files (*.*)|*.*",
                                       PropertyName = "HFBLExtractor",
                               });
                if (string.IsNullOrEmpty(AppSettings.NAVOConfiguration.LFBLEXEPath) ||
                    !File.Exists(AppSettings.NAVOConfiguration.LFBLEXEPath))
                    Panels.Add(
                               new WizardPanelInfo
                               {
                                       DescriptiveText =
                                       "The Low Frequency Bottom Loss (LFBL) databases require a seperate extraction tool for use.\n\n" +
                                       "Please select the LFBL database extractor.\n\n",
                                       DialogTitle = "Locate the Low Frequency Bottom Loss Database Extractor program",
                                       FieldName = "LFBL Extractor",
                                       FileNameFilter =
                                       "Executable files (*.exe)|*.exe|Batch files (*.bat)|*.bat|All files (*.*)|*.*",
                                       PropertyName = "LFBLExtractor",
                               });
                if (string.IsNullOrEmpty(AppSettings.NAEMOTools.RAMExecutable))
                    Panels.Add(
                               new WizardPanelInfo
                               {
                                   DescriptiveText = "Please select the RAM Executable\n\n",
                                       DialogTitle = "Locate the RAM Executable",
                                       FieldName = "RAM Executable",
                                       FileNameFilter = "Executable files (*.exe)|*.exe|All files (*.*)|*.*",
                                       PropertyName = "RAMExe",
                               });


                if (string.IsNullOrEmpty(AppSettings.REFMSSettings.REFMSExecutablePath))
                    Panels.Add(new WizardPanelInfo
                    {
                        DescriptiveText = "Please select the REFMS Executable\n\n",
                            DialogTitle = "Locate the REFMS Executable",
                            FieldName = "REFMS Executable",
                            FileNameFilter = "Executable files (*.exe)|*.exe|All files (*.*)|*.*",
                            PropertyName = "REFMSExe",
                    });

                if (string.IsNullOrEmpty(AppSettings.CASSSettings.CASSExecutablePath) ||
                    !File.Exists(AppSettings.CASSSettings.CASSExecutablePath))
                    Panels.Add(new WizardPanelInfo
                    {
                            DescriptiveText =
                                       "The Comprehensive Acoustic System Simulation (CASS) is a computer program that is designed to investigate the effects of the ocean’s variability on acoustic propagation and underwater acoustic systems." +
                                       "CASS is required for use of the One Navy Model.\n\n" +
                                       "Please select the CASS executable.\n\n",
                            DialogTitle = "Locate the CASS Executable",
                            FieldName = "CASS Executable",
                            FileNameFilter =
                                       "CASS (cass_v4.exe)|cass_v4.exe|Executable Files (*.exe)|*.exe|All files (*.*)|*.*",
                            PropertyName = "CASSExe",
                    });

                if (string.IsNullOrEmpty(AppSettings.CASSSettings.PythonExecutablePath) ||
                    !File.Exists(AppSettings.CASSSettings.PythonExecutablePath))
                    Panels.Add(new WizardPanelInfo
                    {
                            DescriptiveText =
                                       "Some components of the One Navy Model bundled with One Navy Model require Python to run.\n\n" +
                                       "Please select the installed pythonw.exe executable.\n\n" +
                                       "If Python is not installed, the latest version can be downloaded from www.python.org/download/releases/2.7.2/\n\n",
                            DialogTitle = "Locate the Python Executable",
                            FieldName = "Python Executable",
                            FileNameFilter =
                                       "Python (pythonw.exe)|pythonw.exe|Executable Files (*.exe)|*.exe|All files (*.*)|*.*",
                            PropertyName = "PythonExe",
                    });

                if (string.IsNullOrEmpty(AppSettings.CASSSettings.PythonScriptPath) ||
                    !File.Exists(AppSettings.CASSSettings.PythonScriptPath))
                    Panels.Add(new WizardPanelInfo
                    {
                            DescriptiveText =
                                       "The Comprehensive Acoustic System Simulation (CASS) requires a Python script to integrate with the One Navy Model.\n\n",
                            DialogTitle = "Locate the CASS Python script",
                            FieldName = "Python script",
                            FileNameFilter =
                                       "Python Script (mmaea_rev10_7.py)|mmaea_rev10_7.py| Python scripts (*.py)|*.py|All files (*.*)|*.*",
                            PropertyName = "PythonScript",
                    });
            }
            #endregion

            Panels.Add(new WizardPanelInfo
            {
                DescriptiveText =
                           "The wizard has successfully configured all necessary parameters.\n\nPlease press the Finish button to complete configuration and launch One Navy Model, or Cancel to exit without saving changes.",
                IsFileBrowerEnabled = false,
                UserResponse = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
                //will always be a valid location; "next" button always lit.
                PropertyName = "finishScreen",
            });
        }
        #endregion

        public static void LaunchWizardIfNeeded(IUIVisualizerService visualizerService)
        {
            var wizardView = new WizardViewModel();
            if (wizardView.Panels.Count == 2 && wizardView.Panels[0].PropertyName == "welcomeScreen" &&
                wizardView.Panels[1].PropertyName == "finishScreen") wizardView.Panels.Clear(); // if there aren't any configurable panels, delete the welcome screen
            if (wizardView.Panels.Count > 0) //and if any panels remain, show them. 
            {
                var result = visualizerService.ShowDialog("WizardView", wizardView);
                if (result.HasValue && !result.Value) //todo
                {
                    //user pressed cancel.
                    //shut. down. everything.
                }
            }
            //otherwise launch esme.
        }

        #region properties

        #region public AppSettings AppSettings { get; set; }
        static readonly PropertyChangedEventArgs AppSettingsChangedEventArgs =
            ObservableHelper.CreateArgs<WizardViewModel>(x => x.AppSettings);

        AppSettings _appSettings;

        public AppSettings AppSettings
        {
            get { return _appSettings; }
            set
            {
                if (_appSettings == value) return;
                _appSettings = value;
                NotifyPropertyChanged(AppSettingsChangedEventArgs);
            }
        }
        #endregion

        #region public List<WizardPanelInfo> Panels { get; set; }
        static readonly PropertyChangedEventArgs PanelsChangedEventArgs =
            ObservableHelper.CreateArgs<WizardViewModel>(x => x.Panels);

        List<WizardPanelInfo> _panels;

        public List<WizardPanelInfo> Panels
        {
            get { return _panels; }
            set
            {
                if (_panels == value) return;
                _panels = value;
                NotifyPropertyChanged(PanelsChangedEventArgs);
            }
        }
        #endregion

        #region public WizardPanelInfo SelectedPanel { get; set; }
        static readonly PropertyChangedEventArgs SelectedPanelChangedEventArgs =
            ObservableHelper.CreateArgs<WizardViewModel>(x => x.SelectedPanel);

        public int SelectedPanelIndex;

        WizardPanelInfo _selectedPanel;

        public WizardPanelInfo SelectedPanel
        {
            get { return _selectedPanel; }
            set
            {
                if (_selectedPanel == value) return;
                _selectedPanel = value;
                NotifyPropertyChanged(SelectedPanelChangedEventArgs);
            }
        }
        #endregion

        #endregion

        #region commands

        #region NextCommand

        #region public string NextButtonString { get; set; }
        static readonly PropertyChangedEventArgs NextButtonStringChangedEventArgs =
            ObservableHelper.CreateArgs<WizardViewModel>(x => x.NextButtonString);

        string _nextButtonString = "Next";

        public string NextButtonString
        {
            get { return _nextButtonString; }
            set
            {
                if (_nextButtonString == value) return;
                _nextButtonString = value;
                NotifyPropertyChanged(NextButtonStringChangedEventArgs);
            }
        }
        #endregion

        SimpleCommand<object, object> _next;

        public SimpleCommand<object, object> NextCommand
        {
            get
            {
                return _next ??
                       (_next =
                        new SimpleCommand<object, object>(delegate { return SelectedPanel.IsValid; },
                                                          delegate { NextHandler(); }));
            }
        }

        void NextHandler()
        {
            SelectedPanelIndex++;
            if (SelectedPanelIndex < Panels.Count - 1)
            {
                SelectedPanel = Panels[SelectedPanelIndex];
                return;
            }

            if (NextButtonString == "Finish") FinishWizard();

            NextButtonString = "Finish";
            SelectedPanel = Panels.Last();
        }

        void FinishWizard()
        {
            foreach (var panel in Panels)
                switch (panel.PropertyName)
                {
                    case "javaw":
                        AppSettings.NAEMOTools.JavaExecutablePath = panel.UserResponse;
                        break;
                    case "NUWCToolsDirectory":
                        AppSettings.NAEMOTools.NAEMOToolsDirectory = Path.GetDirectoryName(panel.UserResponse);
                        break;
                    case "ScenarioDataDirectory":
                        AppSettings.ValidateScenarioDataDirectory(panel.UserResponse);
                        break;
                    case "GDEM-V":
                        AppSettings.NAVOConfiguration.ValidateGDEMDirectory(panel.UserResponse);
                        break;
                    case "SMGC":
                        AppSettings.NAVOConfiguration.ValidateSMGCDirectory(panel.UserResponse);
                        break;
                    case "BST":
                        AppSettings.NAVOConfiguration.BSTDirectory = panel.UserResponse;
                        break;
                    case "DBDB":
                        AppSettings.NAVOConfiguration.DBDBDirectory = panel.UserResponse;
                        break;
                    case "DBDBExtractor":
                        AppSettings.NAVOConfiguration.DBDBEXEPath = panel.UserResponse;
                        break;
                    case "HFBLExtractor":
                        AppSettings.NAVOConfiguration.HFBLEXEPath = panel.UserResponse;
                        break;
                    case "LFBLExtractor":
                        AppSettings.NAVOConfiguration.LFBLEXEPath = panel.UserResponse;
                        break;
                    case "RAMExe":
                        AppSettings.NAEMOTools.RAMExecutable = panel.UserResponse;
                        break;
                    case "REFMSExe":
                        AppSettings.REFMSSettings.REFMSExecutablePath = panel.UserResponse;
                        break;
                    case "CASSExe":
                        AppSettings.CASSSettings.CASSExecutablePath = panel.UserResponse;
                        break;
                    case "PythonExe":
                        AppSettings.CASSSettings.PythonExecutablePath = panel.UserResponse;
                        break;
                    case "PythonScript":
                        AppSettings.CASSSettings.PythonScriptPath = panel.UserResponse;
                        break;
                    case "welcomeScreen":
                    case "finishScreen":
                        break;
                    default:
                        throw new ApplicationException("typo.");
                }
            AppSettings.Save();
            Globals.AppSettings = AppSettings;
            CloseActivePopUpCommand.Execute(true);
        }
        #endregion

        #region BackCommand
        SimpleCommand<object, object> _back;

        public SimpleCommand<object, object> BackCommand
        {
            get
            {
                return _back ??
                       (_back =
                        new SimpleCommand<object, object>(delegate { return IsBackCommandEnabled; },
                                                          delegate { BackHandler(); }));
            }
        }

        bool IsBackCommandEnabled
        {
            get { return SelectedPanelIndex > 0; }
        }

        void BackHandler()
        {
            SelectedPanelIndex--;
            SelectedPanel = Panels[SelectedPanelIndex];
            if (SelectedPanelIndex < Panels.Count - 1) NextButtonString = "Next";
        }
        #endregion

        #endregion
    }

    public sealed class WizardPanelInfo : ValidatingViewModel
    {
        public WizardPanelInfo()
        {
            IsFileBrowerEnabled = true;
            ValidationRules.AddRange(new List<ValidationRule>
            {
                new ValidationRule
                {
                    PropertyName = "UserResponse",
                    Description = "File must exist.",
                    RuleDelegate = (o, r) =>
                    {
                        var ruleTarget = ((WizardPanelInfo)o).UserResponse;
                        return ((!string.IsNullOrEmpty(ruleTarget)) &&
                                (File.Exists(ruleTarget) || Directory.Exists(ruleTarget)));
                    },
                },
                new ValidationRule
                {
                    PropertyName = "UserResponse",
                    Description = "The GDEM directory must contain many uncompressed .nc files.",
                    RuleDelegate = (o, r) =>
                    {
                        var ruleTarget = ((WizardPanelInfo)o).UserResponse;
                        return PropertyName != "GDEM-V" ||
                               Globals.AppSettings.NAVOConfiguration.ValidateGDEMDirectory(ruleTarget);
                    },
                },
                new ValidationRule
                {
                    PropertyName = "UserResponse",
                    Description = "The SMGC directory must contain many .stt files.",
                    RuleDelegate = (o, r) =>
                    {
                        var ruleTarget = ((WizardPanelInfo)o).UserResponse;
                        return PropertyName != "SMGC" ||
                               Globals.AppSettings.NAVOConfiguration.ValidateSMGCDirectory(ruleTarget);
                    },
                },
                new ValidationRule
                {
                    PropertyName = "UserResponse",
                    Description =
                                         "The Scenario Data Directory must contain the files SimAreas.csv, Species.csv, and PSM.csv.",
                    RuleDelegate = (o, r) =>
                    {
                        var ruleTarget = ((WizardPanelInfo)o).UserResponse;
                        return PropertyName != "ScenarioDataDirectory" ||
                               Globals.AppSettings.ValidateScenarioDataDirectory(ruleTarget);
                    },
                },
                new ValidationRule
                {
                    PropertyName = "UserResponse",
                    Description = "Must be javaw.exe",
                    RuleDelegate = (o, r) =>
                    {
                        var ruleTarget = ((WizardPanelInfo)o).UserResponse;
                        if (PropertyName != "javaw") return true;
                        var fileName = Path.GetFileName(ruleTarget);
                        return fileName != null &&
                               (!string.IsNullOrEmpty(ruleTarget) &&
                                (fileName.ToLowerInvariant() == "javaw.exe"));
                    },
                },
                new ValidationRule
                {
                    PropertyName = "UserResponse",
                    Description = "The directory does not contain the required files",
                    RuleDelegate = (o, r) =>
                    {
                        var ruleTarget = ((WizardPanelInfo)o).UserResponse;
                        if (PropertyName != "NUWCToolsDirectory") return true;
                        if (ruleTarget == null) return false;
                        if (Path.GetFileName(ruleTarget) == "scenario-builder.jar") return File.Exists(ruleTarget);
                        return Directory.Exists(ruleTarget) && (File.Exists(Path.Combine(ruleTarget, "scenario-builder.jar")));
                    },
                },
            });
        }

        #region properties

        #region public string DescriptiveText { get; set; }
        static readonly PropertyChangedEventArgs DescriptiveTextChangedEventArgs =
            ObservableHelper.CreateArgs<WizardPanelInfo>(x => x.DescriptiveText);

        string _descriptiveText;

        #region public Uri Hyperlink { get; set; }
        static readonly PropertyChangedEventArgs HyperlinkChangedEventArgs =
            ObservableHelper.CreateArgs<WizardPanelInfo>(x => x.Hyperlink);

        Uri _hyperlink;

        public Uri Hyperlink
        {
            get { return _hyperlink; }
            set
            {
                if (_hyperlink == value) return;
                _hyperlink = value;
                NotifyPropertyChanged(HyperlinkChangedEventArgs);
            }
        }
        #endregion

        #region public string HyperlinkText { get; set; }
        static readonly PropertyChangedEventArgs HyperlinkTextChangedEventArgs =
            ObservableHelper.CreateArgs<WizardPanelInfo>(x => x.HyperlinkText);

        string _hyperlinkText;

        public string HyperlinkText
        {
            get { return _hyperlinkText; }
            set
            {
                if (_hyperlinkText == value) return;
                _hyperlinkText = value;
                NotifyPropertyChanged(HyperlinkTextChangedEventArgs);
            }
        }
        #endregion

        public string DescriptiveText
        {
            get { return _descriptiveText; }
            set
            {
                if (_descriptiveText == value) return;
                _descriptiveText = value;
                NotifyPropertyChanged(DescriptiveTextChangedEventArgs);
            }
        }
        #endregion

        #region public string UserResponse { get; set; }
        static readonly PropertyChangedEventArgs UserResponseChangedEventArgs =
            ObservableHelper.CreateArgs<WizardPanelInfo>(x => x.UserResponse);

        string _userResponse;

        public string UserResponse
        {
            get { return _userResponse; }
            set
            {
                if (_userResponse == value) return;
                _userResponse = value;
                NotifyPropertyChanged(UserResponseChangedEventArgs);
            }
        }
        #endregion

        public string PropertyName { get; internal set; }

        #region public string FieldName { get; set; }
        static readonly PropertyChangedEventArgs FieldNameChangedEventArgs =
            ObservableHelper.CreateArgs<WizardPanelInfo>(x => x.FieldName);

        string _fieldName;

        public string FieldName
        {
            get { return _fieldName; }
            set
            {
                if (_fieldName == value) return;
                _fieldName = value;
                NotifyPropertyChanged(FieldNameChangedEventArgs);
            }
        }
        #endregion

        #region public string DialogTitle { get; set; }
        static readonly PropertyChangedEventArgs DialogTitleChangedEventArgs =
            ObservableHelper.CreateArgs<WizardPanelInfo>(x => x.DialogTitle);

        string _dialogTitle;

        public string DialogTitle
        {
            get { return _dialogTitle; }
            set
            {
                if (_dialogTitle == value) return;
                _dialogTitle = value;
                NotifyPropertyChanged(DialogTitleChangedEventArgs);
            }
        }
        #endregion

        #region public string FileNameFilter { get; set; }
        static readonly PropertyChangedEventArgs FileNameFilterChangedEventArgs =
            ObservableHelper.CreateArgs<WizardPanelInfo>(x => x.FileNameFilter);

        string _fileNameFilter;

        public string FileNameFilter
        {
            get { return _fileNameFilter; }
            set
            {
                if (_fileNameFilter == value) return;
                _fileNameFilter = value;
                NotifyPropertyChanged(FileNameFilterChangedEventArgs);
            }
        }
        #endregion

        #region public bool IsFileBrowerEnabled { get; set; }
        static readonly PropertyChangedEventArgs IsFileBrowerEnabledChangedEventArgs =
            ObservableHelper.CreateArgs<WizardPanelInfo>(x => x.IsFileBrowerEnabled);

        bool _isFileBrowerEnabled;

        public bool IsFileBrowerEnabled
        {
            get { return _isFileBrowerEnabled; }
            set
            {
                if (_isFileBrowerEnabled == value) return;
                _isFileBrowerEnabled = value;
                NotifyPropertyChanged(IsFileBrowerEnabledChangedEventArgs);
            }
        }
        #endregion

        #endregion
    }
}