using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using ESME.Data;
using Cinch;
using HRC.Validation;

namespace ESME.Views.InstallationWizard
{
    public sealed class WizardViewModel : ViewModelBase
    {
        public WizardViewModel()
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
                                    "Welcome to the ESME Workbench Configuration Wizard.\n\n" +
                                    "The next several panes of this wizard will help complete the configuration of ESME Workbench.\n\n" +
                                    "Please press Next to continue, or Cancel to exit without saving changes.",
                            IsFileBrowerEnabled = false,
                            UserResponse = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
                            //will always be a valid location; "next" button always lit.
                            PropertyName = "welcomeScreen",
                    },
            };

            if (string.IsNullOrEmpty(AppSettings.NAEMOTools.JavaExecutablePath) || !File.Exists(AppSettings.NAEMOTools.JavaExecutablePath))
                Panels.Add(
                           new WizardPanelInfo
                           {
                                   DescriptiveText =
                                   "Some components of the One Navy Model bundled with ESME Workbench require Java to run.\n\n"+
                                   "Please select the installed javaw.exe executable.\n\n" +
                                   "If Java is not installed, the latest Java Runtime Environment (JRE) can be downloaded from http://www.oracle.com/technetwork/java/javase/downloads/index.html",
                                   Hyperlink = new Uri("http://www.oracle.com/technetwork/java/javase/downloads/index.html"),
                                   HyperlinkText = "the Oracle Java download page.",
                                   FieldName = "Java Executable (javaw.exe)",
                                   UserResponse = System.Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFilesX86)+@"\",
                                   FileNameFilter = "Java executable (javaw.exe)|javaw.exe|All files (*.*)|*.*",
                                   PropertyName = "javaw",
                           });

            if (string.IsNullOrEmpty(AppSettings.ScenarioDataDirectory) || !Directory.Exists(AppSettings.ScenarioDataDirectory))
                Panels.Add(
                           new WizardPanelInfo
                           {
                                   DescriptiveText =
                                   "The Scenario Data Directory is the default location for all experimental data.  It is typically named Sim Areas, and must contain " +
                                   "the files SimAreas.csv, PSM.csv, and Species.csv.",
                                   FieldName = "Scenario Data Directory",
                                   DialogTitle = "Locate the SimAreas.csv file",
                                   FileNameFilter = "Simulation Areas File (SimAreas.csv)|SimAreas.csv|All files (*.*)|*.*",
                                   PropertyName = "ScenarioDataDirectory",
                           });

            if (string.IsNullOrEmpty(AppSettings.NAVOConfiguration.GDEMDirectory) || !Directory.Exists(AppSettings.NAVOConfiguration.GDEMDirectory))
                Panels.Add(
                           new WizardPanelInfo
                           {
                                   DescriptiveText =
                                   "The Oceanographic and Atmospheric Master Library (OAML) provides key environmental information necessary for operation.  " +
                                   "The Generalized Digital Environmental Model (GDEM) is a required OAML database organized as a collection of NetCDF files.\n\n" +
                                   "Please select any .nc file in the directory containing all of the GDEM data.\n\n" +
                                   "If you do not have this database, a copy can be downloaded from http://esme.bu.edu",
                                   Hyperlink = new Uri("http://esme.bu.edu"),
                                   HyperlinkText = "the ESME Workbench webpage.",
                                   FieldName = "GDEM-V",
                                   DialogTitle="Choose one GDEM database file",
							       FileNameFilter="NetCDF files (*.nc)|*.nc|All files (*.*)|*.*",
                                   PropertyName = "GDEM-V",
                           });
            if (string.IsNullOrEmpty(AppSettings.NAVOConfiguration.SMGCDirectory) || !Directory.Exists(AppSettings.NAVOConfiguration.SMGCDirectory))
                Panels.Add(
                           new WizardPanelInfo
                           {
                                   DescriptiveText =
                                   "The The Oceanographic and Atmospheric Master Library (OAML) provides key environmental information necessary for operation.  " +
                                   "The Surface Marine Gridded Climatological (SMGC) database is a required OAML database organized as a collection of 64,800 .stt files\n\n " +
                                   "Please select any .stt file a directory tree containing all of the SMGC data.\n\n"+
                                   "If you do not have this database, a copy can be downloaded from http://esme.bu.edu",
                                   Hyperlink = new Uri("http://esme.bu.edu"),
                                   HyperlinkText = "the ESME Workbench webpage.",
                                   FieldName = "SMGC",
                                   DialogTitle="Choose one SMGC database file (*.stt)",
                                   FileNameFilter="SMGC files (*.stt)|*.stt|All files (*.*)|*.*",
                                   PropertyName = "SMGC",
                           });
            if (string.IsNullOrEmpty(AppSettings.NAVOConfiguration.BSTDirectory) || !File.Exists(AppSettings.NAVOConfiguration.BSTDirectory))
                Panels.Add(
                           new WizardPanelInfo
                           {
                                   DescriptiveText =
                                   "The Oceanographic and Atmospheric Master Library (OAML) provides key environmental information necessary for operation.  " +
                                   "The High-Frequency Environment Acoustic (HFEVA) Bottom Sediment Type (BST) database is a required OAML database packaged as a standalone HDF5 file.\n\n" +
                                   "Please select the .h5 file that contains the HFEVA BST database.\n\n"+
                                   "If you do not have this database, a copy can be downloaded from http://esme.bu.edu",
                                   Hyperlink = new Uri("http://esme.bu.edu"),
                                   HyperlinkText = "the ESME Workbench webpage.",
                                   FieldName = "BST",
                                   FileNameFilter = "HDF5 files (*.h5)|*.h5|All files (*.*)|*.*",
                                   PropertyName = "BST",
                           });
            if (string.IsNullOrEmpty(AppSettings.NAVOConfiguration.DBDBDirectory) || !File.Exists(AppSettings.NAVOConfiguration.DBDBDirectory))
                Panels.Add(
                           new WizardPanelInfo
                           {
                                   DescriptiveText =
                                   "The Oceanographic and Atmospheric Master Library (OAML) provides key environmental information necessary for operation.  " +
                                   "The Digital Bathymetric DataBase (DBDB) is a required OAML database packaged as a standalone HDF5 file.\n\n" +
                                   "Please select the .h5 file that contains the DBDB.\n\n"+
                                   "If you do not have this database, a copy can be downloaded from http://esme.bu.edu",
                                   Hyperlink = new Uri("http://esme.bu.edu"),
                                   HyperlinkText = "the ESME Workbench webpage.",
                                   FieldName = "DBDB",
                                   FileNameFilter = "HDF5 files (*.h5)|*.h5|All files (*.*)|*.*",
                                   PropertyName = "DBDB",
                           });
            if (string.IsNullOrEmpty(AppSettings.NAVOConfiguration.DBDBEXEPath) || !File.Exists(AppSettings.NAVOConfiguration.DBDBEXEPath))
                Panels.Add(
                           new WizardPanelInfo
                           {
                                   DescriptiveText =
                                   "The Digital Bathymetric DataBase (DBDB) requires a seperate extraction tool for use.\n\n" +
                                   "Please select the DBDB extractor.\n\n"+
                                   "If you do not have this tool, a copy can be downloaded from http://esme.bu.edu",
                                   Hyperlink = new Uri("http://esme.bu.edu"),
                                   HyperlinkText = "the ESME Workbench webpage.",
                                   FieldName = "DBDB Extractor (dbv5_command.exe)",
                                   FileNameFilter =
                                   "DBDB Extractor (dbv5_command.exe)|dbv5_command.exe|Executable files (*.exe)|*.exe|Batch files (*.bat)|*.bat|All files (*.*)|*.*",
                                   PropertyName = "DBDBExtractor",
                           });

            if (Configuration.IsClassifiedModel && (string.IsNullOrEmpty(AppSettings.NAVOConfiguration.HFBLEXEPath) || !File.Exists(AppSettings.NAVOConfiguration.HFBLEXEPath)))
                Panels.Add(
                           new WizardPanelInfo
                           {
                                   DescriptiveText =
                                   "The High Frequency Bottom Loss (HFBL) database requires a seperate extraction tool for use.\n\n"+
                                   "Please select the HFBL database extractor.\n\n",
                                   FieldName = "HFBL Extractor",
                                   FileNameFilter =
                                   "Executable files (*.exe)|*.exe|Batch files (*.bat)|*.bat|All files (*.*)|*.*",
                                   PropertyName = "HFBLExtractor",
                           });
            if (Configuration.IsClassifiedModel && (string.IsNullOrEmpty(AppSettings.NAVOConfiguration.LFBLEXEPath) || !File.Exists(AppSettings.NAVOConfiguration.LFBLEXEPath)))
                Panels.Add(
                           new WizardPanelInfo
                           {
                                   DescriptiveText =
                                   "The Low Frequency Bottom Loss (LFBL) databases require a seperate extraction tool for use.\n\n"+
                                   "Please select the LFBL database extractor.\n\n",
                                   FieldName = "LFBL Extractor",
                                   FileNameFilter =
                                   "Executable files (*.exe)|*.exe|Batch files (*.bat)|*.bat|All files (*.*)|*.*",
                                   PropertyName = "LFBLExtractor",
                           });

            Panels.Add(new WizardPanelInfo
            {
                    DescriptiveText = "The wizard has successfully configured all necessary parameters.\n\nPlease press the Finish button to complete configuration and launch ESME Workbench, or Cancel to exit without saving changes.",
                    IsFileBrowerEnabled = false,
                    UserResponse = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
                    //will always be a valid location; "next" button always lit.
                    PropertyName = "finishScreen",

            });
        }

        public static void LaunchWizardIfNeeded(IUIVisualizerService visualizerService)
        {
            var wizardView = new WizardViewModel();
            if (wizardView.Panels.Count == 2 && wizardView.Panels[0].PropertyName == "welcomeScreen" && wizardView.Panels[1].PropertyName=="finishScreen") wizardView.Panels.Clear(); // if there aren't any configurable panels, delete the welcome screen
            if (wizardView.Panels.Count > 0) //and if any panels remain, show them. 
            {
                var result = visualizerService.ShowDialog("WizardView", wizardView);
                if (result.HasValue && !result.Value)//todo
                {
                    //user pressed cancel.
                    //shut. down. everything.
                }
            }
            //otherwise launch esme.
        }

        #region public AppSettings AppSettings { get; set; }
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

        static readonly PropertyChangedEventArgs AppSettingsChangedEventArgs =
                ObservableHelper.CreateArgs<WizardViewModel>(x => x.AppSettings);

        AppSettings _appSettings;
        #endregion

        #region public List<WizardPanelInfo> Panels { get; set; }
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

        static readonly PropertyChangedEventArgs PanelsChangedEventArgs =
                ObservableHelper.CreateArgs<WizardViewModel>(x => x.Panels);

        List<WizardPanelInfo> _panels;
        #endregion

        #region public WizardPanelInfo SelectedPanel { get; set; }
        public int SelectedPanelIndex;

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

        static readonly PropertyChangedEventArgs SelectedPanelChangedEventArgs =
                ObservableHelper.CreateArgs<WizardViewModel>(x => x.SelectedPanel);

        WizardPanelInfo _selectedPanel;
        #endregion

        #region NextCommand

        #region public string NextButtonString { get; set; }
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

        static readonly PropertyChangedEventArgs NextButtonStringChangedEventArgs =
                ObservableHelper.CreateArgs<WizardViewModel>(x => x.NextButtonString);

        string _nextButtonString = "Next";
        #endregion

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

        SimpleCommand<object, object> _next;

        void NextHandler()
        {
            SelectedPanelIndex++;
            if (SelectedPanelIndex < Panels.Count-1)
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

        SimpleCommand<object, object> _back;

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
                                if (PropertyName != "GDEM-V") return true;
                                return Globals.AppSettings.NAVOConfiguration.ValidateGDEMDirectory(ruleTarget);
                            },  
                    },
                    new ValidationRule
                    {
                            PropertyName = "UserResponse",
                            Description = "The SMGC directory must contain many .stt files.",
                            RuleDelegate = (o, r) =>
                            {
                                var ruleTarget = ((WizardPanelInfo)o).UserResponse;
                                if (PropertyName != "SMGC") return true;
                                return Globals.AppSettings.NAVOConfiguration.ValidateSMGCDirectory(ruleTarget);
                            },
                    },
                    new ValidationRule
                    {
                            PropertyName = "UserResponse",
                            Description = "The Scenario Data Directory must contain the files SimAreas.csv, Species.csv, and PSM.csv.",
                            RuleDelegate = (o, r) =>
                            {
                                var ruleTarget = ((WizardPanelInfo)o).UserResponse;
                                if (PropertyName != "ScenarioDataDirectory") return true;
                                return Globals.AppSettings.ValidateScenarioDataDirectory(ruleTarget);
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
                                return (!string.IsNullOrEmpty(ruleTarget) && (Path.GetFileName(ruleTarget).ToLowerInvariant()=="javaw.exe"));
                            },
                    },                                                               
            });
        }

        #region public string DescriptiveText { get; set; }
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

        static readonly PropertyChangedEventArgs DescriptiveTextChangedEventArgs =
                ObservableHelper.CreateArgs<WizardPanelInfo>(x => x.DescriptiveText);

        string _descriptiveText;

        #region public Uri Hyperlink { get; set; }

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

        static readonly PropertyChangedEventArgs HyperlinkChangedEventArgs = ObservableHelper.CreateArgs<WizardPanelInfo>(x => x.Hyperlink);
        Uri _hyperlink;

        #endregion

        #region public string HyperlinkText { get; set; }

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

        static readonly PropertyChangedEventArgs HyperlinkTextChangedEventArgs = ObservableHelper.CreateArgs<WizardPanelInfo>(x => x.HyperlinkText);
        string _hyperlinkText;

        #endregion

        #endregion

        #region public string UserResponse { get; set; }
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

        static readonly PropertyChangedEventArgs UserResponseChangedEventArgs =
                ObservableHelper.CreateArgs<WizardPanelInfo>(x => x.UserResponse);

        string _userResponse;
        #endregion

        public string PropertyName { get; internal set; }

        #region public string FieldName { get; set; }
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

        static readonly PropertyChangedEventArgs FieldNameChangedEventArgs =
                ObservableHelper.CreateArgs<WizardPanelInfo>(x => x.FieldName);

        string _fieldName;
        #endregion

        #region public string DialogTitle { get; set; }

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

        static readonly PropertyChangedEventArgs DialogTitleChangedEventArgs = ObservableHelper.CreateArgs<WizardPanelInfo>(x => x.DialogTitle);
        string _dialogTitle;

        #endregion

        #region public string FileNameFilter { get; set; }
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

        static readonly PropertyChangedEventArgs FileNameFilterChangedEventArgs =
                ObservableHelper.CreateArgs<WizardPanelInfo>(x => x.FileNameFilter);

        string _fileNameFilter;
        #endregion

        #region public bool IsFileBrowerEnabled { get; set; }

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

        static readonly PropertyChangedEventArgs IsFileBrowerEnabledChangedEventArgs = ObservableHelper.CreateArgs<WizardPanelInfo>(x => x.IsFileBrowerEnabled);
        bool _isFileBrowerEnabled;

        #endregion
    }
}