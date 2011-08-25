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
            CreatePanels();
            AppSettings = Globals.AppSettings;
            SelectedPanelIndex = 0;
            SelectedPanel = Panels[SelectedPanelIndex];
        }

        void CreatePanels()
        {
            Panels = new List<WizardPanelInfo>
            {
                    new WizardPanelInfo
                    {
                            DescriptiveText ="Welcome to the ESME Workbench Installation Wizard.\n\nThe next several panes of this wizard will guide you in the configuration of required data directories and program locations for proper operation of ESME Workbench.\n\n Please press Next to continue, or Cancel to exit.",
                            IsFileBrowerEnabled = false,
                            UserResponse = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
                            PropertyName = "welcomeScreen"
                    },
                    new WizardPanelInfo
                    {
                            DescriptiveText =
                                    @"Some components of the One Navy Model require Java to run.  Please select the javaw.exe executable installed on your system.  Typically it is installed into Program Files under 'Java'.  If you do not have Java installed, please install the latest Java Runtime Environment (JRE) from http://www.oracle.com/technetwork/java/javase/downloads/index.html . ",
                            FieldName = "Java Executable (javaw.exe)",
                            FileNameFilter = "Java executable (javaw.exe)|javaw.exe|All files (*.*)|*.*",
                            IsDirectoryBrowser = false,
                            PropertyName = "javaw"
                    },
                    new WizardPanelInfo
                    {
                            DescriptiveText =
                                    "The Scenario Data Directory is the default location for all experimental data.  It is typically named Sim Areas, and contains the files SimAreas.csv, PSM.csv, and Species.csv.",
                            FieldName = "Scenario Data Directory",
                            IsDirectoryBrowser = true,
                            PropertyName = "ScenarioDataDirectory",
                    },
                    new WizardPanelInfo
                    {
                            DescriptiveText =
                                    "The Oceanographic and Atmospheric Master Library(OAML) provides key environmental information necessary for operation.  The Generalized Digital Environmental Model (GDEM) is a required OAML database. If you do not have this database, a copy can be downloaded from http://esme.bu.edu/ ",
                            FieldName = "GDEM-V",
                            IsDirectoryBrowser = true,
                            PropertyName = "GDEM-V",
                    },
                    new WizardPanelInfo
                    {
                            DescriptiveText =
                                    "The The Oceanographic and Atmospheric Master Library(OAML) provides key environmental information necessary for operation.  The Surface Marine Gridded Climatological (SMGC) database is a required OAML database. If you do not have this database, a copy can be downloaded from http://esme.bu.edu/ ",
                            FieldName = "SMGC",
                            IsDirectoryBrowser = true,
                            PropertyName = "SMGC",
                    },
                    new WizardPanelInfo
                    {
                            DescriptiveText =
                                    "The Oceanographic and Atmospheric Master Library(OAML) provides key environmental information necessary for operation.  The Bottom Sediment Type (BST) database is a required OAML database. If you do not have this database, a copy can be downloaded from http://esme.bu.edu/ ",
                            FieldName = "BST",
                            IsDirectoryBrowser = false,
                            FileNameFilter = "HDF5 files (*.h5)|*.h5|All files (*.*)|*.*",
                            PropertyName = "BST",
                    },
                    new WizardPanelInfo
                    {
                            DescriptiveText =
                                    "The Oceanographic and Atmospheric Master Library(OAML) provides key environmental information necessary for operation.  The Digital Bathymetric DataBase (DBDB) is a required OAML database. If you do not have this database, a copy can be downloaded from http://esme.bu.edu/ ",
                            FieldName = "DBDB",
                            IsDirectoryBrowser = false,
                            FileNameFilter = "HDF5 files (*.h5)|*.h5|All files (*.*)|*.*",
                            PropertyName = "DBDB",
                    },
                    new WizardPanelInfo
                    {
                            DescriptiveText =
                                    "The Digital Bathymetric DataBase (DBDB) requires a seperate extraction tool for use. If you do not have this tool, a copy can be downloaded from http://esme.bu.edu/  ",
                            FieldName = "DBDB Extractor (dbv5_command.exe)",
                            IsDirectoryBrowser = false,
                            FileNameFilter =
                                    "DBDB Extractor (dbv5_command.exe)|dbv5_command.exe|Executable files (*.exe)|*.exe|Batch files (*.bat)|*.bat|All files (*.*)|*.*",
                            PropertyName = "DBDBExtractor",
                    },
            };
            if (Configuration.IsClassifiedModel)
            {
                Panels.Add(new WizardPanelInfo
                {
                        DescriptiveText =
                                   "The High Frequency Bottom Loss (HFBL) database requires a seperate extraction tool for use.",
                        FieldName = "HFBL Extractor",
                        IsDirectoryBrowser = false,
                        FileNameFilter = "Executable files (*.exe)|*.exe|Batch files (*.bat)|*.bat|All files (*.*)|*.*",
                        PropertyName = "HFBLExtractor",
                });
                Panels.Add(new WizardPanelInfo
                {
                        DescriptiveText =
                                   "The Low Frequency Bottom Loss (LFBL) databases require a seperate extraction tool for use.",
                        FieldName = "LFBL Extractor",
                        IsDirectoryBrowser = false,
                        FileNameFilter = "Executable files (*.exe)|*.exe|Batch files (*.bat)|*.bat|All files (*.*)|*.*",
                        PropertyName = "LFBLExtractor",
                });
            }
        }

        public static void LaunchWizardIfNeeded(IUIVisualizerService visualizerService)
        {
            var wizardView = new WizardViewModel();
            foreach (var panel in wizardView.Panels)
                switch (panel.PropertyName)
                {
                    case "javaw":
                        panel.Skip = (!string.IsNullOrEmpty(Globals.AppSettings.NAEMOTools.JavaExecutablePath)) &&
                                     File.Exists(Globals.AppSettings.NAEMOTools.JavaExecutablePath);
                        break;
                    case "ScenarioDataDirectory":
                        panel.Skip = (!string.IsNullOrEmpty(Globals.AppSettings.ScenarioDataDirectory) &&
                                      Directory.Exists(Globals.AppSettings.ScenarioDataDirectory));
                        break;
                    case "GDEM-V":
                        panel.Skip = (!string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.GDEMDirectory) &&
                                      Directory.Exists(Globals.AppSettings.NAVOConfiguration.GDEMDirectory));
                        break;
                    case "SMGC":
                        panel.Skip = (!string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.SMGCDirectory) &&
                                      Directory.Exists(Globals.AppSettings.NAVOConfiguration.SMGCDirectory));
                        break;
                    case "BST":
                        panel.Skip = (!string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.BSTDirectory) &&
                                      File.Exists(Globals.AppSettings.NAVOConfiguration.BSTDirectory));
                        break;
                    case "DBDB":
                        panel.Skip = (!string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.DBDBDirectory) &&
                                      File.Exists(Globals.AppSettings.NAVOConfiguration.DBDBDirectory));
                        break;
                    case "DBDBExtractor":
                        panel.Skip = (!string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.DBDBEXEPath) &&
                                      File.Exists(Globals.AppSettings.NAVOConfiguration.DBDBEXEPath));
                        break;
                    case "HFBLExtractor":
                        panel.Skip = (!string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.HFBLEXEPath) &&
                                      File.Exists(Globals.AppSettings.NAVOConfiguration.HFBLEXEPath));
                        break;
                    case "LFBLExtractor":
                        panel.Skip = (!string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.LFBLEXEPath) &&
                                      File.Exists(Globals.AppSettings.NAVOConfiguration.LFBLEXEPath));
                        break;
                    case "welcomeScreen":
                        panel.Skip = false;
                        break;
                    default:
                        throw new ApplicationException("typo.");
                }
            wizardView.Panels.RemoveAll(panel => panel.Skip);
            if (wizardView.Panels.Count == 1 && wizardView.Panels[0].PropertyName == "welcomeScreen") wizardView.Panels.RemoveAt(0);
            if (wizardView.Panels.Count > 0)
            {
                var result = visualizerService.ShowDialog("WizardView", wizardView);
                if (result.HasValue && !result.Value)
                {
                    //user pressed cancel.
                    //shut. down. everything.
                }
            }
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
            if (SelectedPanelIndex < Panels.Count)
            {
                SelectedPanel = Panels[SelectedPanelIndex];
                return;
            }
            if (NextButtonString == "Finish")
            {
                FinishWizard();
                return;
            }
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
                        AppSettings.ScenarioDataDirectory = panel.UserResponse;
                        break;
                    case "GDEM-V":
                        AppSettings.NAVOConfiguration.GDEMDirectory = panel.UserResponse;
                        break;
                    case "SMGC":
                        AppSettings.NAVOConfiguration.SMGCDirectory = panel.UserResponse;
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
                            Description = "File must Exist",
                            RuleDelegate = (o, r) =>
                            {
                                var ruleTarget = ((WizardPanelInfo)o).UserResponse;
                                return ((!string.IsNullOrEmpty(ruleTarget)) &&
                                        (File.Exists(ruleTarget) || Directory.Exists(ruleTarget)));
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

        public string PropertyName { get; set; }

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

        #region public bool IsDirectoryBrowser { get; set; }
        public bool IsDirectoryBrowser
        {
            get { return _isDirectoryBrowser; }
            set
            {
                if (_isDirectoryBrowser == value) return;
                _isDirectoryBrowser = value;
                NotifyPropertyChanged(IsDirectoryBrowserChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsDirectoryBrowserChangedEventArgs =
                ObservableHelper.CreateArgs<WizardPanelInfo>(x => x.IsDirectoryBrowser);

        bool _isDirectoryBrowser;
        #endregion

        public bool IsFileBrowerEnabled { get; set; }

        public bool Skip { get; set; }
    }
}