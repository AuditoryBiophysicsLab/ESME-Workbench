using System;
using Cinch;
using ESMEWorkBench.ViewModels.Main;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.Ribbon
{
    [ExportViewModel("RibbonViewModel")]
    public class RibbonViewModel : ViewModelBase
    {
        public RibbonViewModel(MainViewModel mainViewModel, MapViewModel mapViewModel)
        {
            MainViewModel = mainViewModel;
            MapViewModel = mapViewModel;
            DisabledCommand = new SimpleCommand<object, object>(delegate { return false; }, delegate { });
        }

        public MainViewModel MainViewModel { get; private set; }
        public MapViewModel MapViewModel { get; private set; }

        public SimpleCommand<Object, Object> DisabledCommand { get; private set; }

#if false
        void CreateRibbonBindings()
        {
            ApplicationMenuItems = new ApplicationMenuItemList
                                   {
                                       new ApplicationMenuItemDataViewModel
                                       {
                                           RecentFiles = new RecentFileList
                                                         {
                                                             MaxNumberOfFiles = 9,
                                                             Persister = new RegistryPersister(),
                                                         },
                                           MenuItems = new MenuItemList
                                                       {
                                                           new MenuItemDataViewModel
                                                           {
                                                               Label = "Save Experiment",
                                                               LargeImage = new Uri("Images/LargeIcons/save-icon.png", UriKind.Relative),
                                                               SmallImage = new Uri("Images/SmallIcons/save-icon.png", UriKind.Relative),
                                                               ToolTipTitle = "Save Experiment",
                                                               ToolTipDescription = "Save the current experiment",
                                                               Command = DisabledCommand,
                                                           },
                                                           new MenuItemDataViewModel
                                                           {
                                                               Label = "Open Experiment",
                                                               LargeImage = new Uri("Images/LargeIcons/open-icon.png", UriKind.Relative),
                                                               SmallImage = new Uri("Images/SmallIcons/open-icon.png", UriKind.Relative),
                                                               ToolTipTitle = "Open Experiment",
                                                               ToolTipDescription = "Open a previously saved experiment from an experiment file",
                                                               Command = DisabledCommand,
                                                           },
                                                           new MenuItemDataViewModel
                                                           {
                                                               Label = "Close Experiment",
                                                               LargeImage = new Uri("Images/LargeIcons/close-icon.png", UriKind.Relative),
                                                               SmallImage = new Uri("Images/SmallIcons/close-icon.png", UriKind.Relative),
                                                               ToolTipTitle = "Close Experiment",
                                                               ToolTipDescription = "Close the current experiment",
                                                               Command = DisabledCommand,
                                                           },
                                                           new MenuItemDataViewModel
                                                           {
                                                               Label = "New Experiment",
                                                               LargeImage = new Uri("Images/LargeIcons/new-icon.png", UriKind.Relative),
                                                               SmallImage = new Uri("Images/SmallIcons/new-icon.png", UriKind.Relative),
                                                               ToolTipTitle = "New Experiment",
                                                               ToolTipDescription = "Create a new experiment",
                                                               Command = DisabledCommand,
                                                           },
                                                           new MenuItemDataViewModel
                                                           {
                                                               Label = "Info",
                                                               LargeImage = new Uri("Images/LargeIcons/about-icon.png", UriKind.Relative),
                                                               SmallImage = new Uri("Images/SmallIcons/about-icon.png", UriKind.Relative),
                                                               ToolTipTitle = "Info",
                                                               ToolTipDescription = "Experiment information",
                                                               Command = DisabledCommand,
                                                           },
                                                           new MenuItemDataViewModel
                                                           {
                                                               Label = "Help",
                                                               LargeImage = new Uri("Images/LargeIcons/Button-Help-icon.png", UriKind.Relative),
                                                               SmallImage = new Uri("Images/SmallIcons/Button-Help-icon.png", UriKind.Relative),
                                                               ToolTipTitle = "Help",
                                                               ToolTipDescription = "Help and About information for ESME WorkBench",
                                                               Command = DisabledCommand,
                                                           },
                                                           new MenuItemDataViewModel
                                                           {
                                                               Label = "Options",
                                                               LargeImage = new Uri("Images/LargeIcons/Options.png", UriKind.Relative),
                                                               SmallImage = new Uri("Images/SmallIcons/Options.png", UriKind.Relative),
                                                               ToolTipTitle = "Options",
                                                               ToolTipDescription = "Edit application options and settings",
                                                               Command = MainViewModel.EditOptionsCommand,
                                                           },
                                                           new MenuItemDataViewModel
                                                           {
                                                               Label = "Exit",
                                                               LargeImage = new Uri("Images/LargeIcons/Button-Close-icon.png", UriKind.Relative),
                                                               SmallImage = new Uri("Images/SmallIcons/Button-Close-icon.png", UriKind.Relative),
                                                               ToolTipTitle = "Exit",
                                                               ToolTipDescription = "Close the ESME WorkBench",
                                                               Command = ApplicationCommands.Close,
                                                           },
                                                       },
                                       },
                                   };
            Tabs = new TabList
                   {
                       #region Experiment Tab
                       new TabDataViewModel
                       {
                           Header = "Experiment",
                           Groups = new GroupList
                                    {
                                        new GroupDataViewModel
                                        {
                                            Label = "Data",
                                            Controls = new ControlList
                                                       {
                                                           new ButtonDataViewModel
                                                           {
                                                               Label = "Reset",
                                                               LargeImage = new Uri("Images/LargeIcons/new-icon.png", UriKind.Relative),
                                                               SmallImage = new Uri("Images/SmallIcons/new-icon.png", UriKind.Relative),
                                                               ToolTipTitle = "Reset",
                                                               ToolTipDescription = "Clear all layers that have been added to the map",
                                                               Command = MapViewModel.ClearAllLayersCommand,
                                                           },
                                                           new ButtonDataViewModel
                                                           {
                                                               Label = "Shapefile",
                                                               SmallImage = new Uri("Images/SmallIcons/AddFile.png", UriKind.Relative),
                                                               ToolTipTitle = "Shapefile",
                                                               ToolTipDescription = "Add an ESRI Shapefile to the map",
                                                               Command = MapViewModel.AddShapefileCommand,
                                                           },
                                                           new ButtonDataViewModel
                                                           {
                                                               Label = "Overlay",
                                                               SmallImage = new Uri("Images/SmallIcons/AddFile.png", UriKind.Relative),
                                                               ToolTipTitle = "Overlay",
                                                               ToolTipDescription = "Add a NUWC Overlay file to the map",
                                                               Command = MapViewModel.AddOverlayFileCommand,
                                                           },
                                                           new ButtonDataViewModel
                                                           {
                                                               Label = "Scenario",
                                                               SmallImage = new Uri("Images/SmallIcons/AddFile.png", UriKind.Relative),
                                                               ToolTipTitle = "Scenario",
                                                               ToolTipDescription = "Add a NUWC Scenario file to the map",
                                                               Command = MapViewModel.AddScenarioFileCommand, //Command = DisabledCommand,
                                                           },
                                                       },
                                        },
                                        new GroupDataViewModel
                                        {
                                            Label = "Show",
                                            Controls = new ControlList
                                                       {
                                                           new CheckBoxDataViewModel
                                                           {
                                                               Label = "Base Map",
                                                               ToolTipTitle = "Base Map",
                                                               ToolTipDescription = "Toggles the base map display",
                                                               IsChecked = Settings.Default.ShowBasemap,
                                                               Command = MapViewModel.ToggleBaseMapDisplayCommand,
                                                           },
                                                           new CheckBoxDataViewModel
                                                           {
                                                               Label = "Grid Overlay",
                                                               ToolTipTitle = "Grid Overlay",
                                                               ToolTipDescription = "Toggles the grid overlay",
                                                               IsChecked = Settings.Default.ShowGrid,
                                                               Command = MapViewModel.ToggleGridOverlayDisplayCommand,
                                                           },
                                                           new CheckBoxDataViewModel
                                                           {
                                                               Label = "Pan/Zoom",
                                                               ToolTipTitle = "Pan/Zoom",
                                                               ToolTipDescription = "Toggles the pan/zoom control",
                                                               IsChecked = Settings.Default.ShowPanZoom,
                                                               Command = MapViewModel.TogglePanZoomDisplayCommand,
                                                           },
                                                       },
                                        },
                                        new GroupDataViewModel
                                        {
                                            Label = "Sounds",
                                            Controls = new ControlList
                                                       {
                                                           new ButtonDataViewModel
                                                           {
                                                               Label = "Analysis Point",
                                                               LargeImage = new Uri("Images/LargeIcons/bullet-2-icon.png", UriKind.Relative),
                                                               SmallImage = new Uri("Images/SmallIcons/bullet-2-icon.png", UriKind.Relative),
                                                               ToolTipTitle = "Analysis Point",
                                                               ToolTipDescription = "Add a new analysis point to the experiment (not functional)",
                                                               Command = MainViewModel.TestTransmissionLossViewCommand,
                                                           },
                                                           new ButtonDataViewModel
                                                           {
                                                               Label = "Fixed Source",
                                                               LargeImage = new Uri("Images/LargeIcons/Sound.png", UriKind.Relative),
                                                               SmallImage = new Uri("Images/SmallIcons/Sound.png", UriKind.Relative),
                                                               ToolTipTitle = "Fixed Source",
                                                               ToolTipDescription = "Add a new fixed sound source to the experiment (not functional)",
                                                               Command = DisabledCommand,
                                                           },
                                                           new ButtonDataViewModel
                                                           {
                                                               Label = "Quick Look",
                                                               LargeImage = new Uri("Images/LargeIcons/Button-Play-icon.png", UriKind.Relative),
                                                               SmallImage = new Uri("Images/SmallIcons/Button-Play-icon.png", UriKind.Relative),
                                                               ToolTipTitle = "Quick Look",
                                                               ToolTipDescription = "Add a new quick look point to the experiment (not functional)",
                                                               Command = DisabledCommand,
                                                           },
                                                       },
                                        },
                                    },
                       },

                       #endregion
                       #region Scenario Tab
                       new TabDataViewModel
                       {
                           Header = "Scenario",
                           Groups = new GroupList
                                    {
                                        new GroupDataViewModel
                                        {
                                            Label = "Scenario File",
                                            Controls = new ControlList
                                                       {
                                                           new ButtonDataViewModel
                                                           {
                                                               Label = "Open",
                                                               LargeImage = new Uri("Images/LargeIcons/open-icon.png", UriKind.Relative),
                                                               SmallImage = new Uri("Images/SmallIcons/open-icon.png", UriKind.Relative),
                                                               ToolTipTitle = "Open scenario file",
                                                               ToolTipDescription = "Open a scenario file (not functional)",
                                                               Command = DisabledCommand,
                                                           },
                                                           new ButtonDataViewModel
                                                           {
                                                               Label = "Close",
                                                               LargeImage = new Uri("Images/LargeIcons/close-icon.png", UriKind.Relative),
                                                               SmallImage = new Uri("Images/SmallIcons/close-icon.png", UriKind.Relative),
                                                               ToolTipTitle = "Close scenario file",
                                                               ToolTipDescription = "Close the current scenario file (not functional)",
                                                               Command = DisabledCommand,
                                                           },
                                                           new ButtonDataViewModel
                                                           {
                                                               Label = "Edit",
                                                               LargeImage = new Uri("Images/LargeIcons/AddFile.png", UriKind.Relative),
                                                               SmallImage = new Uri("Images/SmallIcons/AddFile.png", UriKind.Relative),
                                                               ToolTipTitle = "Edit scenario file",
                                                               ToolTipDescription = "Launch the scenario editor (not functional)",
                                                               Command = MainViewModel.LaunchExternalProgramCommand,
                                                               CommandParameter = MainViewModel.AppSettings.ScenarioEditorExecutablePath,
                                                           },
                                                           new ButtonDataViewModel
                                                           {
                                                               Label = "Run",
                                                               LargeImage = new Uri("Images/LargeIcons/Button-Play-icon.png", UriKind.Relative),
                                                               SmallImage = new Uri("Images/SmallIcons/Button-Play-icon.png", UriKind.Relative),
                                                               ToolTipTitle = "Run scenario",
                                                               ToolTipDescription = "Run the scenario (not functional)",
                                                               Command = DisabledCommand,
                                                           },
                                                       },
                                        },
                                    },
                       },

                       #endregion
                       #region Environment Tab
                       new TabDataViewModel
                       {
                           Header = "Environment",
                           Groups = new GroupList
                                    {
                                        new GroupDataViewModel
                                        {
                                            Label = "Location",
                                            Controls = new ControlList
                                                       {
                                                           new ButtonDataViewModel
                                                           {
                                                               Label = "Open",
                                                               LargeImage = new Uri("Images/LargeIcons/open-icon.png", UriKind.Relative),
                                                               SmallImage = new Uri("Images/SmallIcons/open-icon.png", UriKind.Relative),
                                                               ToolTipTitle = "Open environment file",
                                                               ToolTipDescription = "Open an environment file (not functional)",
                                                               Command = MapViewModel.OpenEnvironmentFileCommand,
                                                           },
                                                           new ButtonDataViewModel
                                                           {
                                                               Label = "Close",
                                                               LargeImage = new Uri("Images/LargeIcons/close-icon.png", UriKind.Relative),
                                                               SmallImage = new Uri("Images/SmallIcons/close-icon.png", UriKind.Relative),
                                                               ToolTipTitle = "Close environment file",
                                                               ToolTipDescription = "Close the current environment file (not functional)",
                                                               Command = MapViewModel.CloseEnvironmentFileCommand,
                                                           },
                                                           new ButtonDataViewModel
                                                           {
                                                               Label = "Edit",
                                                               LargeImage = new Uri("Images/LargeIcons/AddFile.png", UriKind.Relative),
                                                               SmallImage = new Uri("Images/SmallIcons/AddFile.png", UriKind.Relative),
                                                               ToolTipTitle = "Edit environment file",
                                                               ToolTipDescription = "Launch the environment builder",
                                                               Command = MainViewModel.LaunchExternalProgramCommand,
                                                               CommandParameter = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"EnvironmentBuilder.exe")
                                                           },
                                                       },
                                        },
                                        new GroupDataViewModel
                                        {
                                            Label = "Layers",
                                            Controls = new ControlList
                                                       {
                                                           new ComboBoxDataViewModel
                                                           {
                                                               Label = "Wind", //LargeImage = new Uri("Images/LargeIcons/open-icon.png", UriKind.Relative),
                                                               SmallImage = new Uri("Images/SmallIcons/open-icon.png", UriKind.Relative),
                                                               ToolTipTitle = "Wind data layer",
                                                               ToolTipDescription = "Select a source for wind data (not functional)",
                                                               IsEnabled = false,
                                                               IsEditable = false,
                                                           },
                                                           new ComboBoxDataViewModel
                                                           {
                                                               Label = "Sound Speed", //LargeImage = new Uri("Images/LargeIcons/open-icon.png", UriKind.Relative),
                                                               SmallImage = new Uri("Images/SmallIcons/open-icon.png", UriKind.Relative),
                                                               ToolTipTitle = "Sound speed layer",
                                                               ToolTipDescription = "Select a source for sound speed data (not functional)",
                                                               IsEnabled = false,
                                                               IsEditable = false,
                                                           },
                                                           new ComboBoxDataViewModel
                                                           {
                                                               Label = "Sediment", //LargeImage = new Uri("Images/LargeIcons/open-icon.png", UriKind.Relative),
                                                               SmallImage = new Uri("Images/SmallIcons/open-icon.png", UriKind.Relative),
                                                               ToolTipTitle = "Sediment data layer",
                                                               ToolTipDescription = "Select a source for sediment data (not functional)",
                                                               IsEnabled = false,
                                                               IsEditable = false,
                                                           },
                                                           new ComboBoxDataViewModel
                                                           {
                                                               Label = "Bathymetry", //LargeImage = new Uri("Images/LargeIcons/open-icon.png", UriKind.Relative),
                                                               SmallImage = new Uri("Images/SmallIcons/open-icon.png", UriKind.Relative),
                                                               ToolTipTitle = "Bathymetry data layer",
                                                               ToolTipDescription = "Select a source for bathymetry data (not functional)",
                                                               IsEnabled = false,
                                                               IsEditable = false,
                                                           },
                                                       },
                                        },
                                    },
                       },

                       #endregion
                       #region Animals Tab
                       new TabDataViewModel
                       {
                           Header = "Animals",
                       },

                       #endregion
                       #region Acoustics Tab
                       new TabDataViewModel
                       {
                           Header = "Acoustics",
                       },

                       #endregion
                       #region Reports Tab
                       new TabDataViewModel
                       {
                           Header = "Reports",
                       },

                       #endregion
                   };
        }
#endif
    }
}