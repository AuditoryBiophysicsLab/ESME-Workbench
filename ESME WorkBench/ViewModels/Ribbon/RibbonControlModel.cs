﻿using System;
using System.Collections.Generic;
using System.Windows.Input;
using ESMERibbonDemo.ViewModels.Main;

namespace ESMERibbonDemo.ViewModels.Ribbon
{
#if false
    public static class RibbonControlModel
    {
        #region Scenario Group

        public static ControlDataViewModel Scenario
        {
            get
            {
                lock (_lockObject)
                {
                    const string str = "Scenario";

                    if (!_dataCollection.ContainsKey(str))
                    {
                        _dataCollection[str] = new GroupDataViewModel(str)
                                                   {
                                                       SmallImage =
                                                           new Uri("Images/SmallIcons/AddFile.png", UriKind.Relative),
                                                       LargeImage =
                                                           new Uri("Images/LargeIcons/AddFile.png", UriKind.Relative),
                                                   };
                    }

                    return _dataCollection[str];
                }
            }
        }

        public static ControlDataViewModel LoadScenario
        {
            get
            {
                lock (_lockObject)
                {
                    const string str = "Load";

                    if (!_dataCollection.ContainsKey(str))
                    {
                        var buttonData = new ButtonDataViewModel
                                             {
                                                 Label = str,
                                                 SmallImage = new Uri("Images/SmallIcons/AddFile.png", UriKind.Relative),
                                                 LargeImage = new Uri("Images/LargeIcons/AddFile.png", UriKind.Relative),
                                                 ToolTipTitle = "Load Scenario File (Ctrl+L)",
                                                 ToolTipDescription = "Load a scenario file into the simulation.",
                                                 Command = ApplicationCommands.Cut,
                                                 KeyTip = "L",
                                             };
                        _dataCollection[str] = buttonData;
                    }

                    return _dataCollection[str];
                }
            }
        }

        public static ControlDataViewModel EditScenario
        {
            get
            {
                lock (_lockObject)
                {
                    const string str = "Edit";

                    if (!_dataCollection.ContainsKey(str))
                    {
                        var buttonData = new ButtonDataViewModel
                                             {
                                                 Label = str,
                                                 SmallImage =
                                                     new Uri("Images/SmallIcons/new-icon.png", UriKind.Relative),
                                                 LargeImage =
                                                     new Uri("Images/LargeIcons/new-icon.png", UriKind.Relative),
                                                 ToolTipTitle = "Edit Scenario File (Ctrl+E)",
                                                 ToolTipDescription =
                                                     "Edit the scenario file with the Scenario Builder.",
                                                 Command = ApplicationCommands.Cut,
                                                 KeyTip = "E",
                                             };
                        _dataCollection[str] = buttonData;
                    }

                    return _dataCollection[str];
                }
            }
        }

        #endregion Scenario Group

        #region Map Group

        public static ControlDataViewModel Map
        {
            get
            {
                lock (_lockObject)
                {
                    const string str = "Map";

                    if (!_dataCollection.ContainsKey(str))
                    {
                        _dataCollection[str] = new GroupDataViewModel(str)
                                                   {
                                                       SmallImage =
                                                           new Uri("Images/SmallIcons/System-Globe-icon.png",
                                                                   UriKind.Relative),
                                                       LargeImage =
                                                           new Uri("Images/LargeIcons/System-Globe-icon.png",
                                                                   UriKind.Relative),
                                                   };
                    }

                    return _dataCollection[str];
                }
            }
        }

        public static ControlDataViewModel BaseMap
        {
            get
            {
                lock (_lockObject)
                {
                    const string str = "Base Map";

                    if (!_dataCollection.ContainsKey(str))
                    {
                        _dataCollection[str] = new MenuButtonDataViewModel
                                                   {
                                                       Label = str,
                                                       SmallImage =
                                                           new Uri("Images/SmallIcons/System-Globe-icon.png",
                                                                   UriKind.Relative),
                                                       LargeImage =
                                                           new Uri("Images/LargeIcons/System-Globe-icon.png",
                                                                   UriKind.Relative),
                                                       ToolTipTitle = "Base Map settings",
                                                       ToolTipDescription = "Select the base map image",
                                                       Command = ApplicationCommands.Paste,
                                                   };
                    }

                    return _dataCollection[str];
                }
            }
        }

        public static ControlDataViewModel BaseMapNasa
        {
            get
            {
                lock (_lockObject)
                {
                    const string str = "NASA 1 minute topographic map";

                    if (!_dataCollection.ContainsKey(str))
                    {
                        _dataCollection[str] = new MenuItemDataViewModel
                                                   {
                                                       Label = str,
                                                       SmallImage =
                                                           new Uri("Images/SmallIcons/System-Map-icon.png",
                                                                   UriKind.Relative),
                                                       LargeImage =
                                                           new Uri("Images/LargeIcons/System-Map-icon.png",
                                                                   UriKind.Relative),
                                                       ToolTipTitle = "Base Map Settings",
                                                       ToolTipDescription = "Use this as the base map image",
                                                       Command = ApplicationCommands.Paste,
                                                   };
                    }

                    return _dataCollection[str];
                }
            }
        }

        public static ControlDataViewModel BaseMapOther
        {
            get
            {
                lock (_lockObject)
                {
                    const string str = "Custom base map";

                    if (!_dataCollection.ContainsKey(str))
                    {
                        _dataCollection[str] = new MenuItemDataViewModel
                                                   {
                                                       Label = str,
                                                       SmallImage =
                                                           new Uri("Images/SmallIcons/System-Map-icon.png",
                                                                   UriKind.Relative),
                                                       LargeImage =
                                                           new Uri("Images/LargeIcons/System-Map-icon.png",
                                                                   UriKind.Relative),
                                                       ToolTipTitle = "Base Map Settings",
                                                       ToolTipDescription =
                                                           "Choose your own base map image\nNote that this map must must be full global coverage\nleft edge 180W, right edge 180E, top 90N, bottom 90S, Mercator projection",
                                                       Command = ApplicationCommands.Paste,
                                                   };
                    }

                    return _dataCollection[str];
                }
            }
        }

        public static ControlDataViewModel AddContent
        {
            get
            {
                lock (_lockObject)
                {
                    const string str = "Add Content";

                    if (!_dataCollection.ContainsKey(str))
                    {
                        _dataCollection[str] = new MenuButtonDataViewModel
                                                   {
                                                       Label = str,
                                                       SmallImage =
                                                           new Uri("Images/SmallIcons/Plus.png", UriKind.Relative),
                                                       LargeImage =
                                                           new Uri("Images/LargeIcons/Plus.png", UriKind.Relative),
                                                       ToolTipTitle = "Add Content to the map",
                                                       ToolTipDescription =
                                                           "Select the type of content you wish to add to the map",
                                                   };
                    }

                    return _dataCollection[str];
                }
            }
        }

        public static ControlDataViewModel AddShapefile
        {
            get
            {
                lock (_lockObject)
                {
                    const string str = "ESRI Shapefile (*.shp)";

                    if (!_dataCollection.ContainsKey(str))
                    {
                        _dataCollection[str] = new MenuItemDataViewModel
                                                   {
                                                       Label = str,
                                                       SmallImage =
                                                           new Uri("Images/SmallIcons/Layers-icon.png", UriKind.Relative),
                                                       LargeImage =
                                                           new Uri("Images/LargeIcons/Layers-icon.png", UriKind.Relative),
                                                       ToolTipTitle = "Add Content to the map",
                                                       ToolTipDescription = "Add an ESRI Shapefile to the map",
                                                       Command = MainViewModel.AddShapefileCommand,
                                                   };
                    }

                    return _dataCollection[str];
                }
            }
        }

        public static ControlDataViewModel AddOverlayFile
        {
            get
            {
                lock (_lockObject)
                {
                    const string str = "NUWC Overlay File (*.ovr)";

                    if (!_dataCollection.ContainsKey(str))
                    {
                        _dataCollection[str] = new MenuItemDataViewModel
                                                   {
                                                       Label = str,
                                                       SmallImage =
                                                           new Uri("Images/SmallIcons/Layers-icon.png", UriKind.Relative),
                                                       LargeImage =
                                                           new Uri("Images/LargeIcons/Layers-icon.png", UriKind.Relative),
                                                       ToolTipTitle = "Add Content to the map",
                                                       ToolTipDescription = "Add a NUWC Overlay file to the map",
                                                       Command = MainViewModel.AddOverlayFileCommand,
                                                   };
                    }

                    return _dataCollection[str];
                }
            }
        }

        public static ControlDataViewModel AddScenarioFile
        {
            get
            {
                lock (_lockObject)
                {
                    const string str = "NUWC Secenario File (*.nemo)";

                    if (!_dataCollection.ContainsKey(str))
                    {
                        _dataCollection[str] = new MenuItemDataViewModel
                                                   {
                                                       Label = str,
                                                       SmallImage =
                                                           new Uri("Images/SmallIcons/Layers-icon.png", UriKind.Relative),
                                                       LargeImage =
                                                           new Uri("Images/LargeIcons/Layers-icon.png", UriKind.Relative),
                                                       ToolTipTitle = "Add Content to the map",
                                                       ToolTipDescription = "Add a NUWC Scenario file to the map",
                                                       Command = MainViewModel.AddScenarioFileCommand,
                                                   };
                    }

                    return _dataCollection[str];
                }
            }
        }

        public static ControlDataViewModel PanZoomControl
        {
            get
            {
                lock (_lockObject)
                {
                    const string str = "Pan/Zoom Control";

                    if (!_dataCollection.ContainsKey(str))
                    {
                        var buttonData = new ButtonDataViewModel
                                             {
                                                 Label = str,
                                                 SmallImage =
                                                     new Uri("Images/LargeIcons/System-Map-icon.png", UriKind.Relative),
                                                 LargeImage =
                                                     new Uri("Images/SmallIcons/System-Map-icon.png", UriKind.Relative),
                                                 ToolTipTitle = "Edit Scenario File (Ctrl+E)",
                                                 ToolTipDescription =
                                                     "Edit the scenario file with the Scenario Builder.",
                                                 Command = ApplicationCommands.Cut,
                                                 KeyTip = "E",
                                             };
                        _dataCollection[str] = buttonData;
                    }

                    return _dataCollection[str];
                }
            }
        }

        #endregion Scenario Group

        private static void DefaultExecuted()
        {
        }

        private static bool DefaultCanExecute()
        {
            return true;
        }

        #region Data

        private const string HelpFooterTitle = "Press F1 for more help.";
        private static readonly object _lockObject = new object();
        private static readonly Dictionary<string, ControlDataViewModel> _dataCollection = new Dictionary<string, ControlDataViewModel>();
        public static MainViewModel MainViewModel { get; set; }

        #endregion Data
    }
#endif
}