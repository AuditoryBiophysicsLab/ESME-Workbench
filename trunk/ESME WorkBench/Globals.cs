﻿using Cinch;
using ESMEWorkBench.Data;
using ESMEWorkBench.ViewModels.Main;

namespace ESMEWorkBench
{
    public static class Globals
    {
        static Globals()
        {
            AppSettings = AppSettings.Load();
            EnvironmentDatabaseViewModel = new EnvironmentDatabaseViewModel(AppSettings.EnvironmentDatabaseDirectory);
        }

        public static MainViewModel MainViewModel { get; set; }
        public static MapViewModel MapViewModel { get; set; }
        public static LayerDisplayViewModel LayerDisplayViewModel { get; set; }
        public static IUIVisualizerService UIVisualizerService { get; set; }
        public static IOpenFileService OpenFileService { get; set; }
        public static IViewAwareStatus ViewAwareStatus { get; set; }
        public static IMessageBoxService MessageBoxService { get; set; }
        public static AppSettings AppSettings { get; set; }
        public static EnvironmentDatabaseViewModel EnvironmentDatabaseViewModel { get; set; }
    }
}
