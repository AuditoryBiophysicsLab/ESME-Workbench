using System;
using System.Collections.Generic;
using System.Configuration;
using System.Data;
using System.Linq;
using System.Windows;

namespace EnvironmentFileExplorer
{
    /// <summary>
    /// Interaction logic for App.xaml
    /// </summary>
    public partial class App : Application
    {
        public static string[] Args { get; set; }

        protected override void OnStartup(StartupEventArgs e)
        {
            if (e.Args != null)
                App.Args = e.Args;
            base.OnStartup(e);
        }
    }
}
