using System;
using System.IO;
using System.Security.Principal;
using System.Text;
using System.Windows;
using System.Collections.Generic;
using Cinch;
using System.Reflection;

namespace ESMEWorkBench
{
    /// <summary>
    /// Interaction logic for App.xaml
    /// </summary>
    public partial class App : Application
    {
        private readonly static string Logfile;

        static App()
        {
            Logfile = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), "esme_app_log.txt");
            using (var sw = new StreamWriter(Logfile, false))
                sw.WriteLine("{0} Application starting up", DateTime.Now);
        }
        #region Initialization
        /// <summary>
        /// Initialize Cinch using the CinchBootStrapper. 
        /// </summary>
        public App()
        {
            try
            {
                CinchBootStrapper.Initialise(new List<Assembly> { typeof(App).Assembly });                
            }
            catch (Exception e)
            {
                if (IsAdministrator)
                {
                    using (var sw = new StreamWriter(Logfile, true))
                    {
                        sw.WriteLine("{0} CinchBootStrapper threw an exception:\n{1}", DateTime.Now, e.Message);
                        var inner = e.InnerException;
                        while (inner != null)
                        {
                            sw.WriteLine("\n  Inner exception:\n{0}", inner.Message);
                            if (inner is ReflectionTypeLoadException)
                            {
                                var rtl = inner as ReflectionTypeLoadException;
                                foreach (var exception in rtl.LoaderExceptions)
                                {
                                    sw.WriteLine("\n    Loader Exception: {0}", exception.Message);
                                    if (exception.InnerException != null)
                                        sw.WriteLine("\n      Inner Exception: {0}", exception.InnerException.Message);
                                }
                            }
                            inner = inner.InnerException;
                        }
                    }
                }
                //throw;
            }
            InitializeComponent();
        }
        #endregion

        private void Application_Exit(object sender, ExitEventArgs e)
        {
            using (var sw = new StreamWriter(Logfile, true))
                sw.WriteLine("{0} Application shutting down", DateTime.Now);
            ESMEWorkBench.Properties.Settings.Default.Save();
        }

        private static bool IsAdministrator
        {
            get
            {
                return true;
                var wi = WindowsIdentity.GetCurrent();
                if (wi == null) return false;
                var wp = new WindowsPrincipal(wi);
                return wp.IsInRole(WindowsBuiltInRole.Administrator);
            }
        }
    }
}
