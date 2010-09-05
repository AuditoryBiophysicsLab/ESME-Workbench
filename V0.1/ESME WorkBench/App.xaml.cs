using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Security.Principal;
using System.Threading;
using System.Windows;
using Cinch;
using ESMEWorkBench.Properties;

namespace ESMEWorkBench
{
    /// <summary>
    /// Interaction logic for App.xaml
    /// </summary>
    public partial class App
    {
        private static readonly string Logfile, DumpFile;

        static App()
        {
            Logfile = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), "esme_app_log.txt");
            DumpFile = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), "esme_crash.mdmp");
            using (var sw = new StreamWriter(Logfile, false))
                sw.WriteLine("{0} Application starting up", DateTime.Now);
            AppDomain.CurrentDomain.UnhandledException += CurrentDomain_UnhandledException;
        }

        static void CurrentDomain_UnhandledException(object sender, UnhandledExceptionEventArgs ex)
        {
            if (ex == null)
                return;
            // ExceptionPolicy.HandleException(ex, "Default Policy");
            MessageBox.Show(
                "An unhandled exception occurred, and the application is terminating. For more information, see your Application event log.");

            var pEP = Marshal.GetExceptionPointers();
            CreateDump(Process.GetCurrentProcess().Id, DumpFile, 0x00000002, //MinidumpType.MiniDumpWithFullMemory
                Thread.CurrentThread.ManagedThreadId, pEP);
        }

        #region Initialization

        /// <summary>
        /// Initialize Cinch using the CinchBootStrapper. 
        /// </summary>
        public App()
        {
            try
            {
                CinchBootStrapper.Initialise(new List<Assembly> {typeof (App).Assembly});
            }
            catch (Exception e)
            {
                if (IsAdministrator)
                {
                    using (var sw = new StreamWriter(Logfile, true))
                    {
                        sw.WriteLine("{0} CinchBootStrapper threw an exception:\n{1}", DateTime.Now, e.Message);
                        Exception inner = e.InnerException;
                        while (inner != null)
                        {
                            sw.WriteLine("\n  Inner exception:\n{0}", inner.Message);
                            if (inner is ReflectionTypeLoadException)
                            {
                                var rtl = inner as ReflectionTypeLoadException;
                                foreach (Exception exception in rtl.LoaderExceptions)
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

        private static bool IsAdministrator
        {
            get
            {
                return true;
                WindowsIdentity wi = WindowsIdentity.GetCurrent();
                if (wi == null) return false;
                var wp = new WindowsPrincipal(wi);
                return wp.IsInRole(WindowsBuiltInRole.Administrator);
            }
        }

        private void Application_Exit(object sender, ExitEventArgs e)
        {
            using (var sw = new StreamWriter(Logfile, true))
                sw.WriteLine("{0} Application shutting down", DateTime.Now);
            Settings.Default.Save();
        }

        [DllImport("clrdump.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        private static extern Int32 CreateDump(Int32 processId, string fileName, Int32 dumpType, Int32 excThreadId, IntPtr extPtrs);
    }
}