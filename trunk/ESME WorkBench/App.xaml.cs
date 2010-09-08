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
#if DEBUG
        private static readonly string Logfile, DumpFile;

        static App()
        {
            Logfile = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), "esme_app_log.txt");
            if (OSInfo.OperatingSystemName == "XP")
            {
                DumpFile = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), "esme_crash.mdmp");
                AppDomain.CurrentDomain.UnhandledException += CurrentDomain_UnhandledException;
            }
            using (var sw = new StreamWriter(Logfile, false))
                sw.WriteLine("{0} Application starting up", DateTime.Now);
        }
#endif

        #region Initialization

        /// <summary>
        /// Initialize Cinch using the CinchBootStrapper. 
        /// </summary>
        public App()
        {
            if (OSInfo.OperatingSystemName == "XP")
            {
                MessageBox.Show("Windows XP is not currently supported by this application, pending satisfactory resolution of application startup crash", "Operating system not supported");
                Current.Shutdown();
            }
            try
            {
                CinchBootStrapper.Initialise(new List<Assembly> {typeof (App).Assembly});
            }
            catch (Exception e)
            {
#if DEBUG
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
#else
                Console.WriteLine(e.Message);
#endif
                throw;
            }
            InitializeComponent();
        }

        #endregion

        private void Application_Exit(object sender, ExitEventArgs e)
        {
#if DEBUG
            using (var sw = new StreamWriter(Logfile, true))
                sw.WriteLine("{0} Application shutting down", DateTime.Now);
#endif
            Settings.Default.Save();
        }

#if DEBUG
        [DllImport("clrdump.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        private static extern Int32 CreateDump(Int32 processId, string fileName, Int32 dumpType, Int32 excThreadId, IntPtr extPtrs);

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

        private static bool IsAdministrator
        {
            get
            {
                var wi = WindowsIdentity.GetCurrent();
                if (wi == null) return false;
                var wp = new WindowsPrincipal(wi);
                return wp.IsInRole(WindowsBuiltInRole.Administrator);
            }
        }

#endif
    }
}