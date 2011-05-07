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
using ESME.Views;
using HRC.Utility;
using TransmissionLossCalculator.Properties;

namespace TransmissionLossCalculator
{
    /// <summary>
    ///   Interaction logic for App.xaml
    /// </summary>
    public partial class App
    {
        public static AppEventLog Log { get; private set; }
        public static readonly string Logfile, DumpFile;
        public const string Name = "ESME Transmission Loss Calculator";
        Mutex _mutex;
        const string AppGuidString = "{5B8DF70E-D10C-4901-B666-B43499366439}";

        static App()
        {
            Logfile = Path.Combine(Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), Name), "app.log");
            if (File.Exists(Logfile)) File.Delete(Logfile);
            Trace.Listeners.Add(new TextWriterTraceListener(Logfile, "logfile") { TraceOutputOptions = TraceOptions.None });
            Trace.WriteLine(Name + " initializing");
            if (OSInfo.OperatingSystemName != "XP")
            {
                DumpFile = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), "esme_crash.mdmp");
                AppDomain.CurrentDomain.UnhandledException += LastChanceExceptionHandler;

            }
#if DEBUG
            Log = new AppEventLog(Name);
            Log.Debug(EventLogEntryType.Information, "Starting");
#endif
        }

        #region Initialization

        /// <summary>
        ///   Initialize Cinch using the CinchBootStrapper.
        /// </summary>
        public App()
        {
            // You must close or flush the trace to empty the output buffer.
            Trace.WriteLine(Name + " starting up");

            if (OSInfo.OperatingSystemName == "XP")
            {
                Trace.TraceError("This application is not supported under Windows XP");
                MessageBox.Show("Windows XP is not currently supported by this application, pending satisfactory resolution of application startup crash");
                Current.Shutdown();
                return;
            }

            // This block of code ensures that only one copy of this application will run on the local machine
            try
            {
                _mutex = Mutex.OpenExisting(AppGuidString);

                Current.Shutdown();
                return;
            }
            catch (WaitHandleCannotBeOpenedException)
            {
                _mutex = new Mutex(true, AppGuidString);
            }

            // continue with initialization….
            try
            {
                //ExperimentData.Test();

                CinchBootStrapper.Initialise(new List<Assembly>
                                             {
                                                 typeof (App).Assembly,
                                                 typeof (TestView).Assembly,
                                             });
            }
            catch (Exception e)
            {
                Trace.Indent();
                Trace.TraceError("CinchBootStrapper threw an exception: {0}", e.Message);
                var inner = e.InnerException;
                while (inner != null)
                {
                    Trace.Indent();
                    Trace.TraceError("Inner exception: {0}", inner.Message);
                    if (inner is ReflectionTypeLoadException)
                    {
                        Trace.Indent();
                        var rtl = inner as ReflectionTypeLoadException;
                        foreach (var exception in rtl.LoaderExceptions)
                        {
                            Trace.TraceError("Loader Exception: {0}", exception.Message);
                            if (exception.InnerException != null)
                                Trace.TraceError("Inner Exception: {0}", exception.InnerException.Message);
                        }
                        Trace.Unindent();
                    }
                    inner = inner.InnerException;
                    Trace.Unindent();
                }
                Trace.Unindent();
                Trace.Flush();
                throw;
            }
            InitializeComponent();
            Trace.Flush();
        }

        #endregion

        void ApplicationExit(object sender, ExitEventArgs e)
        {
            Trace.WriteLine(string.Format(Name + " exiting with code {0}", e.ApplicationExitCode));

            Trace.Flush();
            Settings.Default.Save();
        }

        [DllImport("clrdump.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        private static extern Int32 CreateDump(Int32 processId, string fileName, Int32 dumpType, Int32 excThreadId, IntPtr extPtrs);

        static void LastChanceExceptionHandler(object sender, UnhandledExceptionEventArgs ex)
        {
            Trace.TraceError("{0} encountered an unhandled exception and is exiting.  A dump file will be created in {1}", Name, DumpFile);

            // ExceptionPolicy.HandleException(ex, "Default Policy");

            var pEP = Marshal.GetExceptionPointers();
            CreateDump(Process.GetCurrentProcess().Id, DumpFile, 0x00000002, //MinidumpType.MiniDumpWithFullMemory
                Thread.CurrentThread.ManagedThreadId, pEP);
        }

#if DEBUG
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