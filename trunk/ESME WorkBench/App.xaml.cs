using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Windows;
using Cinch;
using ESME.Views;
using ESMEWorkBench.Properties;
using HRC.Utility;
#if DEBUG
using System.Security.Principal;
#endif

namespace ESMEWorkBench
{
    /// <summary>
    ///   Interaction logic for App.xaml
    /// </summary>
    public partial class App
    {
        public static AppEventLog Log { get; private set; }
        public static readonly string Logfile, DumpFile;
        public const string Name = "ESME WorkBench";

        static App()
        {
            Logfile = Path.Combine(Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), Name), "app.log");
            if (File.Exists(Logfile)) File.Delete(Logfile);
            Trace.Listeners.Add(new TextWriterTraceListener(Logfile, "logfile"){TraceOutputOptions = TraceOptions.None});
            Trace.AutoFlush = true;
            Trace.WriteLine(Name + " initializing");
#if DEBUG
            var tr1 = new TextWriterTraceListener(Console.Out);
            Debug.Listeners.Add(tr1);
#endif
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
                throw;
            }
            InitializeComponent();
        }

        #endregion

        void ApplicationExit(object sender, ExitEventArgs e)
        {
            Trace.WriteLine(string.Format(Name + " exiting with code {0}", e.ApplicationExitCode));

            Trace.Flush();
            Settings.Default.Save();
        }

        static void LastChanceExceptionHandler(object sender, UnhandledExceptionEventArgs ex)
        {
            Trace.TraceError("{0} encountered an unhandled exception and is exiting.  A dump file will be created in {1}", Name, DumpFile);

            MiniDump.Write(DumpFile, MiniDump.Option.Normal, MiniDump.ExceptionInfo.Present);
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