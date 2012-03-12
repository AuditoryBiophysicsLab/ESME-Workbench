using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Reflection;
using System.Windows;
using Cinch;
using DavesWPFTester.Properties;

namespace DavesWPFTester
{
    /// <summary>
    ///   Interaction logic for App.xaml
    /// </summary>
    public partial class App
    {
        public static readonly string Logfile, DumpFile;
        public const string Name = "ESME Workbench";

        static App()
        {
            //Logfile = Path.Combine(Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), Name), "app.log");
            //if (File.Exists(Logfile)) File.Delete(Logfile);
            //Trace.Listeners.Add(new TextWriterTraceListener(Logfile, "logfile") { TraceOutputOptions = TraceOptions.None });
            //Trace.AutoFlush = true;
            //Trace.WriteLine(Name + " initializing");
        }

        #region Initialization

        /// <summary>
        ///   Initialize Cinch using the CinchBootStrapper.
        /// </summary>
        public App()
        {
            // You must close or flush the trace to empty the output buffer.
            Trace.WriteLine(Name + " starting up");

            try
            {
                //ExperimentData.Test();

                CinchBootStrapper.Initialise(new List<Assembly>
                                             {
                                                 typeof (App).Assembly,
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
    }
}
