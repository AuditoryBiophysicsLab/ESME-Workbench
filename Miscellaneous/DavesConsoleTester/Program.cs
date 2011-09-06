using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Threading.Tasks;
using ESME.Data;
using ESME.Environment;
using ESME.Environment.NAVO;
using HRC.Navigation;
using HRC.NetCDF;
using HRC.Utility;
using ShoNS.Hosting;
using Environment = System.Environment;

namespace DavesConsoleTester
{
    class Program
    {
        static void Main(string[] args)
        {
            var test = new NetCDF(@"H:\OAML\GDEM\sgdemv3s01.nc");
        }

        static void AsyncTest(string[] args)
        {
            var settings = Path.Combine(Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "ESME WorkBench"), "settings.xml");
            ESME.Globals.AppSettings = AppSettings.Load(settings);

            const double north = 32.964529899922404;
            const double south = 27.555799630786112;
            const double east = -77.1238998087263;
            const double west = -83.37449801842213;
            const string where = @"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Sim Areas\Jacksonville\Data";
            const string overlayName = "Jax_Ops_Area_200km";
            var outputPath = Path.Combine(where, overlayName);
            var months = new List<NAVOTimePeriod> { NAVOTimePeriod.January, NAVOTimePeriod.February, NAVOTimePeriod.March, NAVOTimePeriod.April, NAVOTimePeriod.May, NAVOTimePeriod.June, NAVOTimePeriod.July, NAVOTimePeriod.August, NAVOTimePeriod.September, NAVOTimePeriod.October, NAVOTimePeriod.November, NAVOTimePeriod.December};
            var seasons = new List<NAVOTimePeriod> { NAVOTimePeriod.Spring, NAVOTimePeriod.Summer, NAVOTimePeriod.Fall, NAVOTimePeriod.Winter, NAVOTimePeriod.Warm, NAVOTimePeriod.Cold};
            IProgress<TaskProgressInfo> gdemProgress = new Progress<TaskProgressInfo>(p => Debug.WriteLine("{0} : {1} : {2}%", p.TaskName, p.CurrentActivity, p.ProgressPercent));
            var gdem = TaskEx.Run(() => GDEMBackgroundExtractor.ExtractAsync(false, (float)north, (float)south, (float)east, (float)west, months, outputPath, gdemProgress));
            IProgress<TaskProgressInfo> dbdbProgress = new Progress<TaskProgressInfo>(p => Debug.WriteLine("{0} : {1} : {2}%", p.TaskName, p.CurrentActivity, p.ProgressPercent));
            var dbdb = TaskEx.Run(() => DBDBBackgroundExtractor.ExtractAsync(outputPath + "_0.50min", "0.50", new GeoRect(north, south, east, west), dbdbProgress));
            IProgress<TaskProgressInfo> smgcProgress = new Progress<TaskProgressInfo>(p => Debug.WriteLine("{0} : {1} : {2}%", p.TaskName, p.CurrentActivity, p.ProgressPercent));
            var smgc = TaskEx.Run(() => SMGCBackgroundExtractor.Extract((float)north, (float)south, (float)east, (float)west, months, outputPath, smgcProgress));
            IProgress<TaskProgressInfo> bstProgress = new Progress<TaskProgressInfo>(p => Debug.WriteLine("{0} : {1} : {2}%", p.TaskName, p.CurrentActivity, p.ProgressPercent));
            var bst = TaskEx.Run(() => BSTBackgroundExtractor.Extract((float)north, (float)south, (float)east, (float)west, months, outputPath, bstProgress));
            IProgress<TaskProgressInfo> blProgress = new Progress<TaskProgressInfo>(p => Debug.WriteLine("{0} : {1} : {2}%", p.TaskName, p.CurrentActivity, p.ProgressPercent));
            var bottomLoss = TaskEx.Run(() => BottomLossBackgroundExtractor.Extract(true, true, false, (float)north, (float)south, (float)east, (float)west, outputPath, blProgress));
            Task.WaitAll(gdem, dbdb);
            SoundSpeed temperature = null, salinity = null;
            Bathymetry bathymetry = null;
            Task.WaitAll(TaskEx.Run(() => temperature = SoundSpeed.Load(outputPath + ".temperature")),
                         TaskEx.Run(() => salinity = SoundSpeed.Load(outputPath + ".salinity")),
                         TaskEx.Run(() => bathymetry = Bathymetry.Load(outputPath + "_0.50min.bathymetry")));
            var maxDepth = new EarthCoordinate<float>(bathymetry.Minimum, Math.Abs(bathymetry.Minimum.Data));
            IProgress<TaskProgressInfo> ssProgress = new Progress<TaskProgressInfo>(p => Debug.WriteLine("{0} : {1} : {2}%", p.TaskName, p.CurrentActivity, p.ProgressPercent));
            var soundSpeed = TaskEx.Run(() => GDEMBackgroundExtractor.ComputeSoundSpeeds(outputPath, temperature, salinity, maxDepth, seasons, ssProgress));
            Task.WaitAll(soundSpeed, smgc, bst, bottomLoss);
        }

        static void Sho(string[] args)
        {
            // create the Embedded Sho
            Console.Write("Starting Embedded Sho...");
            // the path argument below should point to what "SHODIR" evaluates
            // to in Sho
            var es = new EmbeddedSho(@"c:\Program Files (x86)\Sho 2.0 for .NET 4");
            Console.Write("done.\n");
            // es.CacheShoOutput will store the output from Sho so you can show
            // it in a form, etc;
            // if you don't call this the default is to print the output to
            //the console.
            es.CacheShoOutput();
            // do some Sho stuff
            es.ExecutePython("a = rand(6,10)");
            es.ExecutePython("foo = a[0,0]");
            // print information from the class
            //es.ExecutePython("print tc.info");
            // print cached output from Sho
            Console.WriteLine("{0}", es.GetOutputText());
            es.ExecutePython("plot(a[0, :], a[1, :])");
            es.ExecutePython("plot(a[2, :], a[3, :])");
            es.ExecutePython("plot(a[4, :], a[5, :])");
            // get the result in res so we can bring it back to C#
            //es.ExecutePython("res = tc.info");
            es.ExecutePython("for x in range(10): print x");
            // print cached output text. This can be called as often as
            //you wish; the text is flushed each time.
            Console.WriteLine("{0}", es.GetOutputText());
            // get back and print Sho values
            // note that GetPythonVariable will return an Object, which we
            //can cast to a known type
            Console.WriteLine("foo: {0}", es.GetPythonVariable("foo"));
            // alternatively, we can use GetPythonVariableAs<type>, which
            //will cast it to the type in one call.
            //Console.WriteLine("res: {0}",
            //es.GetPythonVariableAs<String>("res"));
        }
    }
}
