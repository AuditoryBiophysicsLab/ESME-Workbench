using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading;
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
        static void diffSoundSpeeds(string [] args)
        {
            var lockObject = new object();
            Debug.Assert(args.Length == 2);
            var file1 = SoundSpeed.Load(args[0]);
            var file2 = SoundSpeed.Load(args[1]);
            Debug.Assert(file1.SoundSpeedFields.Count == file2.SoundSpeedFields.Count);
            Console.WriteLine("Six equality tests are being performed in parallel\r\nTests 5 and 6 will take longer than the rest\r\nPlease wait...");
            var tasks = new List<Task>();
            // Make sure all time periods in file1 are also in file2
            tasks.Add(TaskEx.Run(() =>
            {
                foreach (var field in file1.SoundSpeedFields) Debug.Assert(file2[field.TimePeriod] != null);
                lock(lockObject) Console.WriteLine("Test 1 passed.");
            }));

            // Make sure all time periods in file2 are also in file1
            tasks.Add(TaskEx.Run(() =>
            {
                foreach (var field in file2.SoundSpeedFields) Debug.Assert(file1[field.TimePeriod] != null);
                lock (lockObject) Console.WriteLine("Test 2 passed.");
            }));
            
            // Make sure all points in each time period in file1 are also in file2
            tasks.Add(TaskEx.Run(() =>
            {
                foreach (var field in file1.SoundSpeedFields) foreach (var point in field.EnvironmentData) Debug.Assert(file2[field.TimePeriod].EnvironmentData[point] != null);
                lock (lockObject) Console.WriteLine("Test 3 passed.");
            }));

            // Make sure all points in each time period in file2 are also in file1
            tasks.Add(TaskEx.Run(() =>
            {
                foreach (var field in file2.SoundSpeedFields) foreach (var point in field.EnvironmentData) Debug.Assert(file1[field.TimePeriod].EnvironmentData[point] != null);
                lock (lockObject) Console.WriteLine("Test 4 passed.");
            }));

            // Make sure each sample in each point in each time period in file1 is the same as the corresponding sample in file2
            tasks.Add(TaskEx.Run(() =>
            {
                foreach (var field in file1.SoundSpeedFields)
                    foreach (var point in field.EnvironmentData)
                        for (var sampleIndex = 0; sampleIndex < point.Data.Count; sampleIndex++)
                        {
                            var sample = point.Data[sampleIndex];
                            Debug.Assert(Math.Abs(file2[field.TimePeriod].EnvironmentData[point].Data[sampleIndex].Depth - sample.Depth) < .001);
                            Debug.Assert(Math.Abs(file2[field.TimePeriod].EnvironmentData[point].Data[sampleIndex].Value - sample.Value) < .001);
                        }
                lock (lockObject) Console.WriteLine("Test 5 passed.");
            }));

            // Make sure each sample in each point in each time period in file2 is the same as the corresponding sample in file1
            tasks.Add(TaskEx.Run(() =>
            {
                foreach (var field in file2.SoundSpeedFields)
                    foreach (var point in field.EnvironmentData)
                        for (var sampleIndex = 0; sampleIndex < point.Data.Count; sampleIndex++)
                        {
                            var sample = point.Data[sampleIndex];
                            Debug.Assert(Math.Abs(file1[field.TimePeriod].EnvironmentData[point].Data[sampleIndex].Depth - sample.Depth) < .001);
                            Debug.Assert(Math.Abs(file1[field.TimePeriod].EnvironmentData[point].Data[sampleIndex].Value - sample.Value) < .001);
                        }
                lock (lockObject) Console.WriteLine("Test 6 passed.");
            }));

            Task.WaitAll(tasks.ToArray());

            Console.WriteLine("Files are identical.  Press any key to exit.");
            Console.ReadKey();
        }

        static void NcTest(string[] args)
        {
            var test = new NetCDF(@"H:\OAML\GDEM\sgdemv3s01.nc");
            
            var latVar = (NcVarDouble)test.Variables.Find(var => var.Name == "lat");
            latVar.ReadAll();
            var lats = new List<double>();
            for (uint i = 0; i < latVar.Dimensions[0].Length; i++) lats.Add(latVar[i]);

            var lonVar = (NcVarDouble)test.Variables.Find(var => var.Name == "lon");
            lonVar.ReadAll();
            var lons = new List<double>();
            for (uint i = 0; i < lonVar.Dimensions[0].Length; i++) lons.Add(latVar[i]);
            
            var depthVar = (NcVarDouble)test.Variables.Find(var => var.Name == "depth");
            depthVar.ReadAll();
            var depths = new List<double>();
            for (uint i = 0; i < depthVar.Dimensions[0].Length; i++) depths.Add(latVar[i]);

            var data = test.Variables.Find(var => var.Name == "salinity");
            data.ReadAll();
            var scaleFactor = ((NcAttFloat)data.Attributes.Find(att => att.Name == "scale_factor"))[0];
            var addOffset = ((NcAttFloat)data.Attributes.Find(att => att.Name == "add_offset"))[0];
        }
        
        static async void gdem(string[] args)
        {
            var settings = Path.Combine(Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "One Navy Model"), "settings.xml");
            ESME.Globals.AppSettings = AppSettings.Load(settings);

            string outputDirectory = null;
            var north = float.NaN;
            var south = float.NaN;
            var east = float.NaN;
            var west = float.NaN;
            string gdemRootDirectory = null;
            var temperatureFiles = new List<string>();
            var salinityFiles = new List<string>();
            var monthNames = new List<string>();
            var isNewMode = false;
            if (args.Length < 1)
            {
                Usage();
                return;
            }
            for (var i = 0; i < args.Length; i++)
            {
                switch (args[i].ToLower())
                {
                    case "-out":
                    case "-output":
                        outputDirectory = args[++i];
                        break;
                    case "-gdem":
                        gdemRootDirectory = args[++i];
                        break;
                    case "-mons":
                    case "-months":
                        monthNames = GetStringListFromArg(args[++i], ',');
                        break;
                    case "-north":
                        north = float.Parse(args[++i]);
                        break;
                    case "-south":
                        south = float.Parse(args[++i]);
                        break;
                    case "-east":
                        east = float.Parse(args[++i]);
                        break;
                    case "-west":
                        west = float.Parse(args[++i]);
                        break;
                    case "-new":
                        isNewMode = true;
                        break;
                    default:
                        Usage();
                        return;
                }
            }

            if ((outputDirectory == null) || (!Directory.Exists(gdemRootDirectory)) || (monthNames.Count == 0) || (float.IsNaN(north) || float.IsNaN(south) || float.IsNaN(east) || float.IsNaN(west)))
            {
                Usage();
                return;
            }

            if ((north < -90) || (north > 90) || (south < -90) || (south > 90) || (north < south) ||
                (west < -180) || (west > 180) || (east < -180) || (east > 180) || (east < west))
            {
                Usage();
                return;
            }

            Directory.CreateDirectory(outputDirectory);
            var temperatureProgress = new Progress<float>(p => Console.WriteLine("Temperature extraction progress: {0:0.0}%", p));
            var salinityProgress = new Progress<float>(p => Console.WriteLine("Salinity extraction progress: {0:0.0}%", p));
            var soundSpeedProgress = new Progress<float>(p => Console.WriteLine("Soundspeed calculation progress: {0:0.0}%", p));
            var months = monthNames.Select(curMonth => (NAVOTimePeriod)Enum.Parse(typeof(NAVOTimePeriod), curMonth, true)).ToList();
            months.Sort();
            Task<SoundSpeed> temperature = null, salinity = null;
            Task.WaitAll(TaskEx.Run(() => temperature = GDEM.ReadTemperatureAsync(months, north, south, east, west, temperatureProgress)),
                         TaskEx.Run(() => salinity = GDEM.ReadSalinityAsync(months, north, south, east, west, salinityProgress)));
            Console.WriteLine("Waiting for temperature and salinity extraction...");
            Task.WaitAll(temperature, salinity);
            Console.WriteLine("Calculating soundspeed...");
            var soundSpeed = GDEM.CalculateSoundSpeedAsync(temperature.Result, salinity.Result, null, soundSpeedProgress);
            soundSpeed.Wait();
            Task.WaitAll(temperature.ContinueWith(task => task.Result.Save(Path.Combine(outputDirectory, "temperature"))),
                         salinity.ContinueWith(task => task.Result.Save(Path.Combine(outputDirectory, "salinity"))),
                         soundSpeed.ContinueWith(task => task.Result.Save(Path.Combine(outputDirectory, "soundspeed"))));
        }

        static List<string> GetStringListFromArg(string arg, char separator) { return arg.Split(separator).ToList(); }

        static void Usage()
        {
            Console.WriteLine(
                "Usage: ImportNetCDF -output <OutputDirectoryName>\n" +
                "                    -gdem <GDEMRootDirectory>\n" +
                "                    -months <MonthNameList>\n" +
                "                    -north <North>\n" +
                "                    -south <South> \n" +
                "                    -east <East> \n" +
                "                    -west <West> \n" +
                "\n" +
                "Where: <OutputDirectoryName> is the full path to a directory that will contain,\n" +
                "        the output files 'temperature.xml' and 'salinity.xml'.\n" +
                "\n" +
                "       <GDEMRootDirectory> is the root directory to search for the uncompressed\n" +
                "        GDEM netCDF source data. If this directory includes a space in it's name,\n" +
                "        be sure to wrap this entire argument in quotes.\n" +
                "\n" +
                "       <MonthNameList> is a comma-separated list of months to extract temperature\n" +
                "        and salinity data for.  Use complete month names, i.e. 'january' instead of\n" +
                "        'jan', and if you include spaces around commas, put this entire argument in\n" +
                "        quotes.\n" +
                "\n" +
                "       <North> is the northern boundary of the area to be extracted from the\n" +
                "        database.  Valid values are -90 to 90.  Must be greater than <South>.\n" +
                "\n" +
                "       <South> is the southern boundary of the area to be extracted from the\n" +
                "        database.  Valid values are -90 to 90. Must be less than <North>.\n" +
                "\n" +
                "       <East> is the eastern boundary of the area to be extracted from the\n" +
                "        database.  Valid values are -180 to 180.  Must be greater than <West>.\n" +
                "\n" +
                "       <West> is the western boundary of the area to be extracted from the\n" +
                "        database.  Valid values are -180 to 180.  Must be less than <East>.\n" +
                "\n");
        }

        static void Main(string[] args)
        {
            var settings = Path.Combine(Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "One Navy Model"), "settings.xml");
            ESME.Globals.AppSettings = AppSettings.Load(settings);

            const float north = 32.964529899922404f;
            const float south = 27.555799630786112f;
            const float east = -77.1238998087263f;
            const float west = -83.37449801842213f;
            const string where = @"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Sim Areas\Jacksonville\Data";
            const string overlayName = "Jax_Ops_Area_200km";
            var outputPath = Path.Combine(where, overlayName);
            var months = new List<NAVOTimePeriod> { NAVOTimePeriod.January, NAVOTimePeriod.February, NAVOTimePeriod.March, NAVOTimePeriod.April, NAVOTimePeriod.May, NAVOTimePeriod.June, NAVOTimePeriod.July, NAVOTimePeriod.August, NAVOTimePeriod.September, NAVOTimePeriod.October, NAVOTimePeriod.November, NAVOTimePeriod.December};
            var seasons = new List<NAVOTimePeriod> { NAVOTimePeriod.Spring, NAVOTimePeriod.Summer, NAVOTimePeriod.Fall, NAVOTimePeriod.Winter, NAVOTimePeriod.Warm, NAVOTimePeriod.Cold};
            Directory.CreateDirectory(outputPath);

            var temperatureProgress = new Progress<float>(p => Console.WriteLine("Temperature extraction progress: {0:0.0}%", p));
            var salinityProgress = new Progress<float>(p => Console.WriteLine("Salinity extraction progress: {0:0.0}%", p));
            var soundSpeedProgress = new Progress<float>(p => Console.WriteLine("Soundspeed calculation progress: {0:0.0}%", p));
            var windProgress = new Progress<float>(p => Console.WriteLine("Wind extraction progress: {0:0.0}%", p));
            var sedimentProgress = new Progress<float>(p => Console.WriteLine("Sediment extraction progress: {0:0.0}%", p));
            var bottomLossProgress = new Progress<float>(p => Console.WriteLine("Bottom Loss extraction progress: {0:0.0}%", p));
            var bathymetryProgress = new Progress<float>(p => Console.WriteLine("Bathymetry extraction progress: {0:0.0}%", p));
            Task<SoundSpeed> temperature = null, salinity = null;
            Task<Wind> wind = null;
            Task<Sediment> sediment = null;
            Task<BottomLoss> bottomLoss = null;
            Task.WaitAll(TaskEx.Run(() => temperature = GDEM.ReadTemperatureAsync(months, north, south, east, west, temperatureProgress)),
                         TaskEx.Run(() => salinity = GDEM.ReadSalinityAsync(months, north, south, east, west, salinityProgress)),
                         TaskEx.Run(() => sediment = BST.ExtractAsync(north, south, east, west, sedimentProgress)),
                         //TaskEx.Run(() => bottomLoss = BottomLossDatabase.ExtractAsync(true, true, false, north, south, east, west, bottomLossProgress)),
                         TaskEx.Run(() => wind = SMGC.ExtractAsync(months, north, south, east, west, windProgress)));
            Console.WriteLine("Waiting for temperature and salinity extraction...");
            Task.WaitAll(temperature, salinity);
            Console.WriteLine("Calculating soundspeed...");
            var soundSpeed = GDEM.CalculateSoundSpeedAsync(temperature.Result, salinity.Result, null, soundSpeedProgress);
            Task.WaitAll(soundSpeed, wind, sediment);
            Task.WaitAll(temperature.ContinueWith(task => task.Result.Save(Path.Combine(outputPath, "temperature"))),
                         salinity.ContinueWith(task => task.Result.Save(Path.Combine(outputPath, "salinity"))),
                         soundSpeed.ContinueWith(task => task.Result.Save(Path.Combine(outputPath, "soundspeed"))),
                         sediment.ContinueWith(task => task.Result.Save(Path.Combine(outputPath, "sediment"))),
                         wind.ContinueWith(task => task.Result.Save(Path.Combine(outputPath, "wind"))));

#if false
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
#endif
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
