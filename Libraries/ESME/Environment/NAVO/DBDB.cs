using System;
using System.Diagnostics;
using System.Drawing.Imaging;
using System.Linq;
using System.IO;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using System.Windows;
using ESME.Environment.Descriptors;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment.NAVO
{
    public static class DBDB
    {
        readonly static object LockObject = new object();
        static ActionBlock<Tuple<string, float, GeoRect, SampleCountTreeItem, IProgress<string>, IProgress<float>>> _dbdbWorkQueue;
        readonly static DualColormap BathymetryColormap = new DualColormap(Colormap.Summer, Colormap.Jet) { Threshold = 0 };

        public static void ImportAsync(string outputPath, float selectedResolution, GeoRect region, SampleCountTreeItem treeItem, IProgress<string> currentState = null, IProgress<float> progress = null)
        {
            lock (LockObject)
            {
                if (_dbdbWorkQueue == null)
                {
                    _dbdbWorkQueue = new ActionBlock<Tuple<string, float, GeoRect, SampleCountTreeItem, IProgress<string>, IProgress<float>>>(item => TaskEx.Run(() =>
                    {
                        var bathymetryFilename = Path.Combine(item.Item1, string.Format("{0:0.00}min.bathymetry", item.Item2));
                        if (File.Exists(bathymetryFilename)) return;
                        Debug.WriteLine("{0}: About to import bathymetry data for {1}", DateTime.Now, Path.GetFileNameWithoutExtension(item.Item1));
                        var bathymetry = Extract(item.Item2, item.Item3, item.Item6);
                        if (item.Item5 != null) lock (item.Item5) item.Item5.Report("Saving");
                        bathymetry.Save(bathymetryFilename);
                        Debug.WriteLine("{0}: Sediment import completed for {1}", DateTime.Now, Path.GetFileNameWithoutExtension(item.Item1));
                        item.Item4.IsDataAvailable = true;
                        item.Item4.SampleCount = (uint)bathymetry.Samples.Count;
                        item.Item4.GeoRect = bathymetry.Samples.GeoRect;
#if false
                        // Create the bitmap needed to display the bathymetry
                        var bitmapFilename = Path.Combine(item.Item1, string.Format("{0:0.00}min.bmp", item.Item2));
                        var bathysize = Math.Max(bathymetry.Samples.Longitudes.Length, bathymetry.Samples.Latitudes.Length);
                        var screenSize = Math.Min(SystemParameters.PrimaryScreenWidth, SystemParameters.PrimaryScreenHeight);
                        var displayValues = bathymetry.Samples;
                        if (bathysize > screenSize)
                        {
                            var scaleFactor = screenSize / bathysize;
                            displayValues = EnvironmentData<EarthCoordinate<float>>.Decimate(bathymetry.Samples, (int)(bathymetry.Samples.Longitudes.Length * scaleFactor), (int)(bathymetry.Samples.Latitudes.Length * scaleFactor));
                        }
                        var bitmapData = new float[displayValues.Longitudes.Length, displayValues.Latitudes.Length];
                        for (var latIndex = 0; latIndex < bitmapData.GetLength(1); latIndex++)
                            for (var lonIndex = 0; lonIndex < bitmapData.GetLength(0); lonIndex++)
                                bitmapData[lonIndex, latIndex] = displayValues[(uint)lonIndex, (uint)latIndex].Data;
                        var displayBitmap = BathymetryColormap.ToBitmap(bitmapData, bathymetry.Minimum.Data, bathymetry.Maximum.Data < 0 ? bathymetry.Maximum.Data : 8000);
                        displayBitmap.Save(bitmapFilename, ImageFormat.Bmp);
                        Debug.WriteLine("{0}: Bitmap creation completed for {1}", DateTime.Now, Path.GetFileNameWithoutExtension(item.Item1));
#endif
                    }), new ExecutionDataflowBlockOptions
                    {
                        TaskScheduler = TaskScheduler.Default,
                        MaxDegreeOfParallelism = 4,
                    });
                }
            }
            if (currentState != null) lock (currentState) currentState.Report("Queued");
            _dbdbWorkQueue.Post(new Tuple<string, float, GeoRect, SampleCountTreeItem, IProgress<string>, IProgress<float>>(outputPath, selectedResolution, region, treeItem, currentState, progress));
        }

        public static Task<Bathymetry> ExtractAsync(float selectedResolution, GeoRect region, IProgress<float> progress = null)
        {
            return TaskEx.Run(() => Extract(selectedResolution, region, progress));
        }

        public static Bathymetry Extract(float selectedResolution, GeoRect region, IProgress<float> progress = null)
        {
            if (progress != null) lock(progress) progress.Report(0f);

            const float progressStep = 100f / 4;
            var totalProgress = 0f;

            var outputPath = Path.GetTempFileName();
            outputPath = outputPath.Remove(outputPath.Length - 4, 4);

            var outputDirectory = Path.GetDirectoryName(outputPath);
            var commandArgs = string.Format(" area \"{0}\" 0.05min 2.00min nearest 0 meters G {1} {2} {3} {4} {5:0.0##} YXZ=\"{6}\"", Globals.AppSettings.NAVOConfiguration.DBDBDirectory, region.South, region.West, region.North, region.East, selectedResolution, string.Format("{0}.yxz", Path.GetFileName(outputPath)));
            //extract the area and look for success or failure in the output string.

            if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);

            var result = NAVOExtractionProgram.Execute(Globals.AppSettings.NAVOConfiguration.DBDBEXEPath, commandArgs, outputDirectory);
            var resarray = result.Split('\n');
            foreach (var line in resarray.Where(line => line.Contains("ERROR"))) throw new ApplicationException("DigitalBathymetricDatabase: Error extracting requested area: " + line);

            if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);

            var bathymetry = Bathymetry.FromYXZ(Path.Combine(outputPath + ".yxz"), -1);

            if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);

            File.Delete(outputPath + ".yxz");
            File.Delete(outputPath + ".yxz_SECURITY_README.txt");

            if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
            return bathymetry;
        }
    }
}