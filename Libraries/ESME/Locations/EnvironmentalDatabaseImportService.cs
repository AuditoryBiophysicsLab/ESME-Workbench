using System;
using System.ComponentModel.Composition;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using System.Windows;
using System.Windows.Media;
using ESME.Database;
using ESME.Environment;
using ESME.Environment.Descriptors;
using ESME.Plugins;
using HRC.Navigation;
using HRC.Utility;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Locations
{
    [PartCreationPolicy(CreationPolicy.Shared)]
    [ExportService(ServiceType.Both, typeof(EnvironmentalDatabaseImportService))]
    public class EnvironmentalDatabaseImportService
    {
        public EnvironmentalDatabaseImportService() {}
        public EnvironmentalDatabaseImportService(IPluginManagerService pluginManagerService,
                                                  LocationManagerService locationManagerService)
        {
            _pluginManagerService = pluginManagerService;
            _locationManagerService = locationManagerService;
        }

        [Import] IPluginManagerService _pluginManagerService;
        [Import] LocationManagerService _locationManagerService;

        public void BeginImport(EnvironmentalDataSet dataSet, IProgress<float> progress = null )
        {
            if (ImportActionBlock == null) ImportActionBlock = CreateImporter();
            ImportActionBlock.Post(Tuple.Create(dataSet, progress));
        }

        int _busyCount;
        public int BusyCount { get { return _busyCount; } }

        public ActionBlock<Tuple<EnvironmentalDataSet, IProgress<float>>> ImportActionBlock { get; private set; }
        
        ActionBlock<Tuple<EnvironmentalDataSet, IProgress<float>>> CreateImporter(TaskScheduler taskScheduler = null, int boundedCapacity = -1, int maxDegreeOfParallelism = -1)
        {
            if (taskScheduler == null) taskScheduler = TaskScheduler.Default;
            if (_pluginManagerService == null) throw new ServiceNotFoundException("Required service PluginManager was not found");
            if (_locationManagerService == null) throw new ServiceNotFoundException("Required service LocationManager was not found");
            var newImporter = new ActionBlock<Tuple<EnvironmentalDataSet, IProgress<float>>>(job =>
            {
                Interlocked.Increment(ref _busyCount);
                var dataSet = job.Item1;
                var progress = job.Item2;
                var sourcePlugin = (EnvironmentalDataSourcePluginBase)_pluginManagerService[dataSet.EnvironmentalDataSetCollection.SourcePlugin];
                var geoRect = dataSet.EnvironmentalDataSetCollection.Location.GeoRect;
                var resolution = dataSet.Resolution;
                var timePeriod = dataSet.TimePeriod;
                var fileName = Path.Combine(_locationManagerService.LocationRootDirectory, dataSet.EnvironmentalDataSetCollection.Location.StorageDirectory, dataSet.FileName);
                if (progress != null) progress.Report(0f);
                //Console.WriteLine("Importer: About to import {0}[{1}] from region {2} {3} {4} {5}", dataSet.EnvironmentalDataSetCollection.SourcePlugin.PluginSubtype, dataSet.Resolution, geoRect.North, geoRect.South, geoRect.East, geoRect.West);
                //Console.WriteLine("Importer: About to invoke {0} plugin [{1}]", dataSet.EnvironmentalDataSetCollection.SourcePlugin.PluginSubtype, sourcePlugin.PluginName);
                switch (sourcePlugin.EnvironmentDataType)
                {
                    case EnvironmentDataType.Wind:
                        var wind = ((EnvironmentalDataSourcePluginBase<Wind>)sourcePlugin).Extract(geoRect, resolution, timePeriod, progress);
                        dataSet.SampleCount = (from period in wind.TimePeriods select period.EnvironmentData.Count).Sum();
                        wind.Serialize(fileName);
                        break;
                    case EnvironmentDataType.Sediment:
                        var sediment = ((EnvironmentalDataSourcePluginBase<Sediment>)sourcePlugin).Extract(geoRect, resolution, timePeriod, progress);
                        dataSet.SampleCount = sediment.Samples.Count;
                        sediment.Serialize(fileName);
                        break;
                    case EnvironmentDataType.SoundSpeed:
                        var soundSpeed = ((EnvironmentalDataSourcePluginBase<SoundSpeed>)sourcePlugin).Extract(geoRect, resolution, timePeriod, progress);
                        dataSet.SampleCount = (from field in soundSpeed.SoundSpeedFields select field.EnvironmentData.Count).Sum();
                        soundSpeed.Serialize(fileName);
                        break;
                    case EnvironmentDataType.Bathymetry:
                        var bathymetry = ((EnvironmentalDataSourcePluginBase<Bathymetry>)sourcePlugin).Extract(geoRect, resolution, timePeriod, progress);
                        dataSet.SampleCount = bathymetry.Samples.Count;
                        bathymetry.Save(fileName);
                        var colormap = new DualColormap(Colormap.Summer, Colormap.Jet) {Threshold = 0};
                        var bathysize = Math.Max(bathymetry.Samples.Longitudes.Count, bathymetry.Samples.Latitudes.Count);
                        var screenSize = Math.Min(SystemParameters.PrimaryScreenWidth, SystemParameters.PrimaryScreenHeight);
                        var displayValues = bathymetry.Samples;
                        if (bathysize > screenSize)
                        {
                            var scaleFactor = screenSize / bathysize;
                            displayValues = EnvironmentData<Geo<float>>.Decimate(bathymetry.Samples,
                                                                                 (int)(bathymetry.Samples.Longitudes.Count * scaleFactor),
                                                                                 (int)(bathymetry.Samples.Latitudes.Count * scaleFactor));
                        }

                        var imageFilename = Path.GetFileNameWithoutExtension(fileName) + ".bmp";
                        var imagePath = Path.GetDirectoryName(fileName);

                        var bitmapData = new float[displayValues.Longitudes.Count,displayValues.Latitudes.Count];
                        for (var latIndex = 0; latIndex < bitmapData.GetLength(1); latIndex++) for (var lonIndex = 0; lonIndex < bitmapData.GetLength(0); lonIndex++) bitmapData[lonIndex, latIndex] = displayValues[(uint)lonIndex, (uint)latIndex].Data;

                        var displayData = colormap.ToPixelValues(bitmapData,
                                                                 bathymetry.Minimum.Data,
                                                                 bathymetry.Maximum.Data < 0
                                                                     ? bathymetry.Maximum.Data
                                                                     : 8000,
                                                                 Colors.Black);
                        BitmapWriter.Write(Path.Combine(imagePath, imageFilename), displayData);

                        var sb = new StringBuilder();
                        sb.AppendLine(dataSet.Resolution.ToString(CultureInfo.InvariantCulture));
                        sb.AppendLine("0.0");
                        sb.AppendLine("0.0");
                        sb.AppendLine(dataSet.Resolution.ToString(CultureInfo.InvariantCulture));
                        sb.AppendLine(bathymetry.Samples.GeoRect.West.ToString(CultureInfo.InvariantCulture));
                        sb.AppendLine(bathymetry.Samples.GeoRect.North.ToString(CultureInfo.InvariantCulture));
                        using (var writer = new StreamWriter(Path.Combine(imagePath, Path.GetFileNameWithoutExtension(imageFilename) + ".bpw"), false)) writer.Write(sb.ToString());
                        break;
                    default:
                        throw new ApplicationException(string.Format("Unknown environmental data type {0}", sourcePlugin.EnvironmentDataType));
                }
                //Console.WriteLine("Importer: {0} plugin returned {1} samples", dataSet.EnvironmentalDataSetCollection.SourcePlugin.PluginSubtype, dataSet.SampleCount);
                dataSet.FileSize = new FileInfo(fileName).Length;
                _locationManagerService.UpdateEnvironmentDataSetPercentCached(dataSet, 100);
                if (progress != null) progress.Report(100f);
                //Console.WriteLine("Importer: Finished importing {0} at resolution {1}", dataSet.EnvironmentalDataSetCollection.SourcePlugin.PluginSubtype, dataSet.Resolution);
                Interlocked.Decrement(ref _busyCount);
            }, new ExecutionDataflowBlockOptions
            {
                TaskScheduler = taskScheduler,
                BoundedCapacity = boundedCapacity,
                MaxDegreeOfParallelism = maxDegreeOfParallelism,
            });
            return newImporter;
        }
    }
}
