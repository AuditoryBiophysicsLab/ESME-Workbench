using System;
using System.ComponentModel.Composition;
using System.Collections.Concurrent;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using System.Windows;
using System.Windows.Media;
using ESME.Environment;
using ESME.Environment.Descriptors;
using ESME.Plugins;
using HRC.Navigation;
using HRC.Utility;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Locations
{
    [PartCreationPolicy(CreationPolicy.Shared)]
    [ExportService(ServiceType.Both, typeof(EnvironmentalCacheService))]
    public class EnvironmentalCacheService
    {
        public EnvironmentalCacheService() {}
        public EnvironmentalCacheService(IPluginManagerService pluginManagerService,
                                                  MasterDatabaseService masterDatabaseService)
        {
            _pluginManagerService = pluginManagerService;
            _masterDatabaseService = masterDatabaseService;
        }

        [Import] IPluginManagerService _pluginManagerService;
        [Import] MasterDatabaseService _masterDatabaseService;

        public PercentProgressList<PercentProgressList<Location>> ImportMissingDatasets()
        {
            var result = new PercentProgressList<PercentProgressList<Location>>();
            foreach (var locationProgress in _masterDatabaseService.Locations.Select(ImportLocationDatasets).Where(l => l != null))
                result.Add(locationProgress);
            return result;
        }

        public PercentProgressList<Location> ImportLocationDatasets(Location location)
        {
            var result = new PercentProgressList<Location> {ProgressTarget = location};
            foreach (var dataSetProgress in location.EnvironmentalDataSets.Select(ImportDataset).Where(dataSetProgress => dataSetProgress != null)) 
                result.Add(dataSetProgress);
            return result;
        }

        public PercentProgress<EnvironmentalDataSet> ImportDataset(EnvironmentalDataSet dataSet)
        {
            if (ImportActionBlock == null) ImportActionBlock = CreateImporter();
            var job = new PercentProgress<EnvironmentalDataSet> { ProgressTarget = dataSet };
            if (!_importJobsPending.TryAdd(dataSet.Guid, job)) return null;
            ImportActionBlock.Post(job);
            return job;
        }

        readonly ConcurrentDictionary<Guid, PercentProgress<EnvironmentalDataSet>> _importJobsPending = new ConcurrentDictionary<Guid, PercentProgress<EnvironmentalDataSet>>();

        int _busyCount;
        public int BusyCount { get { return _busyCount; } }

        public ActionBlock<PercentProgress<EnvironmentalDataSet>> ImportActionBlock { get; private set; }

        ActionBlock<PercentProgress<EnvironmentalDataSet>> CreateImporter(TaskScheduler taskScheduler = null, int boundedCapacity = -1, int maxDegreeOfParallelism = -1)
        {
            if (taskScheduler == null) taskScheduler = TaskScheduler.Default;
            if (_pluginManagerService == null) throw new ServiceNotFoundException("Required service PluginManager was not found");
            if (_masterDatabaseService == null) throw new ServiceNotFoundException("Required service LocationManager was not found");
            var newImporter = new ActionBlock<PercentProgress<EnvironmentalDataSet>>(job =>
            {
                Interlocked.Increment(ref _busyCount);
                var dataSet = job.ProgressTarget;
                var progress = job;
                var sourcePlugin = (EnvironmentalDataSourcePluginBase)_pluginManagerService[dataSet.SourcePlugin];
                var geoRect = dataSet.Location.GeoRect;
                var resolution = dataSet.Resolution;
                var timePeriod = dataSet.TimePeriod;
                var fileName = Path.Combine(_masterDatabaseService.MasterDatabaseDirectory, dataSet.Location.StorageDirectory, dataSet.FileName);
                progress.Report(0);
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
                progress.Report(100);
                //Console.WriteLine("Importer: Finished importing {0} at resolution {1}", dataSet.EnvironmentalDataSetCollection.SourcePlugin.PluginSubtype, dataSet.Resolution);
                Interlocked.Decrement(ref _busyCount);
                _importJobsPending.TryRemove(dataSet.Guid, out progress);
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
