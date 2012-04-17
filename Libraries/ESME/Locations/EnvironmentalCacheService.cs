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
using ESME.Scenarios;
using HRC.Navigation;
using HRC.Utility;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Locations
{
    [PartCreationPolicy(CreationPolicy.Shared)]
    [ExportService(ServiceType.Both, typeof(EnvironmentalCacheService))]
    public class EnvironmentalCacheService
    {
        public EnvironmentalCacheService() { Location.Cache = this; Scenario.Cache = this; }
        public EnvironmentalCacheService(IPluginManagerService plugins, MasterDatabaseService database) : this()
        {
            _plugins = plugins;
            _database = database;
        }

        [Import] IPluginManagerService _plugins;
        [Import] MasterDatabaseService _database;

        public PercentProgressList<PercentProgressList<Location>> ImportMissingDatasets()
        {
            var result = new PercentProgressList<PercentProgressList<Location>>();
            foreach (var locationProgress in _database.Context.Locations.Select(ImportLocationDatasets).Where(l => l != null))
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

        public void ImportDatasetTest(EnvironmentalDataSet dataSet)
        {
            var job = new PercentProgress<EnvironmentalDataSet> { ProgressTarget = dataSet };
            if (!_importJobsPending.TryAdd(dataSet.Guid, job)) return;
            Import(job);
        }

        readonly ConcurrentDictionary<Guid, PercentProgress<EnvironmentalDataSet>> _importJobsPending = new ConcurrentDictionary<Guid, PercentProgress<EnvironmentalDataSet>>();

        int _busyCount;
        public int BusyCount { get { return _busyCount; } }

        public ActionBlock<PercentProgress<EnvironmentalDataSet>> ImportActionBlock { get; private set; }

        ActionBlock<PercentProgress<EnvironmentalDataSet>> CreateImporter(TaskScheduler taskScheduler = null, int boundedCapacity = -1, int maxDegreeOfParallelism = -1)
        {
            if (taskScheduler == null) taskScheduler = TaskScheduler.Default;
            if (_plugins == null) throw new ServiceNotFoundException("Required service PluginManager was not found");
            if (_database == null) throw new ServiceNotFoundException("Required service MasterDatabaseService was not found");
            if (string.IsNullOrEmpty(_database.MasterDatabaseDirectory)) throw new ServiceNotFoundException("Required service MasterDatabaseService is not properly configured.");
            var newImporter = new ActionBlock<PercentProgress<EnvironmentalDataSet>>(job => Import(job),
                                                                                     new ExecutionDataflowBlockOptions
                                                                                     {
                                                                                         TaskScheduler = taskScheduler,
                                                                                         BoundedCapacity = boundedCapacity,
                                                                                         MaxDegreeOfParallelism = maxDegreeOfParallelism,
                                                                                     });
            return newImporter;
        }

        void Import(PercentProgress<EnvironmentalDataSet> job)
        {
            Interlocked.Increment(ref _busyCount);
            var dataSet = job.ProgressTarget;
            var progress = job;
            var sourcePlugin = (EnvironmentalDataSourcePluginBase)_plugins[dataSet.SourcePlugin];
            var geoRect = dataSet.Location.GeoRect;
            var resolution = dataSet.Resolution;
            var timePeriod = dataSet.TimePeriod;
            var fileName = Path.Combine(_database.MasterDatabaseDirectory, dataSet.Location.StorageDirectory, dataSet.FileName);
            progress.Report(0);
            //Console.WriteLine("Importer: About to import {0}[{1}] from region {2} {3} {4} {5}", dataSet.EnvironmentalDataSetCollection.SourcePlugin.PluginSubtype, dataSet.Resolution, geoRect.North, geoRect.South, geoRect.East, geoRect.West);
            //Console.WriteLine("Importer: About to invoke {0} plugin [{1}]", dataSet.EnvironmentalDataSetCollection.SourcePlugin.PluginSubtype, sourcePlugin.PluginName);
            switch (sourcePlugin.EnvironmentDataType)
            {
                case EnvironmentDataType.Wind:
                    var wind = ((EnvironmentalDataSourcePluginBase<Wind>)sourcePlugin).Extract(geoRect, resolution, timePeriod, progress);
                    dataSet.SampleCount = (from period in wind.TimePeriods select period.EnvironmentData.Count).Sum();
                    wind.Serialize(fileName);
                    ToBitmap(wind[timePeriod].EnvironmentData, fileName, v => v == null ? 0 : v.Data, (data, minValue, maxValue) => Colormap.Summer.ToPixelValues(data, minValue, maxValue));
                    break;
                case EnvironmentDataType.Sediment:
                    var sediment = ((EnvironmentalDataSourcePluginBase<Sediment>)sourcePlugin).Extract(geoRect, resolution, timePeriod, progress);
                    dataSet.SampleCount = sediment.Samples.Count;
                    sediment.Serialize(fileName);
                    ToBitmap(sediment.Samples, fileName, v => v == null ? 0 : v.Data.SampleValue, (data, minValue, maxValue) => Colormap.Sediment.ToPixelValues(data, 0, 23));
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
                    var dualColormap = new DualColormap(Colormap.Summer, Colormap.Jet) { Threshold = 0 };
#if true
                    ToBitmap(bathymetry.Samples, fileName, v => v.Data, (data, minValue, maxValue) => dualColormap.ToPixelValues(data, minValue, maxValue < 0 ? maxValue : 8000, Colors.Black));
#else
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

                    var bitmapData = new float[displayValues.Longitudes.Count, displayValues.Latitudes.Count];
                    for (var latIndex = 0; latIndex < bitmapData.GetLength(1); latIndex++) for (var lonIndex = 0; lonIndex < bitmapData.GetLength(0); lonIndex++) bitmapData[lonIndex, latIndex] = displayValues[(uint)lonIndex, (uint)latIndex].Data;

                    var displayData = dualColormap.ToPixelValues(bitmapData,
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
#endif
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
        }

        public void ToBitmap<T>(EnvironmentData<T> data, string fileName, Func<T, float> valueFunc, Func<float[,], float, float, uint[,]> toPixelValuesFunc) where T: Geo, new()
        {
            var minValue = data.Min(valueFunc);
            var maxValue = data.Max(valueFunc);
            var bathysize = Math.Max(data.Longitudes.Count, data.Latitudes.Count);
            var screenSize = Math.Min(SystemParameters.PrimaryScreenWidth, SystemParameters.PrimaryScreenHeight);
            var displayValues = data;
            if (bathysize > screenSize)
            {
                var scaleFactor = screenSize / bathysize;
                displayValues = EnvironmentData<T>.Decimate(data, (int)(data.Longitudes.Count * scaleFactor), (int)(data.Latitudes.Count * scaleFactor));
            }

            var imageFilename = Path.GetFileNameWithoutExtension(fileName) + ".bmp";
            var imagePath = Path.GetDirectoryName(fileName);

            var bitmapData = new float[displayValues.Longitudes.Count, displayValues.Latitudes.Count];
            for (var latIndex = 0; latIndex < bitmapData.GetLength(1); latIndex++) for (var lonIndex = 0; lonIndex < bitmapData.GetLength(0); lonIndex++) bitmapData[lonIndex, latIndex] = valueFunc(displayValues[(uint)lonIndex, (uint)latIndex]);
            var displayData = toPixelValuesFunc(bitmapData, minValue, maxValue);
            BitmapWriter.Write(Path.Combine(imagePath, imageFilename), displayData);

            var sb = new StringBuilder();
            sb.AppendLine(data.Resolution.ToString(CultureInfo.InvariantCulture));
            sb.AppendLine("0.0");
            sb.AppendLine("0.0");
            sb.AppendLine(data.Resolution.ToString(CultureInfo.InvariantCulture));
            sb.AppendLine(data.GeoRect.West.ToString(CultureInfo.InvariantCulture));
            sb.AppendLine(data.GeoRect.North.ToString(CultureInfo.InvariantCulture));
            using (var writer = new StreamWriter(Path.Combine(imagePath, Path.GetFileNameWithoutExtension(imageFilename) + ".bpw"), false)) writer.Write(sb.ToString());
        }

        readonly ConcurrentDictionary<Guid, EnvironmentalCacheEntry> _cache = new ConcurrentDictionary<Guid, EnvironmentalCacheEntry>();

        public EnvironmentDataSetBase this[EnvironmentalDataSet dataSet]
        {
            get
            {
                EnvironmentalCacheEntry requestedData; 
                var isCached = _cache.TryGetValue(dataSet.Guid, out requestedData);
                if (isCached)
                {
                    while (requestedData == null)
                    {
                        Thread.Sleep(50);
                        requestedData = _cache[dataSet.Guid];
                    }
                    requestedData.LastAccessed = DateTime.Now;
                    return requestedData.Data;
                }
                var added = _cache.TryAdd(dataSet.Guid, null);
                if (!added)
                {
                    while (requestedData == null)
                    {
                        Thread.Sleep(50);
                        requestedData = _cache[dataSet.Guid];
                    }
                    requestedData.LastAccessed = DateTime.Now;
                    return requestedData.Data;
                }
                if (string.IsNullOrEmpty(_database.MasterDatabaseDirectory)) throw new ServiceNotFoundException("Required service MasterDatabaseService is not properly configured.");
                _cache[dataSet.Guid] = EnvironmentalCacheEntry.Load(_database.MasterDatabaseDirectory, dataSet);
                _cache[dataSet.Guid].LastAccessed = DateTime.Now;
                return _cache[dataSet.Guid].Data;
            } 
        }
    }

    internal class EnvironmentalCacheEntry
    {
        public DateTime Loaded { get; protected set; }
        public DateTime LastAccessed { get; internal set; }
        public EnvironmentDataSetBase Data { get; protected set; }
        public static EnvironmentalCacheEntry Load(string databaseDirectory, EnvironmentalDataSet dataSet)
        {
            var locationStorageDirectory = Path.Combine(databaseDirectory, dataSet.Location.StorageDirectory);
            switch ((PluginSubtype)dataSet.SourcePlugin.PluginSubtype)
            {
                case PluginSubtype.Wind:
                    return new EnvironmentalCacheEntry
                    {
                        Loaded = DateTime.Now,
                        Data = Wind.Load(Path.Combine(locationStorageDirectory, dataSet.FileName)),
                    };
                case PluginSubtype.SoundSpeed:
                    return new EnvironmentalCacheEntry
                    {
                        Loaded = DateTime.Now,
                        Data = SoundSpeed.Load(Path.Combine(locationStorageDirectory, dataSet.FileName)),
                    };
                case PluginSubtype.Sediment:
                    return new EnvironmentalCacheEntry
                    {
                        Loaded = DateTime.Now,
                        Data = Sediment.Load(Path.Combine(locationStorageDirectory, dataSet.FileName)),
                    };
                case PluginSubtype.Bathymetry:
                    return new EnvironmentalCacheEntry
                    {
                        Loaded = DateTime.Now,
                        Data = Bathymetry.Load(Path.Combine(locationStorageDirectory, dataSet.FileName)),
                    };
                default:
                    throw new ApplicationException(string.Format("Unknown environmental data type: {0}", dataSet.SourcePlugin.PluginSubtype));
            }
        }
    }
}
