using System;
using System.Collections.Concurrent;
using System.ComponentModel.Composition;
using System.Diagnostics;
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
using ESME.Plugins;
using ESME.Scenarios;
using HRC.Navigation;
using HRC.Utility;
using HRC.ViewModels;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Locations
{
    public interface IEnvironmentalCacheService 
    {
        PercentProgressList<PercentProgressList<Location>> ImportMissingDatasets();
        PercentProgressList<Location> ImportLocationDatasets(Location location);
        PercentProgress<EnvironmentalDataSet> ImportDataset(EnvironmentalDataSet dataSet);
        void ImportDatasetTest(EnvironmentalDataSet dataSet);
        int BusyCount { get; }
        void ToBitmap<T>(EnvironmentData<T> data, string fileName, Func<T, float> valueFunc, Func<float[,], float, float, uint[,]> toPixelValuesFunc) where T: Geo, new();
        bool IsCached(EnvironmentalDataSet dataSet);
        Task<EnvironmentDataSetBase> this[EnvironmentalDataSet dataSet] { get; }
    }

    [PartCreationPolicy(CreationPolicy.Shared)]
    [ExportService(ServiceType.Both, typeof(EnvironmentalCacheService))]
    public class EnvironmentalCacheService : ViewModelBase, IEnvironmentalCacheService
    {
        public EnvironmentalCacheService() { }
        public EnvironmentalCacheService(IPluginManagerService plugins, IMasterDatabaseService database) : this()
        {
            _plugins = plugins;
            _database = database;
        }

        [Import] IPluginManagerService _plugins;
        [Import] IMasterDatabaseService _database;

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

        internal Task<EnvironmentalCacheEntry> CreateImportDatasetTask(EnvironmentalDataSet dataSet)
        {
            var job = new PercentProgress<EnvironmentalDataSet> { ProgressTarget = dataSet };
            if (!_importJobsPending.TryAdd(dataSet.Guid, job)) return null;
            var task = new Task<EnvironmentalCacheEntry>(() =>
            {
                EnvironmentalCacheEntry result;
                Import(job, out result);
                return result;
            });
            task.Start();
            return task;
        }

        public void ImportDatasetTest(EnvironmentalDataSet dataSet)
        {
            var job = new PercentProgress<EnvironmentalDataSet> { ProgressTarget = dataSet };
            if (!_importJobsPending.TryAdd(dataSet.Guid, job)) return;
            EnvironmentalCacheEntry result;
            Import(job, out result);
        }

        readonly ConcurrentDictionary<Guid, PercentProgress<EnvironmentalDataSet>> _importJobsPending = new ConcurrentDictionary<Guid, PercentProgress<EnvironmentalDataSet>>();

        int _busyCount;
        public int BusyCount { get { return _busyCount; } }

        ActionBlock<PercentProgress<EnvironmentalDataSet>> ImportActionBlock { get; set; }

        ActionBlock<PercentProgress<EnvironmentalDataSet>> CreateImporter(TaskScheduler taskScheduler = null, int boundedCapacity = -1, int maxDegreeOfParallelism = -1)
        {
            if (taskScheduler == null) taskScheduler = TaskScheduler.Default;
            if (_plugins == null) throw new ServiceNotFoundException("Required service PluginManager was not found");
            if (_database == null) throw new ServiceNotFoundException("Required service MasterDatabaseService was not found");
            if (string.IsNullOrEmpty(_database.MasterDatabaseDirectory)) throw new ServiceNotFoundException("Required service MasterDatabaseService is not properly configured.");
            var newImporter = new ActionBlock<PercentProgress<EnvironmentalDataSet>>
                (
                    job =>
                    {
                        EnvironmentalCacheEntry cacheEntry;
                        Import(job, out cacheEntry);
                    },
                    new ExecutionDataflowBlockOptions
                    {
                        TaskScheduler = taskScheduler,
                        BoundedCapacity = boundedCapacity,
                        MaxDegreeOfParallelism = maxDegreeOfParallelism,
                    }
                );
            return newImporter;
        }

        void Import(PercentProgress<EnvironmentalDataSet> job, out EnvironmentalCacheEntry cacheEntry)
        {
            cacheEntry = new EnvironmentalCacheEntry();
            Interlocked.Increment(ref _busyCount);
            //OnPropertyChanged("BusyCount");
            var dataSet = job.ProgressTarget;
            var sourcePlugin = (EnvironmentalDataSourcePluginBase)_plugins[dataSet.SourcePlugin];
            var fileName = Path.Combine(dataSet.Location.StorageDirectoryPath, dataSet.FileName);
            job.Report(0);
            Debug.WriteLine("Importer: About to import {0}[{1}] from plugin {2}", (PluginSubtype)dataSet.SourcePlugin.PluginSubtype, dataSet.Resolution, sourcePlugin.PluginName);
            var curJob = job;
            switch (sourcePlugin.EnvironmentDataType)
            {
                case EnvironmentDataType.Wind:
                    cacheEntry.Data = new Task<EnvironmentDataSetBase>(() =>
                    {
                        var wind = ((EnvironmentalDataSourcePluginBase<Wind>)sourcePlugin).Extract(dataSet.Location.GeoRect, dataSet.Resolution, dataSet.TimePeriod, curJob);
                        dataSet.SampleCount = (from period in wind.TimePeriods select period.EnvironmentData.Count).Sum();
                        if (dataSet.SampleCount == 0) return null;
                        wind.Serialize(fileName);
                        dataSet.PluginXml = sourcePlugin.Xml;
                        var windColormap = new Colormap(Colormap.CoolComponents, 512);
                        ToBitmap(wind[dataSet.TimePeriod].EnvironmentData, fileName, v => v == null ? 0 : v.Data, windColormap.ToPixelValues);
                        Debug.WriteLine("Importer: Imported {0}min Wind [{1}] from plugin {2}", dataSet.Resolution, (TimePeriod)dataSet.TimePeriod, sourcePlugin.PluginName);
                        return wind;
                    });
                    break;
                case EnvironmentDataType.Sediment:
                    cacheEntry.Data = new Task<EnvironmentDataSetBase>(() =>
                    {
                        var sediment = ((EnvironmentalDataSourcePluginBase<Sediment>)sourcePlugin).Extract(dataSet.Location.GeoRect, dataSet.Resolution, TimePeriod.Invalid, curJob);
                        dataSet.SampleCount = sediment.Samples.Count;
                        if (dataSet.SampleCount == 0) return null;
                        sediment.Serialize(fileName);
                        var sedimentColormap = new Colormap(Colormap.HotComponents, 24);
                        sedimentColormap.Map.Insert(0, Colors.Black);
                        sedimentColormap.Map.Add(Colors.Black);
                        ToBitmap(sediment.Samples, fileName, v => v == null ? 0 : v.Data.SampleValue, (data, minValue, maxValue) => sedimentColormap.ToPixelValues(data, 1, 24));
                        Debug.WriteLine("Importer: Imported {0}min Sediment from plugin {1}", dataSet.Resolution, sourcePlugin.PluginName);
                        return sediment;
                    });
                    break;
                case EnvironmentDataType.SoundSpeed:
                    cacheEntry.Data = new Task<EnvironmentDataSetBase>(() =>
                    {
                        var soundSpeed = ((EnvironmentalDataSourcePluginBase<SoundSpeed>)sourcePlugin).Extract(dataSet.Location.GeoRect, dataSet.Resolution, dataSet.TimePeriod, curJob);
                        dataSet.SampleCount = (from field in soundSpeed.SoundSpeedFields select field.EnvironmentData.Count).Sum();
                        if (dataSet.SampleCount == 0) return null;
                        soundSpeed.Serialize(fileName);
                        Debug.WriteLine("Importer: Imported {0}min Sound Speed [{1}] from plugin {2}", dataSet.Resolution, (TimePeriod)dataSet.TimePeriod, sourcePlugin.PluginName);
                        return soundSpeed;
                    });
                    break;
                case EnvironmentDataType.Bathymetry:
                    cacheEntry.Data = new Task<EnvironmentDataSetBase>(() =>
                    {
                        var bathymetry = ((EnvironmentalDataSourcePluginBase<Bathymetry>)sourcePlugin).Extract(dataSet.Location.GeoRect, dataSet.Resolution, TimePeriod.Invalid, curJob);
                        dataSet.SampleCount = bathymetry.Samples.Count;
                        if (dataSet.SampleCount == 0) return null;
                        bathymetry.Save(fileName);
                        //var bathymetryColormap = new Colormap(Colormap.OceanComponents, 1024);
                        var dualColormap = new DualColormap(Colormap.Summer, Colormap.Jet) { Threshold = 0 };
                        ToBitmap(bathymetry.Samples, fileName, v => v.Data, (data, minValue, maxValue) => dualColormap.ToPixelValues(data, minValue, maxValue < 0 ? maxValue : 8000, Colors.Black));
                        Debug.WriteLine("Importer: Imported {0}min Bathymetry from plugin {1}", dataSet.Resolution, sourcePlugin.PluginName);
                        return bathymetry;
                    });
                    break;
                default:
                    throw new ApplicationException(string.Format("Unknown environmental data type {0}", sourcePlugin.EnvironmentDataType));
            }
            cacheEntry.Data.Start();
            cacheEntry.Loaded = DateTime.Now;
            job.Report(100);
            Interlocked.Decrement(ref _busyCount);
            //OnPropertyChanged("BusyCount");
            _importJobsPending.TryRemove(dataSet.Guid, out job);
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
            if (imagePath == null) throw new ArgumentException("Must be a fully specified path", "fileName");

            var bitmapData = new float[displayValues.Longitudes.Count, displayValues.Latitudes.Count];
            for (var latIndex = 0; latIndex < bitmapData.GetLength(1); latIndex++) 
                for (var lonIndex = 0; lonIndex < bitmapData.GetLength(0); lonIndex++) 
                    bitmapData[lonIndex, latIndex] = valueFunc(displayValues[(uint)lonIndex, (uint)latIndex]);
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

        public bool IsCached(EnvironmentalDataSet dataSet)
        {
            EnvironmentalCacheEntry requestedData;
            var isCached = _cache.TryGetValue(dataSet.Guid, out requestedData);
            return isCached;
        }

        public Task<EnvironmentDataSetBase> this[EnvironmentalDataSet dataSet]
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
                //Debug.WriteLine(string.Format("Cache: Loading {0}", (PluginSubtype)dataSet.SourcePlugin.PluginSubtype));
                _cache[dataSet.Guid] = EnvironmentalCacheEntry.Load(_database.MasterDatabaseDirectory, dataSet, this);
                _cache[dataSet.Guid].LastAccessed = DateTime.Now;
                return _cache[dataSet.Guid].Data;
            } 
        }
    }

    internal class EnvironmentalCacheEntry
    {
        public DateTime Loaded { get; internal set; }
        public DateTime LastAccessed { get; internal set; }
        public Task<EnvironmentDataSetBase> Data { get; internal set; }
        public static EnvironmentalCacheEntry Load(string databaseDirectory, EnvironmentalDataSet dataSet, EnvironmentalCacheService cache)
        {
            var fileName = Path.Combine(dataSet.Location.StorageDirectoryPath, dataSet.FileName);
            if (!File.Exists(fileName))
            {
                try
                {
                    var importTask = cache.CreateImportDatasetTask(dataSet);
                    return importTask.Result;
                }
                catch (AggregateException e)
                {
                    Debug.WriteLine("Exception caught: {0}", e.Message);
                }
            }
            EnvironmentalCacheEntry result;
            switch ((PluginSubtype)dataSet.SourcePlugin.PluginSubtype)
            {
                case PluginSubtype.Wind:
                    result = new EnvironmentalCacheEntry
                    {
                        Loaded = DateTime.Now,
                        Data = new Task<EnvironmentDataSetBase>(() => Wind.Load(Path.Combine(dataSet.Location.StorageDirectoryPath, dataSet.FileName))),
                    };
                    break;
                case PluginSubtype.SoundSpeed:
                    result = new EnvironmentalCacheEntry
                    {
                        Loaded = DateTime.Now,
                        Data = new Task<EnvironmentDataSetBase>(() => SoundSpeed.Load(Path.Combine(dataSet.Location.StorageDirectoryPath, dataSet.FileName))),
                    };
                    break;
                case PluginSubtype.Sediment:
                    result = new EnvironmentalCacheEntry
                    {
                        Loaded = DateTime.Now,
                        Data = new Task<EnvironmentDataSetBase>(() => Sediment.Load(Path.Combine(dataSet.Location.StorageDirectoryPath, dataSet.FileName))),
                    };
                    break;
                case PluginSubtype.Bathymetry:
                    result = new EnvironmentalCacheEntry
                    {
                        Loaded = DateTime.Now,
                        Data = new Task<EnvironmentDataSetBase>(() => Bathymetry.Load(Path.Combine(dataSet.Location.StorageDirectoryPath, dataSet.FileName))),
                    };
                    break;
                default:
                    throw new ApplicationException(string.Format("Unknown environmental data type: {0}", dataSet.SourcePlugin.PluginSubtype));
            }
            result.Data.Start();
            return result;
        }
    }
}
