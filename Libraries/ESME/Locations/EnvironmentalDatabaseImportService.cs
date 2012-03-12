using System;
using System.ComponentModel.Composition;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using ESME.Database;
using ESME.Environment;
using ESME.Environment.Descriptors;
using ESME.Plugins;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Locations
{
    [PartCreationPolicy(CreationPolicy.Shared)]
    [ExportService(ServiceType.Both, typeof(EnvironmentalDatabaseImportService))]
    public class EnvironmentalDatabaseImportService
    {
        public EnvironmentalDatabaseImportService() { _importer = CreateImporter(); }

        [Import] IPluginManagerService _pluginManagerService;
        [Import] LocationManagerService _locationManagerService;

        public void BeginImport(EnvironmentalDataSet dataSet, IProgress<float> progress = null ) { _importer.Post(Tuple.Create(dataSet, progress)); }

        readonly ActionBlock<Tuple<EnvironmentalDataSet, IProgress<float>>> _importer;
        
        ActionBlock<Tuple<EnvironmentalDataSet, IProgress<float>>> CreateImporter(TaskScheduler taskScheduler = null, int boundedCapacity = -1, int maxDegreeOfParallelism = -1)
        {
            if (taskScheduler == null) taskScheduler = TaskScheduler.Default;
            if (_pluginManagerService == null) throw new ServiceNotFoundException("Required service PluginManager was not found");
            if (_locationManagerService == null) throw new ServiceNotFoundException("Required service LocationManager was not found");
            var newImporter = new ActionBlock<Tuple<EnvironmentalDataSet, IProgress<float>>>(job =>
            {
                var dataSet = job.Item1;
                var progress = job.Item2;
                var sourcePlugin = (EnvironmentalDataSourcePluginBase)_pluginManagerService[dataSet.EnvironmentalDataSetCollection.SourcePlugin];
                var geoRect = dataSet.EnvironmentalDataSetCollection.Location.GeoRect;
                var resolution = dataSet.Resolution;
                var timePeriod = dataSet.TimePeriod;
                var fileName = Path.Combine(_locationManagerService.LocationRootDirectory, dataSet.EnvironmentalDataSetCollection.Location.StorageDirectory, dataSet.FileName);
                switch (sourcePlugin.EnvironmentDataType)
                {
                    case EnvironmentDataType.Wind:
                        var wind = ((EnvironmentalDataSourcePluginBase<Wind>)sourcePlugin).Extract(geoRect, resolution, timePeriod, progress);
                        dataSet.SampleCount = (from period in wind.TimePeriods select period.EnvironmentData.Count).Sum();
                        wind.Save(fileName);
                        break;
                    case EnvironmentDataType.Sediment:
                        var sediment = ((EnvironmentalDataSourcePluginBase<Sediment>)sourcePlugin).Extract(geoRect, resolution, timePeriod, progress);
                        dataSet.SampleCount = sediment.Samples.Count;
                        sediment.Save(fileName);
                        break;
                    case EnvironmentDataType.SoundSpeed:
                        var soundSpeed = ((EnvironmentalDataSourcePluginBase<SoundSpeed>)sourcePlugin).Extract(geoRect, resolution, timePeriod, progress);
                        dataSet.SampleCount = (from field in soundSpeed.SoundSpeedFields select field.EnvironmentData.Count).Sum();
                        soundSpeed.Save(fileName);
                        break;
                    case EnvironmentDataType.Bathymetry:
                        var bathymetry = ((EnvironmentalDataSourcePluginBase<Bathymetry>)sourcePlugin).Extract(geoRect, resolution, timePeriod, progress);
                        dataSet.SampleCount = bathymetry.Samples.Count;
                        bathymetry.Save(fileName);
                        break;
                    default:
                        throw new ApplicationException(string.Format("Unknown environmental data type {0}", sourcePlugin.EnvironmentDataType));
                }
                dataSet.FileSize = new FileInfo(fileName).Length;
                //_locationManagerService.SaveChanges();
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
