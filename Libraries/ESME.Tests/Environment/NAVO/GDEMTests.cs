using System.IO;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using ESME.Environment;
using ESME.Environment.Descriptors;
using ESME.Environment.NAVO;
using ESME.NEMO.Overlay;
using HRC.Navigation;
using NUnit.Framework;

namespace ESME.Tests.Environment.NAVO
{
    public class GDEMTests
    {
        [Test]
        public void AsyncDataflowImportTest()
        {
            var temperatureWorker = new ActionBlock<ImportJobDescriptor>(job =>
            {
                var temperatureField = GDEM.ReadFile(GDEM.FindTemperatureFile(job.TimePeriod), "water_temp", job.TimePeriod, job.GeoRect);
                var temperature = new SoundSpeed();
                temperature.SoundSpeedFields.Add(temperatureField);
                temperature.Serialize(job.DestinationFilename);
                job.Resolution = 15;
                job.SampleCount = (uint)temperature[0].EnvironmentData.Count;
                //job.CompletionTask.Start();
                //await job.CompletionTask;
            },
            new ExecutionDataflowBlockOptions
            {
                TaskScheduler = TaskScheduler.Default,
                BoundedCapacity = 4,
                MaxDegreeOfParallelism = 4,
            });

            var salinityWorker = new ActionBlock<ImportJobDescriptor>(job =>
            {
                var salinityField = GDEM.ReadFile(GDEM.FindSalinityFile(job.TimePeriod), "salinity", job.TimePeriod, job.GeoRect);
                var salinity = new SoundSpeed();
                salinity.SoundSpeedFields.Add(salinityField);
                salinity.Serialize(job.DestinationFilename);
                job.Resolution = 15;
                job.SampleCount = (uint)salinity[0].EnvironmentData.Count;
                //job.CompletionTask.Start();
                //await job.CompletionTask;
            },
            new ExecutionDataflowBlockOptions
            {
                TaskScheduler = TaskScheduler.Default,
                BoundedCapacity = 4,
                MaxDegreeOfParallelism = 4,
            });

            var overlayFile = new OverlayFile(@"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Sim Areas\NBOA\Areas\NBOA.ovr");
            var geoRect = new GeoRect(overlayFile.Shapes[0].BoundingBox);
            temperatureWorker.Post(new ImportJobDescriptor
            {
                GeoRect = geoRect,
                TimePeriod = TimePeriod.January,
                DestinationFilename = @"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Sim Areas\NBOA\Data\january.temperature",
                DataType = EnvironmentDataType.Temperature,
            });
            temperatureWorker.Post(new ImportJobDescriptor
            {
                GeoRect = geoRect,
                TimePeriod = TimePeriod.January,
                DestinationFilename = @"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Sim Areas\NBOA\Data\january.salinity",
                DataType = EnvironmentDataType.Salinity,
            });
            temperatureWorker.Post(new ImportJobDescriptor
            {
                GeoRect = geoRect,
                TimePeriod = TimePeriod.February,
                DestinationFilename = @"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Sim Areas\NBOA\Data\february.temperature",
                DataType = EnvironmentDataType.Temperature,
            });
            temperatureWorker.Post(new ImportJobDescriptor
            {
                GeoRect = geoRect,
                TimePeriod = TimePeriod.February,
                DestinationFilename = @"C:\Users\Dave Anderson\Desktop\NAEMO demos\BU Test Sample\Sim Areas\NBOA\Data\february.salinity",
                DataType = EnvironmentDataType.Salinity,
            });
        }

        ImportJobDescriptor NewImportJobDescriptor(EnvironmentDataType dataType, GeoRect geoRect, TimePeriod timePeriod, string destinationDirectory)
        {
            return new ImportJobDescriptor
            {
                GeoRect = geoRect,
                TimePeriod = TimePeriod.February,
                DestinationFilename = Path.Combine(destinationDirectory, timePeriod.ToString().ToLower() + "." + dataType.ToString().ToLower()),
                DataType = dataType,
            };
        }
    }
}
