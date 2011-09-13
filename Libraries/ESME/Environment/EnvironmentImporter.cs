using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using HRC.Navigation;
using ESME.Environment.NAVO;

namespace ESME.Environment
{
    public static class NAVOImporter
    {
        static readonly object LockObject = new object();
        static ActionBlock<ImportJobDescriptor> _worker, _temperatureWorker, _salinityWorker, _sedimentWorker, _windWorker, _bathymetryWorker, _bottomLossWorker;
        public static void Import(ImportJobDescriptor jobDescriptor)
        {
            lock(LockObject)
            {
                if (_temperatureWorker == null) _temperatureWorker = new ActionBlock<ImportJobDescriptor>(async job =>
                {
                    Debug.WriteLine("{0} About to import {1} {2}", DateTime.Now, job.DataType, job.DestinationFilename);
                    var temperatureField = GDEM.ReadFile(GDEM.FindTemperatureFile(job.TimePeriod), "water_temp", job.TimePeriod, job.GeoRect);
                    var temperature = new SoundSpeed();
                    temperature.SoundSpeedFields.Add(temperatureField);
                    temperature.Save(job.DestinationFilename);
                    job.Resolution = 15;
                    job.SampleCount = (uint)temperatureField.EnvironmentData.Count;
                    job.CompletionAction(job);
                    Debug.WriteLine("{0} Finished importing {1}", DateTime.Now, job.DestinationFilename);
                }, new ExecutionDataflowBlockOptions
                {
                    TaskScheduler = TaskScheduler.Default,
                    MaxDegreeOfParallelism = 1,
                });
                if (_salinityWorker == null) _salinityWorker = new ActionBlock<ImportJobDescriptor>(async job =>
                {
                    Debug.WriteLine("{0} About to import {1} {2}", DateTime.Now, job.DataType, job.DestinationFilename);
                    var salinityField = GDEM.ReadFile(GDEM.FindSalinityFile(job.TimePeriod), "salinity", job.TimePeriod, job.GeoRect);
                    var salinity = new SoundSpeed();
                    salinity.SoundSpeedFields.Add(salinityField);
                    salinity.Save(job.DestinationFilename);
                    job.Resolution = 15;
                    job.SampleCount = (uint)salinityField.EnvironmentData.Count;
                    job.CompletionAction(job);
                    Debug.WriteLine("{0} Finished importing {1}", DateTime.Now, job.DestinationFilename);
                }, new ExecutionDataflowBlockOptions
                {
                    TaskScheduler = TaskScheduler.Default,
                    MaxDegreeOfParallelism = 1,
                });
                if (_sedimentWorker == null) _sedimentWorker = new ActionBlock<ImportJobDescriptor>(async job =>
                {
                    Debug.WriteLine("{0} About to import {1} {2}", DateTime.Now, job.DataType, job.DestinationFilename);
                    var sediment = BST.Extract(job.GeoRect);
                    sediment.Save(job.DestinationFilename);
                    job.SampleCount = (uint)sediment.Samples.Count;
                    job.Resolution = 5;
                    job.CompletionAction(job);
                    Debug.WriteLine("{0} Finished importing {1}", DateTime.Now, job.DestinationFilename);
                }, new ExecutionDataflowBlockOptions
                {
                    TaskScheduler = TaskScheduler.Default,
                    MaxDegreeOfParallelism = 1,
                });
                if (_windWorker == null) _windWorker = new ActionBlock<ImportJobDescriptor>(async job =>
                {
                    Debug.WriteLine("{0} About to import {1} {2}", DateTime.Now, job.DataType, job.DestinationFilename);
                    var wind = await SMGC.NewImportAsync(job.GeoRect);
                    wind.Save(job.DestinationFilename);
                    job.SampleCount = (uint)wind.TimePeriods[0].EnvironmentData.Count;
                    job.Resolution = 60;
                    job.CompletionAction(job);
                    Debug.WriteLine("{0} Finished importing {1}", DateTime.Now, job.DestinationFilename);
                }, new ExecutionDataflowBlockOptions
                {
                    TaskScheduler = TaskScheduler.Default,
                    MaxDegreeOfParallelism = 1,
                });
                if (_bathymetryWorker == null) _bathymetryWorker = new ActionBlock<ImportJobDescriptor>(async job =>
                {
                    Debug.WriteLine("{0} About to import {1} {2}", DateTime.Now, job.DataType, job.DestinationFilename);
                    var bathymetry = DBDB.Extract(job.Resolution, job.GeoRect);
                    bathymetry.Save(job.DestinationFilename);
                    job.SampleCount = (uint)bathymetry.Samples.Count;
                    job.CompletionAction(job);
                    Debug.WriteLine("{0} Finished importing {1}", DateTime.Now, job.DestinationFilename);
                }, new ExecutionDataflowBlockOptions
                {
                    TaskScheduler = TaskScheduler.Default,
                    MaxDegreeOfParallelism = 1,
                });
                if (_bottomLossWorker == null) _bottomLossWorker = new ActionBlock<ImportJobDescriptor>(async job =>
                {
                    Debug.WriteLine("{0} About to import {1} {2}", DateTime.Now, job.DataType, job.DestinationFilename);
                    var bottomLoss = await BottomLossDatabase.ExtractAsync(job.GeoRect);
                    bottomLoss.Save(job.DestinationFilename);
                    job.SampleCount = (uint)bottomLoss.Samples.Count;
                    job.Resolution = 15;
                    job.CompletionAction(job);
                    Debug.WriteLine("{0} Finished importing {1}", DateTime.Now, job.DestinationFilename);
                }, new ExecutionDataflowBlockOptions
                {
                    TaskScheduler = TaskScheduler.Default,
                    MaxDegreeOfParallelism = 1,
                });
                if (_worker == null) _worker = new ActionBlock<ImportJobDescriptor>(async job =>
                {
                    Debug.WriteLine("{0} About to import {1} {2}", DateTime.Now, job.DataType, job.DestinationFilename);
                    switch (job.DataType)
                    {
                        case EnvironmentDataType.Temperature:
                            var temperatureField = GDEM.ReadFile(GDEM.FindTemperatureFile(job.TimePeriod), "water_temp", job.TimePeriod, job.GeoRect);
                            var temperature = new SoundSpeed();
                            temperature.SoundSpeedFields.Add(temperatureField);
                            temperature.Save(job.DestinationFilename);
                            job.Resolution = 15;
                            job.SampleCount = (uint)temperatureField.EnvironmentData.Count;
                            break;
                        case EnvironmentDataType.Salinity:
                            var salinityField = GDEM.ReadFile(GDEM.FindSalinityFile(job.TimePeriod), "salinity", job.TimePeriod, job.GeoRect);
                            var salinity = new SoundSpeed();
                            salinity.SoundSpeedFields.Add(salinityField);
                            salinity.Save(job.DestinationFilename);
                            job.Resolution = 15;
                            job.SampleCount = (uint)salinityField.EnvironmentData.Count;
                            break;
                        case EnvironmentDataType.Sediment:
                            var sediment = BST.Extract(job.GeoRect);
                            sediment.Save(job.DestinationFilename);
                            job.SampleCount = (uint)sediment.Samples.Count;
                            job.Resolution = 5;
                            break;
                        case EnvironmentDataType.Wind:
                            var wind = await SMGC.NewImportAsync(job.GeoRect);
                            wind.Save(job.DestinationFilename);
                            job.SampleCount = (uint)wind.TimePeriods[0].EnvironmentData.Count;
                            job.Resolution = 60;
                            break;
                        case EnvironmentDataType.Bathymetry:
                            var bathymetry = DBDB.Extract(job.Resolution, job.GeoRect);
                            bathymetry.Save(job.DestinationFilename);
                            job.SampleCount = (uint)bathymetry.Samples.Count;
                            break;
                        case EnvironmentDataType.BottomLoss:
                            var bottomLoss = await BottomLossDatabase.ExtractAsync(job.GeoRect);
                            bottomLoss.Save(job.DestinationFilename);
                            job.SampleCount = (uint)bottomLoss.Samples.Count;
                            job.Resolution = 15;
                            break;
                    }
                    job.CompletionAction(job);
                    Debug.WriteLine("{0} Finished importing {1}", DateTime.Now, job.DestinationFilename);
                }, new ExecutionDataflowBlockOptions
                {
                    TaskScheduler = TaskScheduler.Default,
                    MaxDegreeOfParallelism = 1,
                });
            }
            switch (jobDescriptor.DataType)
            {
                case EnvironmentDataType.Bathymetry:
                    _bathymetryWorker.Post(jobDescriptor);
                    break;
                case EnvironmentDataType.BottomLoss:
                    _bottomLossWorker.Post(jobDescriptor);
                    break;
                case EnvironmentDataType.Salinity:
                    _salinityWorker.Post(jobDescriptor);
                    break;
                case EnvironmentDataType.Sediment:
                    _sedimentWorker.Post(jobDescriptor);
                    break;
                case EnvironmentDataType.Temperature:
                    _temperatureWorker.Post(jobDescriptor);
                    break;
                case EnvironmentDataType.Wind:
                    _windWorker.Post(jobDescriptor);
                    break;
            }
        }

        public static void Import(IEnumerable<ImportJobDescriptor> jobDescriptors)
        {
            foreach (var jobDescriptor in jobDescriptors) Import(jobDescriptor);
        }
    }



    public class ImportJobDescriptor
    {
        public EnvironmentDataType DataType { get; set; }
        public GeoRect GeoRect { get; set; }
        public NAVOTimePeriod TimePeriod { get; set; }
        public float Resolution { get; set; }
        public string DestinationFilename { get; set; }
        public uint SampleCount { get; set; }
        public Action<ImportJobDescriptor> CompletionAction { get; set; }
    }

    public enum EnvironmentDataType
    {
        Temperature,
        Salinity,
        Sediment,
        Wind,
        Bathymetry,
        BottomLoss,
    }
}
