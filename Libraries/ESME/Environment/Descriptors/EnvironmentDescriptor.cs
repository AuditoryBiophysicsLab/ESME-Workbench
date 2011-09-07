using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Cinch;
using ESME.Environment.NAVO;
using ESME.NEMO.Overlay;
using HRC.Navigation;

namespace ESME.Environment.Descriptors
{
    [Serializable]
    public class EnvironmentDescriptor : ViewModelBase
    {
        public EnvironmentDescriptor(string rangeComplexName, string overlayName)
        {
            RangeComplexName = rangeComplexName;
            OverlayName = overlayName;
            DataDirectoryPath = Path.Combine(Globals.AppSettings.ScenarioDataDirectory, RangeComplexName, "Data", OverlayName);
            var overlayFilePath = Path.Combine(Globals.AppSettings.ScenarioDataDirectory, RangeComplexName, "Areas", OverlayName + ".ovr");
            if (!File.Exists(overlayFilePath)) throw new ArgumentException("Specified overlay does not exist", "overlayName");
            var overlayFile = new OverlayFile(overlayFilePath);
            Region = new GeoRect(overlayFile.Shapes[0].BoundingBox);
            if (!Directory.Exists(DataDirectoryPath)) Directory.CreateDirectory(DataDirectoryPath);
            _temperatureArgs = new AsyncArgs<SoundSpeed>(_temperatureTask, _temperatureContinuation, SoundSpeed.LoadAsync, () => GDEM.ReadTemperatureAsync(NAVOConfiguration.AllMonths.ToList(), Region), task => Temperature = task.Result);
        }

        readonly object _lockObject = new object();

        #region public string RangeComplexName { get; private set; }

        public string RangeComplexName
        {
            get { return _rangeComplexName; }
            private set
            {
                if (_rangeComplexName == value) return;
                _rangeComplexName = value;
                NotifyPropertyChanged(RangeComplexNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RangeComplexNameChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentDescriptor>(x => x.RangeComplexName);
        string _rangeComplexName;

        #endregion

        #region public string OverlayName { get; private set; }

        public string OverlayName
        {
            get { return _overlayName; }
            private set
            {
                if (_overlayName == value) return;
                _overlayName = value;
                NotifyPropertyChanged(OverlayNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs OverlayNameChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentDescriptor>(x => x.OverlayName);
        string _overlayName;

        #endregion

        #region public string DataDirectoryPath { get; private set; }

        public string DataDirectoryPath
        {
            get { return _dataDirectoryPath; }
            private set
            {
                if (_dataDirectoryPath == value) return;
                _dataDirectoryPath = value;
                NotifyPropertyChanged(DataDirectoryPathChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DataDirectoryPathChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentDescriptor>(x => x.DataDirectoryPath);
        string _dataDirectoryPath;

        #endregion

        #region public GeoRect Region { get; private set; }

        public GeoRect Region
        {
            get { return _region; }
            private set
            {
                if (_region == value) return;
                _region = value;
                NotifyPropertyChanged(RegionChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RegionChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentDescriptor>(x => x.Region);
        GeoRect _region;

        #endregion

        #region public SoundSpeed Temperature { get; private set; }

        public SoundSpeed Temperature
        {
            get
            {
                if (_temperature == null) ReadDataAsync(true, "data.temperature", _temperatureArgs);
                return _temperature;
            }
            private set
            {
                if (_temperature == value) return;
                _temperature = value;
                NotifyPropertyChanged(TemperatureChangedEventArgs);
            }
        }

        async void ReadTemperatureAsync(bool blockMe)
        {
            lock (_lockObject)
            {
                if (_temperatureTask == null)
                {
                    var temperatureFilename = Path.Combine(DataDirectoryPath, "data.temperature");
                    if (File.Exists(temperatureFilename))
                    {
                        _temperatureTask = SoundSpeed.LoadAsync(temperatureFilename);
                    }
                    else
                    {
                        _temperatureTask = GDEM.ReadTemperatureAsync(NAVOConfiguration.AllMonths.ToList(), Region);
                        _temperatureContinuation = _temperatureTask.ContinueWith(task => task.Result.Save(temperatureFilename));
                    }
                }
            }
            if (blockMe)
            {
                _temperatureTask.Wait();
                if (_temperatureContinuation != null) _temperatureContinuation.Wait();
            }
            else
            {
                await _temperatureTask;
                if (_temperatureContinuation != null) await _temperatureContinuation;
            }
            Temperature = _temperatureTask.Result;
        }

        static readonly PropertyChangedEventArgs TemperatureChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentDescriptor>(x => x.Temperature);
        SoundSpeed _temperature;
        Task<SoundSpeed> _temperatureTask;
        Task _temperatureContinuation;
        readonly AsyncArgs<SoundSpeed> _temperatureArgs;
        #endregion

        class AsyncArgs<T> where T : ICanSave
        {
            public AsyncArgs(Task<T> mainTask, Task continuationTask, Func<string, Task<T>> loadFunc, Func<Task<T>> readFunc, Action<Task<T>> completionAction) 
            {
                MainTask = mainTask;
                ContinuationTask = continuationTask;
                LoadFunction = loadFunc;
                ReadFunction = readFunc;
                CompletionAction = completionAction;
            }

            public Task<T> MainTask { get; set; }
            public Task ContinuationTask { get; set; }
            public Func<string, Task<T>> LoadFunction { get; private set; }
            public Func<Task<T>> ReadFunction { get; private set; }
            public Action<Task<T>> CompletionAction { get; private set; }
        }

        async void ReadDataAsync<T>(bool blockMe, string dataFilename, AsyncArgs<T> args) where T : ICanSave
        {
            // args.Item1 = mainTask
            // args.Item2 = continuationTask
            // args.Item3 = loadFunc
            // args.Item4 = readFunc
            // args.Item5 = completionAction
            lock (_lockObject)
            {
                if (args.MainTask == null)
                {
                    var dataFile = Path.Combine(DataDirectoryPath, dataFilename);
                    if (File.Exists(dataFile))
                    {
                        args.MainTask = args.LoadFunction(dataFile);
                    }
                    else
                    {
                        args.MainTask = args.ReadFunction();
                        args.ContinuationTask = args.MainTask.ContinueWith(task => task.Result.Save(dataFile));
                    }
                }
            }
            if (blockMe)
            {
                args.MainTask.Wait();
                if (args.ContinuationTask != null) args.ContinuationTask.Wait();
            }
            else
            {
                await args.MainTask;
                if (args.ContinuationTask != null) await args.ContinuationTask;
            }
            args.CompletionAction(args.MainTask);
        }

        async void ReadDataAsync<T>(bool blockMe, Task<T> mainTask, Task continuationTask, string dataFilename, Func<string, Task<T>> loadFunc, Func<Task<T>> readFunc, Action<Task<T>> completionAction) where T : ICanSave
        {
            lock (_lockObject)
            {
                if (mainTask == null)
                {
                    var dataFile = Path.Combine(DataDirectoryPath, dataFilename);
                    if (File.Exists(dataFile))
                    {
                        mainTask = loadFunc(dataFile);
                    }
                    else
                    {
                        mainTask = readFunc();
                        continuationTask = mainTask.ContinueWith(task => task.Result.Save(dataFile));
                    }
                }
            }
            if (blockMe)
            {
                mainTask.Wait();
                if (continuationTask != null) continuationTask.Wait();
            }
            else
            {
                await mainTask;
                if (continuationTask != null) await continuationTask;
            }
            completionAction(mainTask);
        }
    }
}
