using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Threading;
using Cinch;
using ESME.Environment;
using ESME.Environment.Descriptors;
using ESME.Environment.NAVO;
using ESME.TransmissionLoss.CASS;
using HRC.Navigation;

namespace OneNavyModel.ViewModels.NAVO
{
    public class ExportAllEnvironmentalDataProgressViewModel:ViewModelBase
    {
        #region public int CurFileNumber { get; set; }

        public int CurFileNumber
        {
            get { return _curFileNumber; }
            set
            {
                if (_curFileNumber == value) return;
                _curFileNumber = value;
                NotifyPropertyChanged(CurFileNumberChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CurFileNumberChangedEventArgs = ObservableHelper.CreateArgs<ExportAllEnvironmentalDataProgressViewModel>(x => x.CurFileNumber);
        int _curFileNumber;

        #endregion

        #region public int MaxFileCount { get; set; }

        public int MaxFileCount
        {
            get { return _maxFileCount; }
            set
            {
                if (_maxFileCount == value) return;
                _maxFileCount = value;
                NotifyPropertyChanged(MaxFileCountChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MaxFileCountChangedEventArgs = ObservableHelper.CreateArgs<ExportAllEnvironmentalDataProgressViewModel>(x => x.MaxFileCount);
        int _maxFileCount;

        #endregion

        #region public string Message { get; set; }

        public string Message
        {
            get { return _message; }
            set
            {
                if (_message == value) return;
                _message = value;
                NotifyPropertyChanged(MessageChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MessageChangedEventArgs = ObservableHelper.CreateArgs<ExportAllEnvironmentalDataProgressViewModel>(x => x.Message);
        string _message;

        #endregion

        #region CancelCommand
        public SimpleCommand<object, object> CancelCommand
        {
            get { return _cancel ?? (_cancel = new SimpleCommand<object, object>(delegate { CancelHandler(); })); }
        }

        SimpleCommand<object, object> _cancel;
        bool _isCanceled;

        void CancelHandler() { _isCanceled = true; }
        #endregion


        public ExportAllEnvironmentalDataProgressViewModel(NewRangeComplex rangeComplex, Dispatcher dispatcher) 
        {
            var lockObject = new object();
            //for each op area 
            var bathymetryFileCount = (from area in rangeComplex.AreaList from resolution in area.BathymetryList where resolution.IsCached select 1).Sum();
            var curBathymetryFileNumber = 0;
            dispatcher.InvokeInBackgroundIfRequired(() =>
            {
                MaxFileCount = bathymetryFileCount * 72;
                CurFileNumber = 0;
                Message = "Exporting bathymetry files...";
            });
            foreach (var area in rangeComplex.AreaList)
            {
                foreach (var resolution in area.BathymetryList)
                {
                    if (resolution.IsCached)
                    {
                        var area1 = area;
                        var resolution1 = resolution;
                        var requestedLocations = new List<Geo>();

                        double lat, lon;
                        for (lon = area1.GeoRect.West; lon < area1.GeoRect.East; lon += 0.25)
                        {
                            for (lat = area1.GeoRect.South; lat < area1.GeoRect.North; lat += 0.25) requestedLocations.Add(new Geo(lat, lon));
                            if ((lat - area1.GeoRect.North) < 0.125) requestedLocations.Add(new Geo(lat, lon));
                        }
                        if ((lon - area1.GeoRect.East) < 0.125)
                            for (lat = area1.GeoRect.South; lat < area1.GeoRect.North; lat += 0.25)
                                requestedLocations.Add(new Geo(lat, lon));

                        //bathymetry
                        var bathyFileName = Path.Combine(rangeComplex.BathymetryPath, string.Format("{0}_{1}_bathy.txt", area1.Name, resolution1.Name));
                        var bathyTask = new Task<Bathymetry>(() => Bathymetry.Load(Path.Combine(area1.BathymetryPath, resolution1.FileName)));
                        bathyTask.Start();
                        bathyTask.ContinueWith(task =>
                        {
                            if (_isCanceled)
                            {
                                CloseActivePopUpCommand.Execute(false);
                                return;
                            }
                            if (!File.Exists(bathyFileName))
                            {
                                task.Result.ToYXZ(bathyFileName, -1);
                                dispatcher.InvokeInBackgroundIfRequired(() =>
                                {
                                    curBathymetryFileNumber++;
                                    Message = string.Format("Exported bathymetry file {0} of {1}", curBathymetryFileNumber, bathymetryFileCount);
                                });
                            }
                            var bottomLossTask = new Task<BottomLoss>(() => BottomLoss.Load(Path.Combine(rangeComplex.DataPath, "data.bottomloss")));
                            var sedimentTask = new Task<Sediment>(() => Sediment.Load(Path.Combine(rangeComplex.DataPath, "data.sediment")));

                            bottomLossTask.Start();
                            sedimentTask.Start();
                            TaskEx.WhenAll(bottomLossTask, sedimentTask).ContinueWith(task1 =>
                            {
                                var sedimentPoints = new List<SedimentSample>();
                                var bottomLossPoints = new List<BottomLossSample>();

                                foreach (var location in requestedLocations)
                                {
                                    sedimentPoints.Add(sedimentTask.Result.Samples[location]);
                                    if (bottomLossTask.Result != null && bottomLossTask.Result.Samples != null && bottomLossTask.Result.Samples.Count > 0) bottomLossPoints.Add(bottomLossTask.Result.Samples[location]);
                                }

                                foreach (var timePeriod in NAVOConfiguration.AllTimePeriods)
                                {
                                    var period = timePeriod;
                                    var cassEnvironmentFileName = Path.Combine(rangeComplex.EnvironmentPath, string.Format("{0}_{1}_env_{2}", area1.Name, resolution1.Name, period));
                                    var windTask = new Task<Wind>(() => Wind.Load(Path.Combine(rangeComplex.DataPath, "data.wind")));

                                    var soundSpeedTask = new Task<SoundSpeed>(() => EnvironmentFile.CalculateSoundSpeed(rangeComplex, period, bathyTask, resolution1.GeoRect));
                                    windTask.Start();
                                    soundSpeedTask.Start();

                                    var period1 = timePeriod;
                                    TaskEx.WhenAll(windTask, soundSpeedTask).ContinueWith(task2 =>
                                    {
                                        var soundSpeedProfiles = new List<SoundSpeedProfile>();
                                        var windSamples = new List<WindSample>();
                                        foreach (var location in requestedLocations)
                                        {
                                            soundSpeedProfiles.Add(soundSpeedTask.Result[period1].EnvironmentData[location]);
                                            windSamples.Add((windTask.Result[period1].EnvironmentData[location]));
                                        }
                                        CASSFiles.WriteEnvironmentFiles(period1, requestedLocations, cassEnvironmentFileName, sedimentPoints, soundSpeedProfiles, windSamples, bathyFileName, area1.Name + ".ovr", bottomLossPoints);
                                        dispatcher.InvokeInBackgroundIfRequired(() =>
                                        {
                                            CurFileNumber++;
                                            Message = string.Format("Exported environment file {0} of {1}", CurFileNumber, MaxFileCount);
                                        });
                                        if (_isCanceled)
                                        {
                                            CloseActivePopUpCommand.Execute(false);
                                            return;
                                        }
                                    });
                                }
                            });

                        });
                    }
                }
            }
            
        }

    }
}
