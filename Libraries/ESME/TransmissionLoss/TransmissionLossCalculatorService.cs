using System;
using System.Collections.Specialized;
using System.ComponentModel.Composition;
using System.Linq;
using ESME.Environment;
using ESME.Locations;
using ESME.Plugins;
using ESME.Scenarios;
using ESME.TransmissionLoss.Bellhop;
using HRC;
using HRC.Aspects;
using HRC.Navigation;
using HRC.Utility;
using MEFedMVVM.ViewModelLocator;

namespace ESME.TransmissionLoss
{
    [PartCreationPolicy(CreationPolicy.Shared)]
    [ExportService(ServiceType.Both, typeof(TransmissionLossCalculatorService))]
    public class TransmissionLossCalculatorService
    {
        public TransmissionLossCalculatorService()
        {
            WorkQueue.CollectionChanged += (s, e) =>
            {
                switch (e.Action)
                {
                    case NotifyCollectionChangedAction.Add:
                        break;
                    case NotifyCollectionChangedAction.Remove:
                        break;
                    case NotifyCollectionChangedAction.Replace:
                        break;
                    case NotifyCollectionChangedAction.Reset:
                        break;
                    case NotifyCollectionChangedAction.Move:
                        break;
                    default:
                        throw new ApplicationException(string.Format("Unknown CollectionChanged action: {0}", e.Action));
                }
            };
        }

        public TransmissionLossCalculatorService(MasterDatabaseService databaseService, IPluginManagerService pluginService, EnvironmentalCacheService cacheService)
        {
            _databaseService = databaseService;
            _pluginService = pluginService;
            _cacheService = cacheService;
            var items = from radial in _databaseService.Context.Radials
                        where radial.IsCalculated == false
                        select new PercentProgress<Radial>(radial);
            WorkQueue.AddRange(items);
        }
        [Import] MasterDatabaseService _databaseService;
        [Import] IPluginManagerService _pluginService;
        [Import] EnvironmentalCacheService _cacheService;

        [Initialize, UsedImplicitly] public ObservableList<PercentProgress<Radial>> WorkQueue { get; private set; }

        void Calculate(PercentProgress<Radial> item)
        {
            var radial = item.ProgressTarget;
            var transmissionLoss = radial.TransmissionLoss;
            var analysisPoint = transmissionLoss.AnalysisPoint;
            var scenario = analysisPoint.Scenario;
            var timePeriod = scenario.TimePeriod;
            var wind = (Wind)_cacheService[scenario.Wind];
            var soundSpeed = (SoundSpeed)_cacheService[scenario.SoundSpeed];
            var sediment = (Sediment)_cacheService[scenario.Sediment];
            var bathymetry = (Bathymetry)_cacheService[scenario.Bathymetry];
            var centerPoint = ((Geo)analysisPoint.Geo).Offset(Geo.KilometersToRadians(radial.Length / 2000), Geo.DegreesToRadians(radial.Bearing));
            var windSpeed = wind[timePeriod].EnvironmentData.GetNearestPoint(centerPoint);
            var soundSpeedProfile = soundSpeed[timePeriod].EnvironmentData.GetNearestPoint(centerPoint);
            var sedimentType = sediment.Samples.GetNearestPoint(centerPoint);
            //var bottomProfile = new BottomProfile()
        }
    }
}
