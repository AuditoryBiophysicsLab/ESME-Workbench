using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.IO;
using BellhopPlugin.Controls;
using ESME.Environment;
using ESME.Model;
using ESME.Plugins;
using ESME.Scenarios;
using ESME.TransmissionLoss.Bellhop;
using HRC;
using HRC.Aspects;
using HRC.Utility;
using HRC.ViewModels;

namespace BellhopPlugin
{
    [PartCreationPolicy(CreationPolicy.Shared)]
    [ESMEPlugin(Name = "RAMGeo",
                Description = "RAMGeo")]
    public class RAMGeoEngine : TransmissionLossCalculatorPluginBase
    {
        [UsedImplicitly] PropertyObserver<RAMGeoEngine> _propertyObserver;
        public RAMGeoEngine()
        {
            PluginSubtype = PluginSubtype.RAMGeo;
            ConfigurationControl = new RAMGeoConfigurationControl { DataContext = this };
            Initialize();
            _propertyObserver = new PropertyObserver<RAMGeoEngine>(this)
                .RegisterHandler(p => p.RangeCellSize, Save)
                .RegisterHandler(p => p.DepthCellSize, Save);
        }

        void Initialize()
        {
            SetPropertiesFromAttributes(GetType());
            if (!File.Exists(ConfigurationFile)) Save();
            IsConfigured = true;
        }

        protected override void Save()
        {
            var serializer = new XmlSerializer<RAMGeoEngine> { Data = this };
            serializer.Save(ConfigurationFile, null);
        }

        public override void LoadSettings()
        {
            var settings = XmlSerializer<RAMGeoEngine>.LoadExistingFile(ConfigurationFile, null);
            if (settings == null) return;
            RangeCellSize = settings.RangeCellSize;
            DepthCellSize = settings.DepthCellSize;
        } 

        [Initialize(50.0)] public double RangeCellSize { get; set; }
        [Initialize(25.0)] public double DepthCellSize { get; set; }

        public override void CalculateTransmissionLoss(Platform platform, Mode mode, Radial radial, BottomProfile bottomProfile, SedimentType sedimentType, double windSpeed, IList<Tuple<double, SoundSpeedProfile>> soundSpeedProfilesAlongRadial)
        {
            throw new NotImplementedException();
        }
    }
}
