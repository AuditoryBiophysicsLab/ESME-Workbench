using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.IO;
using ESME.Environment;
using ESME.Model;
using ESME.Plugins;
using ESME.Scenarios;
using ESME.TransmissionLoss.Bellhop;
using HRC;
using HRC.Utility;
using HRC.ViewModels;
using StandardTransmissionLossEngines.Controls;

namespace StandardTransmissionLossEngines
{
    [PartCreationPolicy(CreationPolicy.Shared)]
    [ESMEPlugin(Name = "Bellhop Arrivals",
        Description = "Bellhop is a highly efficient ray tracing program, written by Michael Porter of hlsresearch.com as part of the Acoustic Toolbox.")]
    public class BellhopArrivalsEngine : BellhopEngine
    {
        [UsedImplicitly] PropertyObserver<BellhopArrivalsEngine> _propertyObserver;
        public BellhopArrivalsEngine() 
        {
            PluginSubtype = PluginSubtype.BellhopArrivals;
            ConfigurationControl = new BellhopConfigurationControl { DataContext = this };
            Initialize();
            _propertyObserver = new PropertyObserver<BellhopArrivalsEngine>(this)
                .RegisterHandler(p => p.RangeCellSize, Save)
                .RegisterHandler(p => p.DepthCellSize, Save)
                .RegisterHandler(p => p.UseSurfaceReflection, Save)
                .RegisterHandler(p => p.RayCount, Save);
        }

        void Initialize()
        {
            SetPropertiesFromAttributes(GetType());
            if (!File.Exists(ConfigurationFile)) Save();
            IsConfigured = true;
        }

        protected override void Save()
        {
            var serializer = new XmlSerializer<BellhopArrivalsEngine> { Data = this };
            serializer.Save(ConfigurationFile, null);
        }

        public override void LoadSettings()
        {
            var settings = XmlSerializer<BellhopArrivalsEngine>.LoadExistingFile(ConfigurationFile, null);
            if (settings == null) return;
            RangeCellSize = settings.RangeCellSize;
            DepthCellSize = settings.DepthCellSize;
            UseSurfaceReflection = settings.UseSurfaceReflection;
            RayCount = settings.RayCount;
        }


        public override void CalculateTransmissionLoss(Platform platform, Mode mode, Radial radial, BottomProfile bottomProfile, SedimentType sedimentType, double windSpeed, IList<Tuple<double, SoundSpeedProfile>> soundSpeedProfilesAlongRadial)
        {
            CalculateTransmissionLossInternal(platform, mode, radial, bottomProfile, sedimentType, windSpeed, soundSpeedProfilesAlongRadial, true);
        }
    }
}
