using System.Collections.Generic;
using System.ComponentModel.Composition;
using ESME.Environment;
using ESME.Model;
using ESME.Plugins;
using ESME.Scenarios;
using ESME.TransmissionLoss.Bellhop;

namespace BellhopPlugin
{
    [PartCreationPolicy(CreationPolicy.Shared)]
    [ESMEPlugin(Name = "Bellhop",
                Description = "Bellhop is a highly efficient ray tracing program, written by Michael Porter of hlsresearch.com as part of the Acoustic Toolbox.")]
    public class BellhopPlugin : TransmissionLossCalculatorPluginBase
    {
        public BellhopPlugin() 
        {
            PluginSubtype = PluginSubtype.Bellhop;
            Initialize();
        }

        void Initialize()
        {
            SetPropertiesFromAttributes(GetType());
        }

        public void CreateInputFiles(Mode mode, Radial radial, IEnumerable<SoundSpeedProfile> soundSpeedProfiles, BottomProfile bottomProfile, SedimentType sedimentType, double frequency)
        {
            
        }
    }
}
