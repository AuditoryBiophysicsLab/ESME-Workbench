using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using System.Text;
using ESME.Environment;
using ESME.Plugins;

namespace BellhopPlugin
{
    [PartCreationPolicy(CreationPolicy.Shared)]
    [ESMEPlugin(Name = "Bellhop",
                Description = "Bellhop is a highly efficient ray tracing program, written by Michael Porter of hlsresearch.com as part of the Acoustic Toolbox.")]
    public class BellhopPlugin : TransmissionLossCalculatorPluginBase
    {
        public BellhopPlugin() 
        {
            //SetPropertiesFromAttributes(GetType());
            PluginSubtype = PluginSubtype.Bellhop;
        }
    }
}
