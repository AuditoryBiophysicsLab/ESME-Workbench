using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using System.Text;
using ESME.Scenarios;
using ESME.Settings;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Simulator
{
    [PartCreationPolicy(CreationPolicy.Shared)]
    [ExportService(ServiceType.Both, typeof(ISettingsService))]
    public class SimulatorService
    {
        public SimulatorService() {}

        public void Run(Scenario scenario)
        {
            
        }


    }
}
