using System;
using System.ComponentModel.Composition;
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

        public void Run(Scenario scenario, TimeSpan timeStep)
        {
            if (scenario.Validate() != null) return;
            var timeStepCount = (int)Math.Round(((TimeSpan)scenario.Duration).TotalSeconds / timeStep.TotalSeconds);
            for (var timeStepIndex = 0; timeStepIndex < timeStepCount; timeStepIndex++)
            {
                
            }
        }
    }
}
