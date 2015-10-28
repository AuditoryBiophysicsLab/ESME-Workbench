using System.Collections.Generic;
using System.Linq;
using ESME.PSM;
using ESME.Scenarios;
using HRC.Validation;

namespace ESME.Views.PSM
{
    public class PSMBrowserViewModel : ValidatingViewModel
    {
        public string WindowTitle { get; set; }

        public List<PlatformPSM> Platforms { get; set; }
        public PlatformPSM SelectedPlatform { get; set; }

        public List<SourcePSM> Sources { get; set; }
        public SourcePSM SelectedSource { get; set; }

        public List<ModePSM> Modes { get; set; }
        public ModePSM SelectedMode { get; set; }

        public PSMBrowserViewModel()
        {
            WindowTitle = "PSM Browser";

            Platforms = PSMContext.Query(p => p.Platforms.ToList());
            SelectedPlatform = Platforms.FirstOrDefault();
            
            Sources = PSMContext.Query(p => p.Sources.ToList());
            SelectedSource = Sources.FirstOrDefault();
            
            Modes = PSMContext.Query(p => p.Modes.ToList());
            SelectedMode = Modes.FirstOrDefault();
        }
    }
}