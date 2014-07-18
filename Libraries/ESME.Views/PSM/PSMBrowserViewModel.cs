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

        public List<Platform> Platforms { get; set; }
        public Platform SelectedPlatform { get; set; }

        public List<Source> Sources { get; set; }
        public Source SelectedSource { get; set; }

        public List<Mode> Modes { get; set; }
        public Mode SelectedMode { get; set; }

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