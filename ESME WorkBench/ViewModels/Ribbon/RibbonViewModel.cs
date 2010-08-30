using System.Collections.Generic;
using System.ComponentModel.Composition;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.Ribbon
{
    [ExportViewModel("RibbonViewModel", true)]
    [PartCreationPolicy(CreationPolicy.NonShared)]
    public class RibbonViewModel
    {
        public ApplicationMenuItemList ApplicationMenuItems { get; set; }
        public IEnumerable<string> RecentExperiments { get; private set; }
        public TabList Tabs { get; set; }
    }
}