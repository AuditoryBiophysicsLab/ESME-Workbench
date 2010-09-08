using System.ComponentModel.Composition;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.Ribbon
{
    [ExportViewModel("RibbonViewModel", true)]
    [PartCreationPolicy(CreationPolicy.NonShared)]
    public class RibbonViewModel
    {
        public ApplicationMenuItemList ApplicationMenuItems { get; set; }
        public TabList Tabs { get; set; }
    }
}