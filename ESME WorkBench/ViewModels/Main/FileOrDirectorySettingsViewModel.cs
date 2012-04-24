using HRC.ViewModels;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkbench.ViewModels.Main
{
    [ExportViewModel("FileOrDirectorySettingsViewModel")]
    internal class FileOrDirectorySettingsViewModel : ViewModelBase
    {
        public string Caption { get; set; }
        public string FileName { get; set; }
        public string FileNameFilter { get; set; }
        public bool IsDirectoryBrowser { get; set; }
    }
}