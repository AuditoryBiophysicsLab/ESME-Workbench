using System.IO;
using ESMEWorkBench.ViewModels.Ribbon;

namespace ESMEWorkBench.ViewModels.RecentFiles
{
    public class RecentFile : GalleryItemDataViewModel
    {
        public RecentFile(string filepath)
        {
            OriginalPath = filepath;
            Label = Path.GetFileNameWithoutExtension(OriginalPath);
        }

        public string OriginalPath { get; set; }
    }
}