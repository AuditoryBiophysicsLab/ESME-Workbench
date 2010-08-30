using System.Collections.Generic;

namespace ESMEWorkBench.ViewModels.RecentFileList
{
    public interface IPersist
    {
        List<string> RecentFiles(int max);
        void InsertFile(string filepath, int max);
        void RemoveFile(string filepath, int max);
    }
}