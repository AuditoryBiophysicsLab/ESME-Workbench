using System.Collections.Generic;
using System.Linq;

namespace ESMEWorkBench.ViewModels.RecentFiles
{
    public class RecentFileList : List<RecentFile>
    {
        IPersist _persister;

        public int MaxNumberOfFiles { get; set; }

        public IPersist Persister
        {
            get { return _persister; }
            set
            {
                if (_persister == value) return;
                _persister = value;
                AddRange(_persister.RecentFiles(MaxNumberOfFiles).Select(x => new RecentFile(x)));
            }
        }

        public void RemoveFile(string filepath)
        {
            Persister.RemoveFile(filepath, MaxNumberOfFiles);
        }

        public void InsertFile(string filepath)
        {
            Persister.InsertFile(filepath, MaxNumberOfFiles);
        }
    }
}