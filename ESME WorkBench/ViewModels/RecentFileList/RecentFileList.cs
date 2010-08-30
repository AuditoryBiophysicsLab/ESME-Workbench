using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using ESMEWorkBench.ViewModels.Ribbon;

namespace ESMEWorkBench.ViewModels.RecentFileList
{
    public class RecentFileList : IEnumerable<GalleryItemDataViewModel>
    {
        List<RecentFile> _recentFiles;
        IPersist _persister;

        public RecentFileList()
        {
            MaxNumberOfFiles = 9;

            Persister = new RegistryPersister();
        }

        public int MaxNumberOfFiles { get; set; }
        public IPersist Persister 
        {
            get { return _persister; }
            set
            {
                if (_persister == value) return;
                _persister = value;
                _recentFiles = new List<RecentFile>();
                _recentFiles.AddRange(_persister.RecentFiles(MaxNumberOfFiles).Select(x => new RecentFile(x)));
            }
        }

        #region IEnumerable<GalleryItemDataViewModel> Members

        public IEnumerator<GalleryItemDataViewModel> GetEnumerator()
        {
            return _recentFiles.GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        #endregion

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