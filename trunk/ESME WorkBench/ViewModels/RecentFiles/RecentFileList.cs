using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using ESMEWorkBench.ViewModels.Ribbon;

namespace ESMEWorkBench.ViewModels.RecentFiles
{
    public class RecentFileList : GalleryDataViewModel<RecentFile>
    {
        IPersist _persister;

        public int MaxNumberOfFiles { get; set; }

        public RecentFileList()
        {
            CategoryDataCollection.Add(new GalleryCategoryDataViewModel<RecentFile>());
            CanUserFilter = false;
        }

        public IPersist Persister
        {
            get { return _persister; }
            set
            {
                if (_persister == value) return;
                _persister = value;
                foreach (var file in _persister.RecentFiles(MaxNumberOfFiles))
                    CategoryDataCollection[0].GalleryItemDataCollection.Add(new RecentFile(file));
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