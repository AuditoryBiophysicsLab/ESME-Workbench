using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.IO;
using Cinch;

namespace ESMEWorkBench.ViewModels.RecentFiles
{
    public class RecentFileList : ViewModelBase
    {
        IPersist _persister;

        public int MaxNumberOfFiles { get; set; }

        public RecentFileList()
        {
            Persister = new RegistryPersister();
            MaxNumberOfFiles = 10;
            List = new ObservableCollection<RecentFileDescriptor>();
            UpdateList();
        }

        IPersist Persister
        {
            get { return _persister; }
            set
            {
                if (_persister == value) return;
                _persister = value;
            }
        }

        #region public ObservableCollection<RecentFileDescriptor> List { get; set; }

        public ObservableCollection<RecentFileDescriptor> List
        {
            get { return _list; }
            set
            {
                if (_list == value) return;
                if (_list != null) _list.CollectionChanged -= ListCollectionChanged;
                _list = value;
                if (_list != null) _list.CollectionChanged += ListCollectionChanged;
                NotifyPropertyChanged(ListChangedEventArgs);
            }
        }

        void ListCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(ListChangedEventArgs); }
        static readonly PropertyChangedEventArgs ListChangedEventArgs = ObservableHelper.CreateArgs<RecentFileList>(x => x.List);
        ObservableCollection<RecentFileDescriptor> _list;

        #endregion

        void UpdateList()
        {
            List.Clear();
            foreach (var recent in Persister.RecentFiles(MaxNumberOfFiles))
                List.Add(new RecentFileDescriptor(recent));
        }

        public void RemoveFile(string filepath)
        {
            Persister.RemoveFile(filepath, MaxNumberOfFiles);
            UpdateList();
        }

        public void InsertFile(string filepath)
        {
            Persister.InsertFile(filepath, MaxNumberOfFiles);
            UpdateList();
        }
    }

    public class RecentFileDescriptor
    {
        #region public constructor

        public RecentFileDescriptor(string longName)
        {
            LongName = longName;
            ShortName = Path.GetFileName(longName);
        }

        #endregion
        public string LongName { get; private set; }
        public string ShortName { get; private set; }
    }
}