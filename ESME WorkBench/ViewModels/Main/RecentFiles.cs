using System.Collections.ObjectModel;

namespace ESMEWorkBench.ViewModels.Main
{
    public class RecentFiles : ObservableCollection<MostRecentFile>
    {
        public RecentFiles()
        {
            Add(new MostRecentFile("First document.docx", true));
            Add(new MostRecentFile("Second document.docx", false));
            Add(new MostRecentFile("Ribbon Design Document.docx", false));
        }


        protected override void OnCollectionChanged(System.Collections.Specialized.NotifyCollectionChangedEventArgs e)
        {
            base.OnCollectionChanged(e);
            if (e.NewItems != null)
            {
                foreach (var item in e.NewItems)
                {
                    
                }
            }
        }
    }

    public class MostRecentFile
    {
        public MostRecentFile(string name, bool isFixed)
        {
            Name = name;
            IsFixed = isFixed;
        }

        public string Name { get; set; }
        public string Path
        { 
            get { return _path; }
            set
            {
                if (_path == value) return;
                Name = System.IO.Path.GetFileName(_path);
                _path = value;
            }
        }

        string _path;

        public bool IsFixed { get; set; }
    }
}