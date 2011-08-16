using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Windows.Threading;
using HRC.Utility;

namespace ESME.Environment.Descriptors
{
    public abstract class NAEMODescriptors<T> : ObservableList<KeyValuePair<string, T>> where T : NAEMODescriptor, new()
    {
        public delegate KeyValuePair<string, T> NewDescriptor(string sourceFilename);

        public delegate IEnumerable<string> FilenameFilter(IEnumerable<string> fileName);

        public Dispatcher Dispatcher { get; set; }
        protected NAEMODescriptors() { Dispatcher = Dispatcher.CurrentDispatcher; }

        protected string RangeComplexName;
        protected string SubDirectoryName;
        protected string SearchPattern;
        protected FilenameFilter FilenameFilterDelegate;

        protected NAEMODescriptors(string selectedRangeComplexName, string subDirectoryName, string searchPattern, FilenameFilter filenameFilter = null)
        {
            RangeComplexName = selectedRangeComplexName;
            SubDirectoryName = subDirectoryName;
            SearchPattern = searchPattern;
            FilenameFilterDelegate = filenameFilter;
            if (string.IsNullOrEmpty(RangeComplexName)) throw new ApplicationException("Range complex name cannot be null or empty");
            Refresh();
        }

        protected void Refresh(BackgroundTask backgroundTask = null)
        {
            Clear();
            //Console.WriteLine("{0} Entered NAEMODescriptors constructor", DateTime.Now);
            var rangeComplexPath = Path.Combine(Globals.AppSettings.ScenarioDataDirectory, RangeComplexName);
            if (!Directory.Exists(rangeComplexPath)) return;
            var subDirectoryPath = Path.Combine(rangeComplexPath, SubDirectoryName);
            if (!Directory.Exists(subDirectoryPath)) return;
            var files = Directory.GetFiles(subDirectoryPath, SearchPattern);
            //Console.WriteLine("{0} Got directory listing containing {1} files", DateTime.Now, files.Length);
            IEnumerable<string> filteredFiles = files;
            if (FilenameFilterDelegate != null)
            {
                filteredFiles = FilenameFilterDelegate(files);
                //Console.WriteLine("{0} Filtered directory listing, now contains {1} files", DateTime.Now, filteredFiles.Count());
            }
            if (backgroundTask != null)
            {
                backgroundTask.Maximum = filteredFiles.Count() * 2;
                backgroundTask.Value = 0;
            }
            //Console.WriteLine("{0} About to call AddRange", DateTime.Now);
            Add(new KeyValuePair<string, T>("[None]", null));
            foreach (var file in filteredFiles)
            {
                Add(new KeyValuePair<string, T>(Path.GetFileNameWithoutExtension(file), new T {DataFilename = file}));
                if (backgroundTask != null) backgroundTask.Value++;
            }
            Sort();
            //Console.WriteLine("{0} Leaving NAEMODescriptors constructor", DateTime.Now);
        }

        public void Add(string fileName)
        {
            Add(new KeyValuePair<string, T>(Path.GetFileNameWithoutExtension(fileName), new T { DataFilename = fileName }));  
            Sort((x, y) => x.Key.CompareTo(y.Key));
        }

        public new void Sort()
        {
            Sort((x, y) => x.Key.CompareTo(y.Key));
        }

        public virtual NAEMODescriptor this[string overlayKey]
        {
            get { return this.FirstOrDefault(f => f.Key == overlayKey).Value; }
        }

        public bool Remove(T item) { return Remove(Find(entry => entry.Value == item)); }
    }
}