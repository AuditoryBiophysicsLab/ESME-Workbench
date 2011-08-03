using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.IO;
using System.Linq;
using System.Windows.Threading;
using Cinch;
using HRC.Utility;

namespace ESME.Environment.Descriptors
{
    public abstract class NAEMODescriptors<T> : List<KeyValuePair<string, T>>, INotifyCollectionChanged where T : NAEMODescriptor, new()
    {
        public delegate KeyValuePair<string, T> NewDescriptor(string sourceFilename);

        public delegate IEnumerable<string> FilenameFilter(IEnumerable<string> fileName);

        public Dispatcher Dispatcher { get; set; }
        protected NAEMODescriptors() { }

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
            var files = Directory.GetFiles(Path.Combine(Globals.AppSettings.ScenarioDataDirectory, RangeComplexName, SubDirectoryName), SearchPattern);
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
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Reset));
        }

        public new void Sort()
        {
            Sort((x, y) => x.Key.CompareTo(y.Key));
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Reset));
        }

        public virtual NAEMODescriptor this[string overlayKey]
        {
            get { return this.FirstOrDefault(f => f.Key == overlayKey).Value; }
        }

        #region Implementation of INotifyCollectionChanged
        public event NotifyCollectionChangedEventHandler CollectionChanged;
        protected virtual void OnCollectionChanged(NotifyCollectionChangedEventArgs e)
        {
            if (CollectionChanged == null) return;
            if (Dispatcher != null) Dispatcher.InvokeIfRequired(() => CollectionChanged(this, e), DispatcherPriority.Normal);
            else CollectionChanged(this, e);
        }
        #endregion

        #region Implementation of ICollection<KeyValuePair<string,T>>
        /// <summary>
        /// Adds an item to the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <param name="item">The object to add to the <see cref="T:System.Collections.Generic.ICollection`1"/>.</param><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only.</exception>
        public new void Add(KeyValuePair<string, T> item)
        {
            base.Add(item);
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Add, item));
        }

        /// <summary>
        /// Removes all items from the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only. </exception>
        public new void Clear()
        {
            base.Clear();
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Reset));
        }

        /// <summary>
        /// Removes the first occurrence of a specific object from the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <returns>
        /// true if <paramref name="item"/> was successfully removed from the <see cref="T:System.Collections.Generic.ICollection`1"/>; otherwise, false. This method also returns false if <paramref name="item"/> is not found in the original <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </returns>
        /// <param name="item">The object to remove from the <see cref="T:System.Collections.Generic.ICollection`1"/>.</param><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only.</exception>
        public new bool Remove(KeyValuePair<string, T> item)
        {
            var result = base.Remove(item);
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Remove, item));
            return result;
        }

        public bool Remove(T item) { return Remove(Find(entry => entry.Value == item)); }
        #endregion

        #region Implementation of IList<KeyValuePair<string,T>>
        /// <summary>
        /// Inserts an item to the <see cref="T:System.Collections.Generic.IList`1"/> at the specified index.
        /// </summary>
        /// <param name="index">The zero-based index at which <paramref name="item"/> should be inserted.</param><param name="item">The object to insert into the <see cref="T:System.Collections.Generic.IList`1"/>.</param><exception cref="T:System.ArgumentOutOfRangeException"><paramref name="index"/> is not a valid index in the <see cref="T:System.Collections.Generic.IList`1"/>.</exception><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.IList`1"/> is read-only.</exception>
        public new void Insert(int index, KeyValuePair<string, T> item) { throw new NotImplementedException(); }

        /// <summary>
        /// Removes the <see cref="T:System.Collections.Generic.IList`1"/> item at the specified index.
        /// </summary>
        /// <param name="index">The zero-based index of the item to remove.</param><exception cref="T:System.ArgumentOutOfRangeException"><paramref name="index"/> is not a valid index in the <see cref="T:System.Collections.Generic.IList`1"/>.</exception><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.IList`1"/> is read-only.</exception>
        public new void RemoveAt(int index) { throw new NotImplementedException(); }
        #endregion
    }
}