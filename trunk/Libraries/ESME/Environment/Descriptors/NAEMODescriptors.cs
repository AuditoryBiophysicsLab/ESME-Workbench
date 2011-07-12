using System.Collections.Generic;
using System.IO;
using System.Linq;
using HRC.Utility;

namespace ESME.Environment.Descriptors
{
    public abstract class NAEMODescriptors<T> : List<System.Collections.Generic.KeyValuePair<string, T>> where T : NAEMODescriptor, new()
    {
        public delegate System.Collections.Generic.KeyValuePair<string, T> NewDescriptor(string sourceFilename);

        public delegate IEnumerable<string> FilenameFilter(IEnumerable<string> fileName);

        protected NAEMODescriptors()
        {
            
        }

        protected NAEMODescriptors(string selectedRangeComplexName, string subDirectoryName, string searchPattern, FilenameFilter filenameFilter = null, BackgroundTask backgroundTask = null)
        {
            if (string.IsNullOrEmpty(selectedRangeComplexName)) return;
            //Console.WriteLine("{0} Entered NAEMODescriptors constructor", DateTime.Now);
            var files = Directory.GetFiles(Path.Combine(Globals.AppSettings.ScenarioDataDirectory, selectedRangeComplexName, subDirectoryName), searchPattern);
            //Console.WriteLine("{0} Got directory listing containing {1} files", DateTime.Now, files.Length);
            IEnumerable<string> filteredFiles = files;
            if (filenameFilter != null)
            {
                filteredFiles = filenameFilter(files);
                //Console.WriteLine("{0} Filtered directory listing, now contains {1} files", DateTime.Now, filteredFiles.Count());
            }
            if (backgroundTask != null)
            {
                backgroundTask.Maximum = filteredFiles.Count() * 2;
                backgroundTask.Value = 0;
            }
            //Console.WriteLine("{0} About to call AddRange", DateTime.Now);
            foreach (var file in filteredFiles)
            {
                Add(new System.Collections.Generic.KeyValuePair<string, T>(Path.GetFileNameWithoutExtension(file), new T { DataFilename = file }));
                if (backgroundTask != null) backgroundTask.Value++;
            }
            //AddRange(filteredFiles.Select(file => new System.Collections.Generic.KeyValuePair<string, T>(Path.GetFileNameWithoutExtension(file), new T { DataFilename = file })));
            //Console.WriteLine("{0} Leaving NAEMODescriptors constructor", DateTime.Now);
        }

        public virtual NAEMODescriptor this[string overlayKey]
        {
            get { return this.FirstOrDefault(f => f.Key == overlayKey).Value; }
        }
    }
}