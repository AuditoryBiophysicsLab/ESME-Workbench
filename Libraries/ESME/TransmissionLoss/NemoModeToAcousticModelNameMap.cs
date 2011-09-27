using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Linq;
namespace ESME.TransmissionLoss
{
    [Serializable]
    public class NemoModeToAcousticModelNameMap : List<HRC.Collections.EditableKeyValuePair<string, TransmissionLossAlgorithm>>, INotifyCollectionChanged
    {
        public NemoModeToAcousticModelNameMap() {  }
        public NemoModeToAcousticModelNameMap(IEnumerable<string> distinctModePSMNames, TransmissionLossAlgorithm defaultAcousticModel)
        {
            foreach (var curMode in distinctModePSMNames)
                Add(new HRC.Collections.EditableKeyValuePair<string, TransmissionLossAlgorithm>(curMode, defaultAcousticModel));
        }

        public void UpdateModes(List<string> distinctModePSMNames, TransmissionLossAlgorithm defaultAcousticModel)
        {
            var orphansFound = true;

            while (orphansFound)
            {
                orphansFound = false;
                // Loop through each entry in the current dictionary
                foreach (var curEntry in this)
                {
                    // Loop through each entry in the new master list
                    var foundMode = distinctModePSMNames.Any(curMode => curEntry.Key == curMode);
                    // If we found the current entry in the new master list, then check the next entry
                    if (foundMode) continue;
                    // Remove the dictionary entry that's not in the new master list
                    Remove(curEntry);
                    // Indicate we've found an orphan entry
                    orphansFound = true;
                    // and start again from the beginning
                    break;
                }
            }

            // Add any modes in the new master list that are not already in the dictionary
            foreach (var curMode in distinctModePSMNames)
                if (this[curMode] == TransmissionLossAlgorithm.NoneAssigned) Add(new HRC.Collections.EditableKeyValuePair<string, TransmissionLossAlgorithm>(curMode, defaultAcousticModel));
        }

        public TransmissionLossAlgorithm this[string key]
        {
            get { return this.Where(curEntry => curEntry.Key == key).Select(curEntry => curEntry.Value).FirstOrDefault(); }
            set
            {
                foreach (var curEntry in this.Where(curEntry => curEntry.Key == key))
                {
                    var oldEntry = curEntry;
                    curEntry.Value = value;
                    OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Replace, curEntry, oldEntry));
                    return;
                }
                var newEntry = new HRC.Collections.EditableKeyValuePair<string, TransmissionLossAlgorithm>(key, value);
                Add(newEntry);
                OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Add, newEntry));
            }
        }

        protected virtual void OnCollectionChanged(NotifyCollectionChangedEventArgs e)
        {
            if (CollectionChanged != null) CollectionChanged(this, e);
        }

        public event NotifyCollectionChangedEventHandler CollectionChanged;
    }
}
