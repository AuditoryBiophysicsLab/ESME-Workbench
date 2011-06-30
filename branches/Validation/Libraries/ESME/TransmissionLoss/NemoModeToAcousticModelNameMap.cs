﻿using System.Collections.Generic;
using System.Linq;
namespace ESME.TransmissionLoss
{
    public class NemoModeToAcousticModelNameMap : List<HRC.Utility.KeyValuePair<string, string>>
    {
        public NemoModeToAcousticModelNameMap() {  }
        public NemoModeToAcousticModelNameMap(IEnumerable<string> distinctModePSMNames, string defaultAcousticModelName)
        {
            foreach (var curMode in distinctModePSMNames)
                Add(new HRC.Utility.KeyValuePair<string, string>(curMode, defaultAcousticModelName));
        }

        public void UpdateModes(IEnumerable<string> distinctModePSMNames, string defaultAcousticModelName)
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
                if (this[curMode] == null) Add(new HRC.Utility.KeyValuePair<string, string>(curMode, defaultAcousticModelName));
        }

        public string this[string key]
        {
            get { return this.Where(curEntry => curEntry.Key == key).Select(curEntry => curEntry.Value).FirstOrDefault(); }
            set
            {
                foreach (var curEntry in this.Where(curEntry => curEntry.Key == key))
                {
                    curEntry.Value = value;
                    return;
                }
                Add(new HRC.Utility.KeyValuePair<string, string>(key, value));
            }
        }
    }
}