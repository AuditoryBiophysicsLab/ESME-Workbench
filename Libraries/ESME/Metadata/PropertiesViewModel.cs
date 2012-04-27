using System.Collections.Generic;
using HRC.Aspects;
using HRC.ViewModels;

namespace ESME.Metadata
{
    [NotifyPropertyChanged]
    public class PropertiesViewModel : ViewModelBase
    {
        public string WindowTitle { get; set; }
        public string Name { get; set; }
        public IEnumerable<KeyValuePair<string, string>> KeyValuePairs { get; set; }
    }
}
