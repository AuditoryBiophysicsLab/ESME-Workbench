using System.Collections.Generic;
using HRC.ViewModels;

namespace ESME.Metadata
{
    public class PropertiesViewModel : ViewModelBase
    {
        public string WindowTitle { get; set; }
        public string Name { get; set; }
        public IEnumerable<KeyValuePair<string, string>> KeyValuePairs { get; set; }
    }
}
