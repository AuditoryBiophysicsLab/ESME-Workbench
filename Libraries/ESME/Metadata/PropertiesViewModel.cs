using System.Collections.Generic;
using System.ComponentModel;
using Cinch;

namespace ESME.Metadata
{
    public class PropertiesViewModel : ViewModelBase
    {
        #region public string WindowTitle { get; set; }

        public string WindowTitle
        {
            get { return _windowTitle; }
            set
            {
                if (_windowTitle == value) return;
                _windowTitle = value;
                NotifyPropertyChanged(WindowTitleChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs WindowTitleChangedEventArgs = ObservableHelper.CreateArgs<PropertiesViewModel>(x => x.WindowTitle);
        string _windowTitle;

        #endregion

        #region public string Name { get; set; }

        public string Name
        {
            get { return _name; }
            set
            {
                if (_name == value) return;
                _name = value;
                NotifyPropertyChanged(NameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs NameChangedEventArgs = ObservableHelper.CreateArgs<PropertiesViewModel>(x => x.Name);
        string _name;

        #endregion

        #region public IEnumerable<KeyValuePair<string, string>> KeyValuePairs { get; set; }

        public IEnumerable<KeyValuePair<string, string>> KeyValuePairs
        {
            get { return _keyValuePairs; }
            set
            {
                if (_keyValuePairs == value) return;
                _keyValuePairs = value;
                NotifyPropertyChanged(KeyValuePairsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs KeyValuePairsChangedEventArgs = ObservableHelper.CreateArgs<PropertiesViewModel>(x => x.KeyValuePairs);
        IEnumerable<KeyValuePair<string, string>> _keyValuePairs;

        #endregion

    }
}
