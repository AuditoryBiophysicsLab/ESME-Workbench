using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using Cinch;

namespace ESME.Overlay
{
    public class OverlayFiles : List<KeyValuePair<string, OverlayFile>>, INotifyPropertyChanged
    {
        public OverlayFiles(string selectedRangeComplexName)
        {
            Refresh(selectedRangeComplexName);
        }

        public void Refresh(string selectedRangeComplexName)
        {
            if (string.IsNullOrEmpty(selectedRangeComplexName)) return;
            Clear();
            var files = Directory.GetFiles(Path.Combine(Globals.AppSettings.ScenarioDataDirectory, selectedRangeComplexName, "Areas"), "*.ovr");
            AddRange(files.Select(file => new KeyValuePair<string, OverlayFile>(Path.GetFileNameWithoutExtension(file), new OverlayFile(file))));
        }

        public OverlayFile this[string overlayKey]
        {
            get { return this.FirstOrDefault(f => f.Key == overlayKey).Value; }
        }

        #region public List<string> OverlayKeys { get; set; }

        public List<string> OverlayKeys
        {
            get { return _overlayKeys; }
            set
            {
                if (_overlayKeys == value) return;
                _overlayKeys = value;
                NotifyPropertyChanged(OverlaysChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs OverlaysChangedEventArgs = ObservableHelper.CreateArgs<OverlayFiles>(x => x.OverlayKeys);
        List<string> _overlayKeys;

        #endregion

        #region public string SelectedOverlayKey { get; set; }

        public string SelectedOverlayKey
        {
            get { return _selectedOverlayKey; }
            set
            {
                if (_selectedOverlayKey == value) return;
                _selectedOverlayKey = value;
                NotifyPropertyChanged(SelectedOverlayKeyChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedOverlayKeyChangedEventArgs = ObservableHelper.CreateArgs<OverlayFiles>(x => x.SelectedOverlayKey);
        string _selectedOverlayKey;

        #endregion

        #region INotifyPropertyChanged Members

        public event PropertyChangedEventHandler PropertyChanged;
        protected void NotifyPropertyChanged(PropertyChangedEventArgs args) { if (PropertyChanged != null) PropertyChanged(this, args); }

        #endregion

    }
}