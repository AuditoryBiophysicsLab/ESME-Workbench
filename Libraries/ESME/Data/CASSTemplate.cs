using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using Cinch;
using HRC.Utility;

namespace ESME.Data
{
    public class CASSTemplate : PropertyChangedBase
    {
        #region public string MatchString { get; set; }

        static readonly PropertyChangedEventArgs MatchStringChangedEventArgs = ObservableHelper.CreateArgs<CASSTemplate>(x => x.MatchString);
        string _matchString;

        public string MatchString
        {
            get { return _matchString; }
            set
            {
                if (_matchString == value) return;
                _matchString = value;
                NotifyPropertyChanged(MatchStringChangedEventArgs);
            }
        }

        #endregion

        #region public string FileName { get; set; }

        static readonly PropertyChangedEventArgs FileNameChangedEventArgs = ObservableHelper.CreateArgs<CASSTemplate>(x => x.FileName);
        string _fileName;

        public string FileName
        {
            get { return _fileName; }
            set
            {
                if (_fileName == value) return;
                _fileName = value;
                NotifyPropertyChanged(FileNameChangedEventArgs);
            }
        }

        #endregion

        #region public bool IsEnabled { get; set; }

        static readonly PropertyChangedEventArgs IsEnabledChangedEventArgs = ObservableHelper.CreateArgs<CASSTemplate>(x => x.IsEnabled);
        bool _isEnabled;

        public bool IsEnabled
        {
            get { return _isEnabled; }
            set
            {
                if (_isEnabled == value) return;
                _isEnabled = value;
                NotifyPropertyChanged(IsEnabledChangedEventArgs);
            }
        }

        #endregion

        #region EditParameterFileCommand

        SimpleCommand<object, object> _editParameterFile;

        public SimpleCommand<object, object> EditParameterFileCommand
        {
            get
            {
                return _editParameterFile ?? (_editParameterFile = new SimpleCommand<object, object>(delegate { return File.Exists(FileName); }, delegate
                                                                                                                                       {
                                                                                                                                           new Process
                                                                                                                                           {
                                                                                                                                               StartInfo =
                                                                                                                                                   {
                                                                                                                                                       FileName = "notepad.exe",
                                                                                                                                                       Arguments = FileName,
                                                                                                                                                   }
                                                                                                                                           }.Start();
                                                                                                                                       }));
            }
        }

        #endregion
    }
}