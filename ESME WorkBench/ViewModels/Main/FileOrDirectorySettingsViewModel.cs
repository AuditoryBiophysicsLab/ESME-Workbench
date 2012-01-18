using System.ComponentModel;
using Cinch;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.Main
{
    [ExportViewModel("FileOrDirectorySettingsViewModel")]
    internal class FileOrDirectorySettingsViewModel : ViewModelBase
    {
        static readonly PropertyChangedEventArgs CaptionChangedEventArgs = ObservableHelper.CreateArgs<FileOrDirectorySettingsViewModel>(x => x.Caption);
        static readonly PropertyChangedEventArgs FileNameChangedEventArgs = ObservableHelper.CreateArgs<FileOrDirectorySettingsViewModel>(x => x.FileName);
        static readonly PropertyChangedEventArgs FileNameFilterChangedEventArgs = ObservableHelper.CreateArgs<FileOrDirectorySettingsViewModel>(x => x.FileNameFilter);
        static readonly PropertyChangedEventArgs IsDirectoryBrowserChangedEventArgs = ObservableHelper.CreateArgs<FileOrDirectorySettingsViewModel>(x => x.IsDirectoryBrowser);

        string _caption;
        string _fileName;
        string _fileNameFilter;
        bool _isDirectoryBrowser;

        public string Caption
        {
            get { return _caption; }
            set
            {
                if (_caption == value) return;
                _caption = value;
                NotifyPropertyChanged(CaptionChangedEventArgs);
            }
        }

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

        public string FileNameFilter
        {
            get { return _fileNameFilter; }
            set
            {
                if (_fileNameFilter == value) return;
                _fileNameFilter = value;
                NotifyPropertyChanged(FileNameFilterChangedEventArgs);
            }
        }

        public bool IsDirectoryBrowser
        {
            get { return _isDirectoryBrowser; }
            set
            {
                if (_isDirectoryBrowser == value) return;
                _isDirectoryBrowser = value;
                NotifyPropertyChanged(IsDirectoryBrowserChangedEventArgs);
            }
        }
    }
}