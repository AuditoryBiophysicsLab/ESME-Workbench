using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using Cinch;

namespace TransmissionLossCalculator
{
    public class WorkDirectoryScanner : ViewModelBase
    {
        #region public string DirectoryPath { get; set; }

        public string DirectoryPath
        {
            get { return _directoryPath; }
            set
            {
                if (_directoryPath == value) return;
                _directoryPath = value;
                NotifyPropertyChanged(DirectoryPathChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DirectoryPathChangedEventArgs = ObservableHelper.CreateArgs<WorkDirectoryScanner>(x => x.DirectoryPath);
        string _directoryPath;

        #endregion

    }
}
