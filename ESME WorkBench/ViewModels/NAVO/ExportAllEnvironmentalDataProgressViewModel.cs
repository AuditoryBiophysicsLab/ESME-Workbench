using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using Cinch;

namespace OneNavyModel.ViewModels.NAVO
{
    public class ExportAllEnvironmentalDataProgressViewModel:ViewModelBase
    {
        #region public int CurFileNumber { get; set; }

        public int CurFileNumber
        {
            get { return _curFileNumber; }
            set
            {
                if (_curFileNumber == value) return;
                _curFileNumber = value;
                NotifyPropertyChanged(CurFileNumberChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CurFileNumberChangedEventArgs = ObservableHelper.CreateArgs<ExportAllEnvironmentalDataProgressViewModel>(x => x.CurFileNumber);
        int _curFileNumber;

        #endregion

        #region public int MaxFileCount { get; set; }

        public int MaxFileCount
        {
            get { return _maxFileCount; }
            set
            {
                if (_maxFileCount == value) return;
                _maxFileCount = value;
                NotifyPropertyChanged(MaxFileCountChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MaxFileCountChangedEventArgs = ObservableHelper.CreateArgs<ExportAllEnvironmentalDataProgressViewModel>(x => x.MaxFileCount);
        int _maxFileCount;

        #endregion

        #region public string Message { get; set; }

        public string Message
        {
            get { return _message; }
            set
            {
                if (_message == value) return;
                _message = value;
                NotifyPropertyChanged(MessageChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MessageChangedEventArgs = ObservableHelper.CreateArgs<ExportAllEnvironmentalDataProgressViewModel>(x => x.Message);
        string _message;

        #endregion

        public ExportAllEnvironmentalDataProgressViewModel() {
            
        }

    }
}
