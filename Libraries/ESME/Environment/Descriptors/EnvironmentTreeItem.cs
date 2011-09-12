using System;
using System.Linq;
using System.Collections.Generic;
using System.ComponentModel;
using System.Threading.Tasks;
using Cinch;

namespace ESME.Environment.Descriptors
{
    public class EnvironmentTreeItem : ViewModelBase
    {
        public void CreateProgressTrackers() 
        {
            Status = new Progress<string>(p => StatusText = p);
            Progress = new Progress<float>(p => ProgressPercent = (int)p);
        }

        public EnvironmentTreeItem() 
        {
            Children = new List<EnvironmentTreeItem>();
        }

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

        static readonly PropertyChangedEventArgs NameChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentTreeItem>(x => x.Name);
        string _name;

        #endregion

        #region public bool IsEnabled { get; set; }

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

        static readonly PropertyChangedEventArgs IsEnabledChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentTreeItem>(x => x.IsEnabled);
        bool _isEnabled;

        #endregion

        #region public string ToolTip { get; set; }

        public string ToolTip
        {
            get { return _toolTip; }
            set
            {
                if (_toolTip == value) return;
                _toolTip = value;
                NotifyPropertyChanged(ToolTipChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ToolTipChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentTreeItem>(x => x.ToolTip);
        string _toolTip;

        #endregion

        #region public Progress<string> Status { get; set; }

        public Progress<string> Status
        {
            get { return _status; }
            set
            {
                if (_status == value) return;
                _status = value;
                NotifyPropertyChanged(StatusChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs StatusChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentTreeItem>(x => x.Status);
        Progress<string> _status;

        #endregion

        #region public Progress<float> Progress { get; set; }

        public Progress<float> Progress
        {
            get { return _progress; }
            set
            {
                if (_progress == value) return;
                _progress = value;
                NotifyPropertyChanged(ProgressChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ProgressChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentTreeItem>(x => x.Progress);
        Progress<float> _progress;

        #endregion

        #region public bool IsInitializing { get; set; }

        public bool IsInitializing
        {
            get { return _isInitializing; }
            set
            {
                if (_isInitializing == value) return;
                _isInitializing = value;
                NotifyPropertyChanged(IsInitializingChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsInitializingChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentTreeItem>(x => x.IsInitializing);
        bool _isInitializing;

        #endregion

        #region public string StatusText { get; set; }

        public string StatusText
        {
            get { return _statusText; }
            set
            {
                if (_statusText == value) return;
                _statusText = value;
                NotifyPropertyChanged(StatusTextChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs StatusTextChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentTreeItem>(x => x.StatusText);
        string _statusText;

        #endregion

        #region public int ProgressPercent { get; set; }

        public int ProgressPercent
        {
            get { return _progressPercent; }
            set
            {
                if (_progressPercent == value) return;
                _progressPercent = value;
                NotifyPropertyChanged(ProgressPercentChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ProgressPercentChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentTreeItem>(x => x.ProgressPercent);
        int _progressPercent;

        #endregion

        #region public List<TreeItem> Children { get; set; }

        public List<EnvironmentTreeItem> Children
        {
            get { return _children; }
            set
            {
                if (_children == value) return;
                _children = value;
                NotifyPropertyChanged(ChildrenChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ChildrenChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentTreeItem>(x => x.Children);
        List<EnvironmentTreeItem> _children;

        #endregion

        public Task CompletionTask { get; set; }

        public EnvironmentTreeItem this[string childName]
        {
            get
            {
                var result = Children.FindAll(child => child.Name == childName);
                if (result.Count == 0) throw new IndexOutOfRangeException(string.Format("Requested child \"{0}\" not found", childName));
                if (result.Count > 1) throw new IndexOutOfRangeException(string.Format("Multiple children matching \"{0}\" were found", childName));
                return result[0];
            }
        }
    }
}