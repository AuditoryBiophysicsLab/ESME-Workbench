using System;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Windows.Input;
using Cinch;
using ESME;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.Ribbon
{
    [ExportViewModel("ControlDataViewModel")]
    [PartCreationPolicy(CreationPolicy.NonShared)]
    public class ControlDataViewModel : ViewModelBase, IHasName
    {
        static readonly PropertyChangedEventArgs LabelChangedEventArgs = ObservableHelper.CreateArgs<ControlDataViewModel>(x => x.Label);

        static readonly PropertyChangedEventArgs LargeImageChangedEventArgs = ObservableHelper.CreateArgs<ControlDataViewModel>(x => x.LargeImage);

        static readonly PropertyChangedEventArgs SmallImageChangedEventArgs = ObservableHelper.CreateArgs<ControlDataViewModel>(x => x.SmallImage);

        static readonly PropertyChangedEventArgs ToolTipTitleChangedEventArgs = ObservableHelper.CreateArgs<ControlDataViewModel>(x => x.ToolTipTitle);

        static readonly PropertyChangedEventArgs ToolTipDescriptionChangedEventArgs = ObservableHelper.CreateArgs<ControlDataViewModel>(x => x.ToolTipDescription);

        static readonly PropertyChangedEventArgs ToolTipImageChangedEventArgs = ObservableHelper.CreateArgs<ControlDataViewModel>(x => x.ToolTipImage);

        static readonly PropertyChangedEventArgs ToolTipFooterTitleChangedEventArgs = ObservableHelper.CreateArgs<ControlDataViewModel>(x => x.ToolTipFooterTitle);

        static readonly PropertyChangedEventArgs ToolTipFooterDescriptionChangedEventArgs = ObservableHelper.CreateArgs<ControlDataViewModel>(x => x.ToolTipFooterDescription);

        static readonly PropertyChangedEventArgs ToolTipFooterImageChangedEventArgs = ObservableHelper.CreateArgs<ControlDataViewModel>(x => x.ToolTipFooterImage);

        static readonly PropertyChangedEventArgs CommandChangedEventArgs = ObservableHelper.CreateArgs<ControlDataViewModel>(x => x.Command);

        static readonly PropertyChangedEventArgs CommandParameterChangedEventArgs = ObservableHelper.CreateArgs<ControlDataViewModel>(x => x.CommandParameter);

        static readonly PropertyChangedEventArgs KeyTipChangedEventArgs = ObservableHelper.CreateArgs<ControlDataViewModel>(x => x.KeyTip);

        static readonly PropertyChangedEventArgs IsEnabledChangedEventArgs = ObservableHelper.CreateArgs<ControlDataViewModel>(x => x.IsEnabled);

        static readonly PropertyChangedEventArgs IsEditableChangedEventArgs = ObservableHelper.CreateArgs<ControlDataViewModel>(x => x.IsEditable);
        ICommand _command;
        object _commandParameter;
        bool _isEditable;
        bool _isEnabled;
        string _keyTip;
        string _label;
        Uri _largeImage;
        Uri _smallImage;
        string _toolTipDescription;
        string _toolTipFooterDescription;
        Uri _toolTipFooterImage;
        string _toolTipFooterTitle;
        Uri _toolTipImage;
        string _toolTipTitle;

        public string Label
        {
            get { return _label; }
            set
            {
                if (_label == value) return;
                _label = value;
                NotifyPropertyChanged(LabelChangedEventArgs);
            }
        }

        public Uri LargeImage
        {
            get { return _largeImage; }
            set
            {
                if (_largeImage == value) return;
                _largeImage = value;
                NotifyPropertyChanged(LargeImageChangedEventArgs);
            }
        }

        public Uri SmallImage
        {
            get { return _smallImage; }
            set
            {
                if (_smallImage == value) return;
                _smallImage = value;
                NotifyPropertyChanged(SmallImageChangedEventArgs);
            }
        }

        public string ToolTipTitle
        {
            get { return _toolTipTitle; }
            set
            {
                if (_toolTipTitle == value) return;
                _toolTipTitle = value;
                NotifyPropertyChanged(ToolTipTitleChangedEventArgs);
            }
        }

        public string ToolTipDescription
        {
            get { return _toolTipDescription; }
            set
            {
                if (_toolTipDescription == value) return;
                _toolTipDescription = value;
                NotifyPropertyChanged(ToolTipDescriptionChangedEventArgs);
            }
        }

        public Uri ToolTipImage
        {
            get { return _toolTipImage; }
            set
            {
                if (_toolTipImage == value) return;
                _toolTipImage = value;
                NotifyPropertyChanged(ToolTipImageChangedEventArgs);
            }
        }

        public string ToolTipFooterTitle
        {
            get { return _toolTipFooterTitle; }
            set
            {
                if (_toolTipFooterTitle == value) return;
                _toolTipFooterTitle = value;
                NotifyPropertyChanged(ToolTipFooterTitleChangedEventArgs);
            }
        }

        public string ToolTipFooterDescription
        {
            get { return _toolTipFooterDescription; }
            set
            {
                if (_toolTipFooterDescription == value) return;
                _toolTipFooterDescription = value;
                NotifyPropertyChanged(ToolTipFooterDescriptionChangedEventArgs);
            }
        }

        public Uri ToolTipFooterImage
        {
            get { return _toolTipFooterImage; }
            set
            {
                if (_toolTipFooterImage == value) return;
                _toolTipFooterImage = value;
                NotifyPropertyChanged(ToolTipFooterImageChangedEventArgs);
            }
        }

        public ICommand Command
        {
            get { return _command; }
            set
            {
                if (_command == value) return;
                _command = value;
                NotifyPropertyChanged(CommandChangedEventArgs);
            }
        }

        public object CommandParameter
        {
            get { return _commandParameter; }
            set
            {
                if (_commandParameter == value) return;
                _commandParameter = value;
                NotifyPropertyChanged(CommandParameterChangedEventArgs);
            }
        }

        public string KeyTip
        {
            get { return _keyTip; }
            set
            {
                if (_keyTip == value) return;
                _keyTip = value;
                NotifyPropertyChanged(KeyTipChangedEventArgs);
            }
        }

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

        public bool IsEditable
        {
            get { return _isEditable; }
            set
            {
                if (_isEditable == value) return;
                _isEditable = value;
                NotifyPropertyChanged(IsEditableChangedEventArgs);
            }
        }

        #region IHasName Members

        string IHasName.Name
        {
            get { return Label; }
            set { Label = value; }
        }

        #endregion
    }
}