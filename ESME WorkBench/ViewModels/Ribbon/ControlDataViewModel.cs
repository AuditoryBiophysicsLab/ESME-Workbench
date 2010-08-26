using System;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Windows.Input;
using Cinch;
using MEFedMVVM.ViewModelLocator;

namespace ESMERibbonDemo.ViewModels.Ribbon
{
    [ExportViewModel("ControlDataViewModel")]
    [PartCreationPolicy(CreationPolicy.NonShared)]
    public class ControlDataViewModel : ViewModelBase, IHasName
    {
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
        private static readonly PropertyChangedEventArgs LabelChangedEventArgs = ObservableHelper.CreateArgs<ControlDataViewModel>(x => x.Label);
        private string _label;

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
        private static readonly PropertyChangedEventArgs LargeImageChangedEventArgs = ObservableHelper.CreateArgs<ControlDataViewModel>(x => x.LargeImage);
        private Uri _largeImage;

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
        private static readonly PropertyChangedEventArgs SmallImageChangedEventArgs = ObservableHelper.CreateArgs<ControlDataViewModel>(x => x.SmallImage);
        private Uri _smallImage;

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
        private static readonly PropertyChangedEventArgs ToolTipTitleChangedEventArgs = ObservableHelper.CreateArgs<ControlDataViewModel>(x => x.ToolTipTitle);
        private string _toolTipTitle;

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
        private static readonly PropertyChangedEventArgs ToolTipDescriptionChangedEventArgs = ObservableHelper.CreateArgs<ControlDataViewModel>(x => x.ToolTipDescription);
        private string _toolTipDescription;

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
        private static readonly PropertyChangedEventArgs ToolTipImageChangedEventArgs = ObservableHelper.CreateArgs<ControlDataViewModel>(x => x.ToolTipImage);
        private Uri _toolTipImage;

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
        private static readonly PropertyChangedEventArgs ToolTipFooterTitleChangedEventArgs = ObservableHelper.CreateArgs<ControlDataViewModel>(x => x.ToolTipFooterTitle);
        private string _toolTipFooterTitle;

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
        private static readonly PropertyChangedEventArgs ToolTipFooterDescriptionChangedEventArgs = ObservableHelper.CreateArgs<ControlDataViewModel>(x => x.ToolTipFooterDescription);
        private string _toolTipFooterDescription;

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
        private static readonly PropertyChangedEventArgs ToolTipFooterImageChangedEventArgs = ObservableHelper.CreateArgs<ControlDataViewModel>(x => x.ToolTipFooterImage);
        private Uri _toolTipFooterImage;

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
        private static readonly PropertyChangedEventArgs CommandChangedEventArgs = ObservableHelper.CreateArgs<ControlDataViewModel>(x => x.Command);
        private ICommand _command;

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
        private static readonly PropertyChangedEventArgs KeyTipChangedEventArgs = ObservableHelper.CreateArgs<ControlDataViewModel>(x => x.KeyTip);
        private string _keyTip;

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
        private static readonly PropertyChangedEventArgs IsEnabledChangedEventArgs = ObservableHelper.CreateArgs<ControlDataViewModel>(x => x.IsEnabled);
        bool _isEnabled;

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
        private static readonly PropertyChangedEventArgs IsEditableChangedEventArgs = ObservableHelper.CreateArgs<ControlDataViewModel>(x => x.IsEditable);
        bool _isEditable;

        string IHasName.Name
        {
            get { return Label; }
            set { Label = value; }
        }
    }
}
