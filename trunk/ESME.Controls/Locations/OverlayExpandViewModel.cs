using System.ComponentModel;
using System.Windows.Controls;
using Cinch;
using ESME.Metadata;

namespace ESME.Views.Locations
{
    public class OverlayExpandViewModel : ViewModelBase
    {
        public OverlayExpandViewModel(NAEMOOverlayMetadata naemoOverlayMetadata)
        {
            NAEMOOverlayMetadata = naemoOverlayMetadata;
        }

        #region public NAEMOOverlayMetadata NAEMOOverlayMetadata { get; set; }

        private static readonly PropertyChangedEventArgs NAEMOOverlayMetadataChangedEventArgs =
            ObservableHelper.CreateArgs<OverlayExpandViewModel>(x => x.NAEMOOverlayMetadata);

        private NAEMOOverlayMetadata _nAEMOOverlayMetadata;

        public NAEMOOverlayMetadata NAEMOOverlayMetadata
        {
            get { return _nAEMOOverlayMetadata; }
            set
            {
                if (_nAEMOOverlayMetadata == value) return;
                _nAEMOOverlayMetadata = value;
                NotifyPropertyChanged(NAEMOOverlayMetadataChangedEventArgs);
            }
        }

        #endregion

        #region public float BufferZoneSize { get; set; }

        private static readonly PropertyChangedEventArgs BufferZoneSizeChangedEventArgs =
            ObservableHelper.CreateArgs<OverlayExpandViewModel>(x => x.BufferZoneSize);

        private float _bufferZoneSize;

        public float BufferZoneSize
        {
            get { return _bufferZoneSize; }
            set
            {
                if (_bufferZoneSize == value) return;
                if (value > 0) _bufferZoneSize = value;
                NotifyPropertyChanged(BufferZoneSizeChangedEventArgs);
            }
        }

        #endregion

        #region public string SelectedItem { get; set; }

        private static readonly PropertyChangedEventArgs SelectedItemChangedEventArgs =
            ObservableHelper.CreateArgs<OverlayExpandViewModel>(x => x.SelectedItem);

        private string _selectedItem;

        public string SelectedItem
        {
            get { return _selectedItem; }
            set
            {
                if (_selectedItem == value) return;
                _selectedItem = value;
                NotifyPropertyChanged(SelectedItemChangedEventArgs);
            }
        }

        #endregion

        #region BufferZoneSizeTextChangedCommand

        private SimpleCommand<object, object> _bufferZoneSizeTextChanged;

        public SimpleCommand<object, object> BufferZoneSizeTextChangedCommand
        {
            get
            {
                return _bufferZoneSizeTextChanged ??
                       (_bufferZoneSizeTextChanged =
                        new SimpleCommand<object, object>(delegate(object cinchArgs)
                                                              {
                                                                  var sender =
                                                                      (TextBox) ((EventToCommandArgs) cinchArgs).Sender;
                                                                  if (sender != null &&
                                                                      !string.IsNullOrEmpty(sender.Text))
                                                                      BufferZoneSize = float.Parse(sender.Text);
                                                              }));
            }
        }
        #endregion

        #region OKCommand

        private SimpleCommand<object, object> _oK;

        public SimpleCommand<object, object> OKCommand
        {
            get
            {
                return _oK ??
                       (_oK =
                        new SimpleCommand<object, object>(delegate { return IsOKCommandEnabled; },
                                                          delegate { OKHandler(); }));
            }
        }

        private bool IsOKCommandEnabled
        {
            get { return BufferZoneSize > 0; }
        }

        private void OKHandler()
        {
            CloseActivePopUpCommand.Execute(true);
        }

        #endregion
    }
}