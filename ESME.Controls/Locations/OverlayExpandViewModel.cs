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

        #region public float BufferZoneSize { get; set; } [changed]

        public float BufferZoneSize
        {
            get { return _bufferZoneSize; }
            set
            {
                if (_bufferZoneSize == value) return;
                _bufferZoneSize = value;
                NotifyPropertyChanged(BufferZoneSizeChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs BufferZoneSizeChangedEventArgs = ObservableHelper.CreateArgs<OverlayExpandViewModel>(x => x.BufferZoneSize);
        private float _bufferZoneSize;

        #region BufferZoneSizeChangedCommand

        private SimpleCommand<object, object> _bufferZoneSizeChanged;

        public SimpleCommand<object, object> BufferZoneSizeChangedCommand
        {
            get
            {
                return _bufferZoneSizeChanged ??
                       (_bufferZoneSizeChanged =
                        new SimpleCommand<object, object>(delegate(object cinchArgs)
                                                              {
                                                                  var sender =
                                                                      (TextBox) ((EventToCommandArgs) cinchArgs).Sender;
                                                                  float temp;
                                                                  if (sender != null &&
                                                                      !string.IsNullOrEmpty(sender.Text))
                                                                      BufferZoneSize = float.TryParse(sender.Text, out temp)
                                                                              ? temp
                                                                              : float.NaN;

                                                              }));
            }
        }

        #endregion

        #region public string BufferZoneSizeString { get; set; }

        public string BufferZoneSizeString
        {
            get { return _bufferZoneSizeString; }
            set
            {
                if (_bufferZoneSizeString == value) return;
                _bufferZoneSizeString = value;
                NotifyPropertyChanged(BufferZoneSizeStringChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs BufferZoneSizeStringChangedEventArgs =
            ObservableHelper.CreateArgs<OverlayExpandViewModel>(x => x.BufferZoneSizeString);

        private string _bufferZoneSizeString;

        #endregion

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