using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using Cinch;
using ESME.Metadata;
using HRC.Navigation;

namespace ESME.Views.Locations
{
    public class OverlayExpandViewModel:ViewModelBase
    {
        public OverlayExpandViewModel(NAEMOOverlayMetadata naemoOverlayMetadata)
        {
            TitleString = naemoOverlayMetadata.Filename;
            NAEMOOverlayMetadata = naemoOverlayMetadata;
        }

        #region public string TitleString { get; set; }

        public string TitleString
        {
            get { return _titleString; }
            set
            {
                if (_titleString == value) return;
                _titleString = value;
                NotifyPropertyChanged(TitleStringChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs TitleStringChangedEventArgs = ObservableHelper.CreateArgs<OverlayExpandViewModel>(x => x.TitleString);
        private string _titleString;

        #endregion

        #region public NAEMOOverlayMetadata NAEMOOverlayMetadata { get; set; }

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

        private static readonly PropertyChangedEventArgs NAEMOOverlayMetadataChangedEventArgs = ObservableHelper.CreateArgs<OverlayExpandViewModel>(x => x.NAEMOOverlayMetadata);
        private NAEMOOverlayMetadata _nAEMOOverlayMetadata;

        #endregion

        
        #region public float BufferZoneSize { get; set; }

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

        private static readonly PropertyChangedEventArgs BufferZoneSizeChangedEventArgs = ObservableHelper.CreateArgs<OverlayExpandViewModel>(x => x.BufferZoneSize);
        private float _bufferZoneSize;

        #endregion


        #region public string SelectedItem { get; set; }

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

        private static readonly PropertyChangedEventArgs SelectedItemChangedEventArgs = ObservableHelper.CreateArgs<OverlayExpandViewModel>(x => x.SelectedItem);
        private string _selectedItem;

        #endregion

        public List<string> ListItems
        {
            get
            {
                return _listItems ?? (_listItems = new List<string>
                                                         {
                                                             "km",
                                                             "m",
                                                             "nm",
                                                             
                                                         });
            }
        }

        private static List<string> _listItems;


        #region OKCommand

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

        private SimpleCommand<object, object> _oK;

        private bool IsOKCommandEnabled
        {
            get { return BufferZoneSize>0; }
        }

        private void OKHandler()
        {
            CloseActivePopUpCommand.Execute(true);
        }

        #endregion
    }
}
