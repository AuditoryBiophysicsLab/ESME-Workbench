using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using Cinch;
using ESME.Metadata;

namespace ESME.Views.Locations
{
    public class OverlayExpandViewModel:ViewModelBase
    {
        public OverlayExpandViewModel(NAEMOOverlayMetadata naemoOverlayMetadata)
        {
            TitleString = naemoOverlayMetadata.Filename;
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
            get { return true; }
        }

        private void OKHandler()
        {
            CloseActivePopUpCommand.Execute(true);
        }

        #endregion
    }
}
