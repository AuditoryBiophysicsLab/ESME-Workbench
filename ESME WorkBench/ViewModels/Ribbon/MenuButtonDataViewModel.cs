using System.ComponentModel;
using System.ComponentModel.Composition;
using Cinch;
using MEFedMVVM.ViewModelLocator;

namespace ESMERibbonDemo.ViewModels.Ribbon
{
    [ExportViewModel("MenuButtonDataViewModel")]
    [PartCreationPolicy(CreationPolicy.NonShared)]
    public class MenuButtonDataViewModel : ControlDataViewModel
    {
        public bool IsVerticallyResizable
        {
            get { return _isVerticallyResizable; }
            set
            {
                if (_isVerticallyResizable == value) return;
                _isVerticallyResizable = value;
                NotifyPropertyChanged(IsVerticallyResizableChangedEventArgs);
            }
        }
        private static readonly PropertyChangedEventArgs IsVerticallyResizableChangedEventArgs = ObservableHelper.CreateArgs<MenuButtonDataViewModel>(x => x.IsVerticallyResizable);
        private bool _isVerticallyResizable;

        public bool IsHorizontallyResizable
        {
            get { return _isHorizontallyResizable; }
            set
            {
                if (_isHorizontallyResizable == value) return;
                _isHorizontallyResizable = value;
                NotifyPropertyChanged(IsHorizontallyResizableChangedEventArgs);
            }
        }
        private static readonly PropertyChangedEventArgs IsHorizontallyResizableChangedEventArgs = ObservableHelper.CreateArgs<MenuButtonDataViewModel>(x => x.IsHorizontallyResizable);
        private bool _isHorizontallyResizable;

        public MenuItemList MenuItems
        {
            get { return _menuItems; }
            set
            {
                if (_menuItems == value) return;
                _menuItems = value;
                NotifyPropertyChanged(MenuItemsChangedEventArgs);
            }
        }
        private static readonly PropertyChangedEventArgs MenuItemsChangedEventArgs = ObservableHelper.CreateArgs<MenuButtonDataViewModel>(x => x.MenuItems);
        private MenuItemList _menuItems;
    }
}
