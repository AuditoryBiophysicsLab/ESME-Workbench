using System.ComponentModel;
using System.ComponentModel.Composition;
using Cinch;
using MEFedMVVM.ViewModelLocator;

namespace OneNavyModel.ViewModels.Ribbon
{
    [ExportViewModel("MenuButtonDataViewModel")]
    [PartCreationPolicy(CreationPolicy.NonShared)]
    public class MenuButtonDataViewModel : ControlDataViewModel
    {
        static readonly PropertyChangedEventArgs IsVerticallyResizableChangedEventArgs = ObservableHelper.CreateArgs<MenuButtonDataViewModel>(x => x.IsVerticallyResizable);

        static readonly PropertyChangedEventArgs IsHorizontallyResizableChangedEventArgs = ObservableHelper.CreateArgs<MenuButtonDataViewModel>(x => x.IsHorizontallyResizable);

        static readonly PropertyChangedEventArgs MenuItemsChangedEventArgs = ObservableHelper.CreateArgs<MenuButtonDataViewModel>(x => x.MenuItems);
        bool _isHorizontallyResizable;
        bool _isVerticallyResizable;
        MenuItemList _menuItems;

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
    }
}