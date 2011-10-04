using System.Collections.Generic;
using ESME;

namespace OneNavyModel.ViewModels.Ribbon
{
    public class NamedList<T> : List<T> where T : IHaveAName
    {
        public T this[string name]
        {
            get { return Find(x => x.Name == name); }
        }
    }

    public class ControlList : NamedList<ControlDataViewModel> {}

    public class TabList : NamedList<TabDataViewModel> {}

    public class MenuItemList : NamedList<MenuItemDataViewModel> {}

    public class GroupList : NamedList<GroupDataViewModel> {}

    public class ApplicationMenuItemList : NamedList<ApplicationMenuItemDataViewModel> {}
}