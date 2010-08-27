using System.Collections.Generic;
using ESMERibbonDemo.ViewModels;

namespace ESMEWorkBench.ViewModels.Ribbon
{
    public class NamedList<T> : List<T> where T : IHasName
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
}