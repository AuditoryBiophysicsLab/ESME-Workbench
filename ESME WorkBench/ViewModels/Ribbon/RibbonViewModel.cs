using System.Collections.Generic;

namespace ESMERibbonDemo.ViewModels.Ribbon
{
    public class TabList : List<TabData>
    {
        public TabData this[string name]
        {
            get { return Find(x => x.Header == name); }
        }
    }

    public class ControlList : List<ControlData>
    {
        public ControlData this[string name]
        {
            get { return Find(x => x.Label == name); }
        }
    }
}