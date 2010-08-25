using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Cinch;

namespace ESME.View.ViewModels.Ribbon
{
    public class TabList : List<TabData>
    {
        public TabData this[string name] { get { return Find(x => x.Header == name); } }
    }

    public class ControlList : List<ControlData>
    {
        public ControlData this[string name] { get { return Find(x => x.Label == name); } }
    }
}
