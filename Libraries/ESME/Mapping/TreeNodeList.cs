using System.Text.RegularExpressions;
using HRC.Utility;

namespace ESME.Mapping
{
    public class TreeNodeList : ObservableList<IHaveAName>
    {
        public IHaveAName this[string name] { get { return Find(item => item.Name == name); } }
        public void Remove(string name)
        {
            var target = this[name];
            if (target != null) Remove(target);
        }
        public void Remove(Regex regex)
        {
            var target = Find(item => regex.IsMatch(item.Name));
            if (target != null) Remove(target);
        }
    }
}