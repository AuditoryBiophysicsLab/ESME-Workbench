using System.Text.RegularExpressions;
using HRC.Utility;

namespace ESME.Mapping
{
    public class TreeNodeList : ObservableList<TreeNode>
    {
        public TreeNode this[string name] { get { return Find(item => item.Name == name); } }
        public void Remove(string name)
        {
            var target = this[name];
            if (target != null) Remove(target);
        }
        public void RemoveAll(Regex regex)
        {
            RemoveAll(item => regex.IsMatch(item.Name));
        }
    }
}