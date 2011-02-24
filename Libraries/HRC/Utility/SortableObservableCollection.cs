using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace HRC.Utility
{
    internal class SortableObservableCollection<T> : ObservableCollection<T>
    {
        public void Sort()
        {
            var tmp = new List<T>();
            tmp.AddRange(this);
            tmp.Sort();
            Clear();
            foreach (var value in tmp) Add(value);
        }
    }
}