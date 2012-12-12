using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace HRC.Collections
{
    public static class LinqExtensions
    {
        // fallback option
        public static IEnumerable<IEnumerable<T>> Split<T>(this IEnumerable<T> items, int count) { return Split(new List<T>(items), count); }

        // perferred option
        public static IEnumerable<IEnumerable<T>> Split<T>(this ICollection<T> items, int count)
        {
            if (count <= 0) throw new ArgumentOutOfRangeException("count");
            var size = items.Count / count;
            if ((items.Count % count) != 0) size++;
            var array = new T[size];
            var index = 0;
            foreach (var item in items)
            {
                array[index++] = item;
                if (index == size)
                {
                    yield return new ReadOnlyCollection<T>(array);
                    index = 0;
                }
            }
            Array.Resize(ref array, index);
            if (array.Length > 0) yield return new ReadOnlyCollection<T>(array);
        }
    }
}
