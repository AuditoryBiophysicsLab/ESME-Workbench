using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;

namespace DavesWPFTester.AxisLabeling.Language
{
    public static class Utilities
    {
        public static IEnumerable<TAccumulate> Accumulate<TSource, TAccumulate>(
            this IEnumerable<TSource> source,
            TAccumulate seed,
            Func<TAccumulate, TSource, TAccumulate> func)
        {
            if (source == null) throw new ArgumentNullException("source", "Value cannot be null.");

            if (func == null) throw new ArgumentNullException("func", "Value cannot be null.");

            var accumulator = seed;
            foreach (var item in source)
            {
                accumulator = func(accumulator, item);
                yield return accumulator;
            }
        }

        public static void AddRange<T>(this IList<T> list, IEnumerable<T> items)
        {
            if (list == null) throw new ArgumentNullException("list");
            if (items == null) throw new ArgumentNullException("items");
            if (list is BindingList<T>) (list as BindingList<T>).RaiseListChangedEvents = false;
            foreach (var item in items) list.Add(item);
            if (!(list is BindingList<T>)) return;
            (list as BindingList<T>).RaiseListChangedEvents = true;
            (list as BindingList<T>).ResetBindings();
        }

        public static void InsertRange<T>(this IList<T> list, int i, IEnumerable<T> items)
        {
            if (list == null) throw new ArgumentNullException("list");
            if (items == null) throw new ArgumentNullException("items");
            if (list is BindingList<T>) (list as BindingList<T>).RaiseListChangedEvents = false;
            foreach (var item in items)
            {
                list.Insert(i, item);
                i++;
            }

            if (!(list is BindingList<T>)) return;
            (list as BindingList<T>).RaiseListChangedEvents = true;
            (list as BindingList<T>).ResetBindings();
        }

        public static int RemoveAll<T>(this IList<T> list, Func<T, bool> match)
        {
            if (list == null) throw new ArgumentNullException("list");
            if (list is BindingList<T>) (list as BindingList<T>).RaiseListChangedEvents = false;
            var shouldRemove = list.Where(match).ToList();
            foreach (var t in shouldRemove) list.Remove(t);
            if (list is BindingList<T>)
            {
                (list as BindingList<T>).RaiseListChangedEvents = true;
                (list as BindingList<T>).ResetBindings();
            }
            return shouldRemove.Count();
        }

        public static bool ElementwiseEquals<T>(this ICollection<T> a, ICollection<T> b)
        {
            if (a == null || b == null) return false;
            return a.Count == b.Count && (from pair in a.Zip(b, (x, y) => new { first = x, second = y }) select pair.first.Equals(pair.second)).All(d => d);
        }

        public static int GetElementwiseHashCode<T>(this IEnumerable<T> a)
        {
            return a.Aggregate(0, (current, t) => current ^ t.GetHashCode());
        }
    }
}