﻿using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using C5;
using HRC.Utility;

namespace HRC.Collections
{
    public static class ParallelExtensionMethods
    {
        public static void ParallelSort<T>(this T[] array, IComparer<T> comparer = null, BackgroundTask backgroundTask = null) where T : IComparer<T>
        {
            if (comparer == null) comparer = Comparer<T>.Default;
            if (array.Length < 10000)
            {
                Sorting.IntroSort(array, 0, array.Length, comparer);
                return;
            }

            var cpuCount = Environment.ProcessorCount;
            while (cpuCount > 1)
            {
                if (backgroundTask != null) backgroundTask.Maximum += cpuCount;
                cpuCount /= 2;
            }
            if (backgroundTask != null) backgroundTask.Maximum++;

            cpuCount = Environment.ProcessorCount;
            var pass = 0;
            var started = DateTime.Now;
            while (cpuCount > 1)
            {
                pass++;
                if (backgroundTask != null) backgroundTask.Status += string.Format("Sorting (parallel pass {0}) [{1} threads]", pass, cpuCount);
                System.Diagnostics.Debug.WriteLine("{0}: ParallelSort: Starting sort pass {1} on {2} threads", DateTime.Now, pass, cpuCount);
                var totalItemsSorted = 0;
                var arraySliceLength = array.Length / cpuCount;

                Parallel.For(0, cpuCount, () => 0, (i, loop, j) =>
                {
                    System.Diagnostics.Debug.WriteLine("{0}: ParallelSort: Starting thread to sort from index {1} to {2}", DateTime.Now, arraySliceLength * i, arraySliceLength * (i + 1));
                    Sorting.ParallelIntroSort(array, arraySliceLength * i, arraySliceLength, comparer);
                    System.Diagnostics.Debug.WriteLine("{0}: ParallelSort: Finished sorting from index {1} to {2}", DateTime.Now, arraySliceLength * i, arraySliceLength * (i + 1));
                    return arraySliceLength;
                },
                x => 
                { 
                    if (backgroundTask != null) backgroundTask.Value++;
                    Interlocked.Add(ref totalItemsSorted, x);
                });
                System.Diagnostics.Debug.WriteLine("{0}: ParallelSort: Finished sort pass {1}", DateTime.Now, pass);
                cpuCount /= 2;
            }
            System.Diagnostics.Debug.WriteLine("{0}: ParallelSort: Starting final sort pass ({1} items)", DateTime.Now, array.Length);
            Sorting.IntroSort(array, 0, array.Length, comparer);
            var elapsed = DateTime.Now - started;
            System.Diagnostics.Debug.WriteLine("{0}: ParallelSort: Final sort pass complete.  Total time: {1} seconds", DateTime.Now, elapsed.TotalSeconds);
        }

        public static bool GetIsSorted<T>(this System.Collections.Generic.IList<T> array, IComparer<T> comparer = null, BackgroundTask backgroundTask = null) where T : IComparer<T>
        {
            if (comparer == null) comparer = Comparer<T>.Default;

            var cpuCount = Environment.ProcessorCount;
            if (backgroundTask != null) backgroundTask.Maximum += cpuCount;

            cpuCount = Environment.ProcessorCount;
            var started = DateTime.Now;
            var listIsSorted = true;
            if (backgroundTask != null) backgroundTask.Status += string.Format("Checking if list is sorted [{0} threads]", cpuCount);
            System.Diagnostics.Debug.WriteLine("{0}: GetIsSorted: Starting check on {1} threads", DateTime.Now, cpuCount);
                
            var arraySliceLength = array.Count / cpuCount;

            Parallel.For(0, cpuCount, () => false, (i, loop, j) =>
            {
                System.Diagnostics.Debug.WriteLine("{0}: GetIsSorted: Starting thread to check sort order from index {1} to {2}", DateTime.Now, arraySliceLength * i, arraySliceLength * (i + 1));
                var result = IsSubListSorted(array, arraySliceLength * i, arraySliceLength * (i + 1), comparer);
                System.Diagnostics.Debug.WriteLine("{0}: GetIsSorted: Finished checking sort order from index {1} to {2}.  Sublist appears to be {3}", DateTime.Now, arraySliceLength * i, arraySliceLength * (i + 1), result ? "sorted" : "unsorted");
                return result;
            },
            x =>
            {
                if (backgroundTask != null) backgroundTask.Value++;
                lock (array)
                {
                    listIsSorted &= x;
                }
            });
            for (var i = 0; i < cpuCount - 1; i++)
            {
                var baseIndex = arraySliceLength * (i + 1);
                listIsSorted &= comparer.Compare(array[baseIndex - 1], array[baseIndex]) <= 0;
            }
            System.Diagnostics.Debug.WriteLine("{0}: GetIsSorted: Finished parallel sort order check.  List appears to be {1}", DateTime.Now, listIsSorted ? "sorted" : "unsorted");
            return listIsSorted;
        }

        static bool IsSubListSorted<T>(System.Collections.Generic.IList<T> array, int startIndex, int endIndex, IComparer<T> comparer)
        {
            for (var i = startIndex; i < endIndex - 1; i++)
            {
                if (comparer.Compare(array[i], array[i + 1]) <= 0) continue;
                return false;
            }
            return true;
        }

    }
}