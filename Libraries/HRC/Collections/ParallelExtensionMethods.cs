using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using C5;

namespace HRC.Collections
{
    public static class ParallelExtensionMethods
    {
        public static void ParallelSort<T>(this T[] array, IComparer<T> comparer = null) where T : IComparer<T>
        {
            if (comparer == null) comparer = Comparer<T>.Default;
            if (array.Length < 100000)
            {
                Sorting.IntroSort(array, 0, array.Length, comparer);
                return;
            }

            var cpuCount = Environment.ProcessorCount;
            //var pass = 0;
            //var started = DateTime.Now;
            while (cpuCount > 1)
            {
                //pass++;
                //System.Diagnostics.Debug.WriteLine("{0}: ParallelSort: Starting sort pass {1} on {2} threads", DateTime.Now, pass, cpuCount);
                var totalItemsSorted = 0;
                var arraySliceLength = array.Length / cpuCount;

                Parallel.For(0, cpuCount, () => 0, (i, loop, j) =>
                {
                    //System.Diagnostics.Debug.WriteLine("{0}: ParallelSort: Starting thread to sort from index {1} to {2}", DateTime.Now, arraySliceLength * i, arraySliceLength * (i + 1));
                    Sorting.IntroSort(array, arraySliceLength * i, arraySliceLength, comparer);
                    //System.Diagnostics.Debug.WriteLine("{0}: ParallelSort: Finished sorting from index {1} to {2}", DateTime.Now, arraySliceLength * i, arraySliceLength * (i + 1));
                    return arraySliceLength;
                },
                x => Interlocked.Add(ref totalItemsSorted, x));
                //System.Diagnostics.Debug.WriteLine("{0}: ParallelSort: Finished sort pass {1}", DateTime.Now, pass);
                cpuCount /= 2;
            }
            //System.Diagnostics.Debug.WriteLine("{0}: ParallelSort: Starting final sort pass ({1} items)", DateTime.Now, array.Length);
            Sorting.IntroSort(array, 0, array.Length, comparer);
            //var elapsed = DateTime.Now - started;
            //System.Diagnostics.Debug.WriteLine("{0}: ParallelSort: Final sort pass complete.  Total time: {1} seconds", DateTime.Now, elapsed.TotalSeconds);
        }

        public static bool GetIsSorted<T>(this System.Collections.Generic.IList<T> array, IComparer<T> comparer = null) where T : IComparer<T>
        {
            if (comparer == null) comparer = Comparer<T>.Default;
            return IsSubListSorted(array, 0, array.Count - 1, comparer);
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
