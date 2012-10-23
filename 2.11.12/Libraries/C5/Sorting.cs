/*
 Copyright (c) 2003-2006 Niels Kokholm and Peter Sestoft
 Permission is hereby granted, free of charge, to any person obtaining a copy
 of this software and associated documentation files (the "Software"), to deal
 in the Software without restriction, including without limitation the rights
 to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:
 
 The above copyright notice and this permission notice shall be included in
 all copies or substantial portions of the Software.
 
 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 SOFTWARE.
*/
using System;
using System.Threading.Tasks;
using SCG = System.Collections.Generic;
namespace C5
{
    /// <summary>
    /// A utility class with functions for sorting arrays with respect to an IComparer&lt;T&gt;
    /// </summary>
    public class Sorting
    {
        Sorting() { }

        /// <summary>
        /// Sort part of array in place using IntroSort
        /// </summary>
        /// <exception cref="ArgumentOutOfRangeException">If the <code>start</code>
        /// and <code>count</code> arguments does not describe a valid range.</exception>
        /// <param name="array">Array to sort</param>
        /// <param name="start">Index of first position to sort</param>
        /// <param name="count">Number of elements to sort</param>
        /// <param name="comparer">IComparer&lt;T&gt; to sort by</param>
        public static void IntroSort<T>(T[] array, int start, int count, SCG.IComparer<T> comparer)
        {
            if (start < 0 || count < 0 || start + count > array.Length)
                throw new ArgumentOutOfRangeException();
            new Sorter<T>(array, comparer).IntroSort(start, start + count);
        }

        /// <summary>
        /// Sort part of array in place using IntroSort
        /// </summary>
        /// <exception cref="ArgumentOutOfRangeException">If the <code>start</code>
        /// and <code>count</code> arguments does not describe a valid range.</exception>
        /// <param name="array">Array to sort</param>
        /// <param name="start">Index of first position to sort</param>
        /// <param name="count">Number of elements to sort</param>
        /// <param name="comparer">IComparer&lt;T&gt; to sort by</param>
        public static void ParallelIntroSort<T>(T[] array, int start, int count, SCG.IComparer<T> comparer)
        {
            if (start < 0 || count < 0 || start + count > array.Length)
                throw new ArgumentOutOfRangeException();
            new Sorter<T>(array, comparer).ParallelIntroSort(start, start + count);
        }

        /// <summary>
        /// Sort an array in place using IntroSort and default comparer
        /// </summary>
        /// <exception cref="NotComparableException">If T is not comparable</exception>
        /// <param name="array">Array to sort</param>
        public static void IntroSort<T>(T[] array)
        {
            new Sorter<T>(array, SCG.Comparer<T>.Default).IntroSort(0, array.Length);
        }


        /// <summary>
        /// Sort part of array in place using Insertion Sort
        /// </summary>
        /// <exception cref="ArgumentOutOfRangeException">If the <code>start</code>
        /// and <code>count</code> arguments does not describe a valid range.</exception>
        /// <param name="array">Array to sort</param>
        /// <param name="start">Index of first position to sort</param>
        /// <param name="count">Number of elements to sort</param>
        /// <param name="comparer">IComparer&lt;T&gt; to sort by</param>
        public static void InsertionSort<T>(T[] array, int start, int count, SCG.IComparer<T> comparer)
        {
            if (start < 0 || count < 0 || start + count > array.Length)
                throw new ArgumentOutOfRangeException();
            new Sorter<T>(array, comparer).InsertionSort(start, start + count);
        }


        /// <summary>
        /// Sort part of array in place using Heap Sort
        /// </summary>
        /// <exception cref="ArgumentOutOfRangeException">If the <code>start</code>
        /// and <code>count</code> arguments does not describe a valid range.</exception>
        /// <param name="array">Array to sort</param>
        /// <param name="start">Index of first position to sort</param>
        /// <param name="count">Number of elements to sort</param>
        /// <param name="comparer">IComparer&lt;T&gt; to sort by</param>
        public static void HeapSort<T>(T[] array, int start, int count, SCG.IComparer<T> comparer)
        {
            if (start < 0 || count < 0 || start + count > array.Length)
                throw new ArgumentOutOfRangeException();
            new Sorter<T>(array, comparer).HeapSort(start, start + count);
        }


        class Sorter<T>
        {
            readonly T[] _a;

            readonly SCG.IComparer<T> _c;


            internal Sorter(T[] a, SCG.IComparer<T> c) { _a = a; _c = c; }


            internal void IntroSort(int f, int b)
            {
                if (b - f > 31)
                {
                    var depthLimit = (int)Math.Floor(2.5 * Math.Log(b - f, 2));

                    IntroSort(f, b, depthLimit);
                }
                else
                    InsertionSort(f, b);
            }

            internal void ParallelIntroSort(int f, int b)
            {
                if (b - f > 31)
                {
                    var depthLimit = (int)Math.Floor(2.5 * Math.Log(b - f, 2));

                    ParallelIntroSort(f, b, depthLimit);
                }
                else
                    InsertionSort(f, b);
            }


            private void IntroSort(int f, int b, int depthLimit)
            {
                const int sizeThreshold = 14;//24;

                if (depthLimit-- == 0)
                    HeapSort(f, b);
                else if (b - f <= sizeThreshold)
                    InsertionSort(f, b);
                else
                {
                    var p = Partition(f, b);

                    IntroSort(f, p, depthLimit);
                    IntroSort(p, b, depthLimit);
                }
            }

            private void ParallelIntroSort(int f, int b, int depthLimit)
            {
                const int sizeThreshold = 14;//24;

                if (depthLimit-- == 0)
                    HeapSort(f, b);
                else if (b - f <= sizeThreshold)
                    InsertionSort(f, b);
                else
                {
                    var p = Partition(f, b);
                    Parallel.Invoke(() => IntroSort(f, p, depthLimit),
                                    () => IntroSort(p, b, depthLimit));
                }
            }


            private int Compare(T i1, T i2) { return _c.Compare(i1, i2); }


            private int Partition(int f, int b)
            {
                int bot = f, mid = (b + f) / 2, top = b - 1;
                T abot = _a[bot], amid = _a[mid], atop = _a[top];

                if (Compare(abot, amid) < 0)
                {
                    if (Compare(atop, abot) < 0)//atop<abot<amid
                    { _a[top] = amid; amid = _a[mid] = abot; _a[bot] = atop; }
                    else if (Compare(atop, amid) < 0) //abot<=atop<amid
                    { _a[top] = amid; amid = _a[mid] = atop; }
                    //else abot<amid<=atop
                }
                else
                {
                    if (Compare(amid, atop) > 0) //atop<amid<=abot
                    { _a[bot] = atop; _a[top] = abot; }
                    else if (Compare(abot, atop) > 0) //amid<=atop<abot
                    { _a[bot] = amid; amid = _a[mid] = atop; _a[top] = abot; }
                    else //amid<=abot<=atop
                    { _a[bot] = amid; amid = _a[mid] = abot; }
                }

                int i = bot, j = top;

                while (true)
                {
                    while (Compare(_a[++i], amid) < 0) {}

                    while (Compare(amid, _a[--j]) < 0) {}

                    if (i >= j) return i;
                    var tmp = _a[i];
                    _a[i] = _a[j];
                    _a[j] = tmp;
                }
            }


            internal void InsertionSort(int f, int b)
            {
                for (var j = f + 1; j < b; j++)
                {
                    T key = _a[j], other;
                    var i = j - 1;

                    if (_c.Compare(other = _a[i], key) <= 0) continue;
                    _a[j] = other;
                    while (i > f && _c.Compare(other = _a[i - 1], key) > 0) { _a[i--] = other; }

                    _a[i] = key;
                }
            }


            internal void HeapSort(int f, int b)
            {
                for (var i = (b + f) / 2; i >= f; i--) Heapify(f, b, i);

                for (var i = b - 1; i > f; i--)
                {
                    var tmp = _a[f]; _a[f] = _a[i]; _a[i] = tmp;
                    Heapify(f, i, f);
                }
            }


            private void Heapify(int f, int b, int i)
            {
                var pv = _a[i];
                var max = pv;
                int j = i, maxpt = j;

                while (true)
                {
                    int l = 2 * j - f + 1, r = l + 1;

                    T lv;
                    if (l < b && Compare(lv = _a[l], max) > 0) { maxpt = l; max = lv; }

                    T rv;
                    if (r < b && Compare(rv = _a[r], max) > 0) { maxpt = r; max = rv; }

                    if (maxpt == j)
                        break;

                    _a[j] = max;
                    max = pv;
                    j = maxpt;
                }

                if (j > i)
                    _a[j] = pv;
            }
        }
    }
}