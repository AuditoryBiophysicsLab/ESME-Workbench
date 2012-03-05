using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using HRC.Navigation;

namespace HRC.Collections
{
    public class KeyedList<T> : IList<T> where T : class, IHaveKey
    {
        List<LongKeyValuePair<T>> _list = new List<LongKeyValuePair<T>>();
        bool _isSorted;
        readonly RedBlack<T> _redBlack = new RedBlack<T>();

        public KeyedList() { IsReadOnly = false; }

        #region Implementation of IEnumerable
        /// <summary>
        /// Returns an enumerator that iterates through the collection.
        /// </summary>
        /// <returns>
        /// A <see cref="T:System.Collections.Generic.IEnumerator`1"/> that can be used to iterate through the collection.
        /// </returns>
        /// <filterpriority>1</filterpriority>
        public IEnumerator<T> GetEnumerator() { return _list.Select(item => item.Value).GetEnumerator(); }

        /// <summary>
        /// Returns an enumerator that iterates through a collection.
        /// </summary>
        /// <returns>
        /// An <see cref="T:System.Collections.IEnumerator"/> object that can be used to iterate through the collection.
        /// </returns>
        /// <filterpriority>2</filterpriority>
        IEnumerator IEnumerable.GetEnumerator() { return GetEnumerator(); }
        #endregion

        #region Implementation of ICollection<T>
        /// <summary>
        /// Adds an item to the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <param name="item">The object to add to the <see cref="T:System.Collections.Generic.ICollection`1"/>.</param><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only.</exception>
        public void Add(T item)
        {
            _list.Add(new LongKeyValuePair<T>(item.Key, item));
            _isSorted = false;
        }

        /// <summary>
        /// Removes all items from the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only. </exception>
        public void Clear() { _list.Clear(); }

        /// <summary>
        /// Determines whether the <see cref="T:System.Collections.Generic.ICollection`1"/> contains a specific value.
        /// </summary>
        /// <returns>
        /// true if <paramref name="item"/> is found in the <see cref="T:System.Collections.Generic.ICollection`1"/>; otherwise, false.
        /// </returns>
        /// <param name="item">The object to locate in the <see cref="T:System.Collections.Generic.ICollection`1"/>.</param>
        public bool Contains(T item) { return IndexOf(item) != -1; }

        /// <summary>
        /// Copies the elements of the <see cref="T:System.Collections.Generic.ICollection`1"/> to an <see cref="T:System.Array"/>, starting at a particular <see cref="T:System.Array"/> index.
        /// </summary>
        /// <param name="array">The one-dimensional <see cref="T:System.Array"/> that is the destination of the elements copied from <see cref="T:System.Collections.Generic.ICollection`1"/>. The <see cref="T:System.Array"/> must have zero-based indexing.</param><param name="arrayIndex">The zero-based index in <paramref name="array"/> at which copying begins.</param><exception cref="T:System.ArgumentNullException"><paramref name="array"/> is null.</exception><exception cref="T:System.ArgumentOutOfRangeException"><paramref name="arrayIndex"/> is less than 0.</exception><exception cref="T:System.ArgumentException"><paramref name="array"/> is multidimensional.-or-The number of elements in the source <see cref="T:System.Collections.Generic.ICollection`1"/> is greater than the available space from <paramref name="arrayIndex"/> to the end of the destination <paramref name="array"/>.-or-Type <paramref>
        ///                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        <name>T</name>
        ///                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      </paramref>
        ///                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     cannot be cast automatically to the type of the destination <paramref name="array"/>.</exception>
        public void CopyTo(T[] array, int arrayIndex) { throw new NotImplementedException(); }

        /// <summary>
        /// Removes the first occurrence of a specific object from the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <returns>
        /// true if <paramref name="item"/> was successfully removed from the <see cref="T:System.Collections.Generic.ICollection`1"/>; otherwise, false. This method also returns false if <paramref name="item"/> is not found in the original <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </returns>
        /// <param name="item">The object to remove from the <see cref="T:System.Collections.Generic.ICollection`1"/>.</param><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only.</exception>
        public bool Remove(T item) 
        {
            var itemIndex = IndexOf(item);
            if (itemIndex == -1) return false;
            _list.RemoveAt(itemIndex);
            return true;
        }

        /// <summary>
        /// Gets the number of elements contained in the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <returns>
        /// The number of elements contained in the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </returns>
        public int Count
        {
            get { return _list.Count; }
        }

        /// <summary>
        /// Gets a value indicating whether the <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only.
        /// </summary>
        /// <returns>
        /// true if the <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only; otherwise, false.
        /// </returns>
        public bool IsReadOnly { get; private set; }

        #endregion

        #region Implementation of IList<T>
        /// <summary>
        /// Determines the index of a specific item in the <see cref="T:System.Collections.Generic.IList`1"/>.
        /// </summary>
        /// <returns>
        /// The index of <paramref name="item"/> if found in the list; otherwise, -1.
        /// </returns>
        /// <param name="item">The object to locate in the <see cref="T:System.Collections.Generic.IList`1"/>.</param>
        public int IndexOf(T item)
        {
            if (!_isSorted) throw new NotImplementedException("IndexOf() operation is not supported on an unsorted list.  Call Sort() first and try again.");
            int itemIndex;
            BinarySearch(_list, item.Key, 0, _list.Count, out itemIndex);
            return itemIndex;
        }

        /// <summary>
        /// Inserts an item to the <see cref="T:System.Collections.Generic.IList`1"/> at the specified index.
        /// </summary>
        /// <param name="index">The zero-based index at which <paramref name="item"/> should be inserted.</param><param name="item">The object to insert into the <see cref="T:System.Collections.Generic.IList`1"/>.</param><exception cref="T:System.ArgumentOutOfRangeException"><paramref name="index"/> is not a valid index in the <see cref="T:System.Collections.Generic.IList`1"/>.</exception><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.IList`1"/> is read-only.</exception>
        public void Insert(int index, T item) { _list.Insert(index, new LongKeyValuePair<T>(item.Key, item)); }

        /// <summary>
        /// Removes the <see cref="T:System.Collections.Generic.IList`1"/> item at the specified index.
        /// </summary>
        /// <param name="index">The zero-based index of the item to remove.</param><exception cref="T:System.ArgumentOutOfRangeException"><paramref name="index"/> is not a valid index in the <see cref="T:System.Collections.Generic.IList`1"/>.</exception><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.IList`1"/> is read-only.</exception>
        public void RemoveAt(int index) { _list.RemoveAt(index); }

        /// <summary>
        /// Gets or sets the element at the specified index.
        /// </summary>
        /// <returns>
        /// The element at the specified index.
        /// </returns>
        /// <param name="index">The zero-based index of the element to get or set.</param><exception cref="T:System.ArgumentOutOfRangeException"><paramref name="index"/> is not a valid index in the <see cref="T:System.Collections.Generic.IList`1"/>.</exception><exception cref="T:System.NotSupportedException">The property is set and the <see cref="T:System.Collections.Generic.IList`1"/> is read-only.</exception>
        public T this[int index]
        {
            get { return _list[index].Value; }
            set { throw new NotImplementedException(); }
        }
        #endregion

        public void AddRange(IEnumerable<T> collection)
        {
            foreach (var item in collection)
                _list.Add(new LongKeyValuePair<T>(item.Key, item));
            _isSorted = false;
        }

        public void AddToTree(IEnumerable<T> collection)
        {
            foreach (var item in collection)
                _redBlack.Add(item.Key, item);
        }

        public void AddToTree(T item)
        {
            _redBlack.Add(item.Key, item);
        }

        public void TreeSort()
        {
            _list.Clear();
            var enumerator = _redBlack.GetEnumerator();
            while (enumerator.HasMoreElements())
            {
                var curValue = enumerator.Value;
                _list.Add(new LongKeyValuePair<T>(curValue.Key, curValue));
            }
        }

        public void MakeDistinct()
        {
            if (!_isSorted) throw new NotImplementedException("MakeDistinct() operation is not supported on an unsorted list.  Call Sort() first and try again.");
            var distinctList = new List<LongKeyValuePair<T>>();
            foreach (var item in _list)
            {
                if (distinctList.Count == 0) distinctList.Add(item);
                else if (item.Key != distinctList.Last().Key) distinctList.Add(item);
            }
            _list = distinctList;
            Sort();
        }

        public void Sort()
        {
            if (_isSorted) return;
            RecursiveMergeSort(_list);
            _isSorted = true;
        }

        public T Find(T item)
        {
            if (!_isSorted) throw new NotImplementedException("Find() operation is not supported on an unsorted list.  Call Sort() first and try again.");
            int itemIndex;
            return BinarySearch(_list, item.Key, 0, _list.Count, out itemIndex);
        }

        private static T BinarySearch(IList<LongKeyValuePair<T>> a, long key, int startIndex, int endIndex, out int itemIndex)
        {
            itemIndex = -1;
            if (endIndex < startIndex) return null; // value was not found
            var mid = startIndex + (endIndex - startIndex) / 2;
            if (a[mid].Key > key) return BinarySearch(a, key, startIndex, mid - 1, out itemIndex);
            if (a[mid].Key < key) return BinarySearch(a, key, mid + 1, endIndex, out itemIndex);
            itemIndex = mid;
            return a[mid].Value;
        }

        private static void QuickSort(IList<LongKeyValuePair<T>> a, int startIndex, int endIndex)
        {
            if (startIndex >= endIndex) return;
            var q = Partition(a, startIndex, endIndex);
            QuickSort(a, startIndex, q);
            QuickSort(a, q + 1, endIndex);
        }

        private static int Partition(IList<LongKeyValuePair<T>> a, int p, int r)
        {
            var x = a[p].Key;
            var i = p - 1;
            var j = r + 1;
            while (true)
            {
                do j--; while (a[j].Key > x);
                do i++; while (a[i].Key < x);
                if (i >= j) return j;
                var tmp = a[i];
                a[i] = a[j];
                a[j] = tmp;
            }
        }

        static List<LongKeyValuePair<T>> Merge(IList<LongKeyValuePair<T>> left, IList<LongKeyValuePair<T>> right)
        {
            var result = new List<LongKeyValuePair<T>>();
            while (left.Count > 0 || right.Count > 0)
            {
                if (left.Count > 0 && right.Count > 0)
                {
                    var firstLeft = left[0];
                    var firstRight = right[0];
                    if (firstLeft.Key <= firstRight.Key)
                    {
                        result.Add(firstLeft);
                        left.RemoveAt(0);
                    }
                    else
                    {
                        result.Add(firstRight);
                        right.RemoveAt(0);
                    }
                }
                else if (left.Count > 0) result.AddRange(left);
                else if (right.Count > 0) result.AddRange(right);
            }
            return result;
        }

        static List<LongKeyValuePair<T>> RecursiveMergeSort(List<LongKeyValuePair<T>> listToSort)
        {
            if (listToSort.Count <= 1) return listToSort;
            var left = new List<LongKeyValuePair<T>>();
            var right = new List<LongKeyValuePair<T>>();
            var mid = listToSort.Count / 2;
            for (var i = 0; i < mid; i++) left.Add(listToSort[i]);
            for (var i = mid; i < listToSort.Count; i++) right.Add(listToSort[i]);
            left = RecursiveMergeSort(left);
            right = RecursiveMergeSort(right);
            return Merge(left, right);
        }

        private static void IterativeQuickSort(IList<LongKeyValuePair<T>> listToSort, int startIndex, int endIndex)
        {
            var nStkPtr = 0;
            const bool ascendingSort = true;    // Convert to a parameter if desired

            // get the maximum size of stack required:
            var nStackMax = (int)((Math.Log(endIndex) + 3) * 2); // from Knuth Vol 3.
            // Note, +3 is added because: 
            // +1 to round up rather than down, 
            // +1 because it's a full bottom-up stack (ie Stack[0] is never used),
            // +1 because data array is zero-indexed.

            var nStack = new int[nStackMax,2];

            do
            {
                do
                {
                    var i = startIndex;
                    var j = endIndex;
                    var bDirection = true;

                    do
                    {
                        if ((listToSort[i].Key > listToSort[j].Key) == ascendingSort)
                        {
                            // Swap the two items in the list pointed to by i and j
                            var nTmp = listToSort[i];
                            listToSort[i] = listToSort[j];
                            listToSort[j] = nTmp;
                            bDirection = !bDirection;
                        }

                        if (bDirection) j--;
                        else i++;

                    } while (i < j);

                    if (i + 1 < endIndex)
                    {
                        // There's another partition to be sorted
                        nStkPtr++;
                        nStack[nStkPtr, 0] = i + 1;
                        nStack[nStkPtr, 1] = endIndex;
                    }
                    endIndex = i - 1;

                } while (startIndex < endIndex);

                if (nStkPtr == 0)
                {
                    // No more partitions to sort, so by definition we've finished!
                    return;
                }

                // Pop the most recently stored partition and sort that
                startIndex = nStack[nStkPtr, 0];
                endIndex = nStack[nStkPtr, 1];
                nStkPtr--;
            } while (true);
        }


        private class LongKeyValuePair<TValue> : IComparer<LongKeyValuePair<TValue>>
        {
            public LongKeyValuePair(long key, TValue value)
            {
                Key = key;
                Value = value;
            }

            public readonly long Key;
            public readonly TValue Value;

            #region Implementation of IComparer<in KeyedList<T>.LongKeyValuePair<TValue>>
            /// <summary>
            /// Compares two objects and returns a value indicating whether one is less than, equal to, or greater than the other.
            /// </summary>
            /// <returns>
            /// A signed integer that indicates the relative values of <paramref name="x"/> and <paramref name="y"/>, as shown in the following table.Value Meaning Less than zero<paramref name="x"/> is less than <paramref name="y"/>.Zero<paramref name="x"/> equals <paramref name="y"/>.Greater than zero<paramref name="x"/> is greater than <paramref name="y"/>.
            /// </returns>
            /// <param name="x">The first object to compare.</param><param name="y">The second object to compare.</param>
            public int Compare(LongKeyValuePair<TValue> x, LongKeyValuePair<TValue> y)
            {
                if (x.Key < y.Key) return -1;
                return x.Key > y.Key ? 1 : 0;
            }
            #endregion
        }

        private class LongKeyValuePairComparer<TValue> : IComparer<LongKeyValuePair<TValue>> 
        {
            #region Implementation of IComparer<in KeyedList<T>.LongKeyValuePair<TValue>>
            /// <summary>
            /// Compares two objects and returns a value indicating whether one is less than, equal to, or greater than the other.
            /// </summary>
            /// <returns>
            /// A signed integer that indicates the relative values of <paramref name="x"/> and <paramref name="y"/>, as shown in the following table.Value Meaning Less than zero<paramref name="x"/> is less than <paramref name="y"/>.Zero<paramref name="x"/> equals <paramref name="y"/>.Greater than zero<paramref name="x"/> is greater than <paramref name="y"/>.
            /// </returns>
            /// <param name="x">The first object to compare.</param><param name="y">The second object to compare.</param>
            public int Compare(LongKeyValuePair<TValue> x, LongKeyValuePair<TValue> y)
            {
                if (x.Key < y.Key) return -1;
                return x.Key > y.Key ? 1 : 0;
            }
            #endregion
        }
    }


    public interface IHaveKey
    {
        long Key { get; }
    }

    public class EarthCoordinateWithKey<T> : Geo<T>, IHaveKey
    {
        public long Key
        {
            get
            {
                if (_isKeyValid) return _key;
                var lon = Longitude;
                if (lon < 0) lon += 360;
                var lat = Latitude;
                if (lat < 0) lat += 90;
                var lonKeyValue = (int)Math.Round(lon * 10000);
                var latKeyValue = (int)Math.Round(lat * 10000);
                _isKeyValid = true;
                _key = (latKeyValue << 32) | lonKeyValue;
                return _key;
            }
        }

        bool _isKeyValid = false;
        long _key;
    }

    public class LatitudeFirst : Comparer<Geo>
    {
        /// <summary>
        /// When overridden in a derived class, performs a comparison of two objects of the same type and returns a value indicating whether one object is less than, equal to, or greater than the other.
        /// </summary>
        /// <returns>
        /// A signed integer that indicates the relative values of <paramref name="x"/> and <paramref name="y"/>, as shown in the following table.Value Meaning Less than zero <paramref name="x"/> is less than <paramref name="y"/>.Zero <paramref name="x"/> equals <paramref name="y"/>.Greater than zero <paramref name="x"/> is greater than <paramref name="y"/>.
        /// </returns>
        /// <param name="x">The first object to compare.</param><param name="y">The second object to compare.</param><exception cref="T:System.ArgumentException">Type <paramref name="T"/> does not implement either the <see cref="T:System.IComparable`1"/> generic interface or the <see cref="T:System.IComparable"/> interface.</exception>
        public override int Compare(Geo x, Geo y) { throw new NotImplementedException(); }
    }
}
