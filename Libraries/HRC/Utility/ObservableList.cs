using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Diagnostics;
using System.Runtime.Serialization;
using System.Windows.Data;
using System.Windows.Threading;
using System.Xml.Serialization;
using Cinch;
using HRC.Collections;

namespace HRC.Utility
{
    [Serializable]
    public class ObservableList<T> : List<T>, ICollection<T>, INotifyCollectionChanged, INotifyPropertyChanged, IDeserializationCallback
    {
        object _lockObject = new object();

        public static ObservableList<T> FromObservableConcurrentDictionary<TKey, TValue>(ObservableConcurrentDictionary<TKey, TValue> dict, Func<KeyValuePair<TKey, TValue>, T> convert, Func<KeyValuePair<TKey, TValue>, T, bool> equate, Func<KeyValuePair<TKey, TValue>, string> log = null, string listName = null)
        {
            var list = new ObservableList<T>();
            if (listName != null) list.Name = listName;
            foreach (var item in dict) list.Add(convert(item));
            ((INotifyCollectionChanged)dict).CollectionChanged += (s, e) =>
            {
                lock (list)
                {
                    switch (e.Action)
                    {
                        case NotifyCollectionChangedAction.Add:
                            foreach (KeyValuePair<TKey, TValue> newItem in e.NewItems)
                            {
                                if (!string.IsNullOrEmpty(list.Name) && (log != null)) Debug.WriteLine("{0}: About to add \"{1}\", Count={2}", list.Name, log(newItem), list.Count); 
                                list.Add(convert(newItem));
                                if (!string.IsNullOrEmpty(list.Name) && (log != null)) Debug.WriteLine("{0}: After adding \"{1}\", Count={2}", list.Name, log(newItem), list.Count);
                            }
                            break;
                        case NotifyCollectionChangedAction.Remove:
                            foreach (KeyValuePair<TKey, TValue> oldItem in e.OldItems)
                            {
                                if (!string.IsNullOrEmpty(list.Name) && (log != null)) Debug.WriteLine("{0}: About to remove \"{1}\", Count={2}", list.Name, log(oldItem), list.Count);
                                list.RemoveAll(listItem => equate(oldItem, listItem));
                                if (!string.IsNullOrEmpty(list.Name) && (log != null)) Debug.WriteLine("{0}: After removing \"{1}\", Count={2}", list.Name, log(oldItem), list.Count);
                            }
                            break;
                        case NotifyCollectionChangedAction.Replace:
                            for (var i = 0; i < e.OldItems.Count; i++)
                            {
                                var oldItem = (KeyValuePair<TKey, TValue>)e.OldItems[i];
                                var newItem = (KeyValuePair<TKey, TValue>)e.NewItems[i];
                                var listIndex = list.IndexOf(list.Where(listItem => equate(oldItem, listItem)).Single());
                                if (!string.IsNullOrEmpty(list.Name) && (log != null)) Debug.WriteLine("{0}: About to replace \"{1}\" with \"{2}\" at index {3}, Count={4}", list.Name, log(oldItem), log(newItem), listIndex, list.Count);
                                if (!string.IsNullOrEmpty(list.Name) && (log != null)) Debug.WriteLine("{0}: Before replace, list[{0}] is currently \"{1}\"", list.Name, listIndex, list[listIndex]);
                                list[listIndex] = convert(newItem);
                                if (!string.IsNullOrEmpty(list.Name) && (log != null)) Debug.WriteLine("{0}: After replacing \"{1}\" with \"{2}\", Count={3}", list.Name, log(oldItem), log(newItem), list.Count);
                                if (!string.IsNullOrEmpty(list.Name) && (log != null)) Debug.WriteLine("{0}: After replace, list[{1}] is currently \"{2}\"", list.Name, listIndex, list[listIndex]);
                            }
                            break;
                        case NotifyCollectionChangedAction.Reset:
                            if (!string.IsNullOrEmpty(list.Name) && (log != null)) Debug.WriteLine("{0}: About to reset, Count={1}", list.Name, list.Count);
                            list.Clear();
#if false
                            foreach (var item in dict)
                            {
                                list.Add(convert(item));
                                if (!string.IsNullOrEmpty(list.Name) && (log != null)) Debug.WriteLine("{0}: Added \"{1}\", Count={2}", list.Name, log(item), list.Count);
                            }
#endif
                            if (!string.IsNullOrEmpty(list.Name) && (log != null)) Debug.WriteLine("{0}: After reset, Count={1}", list.Name, list.Count);
                            break;
                    }
                }
            };
            return list;
        }

        public string Name { get; set; }

        public new void Add(T item)
        {
            lock (_lockObject) base.Add(item);
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Add, item));
        }

        void IDeserializationCallback.OnDeserialization(Object sender) { _lockObject = new object(); }

// ReSharper disable StaticFieldInGenericType
        static readonly PropertyChangedEventArgs ObservableListCountChangedEventArgs = ObservableHelper.CreateArgs<ObservableList<T>>(x => x.Count);
// ReSharper restore StaticFieldInGenericType

        public new void AddRange(IEnumerable<T> items)
        {
            lock(_lockObject) base.AddRange(items);
            OnCollectionChangedMultiItem(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Add, items));
        }

        public new void Clear()
        {
            //List<T> items;
            lock (_lockObject)
            {
                //items = GetRange(0, Count);
                base.Clear();
            }
            //OnCollectionChangedMultiItem(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Remove, items));
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Reset));
        }

        public new void Insert(int index, T item)
        {
            lock (_lockObject) base.Insert(index, item);
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Add, item));
        }

        public new void InsertRange(int index, IEnumerable<T> items)
        {
            lock (_lockObject) base.InsertRange(index, items);
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Add, items));
        }

        public new bool Remove(T item)
        {
            bool result;
            lock (_lockObject) result = base.Remove(item);
            if (result) OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Remove, item));
            return result;
        }

        public new int RemoveAll(Predicate<T> match)
        {
            List<T> items;
            int result;
            lock (_lockObject)
            {
                items = FindAll(match);
                result = base.RemoveAll(match);
            }
            if (result > 0) OnCollectionChangedMultiItem(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Remove, items));
            return result;
        }

        public new void RemoveRange(int index, int count)
        {
            List<T> items;
            lock (_lockObject)
            {
                items = GetRange(index, count);
                base.RemoveRange(index, count);
            }
            if (items.Count > 0) OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Remove, items));
        }

        public new void RemoveAt(int index)
        {
            T item;
            lock (_lockObject)
            {
                item = this[index];
                base.RemoveAt(index);
            }
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Remove, item));
        }

        public new void Sort()
        {
            lock(_lockObject) base.Sort();
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Reset));
        }

        public new void Sort(Comparison<T> comparison)
        {
            lock (_lockObject) base.Sort(comparison);
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Reset));
        }

        public new void Sort(IComparer<T> comparer)
        {
            lock (_lockObject) base.Sort(comparer);
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Reset));
        }

        public new void Sort(int index, int count, IComparer<T> comparer)
        {
            lock (_lockObject) base.Sort(index, count, comparer);
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Reset));
        }

        public void Move(int oldIndex, int newIndex)
        {
            T item;
            lock (_lockObject)
            {
                item = base[oldIndex];
                base.RemoveAt(oldIndex);
                if (newIndex > oldIndex) newIndex--;
                base.Insert(newIndex, item);
            }
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Move, item, newIndex, oldIndex));
        }

        [XmlIgnore] 
        public new T this[int index]
        {
            get { return base[index]; }
            set
            {
                T item;
                lock (_lockObject)
                {
                    item = base[index];
                    base[index] = value;
                }
                OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Replace, value, item, index));
            }
        }

        public event NotifyCollectionChangedEventHandler CollectionChanged
        {
            add { CollectionChangedEvent += value; }
            remove { CollectionChangedEvent -= value; }
        }
        event NotifyCollectionChangedEventHandler CollectionChangedEvent;

        protected void ClearCollectionChangedHandlers()
        {
            var handlers = CollectionChangedEvent;
            if (handlers == null) return;
            foreach (NotifyCollectionChangedEventHandler handler in handlers.GetInvocationList()) CollectionChanged -= handler;
        }

        protected virtual void OnCollectionChanged(NotifyCollectionChangedEventArgs e)
        {
            //if (Name != null) Debug.WriteLine("{0} {1} [{2}]", DateTime.Now, Name, e.Action);
            var handlers = CollectionChangedEvent;
            if (handlers == null) return;
            foreach (NotifyCollectionChangedEventHandler handler in handlers.GetInvocationList())
            {
                var localHandler = handler;
                try
                {
                    if (handler.Target is DispatcherObject)
                        ((DispatcherObject)handler.Target).Dispatcher.InvokeIfRequired(() => localHandler(this, e));
                    else
                        handler(this, e);
                }
                catch (Exception)
                {}
            }
            NotifyPropertyChanged(ObservableListCountChangedEventArgs);
        }

        protected virtual void OnCollectionChangedMultiItem(NotifyCollectionChangedEventArgs e)
        {
            var handlers = CollectionChangedEvent;
            if (handlers == null) return;
            foreach (NotifyCollectionChangedEventHandler handler in handlers.GetInvocationList())
            {
                var curHandler = handler;
                if (handler.Target is CollectionView)
                    ((CollectionView)handler.Target).Dispatcher.InvokeIfRequired(() => ((CollectionView)curHandler.Target).Refresh());
                else
                    handler(this, e);
            }
            NotifyPropertyChanged(ObservableListCountChangedEventArgs);
        }

        void CollectionChangedHandler(object sender, NotifyCollectionChangedEventArgs e)
        {
            switch (e.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    if (e.NewItems != null)
                        foreach (T item in e.NewItems)
                        {
                        }
                    break;
                case NotifyCollectionChangedAction.Move:
                    Debug.WriteLine("NotifyCollectionChangedAction.Move");
                    break;
                case NotifyCollectionChangedAction.Remove:
                    foreach (T item in e.OldItems)
                    {
                    }
                    break;
                case NotifyCollectionChangedAction.Replace:
                    foreach (T item in e.OldItems)
                    {
                    }
                    foreach (T item in e.NewItems)
                    {
                    }
                    break;
                case NotifyCollectionChangedAction.Reset:
                    Debug.WriteLine("NotifyCollectionChangedAction.Reset");
                    break;
            }
        }

        #region INotifyPropertyChanged Members

        public event PropertyChangedEventHandler PropertyChanged;
        protected void NotifyPropertyChanged(PropertyChangedEventArgs e)
        {
            var handlers = PropertyChanged;
            if (handlers == null) return;
            foreach (PropertyChangedEventHandler handler in handlers.GetInvocationList())
            {
                var localHandler = handler;
                if (handler.Target is DispatcherObject)
                    ((DispatcherObject)handler.Target).Dispatcher.InvokeIfRequired(() => localHandler(this, e));
                else
                    handler(this, e);
            }
        }
        #endregion
    }
}