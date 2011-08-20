using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Diagnostics;
using System.Windows.Data;
using System.Windows.Threading;
using System.Xml.Serialization;
using Cinch;

namespace HRC.Utility
{
    [Serializable]
    public class ObservableList<T> : List<T>, INotifyCollectionChanged
    {
        public new void Add(T item)
        {
            base.Add(item);
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Add, item));
        }

        public new void AddRange(IEnumerable<T> items)
        {
            base.AddRange(items);
            OnCollectionChangedMultiItem(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Add, items));
        }

        public new void Clear() 
        {
            var items = GetRange(0, Count);
            base.Clear();
            OnCollectionChangedMultiItem(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Remove, items));
        }

        public new void Insert(int index, T item)
        {
            base.Insert(index, item);
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Add, item));
        }

        public new void InsertRange(int index, IEnumerable<T> items)
        {
            base.InsertRange(index, items);
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Add, items));
        }

        public new bool Remove(T item) 
        {
            var result = base.Remove(item);
            if (result) OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Remove, item));
            return result;
        }

        public new int RemoveAll(Predicate<T> match)
        {
            var items = FindAll(match);
            var result = base.RemoveAll(match);
            if (result > 0) OnCollectionChangedMultiItem(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Remove, items));
            return result;
        }

        public new void RemoveRange(int index, int count)
        {
            var items = GetRange(index, count);
            base.RemoveRange(index, count);
            if (items.Count > 0) OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Remove, items));
        }

        public new void RemoveAt(int index)
        {
            var item = this[index];
            base.RemoveAt(index);
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Remove, item));
        }

        public new void Sort()
        {
            base.Sort();
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Reset));
        }

        public new void Sort(Comparison<T> comparison)
        {
            base.Sort(comparison);
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Reset));
        }

        public new void Sort(IComparer<T> comparer)
        {
            base.Sort(comparer);
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Reset));
        }

        public new void Sort(int index, int count, IComparer<T> comparer)
        {
            base.Sort(index, count, comparer);
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Reset));
        }

        public void Move(int oldIndex, int newIndex)
        {
            var item = base[oldIndex];
            base.RemoveAt(oldIndex);
            if (newIndex > oldIndex) newIndex--;
            base.Insert(newIndex, item);
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Move, item, newIndex, oldIndex));
        }

        [XmlIgnore] 
        public new T this[int index]
        {
            get { return base[index]; }
            set
            {
                var item = base[index];
                base[index] = value;
                OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Replace, value, item, index));
            }
        }

        public event NotifyCollectionChangedEventHandler CollectionChanged
        {
            add { CollectionChangedEvent += value; }
            remove { CollectionChangedEvent -= value; }
        }
        event NotifyCollectionChangedEventHandler CollectionChangedEvent;

        protected virtual void OnCollectionChanged(NotifyCollectionChangedEventArgs e)
        {
            var handlers = CollectionChangedEvent;
            if (handlers == null) return;
            foreach (NotifyCollectionChangedEventHandler handler in handlers.GetInvocationList())
            {
                try
                {
                    if (handler.Target is DispatcherObject)
                        ((DispatcherObject)handler.Target).Dispatcher.InvokeIfRequired(() => handler(this, e));
                    else
                        handler(this, e);
                }
                catch (Exception)
                {}
            }
        }

        protected virtual void OnCollectionChangedMultiItem(NotifyCollectionChangedEventArgs e)
        {
            var handlers = CollectionChangedEvent;
            if (handlers == null) return;
            foreach (NotifyCollectionChangedEventHandler handler in handlers.GetInvocationList())
            {
                if (handler.Target is CollectionView)
                    ((CollectionView)handler.Target).Refresh();
                else
                    handler(this, e);
            }
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
 
    }
}