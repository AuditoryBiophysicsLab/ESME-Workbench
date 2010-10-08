using System;
using System.Collections.ObjectModel;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ESME.Model
{
    public class ItemDeletedEventArgs<T> : EventArgs
    {
        public T Item;
    }

    public interface IHasNameField
    {
        string Name { get; set; }
    }

    public interface IHasIDField
    {
        int IDField { get; set; }
    }

    public class AutoIncrementList<T> : List<T>
        where T : IHasIDField
    {
        private int HighestIDAssigned = 0;

        public event EventHandler<ItemDeletedEventArgs<T>> ItemDeleted;

        protected virtual void OnItemDeleted(ItemDeletedEventArgs<T> e)
        {
            if (ItemDeleted != null)
                ItemDeleted(this, e);
        }

        internal void Initialize()
        {
            foreach (T item in this)
                HighestIDAssigned = Math.Max(HighestIDAssigned, item.IDField);
        }

        public new void Add(T item)
        {
            item.IDField = ++HighestIDAssigned;
            base.Add(item);
        }

        public new void Remove(T item)
        {
            base.Remove(item);
            OnItemDeleted(new ItemDeletedEventArgs<T> { Item = item });
        }

        private void Renumber()
        {
            for (int i = 0; i < this.Count(); i++)
                this[i].IDField = i;
        }
    }

    public class UniqueAutoIncrementList<T> : AutoIncrementList<T>
        where T : IHasIDField, IEquatable<T>
    {
        private new void Add(T item)
        {
            if (this.Find(s => s.Equals(item)) == null)
                base.Add(item);
        }
    }
}
