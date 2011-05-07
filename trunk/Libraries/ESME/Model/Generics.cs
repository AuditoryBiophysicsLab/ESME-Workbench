using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;

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
        ulong IDField { get; set; }
    }

    public interface ISupportValidation : INotifyPropertyChanged
    {
        bool IsValid { get; }
        string ValidationErrorText { get; }
        void Validate();
    }

    public class AutoIncrementList<T> : List<T>
        where T : IHasIDField
    {
        ulong _highestIDAssigned;

        public event EventHandler<ItemDeletedEventArgs<T>> ItemDeleted;

        protected virtual void OnItemDeleted(ItemDeletedEventArgs<T> e) { if (ItemDeleted != null) ItemDeleted(this, e); }

        internal void Initialize() { foreach (var item in this) _highestIDAssigned = Math.Max(_highestIDAssigned, item.IDField); }

        public new void Add(T item)
        {
            item.IDField = ++_highestIDAssigned;
            base.Add(item);
        }

        public new void Remove(T item)
        {
            base.Remove(item);
            OnItemDeleted(new ItemDeletedEventArgs<T>
                          {
                              Item = item
                          });
        }

        //void Renumber() { for (ulong i = 0; i < this.Count(); i++) this[i].IDField = i; }
    }

    public class UniqueAutoIncrementList<T> : AutoIncrementList<T>
        where T : IHasIDField, IEquatable<T>
    {
        new void Add(T item) { if (this.Find(s => s.Equals(item)) == null) base.Add(item); }
    }
}