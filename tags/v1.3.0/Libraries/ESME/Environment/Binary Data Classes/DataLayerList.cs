using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;

namespace ESME.Environment
{
    public class DataLayerList : List<DataLayer>
    {
        public DataLayerList() { }

        public DataLayerList(DataFile DataFile)
        {
            _dataFile = DataFile;
        }

        public DataLayer this[string name]
        {
            get { return Find(x => x.Name == name); }
        }

        public new void Add(DataLayer DataLayer)
        {
            if (_dataFile != null)
                DataLayer.DataFile = _dataFile;
            base.Add(DataLayer);
            OnListChanged();
        }

        public new bool Remove(DataLayer DataLayer)
        {
            bool result = base.Remove(DataLayer);
            OnListChanged();
            return result;
        }

        #region ListChanged event

        public event EventHandler ListChanged;

        protected virtual void OnListChanged()
        {
            if (ListChanged != null)
                ListChanged(this, new EventArgs());
        }

        #endregion

        DataFile _dataFile = null;
    }

    public class EventArgs<T> : EventArgs
    {
        public EventArgs(T value)
        {
            Value = value;
        }

        public T Value { get; private set; }
    }
}
