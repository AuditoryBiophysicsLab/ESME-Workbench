//-------------------------------------------------------------------------- 
//  
//  Copyright (c) Microsoft Corporation.  All rights reserved.  
//  
//  File: ObservableConcurrentDictionary.cs 
// 
//-------------------------------------------------------------------------- 

using System;
using System.Collections;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Diagnostics;
using System.Runtime.CompilerServices;
using System.Runtime.Serialization;
using System.Threading;
using System.Windows.Threading;
using System.Xml;
using System.Xml.Schema;
using System.Xml.Serialization;
using Cinch;

namespace HRC.Collections
{
    /// <summary>
    ///   Provides a thread-safe dictionary for use with data binding.  
    ///   Now XML Serializable, thanks to http://weblogs.asp.net/pwelter34/archive/2006/05/03/444961.aspx
    /// </summary>
    /// <typeparam name = "TKey">Specifies the type of the keys in this collection.</typeparam>
    /// <typeparam name = "TValue">Specifies the type of the values in this collection.</typeparam>
    [DebuggerDisplay("Count={Count}"), Serializable]
    public class ObservableConcurrentDictionary<TKey, TValue> : IDictionary<TKey, TValue>, INotifyCollectionChanged,
                                                                INotifyPropertyChanged, IDeserializationCallback, IXmlSerializable
    {
        [NonSerialized] SynchronizationContext _context;
        readonly ConcurrentDictionary<TKey, TValue> _dictionary;

        /// <summary>
        ///   Initializes an instance of the ObservableConcurrentDictionary class.
        /// </summary>
        public ObservableConcurrentDictionary()
        {
            _context = AsyncOperationManager.SynchronizationContext;
            _dictionary = new ConcurrentDictionary<TKey, TValue>();
        }

        void IDeserializationCallback.OnDeserialization(Object sender) { _context = AsyncOperationManager.SynchronizationContext; }

        /// <summary>
        ///   Event raised when the collection changes.
        /// </summary>
        [NonSerialized]
        private NotifyCollectionChangedEventHandler _collectionChanged;
        public event NotifyCollectionChangedEventHandler CollectionChanged
        {
            [MethodImpl(MethodImplOptions.Synchronized)]
            add
            {
                _collectionChanged = (NotifyCollectionChangedEventHandler)Delegate.Combine(_collectionChanged, value);
            }
            [MethodImpl(MethodImplOptions.Synchronized)]
            remove
            {
                _collectionChanged = (NotifyCollectionChangedEventHandler)Delegate.Remove(_collectionChanged, value);
            }
        }
        /// <summary>
        ///   Event raised when a property on the collection changes.
        /// </summary>
        [NonSerialized]
        private PropertyChangedEventHandler _propertyChanged;
        public event PropertyChangedEventHandler PropertyChanged
        {
            [MethodImpl(MethodImplOptions.Synchronized)]
            add
            {
                _propertyChanged = (PropertyChangedEventHandler)Delegate.Combine(_propertyChanged, value);
            }
            [MethodImpl(MethodImplOptions.Synchronized)]
            remove
            {
                _propertyChanged = (PropertyChangedEventHandler)Delegate.Remove(_propertyChanged, value);
            }
        }

        protected virtual void OnCollectionChanged(NotifyCollectionChangedEventArgs e)
        {
            //if (Name != null) Debug.WriteLine("{0} {1} [{2}]", DateTime.Now, Name, e.Action);
            var handlers = _collectionChanged;
            _context.Post(s =>
            {
                if (handlers != null)
                {
                    foreach (NotifyCollectionChangedEventHandler handler in handlers.GetInvocationList())
                    {
                        var localHandler = handler;
                        try
                        {
                            if (handler.Target is DispatcherObject) ((DispatcherObject)handler.Target).Dispatcher.InvokeIfRequired(() => localHandler(this, e));
                            else handler(this, e);
                        }
// ReSharper disable EmptyGeneralCatchClause
                        catch (Exception) {}
// ReSharper restore EmptyGeneralCatchClause
                    }
                }
                if (_propertyChanged == null) return;
                _propertyChanged(this, new PropertyChangedEventArgs("Count"));
                _propertyChanged(this, new PropertyChangedEventArgs("Keys"));
                _propertyChanged(this, new PropertyChangedEventArgs("Values"));
            }, null);
        }

        /// <summary>
        ///   Attempts to add an item to the dictionary, notifying observers of any changes.
        /// </summary>
        /// <param name = "item">The item to be added.</param>
        /// <returns>Whether the add was successful.</returns>
        void TryAddWithNotification(KeyValuePair<TKey, TValue> item)
        {
            TryAddWithNotification(item.Key, item.Value);
        }

        /// <summary>
        ///   Attempts to add an item to the dictionary, notifying observers of any changes.
        /// </summary>
        /// <param name = "key">The key of the item to be added.</param>
        /// <param name = "value">The value of the item to be added.</param>
        /// <returns>Whether the add was successful.</returns>
        void TryAddWithNotification(TKey key, TValue value)
        {
            var result = _dictionary.TryAdd(key, value);
            if (result) OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Add, new KeyValuePair<TKey, TValue>(key, value)));
        }

        /// <summary>
        ///   Attempts to remove an item from the dictionary, notifying observers of any changes.
        /// </summary>
        /// <param name = "key">The key of the item to be removed.</param>
        /// <param name = "value">The value of the item removed.</param>
        /// <returns>Whether the removal was successful.</returns>
        bool TryRemoveWithNotification(TKey key, out TValue value)
        {
            var result = _dictionary.TryRemove(key, out value);
            if (result) OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Remove, new KeyValuePair<TKey, TValue>(key, value)));
            return result;
        }

        /// <summary>
        ///   Attempts to add or update an item in the dictionary, notifying observers of any changes.
        /// </summary>
        /// <param name = "key">The key of the item to be updated.</param>
        /// <param name = "value">The new value to set for the item.</param>
        /// <returns>Whether the update was successful.</returns>
        void UpdateWithNotification(TKey key, TValue value)
        {
            var oldItem = default(TValue);
            var isReplace = _dictionary.ContainsKey(key);
            if (isReplace) oldItem = _dictionary[key];
            _dictionary[key] = value;
            OnCollectionChanged(isReplace
                                    ? new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Replace, new KeyValuePair<TKey, TValue>(key, value), new KeyValuePair<TKey, TValue>(key, oldItem))
                                    : new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Add, new KeyValuePair<TKey, TValue>(key, value)));
        }

        #region ICollection<KeyValuePair<TKey,TValue>> Members
        void ICollection<KeyValuePair<TKey, TValue>>.Add(KeyValuePair<TKey, TValue> item) { TryAddWithNotification(item); }

        public void Clear()
        {
            ((ICollection<KeyValuePair<TKey, TValue>>)_dictionary).Clear();
            OnCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Reset));
        }

        public bool Contains(KeyValuePair<TKey, TValue> item) { return ((ICollection<KeyValuePair<TKey, TValue>>)_dictionary).Contains(item); }

        public void CopyTo(KeyValuePair<TKey, TValue>[] array, int arrayIndex) { ((ICollection<KeyValuePair<TKey, TValue>>)_dictionary).CopyTo(array, arrayIndex); }

        public int Count
        {
            get { return ((ICollection<KeyValuePair<TKey, TValue>>)_dictionary).Count; }
        }

        public bool IsReadOnly
        {
            get { return ((ICollection<KeyValuePair<TKey, TValue>>)_dictionary).IsReadOnly; }
        }

        public bool Remove(KeyValuePair<TKey, TValue> item)
        {
            TValue temp;
            return TryRemoveWithNotification(item.Key, out temp);
        }
        #endregion

        #region IEnumerable<KeyValuePair<TKey,TValue>> Members
        IEnumerator<KeyValuePair<TKey, TValue>> IEnumerable<KeyValuePair<TKey, TValue>>.GetEnumerator() { return ((ICollection<KeyValuePair<TKey, TValue>>)_dictionary).GetEnumerator(); }

        IEnumerator IEnumerable.GetEnumerator() { return ((ICollection<KeyValuePair<TKey, TValue>>)_dictionary).GetEnumerator(); }

        public IEnumerator<KeyValuePair<TKey, TValue>> GetEnumerator() { return ((ICollection<KeyValuePair<TKey, TValue>>)_dictionary).GetEnumerator(); }
        #endregion

        #region IDictionary<TKey,TValue> Members
        public void Add(TKey key, TValue value) { TryAddWithNotification(key, value); }

        public bool ContainsKey(TKey key) { return _dictionary.ContainsKey(key); }

        public ICollection<TKey> Keys
        {
            get { return _dictionary.Keys; }
        }

        public bool Remove(TKey key)
        {
            TValue temp;
            return TryRemoveWithNotification(key, out temp);
        }

        public bool TryGetValue(TKey key, out TValue value) { return _dictionary.TryGetValue(key, out value); }

        public ICollection<TValue> Values
        {
            get { return _dictionary.Values; }
        }

        public TValue this[TKey key]
        {
            get { return _dictionary[key]; }
            set { UpdateWithNotification(key, value); }
        }
        #endregion

        public XmlSchema GetSchema() { return null; }
        public void ReadXml(XmlReader reader)
        {
            var keySerializer = new XmlSerializer(typeof(TKey));
            var valueSerializer = new XmlSerializer(typeof(TValue));

            var wasEmpty = reader.IsEmptyElement;
            reader.Read();

            if (wasEmpty)
                return;

            while (reader.NodeType != XmlNodeType.EndElement)
            {
                reader.ReadStartElement("item");

                reader.ReadStartElement("key");
                var key = (TKey)keySerializer.Deserialize(reader);
                reader.ReadEndElement();

                reader.ReadStartElement("value");
                var value = (TValue)valueSerializer.Deserialize(reader);
                reader.ReadEndElement();

                Add(key, value);

                reader.ReadEndElement();
                reader.MoveToContent();
            }
            reader.ReadEndElement();
        }
        public void WriteXml(XmlWriter writer) 
        {
            var keySerializer = new XmlSerializer(typeof(TKey));
            var valueSerializer = new XmlSerializer(typeof(TValue));

            foreach (var key in _dictionary.Keys)
            {
                writer.WriteStartElement("item");

                writer.WriteStartElement("key");
                keySerializer.Serialize(writer, key);
                writer.WriteEndElement();

                writer.WriteStartElement("value");
                var value = this[key];
                valueSerializer.Serialize(writer, value);
                writer.WriteEndElement();

                writer.WriteEndElement();
            }
        }
    }
}