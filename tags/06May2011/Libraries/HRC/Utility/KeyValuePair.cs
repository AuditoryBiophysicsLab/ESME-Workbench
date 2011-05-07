using System;
using System.Data;

namespace HRC.Utility
{
    [Serializable]
    public class KeyValuePair<TKey, TValue> where TKey : IComparable<TKey>
    {
        public KeyValuePair()
        {
            Key = default(TKey);
            Value = default(TValue);
        }

        public KeyValuePair(TKey key, TValue value)
        {
            Key = key;
            Value = value;
        }

        #region public TKey Key { get; set; }

        public TKey Key
        {
            get { return _key; }
            set
            {
                // If key is not the default value for a TKey, then don't allow the key to be set.
                //if (_key.CompareTo(default(TKey)) != 0) throw new ReadOnlyException("Key is read-only");
                _key = value;
            }
        }

        TKey _key;

        #endregion

        public TValue Value { get; set; }
    }
}
