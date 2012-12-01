using System.Collections.Generic;

namespace HRC.Collections
{
    public class HeaderedKeyValuePairListBase<THeader, TKey, TValue>
    {
        public HeaderedKeyValuePairListBase(THeader header) : this() { Header = header; }
        public HeaderedKeyValuePairListBase() { Items = new List<KeyValuePair<TKey, TValue>>(); }

        public THeader Header { get; set; }
        public List<KeyValuePair<TKey, TValue>> Items { get; set; }
    }

    public class HeaderedPropertyDisplay : HeaderedKeyValuePairListBase<string, string, string>{}
}
