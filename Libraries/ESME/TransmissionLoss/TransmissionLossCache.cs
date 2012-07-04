using System;
using System.Collections.Specialized;
using System.Threading.Tasks;
using System.Runtime.Caching;
using ESME.Scenarios;

namespace ESME.TransmissionLoss
{
    public class TransmissionLossCache : MemoryCache
    {
        public TransmissionLossCache(string name, NameValueCollection config) : base(name, config)
        {
            DefaultCacheItemPolicy = new CacheItemPolicy { SlidingExpiration = new TimeSpan(0, 0, 5, 0) };
            //DefaultCacheItemPolicy.RemovedCallback += arguments => Debug.WriteLine(string.Format("{0}: Removing radial {1} from cache", DateTime.Now, arguments.CacheItem.Key));
        }

        public CacheItemPolicy DefaultCacheItemPolicy { get; set; }

        public Task<Radial> this[Radial radial]
        {
            get
            {
                var guid = radial.Guid.ToString();
                var requestedData = (Task<Radial>)this[guid];
                if (requestedData == null)
                {
                    lock (radial)
                    {
                        requestedData = (Task<Radial>)this[guid];
                        if (requestedData == null)
                        {
                            requestedData = radial.LoadAsync();
                            Add(guid, requestedData, DefaultCacheItemPolicy);
                            //Debug.WriteLine(string.Format("{0}: Adding radial {1} to cache", DateTime.Now, guid));
                        }
                    }
                }
                return requestedData;
            }
        }
    }
}
