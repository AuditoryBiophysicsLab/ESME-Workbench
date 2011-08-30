using System.Text;

namespace NetCDF
{
    public class NcDim : NcComponent
    {
        private readonly bool _isUnlimited;
        private readonly long _size;

        public NcDim(int ncID, int dimID, bool isUnlimited)
            : base(ncID, dimID)
        {
            StringBuilder curdim;
            int curlen;

            NcInqDim(ncID, dimID, out curdim, out curlen);

            ComponentName = curdim.ToString();
            _isUnlimited = isUnlimited;
            _size = curlen;
        }

        public bool IsUnlimited { get { return _isUnlimited; } }
        public long Size { get { return _size; } }
    }
}
