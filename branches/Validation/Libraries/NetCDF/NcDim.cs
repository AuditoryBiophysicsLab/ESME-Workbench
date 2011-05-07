using System;
using System.Collections.Generic;
using System.Text;

namespace NetCDF
{
    public class NcDim : NcComponent
    {
        private bool is_unlimited;
        private long size;

        public NcDim(int NcID, int DimID, bool IsUnlimited)
            : base(NcID, DimID)
        {
            StringBuilder curdim;
            int curlen;

            NC_inq_dim(NcID, DimID, out curdim, out curlen);

            component_name = curdim.ToString();
            is_unlimited = IsUnlimited;
            size = curlen;
        }

        public bool IsUnlimited { get { return is_unlimited; } }
        public long Size { get { return size; } }
    }
}
