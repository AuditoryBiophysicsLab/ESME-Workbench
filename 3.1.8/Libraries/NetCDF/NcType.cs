using System;
using System.Collections.Generic;
using System.Text;

namespace NetCDF
{
    public enum NcType : int
    {
        ncNat = 0,	    /* NAT = 'Not A Type' (c.f. NaN) */
        ncByte = 1,	/* signed 1 byte integer */
        ncChar = 2,	/* ISO/ASCII character */
        ncShort = 3,	/* signed 2 byte integer */
        ncInt = 4,	    /* signed 4 byte integer */
        ncFloat = 5,	/* single precision floating point number */
        ncDouble = 6	/* double precision floating point number */
    }
}
