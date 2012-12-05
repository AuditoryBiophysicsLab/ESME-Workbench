using System;
using System.Collections.Generic;
using System.Text;

namespace NetCDF
{
    public enum NcResult : int
    {
        NC_NOERR = 0,           /* No Error */

        NC2_ERR = -1,           /* Returned for all errors in the v2 API. */
        NC_EBADID = -33,	    /* Not a netcdf id */
        NC_ENFILE = -34,	    /* Too many netcdfs open */
        NC_EEXIST = -35,	    /* netcdf file exists && NC_NOCLOBBER */
        NC_EINVAL = -36,	    /* Invalid Argument */
        NC_EPERM = -37,	        /* Write to read only */
        NC_ENOTINDEFINE = -38,	/* Operation not allowed in data mode */
        NC_EINDEFINE = -39,	    /* Operation not allowed in define mode */
        NC_EINVALCOORDS = -40,	/* Index exceeds dimension bound */
        NC_EMAXDIMS = -41,	    /* NC_MAX_DIMS exceeded */
        NC_ENAMEINUSE = -42,	/* String match to name in use */
        NC_ENOTATT = -43,	    /* Attribute not found */
        NC_EMAXATTS = -44,	    /* NC_MAX_ATTRS exceeded */
        NC_EBADTYPE = -45,	    /* Not a netcdf data type */
        NC_EBADDIM = -46,	    /* Invalid dimension id or name */
        NC_EUNLIMPOS = -47,	    /* NC_UNLIMITED in the wrong index */
        NC_EMAXVARS = -48,	    /* NC_MAX_VARS exceeded */
        NC_ENOTVAR = -49,	    /* Variable not found */
        NC_EGLOBAL = -50,	    /* Action prohibited on NC_GLOBAL varid */
        NC_ENOTNC = -51,	    /* Not a netcdf file */
        NC_ESTS = -52,	        /* In Fortran, string too short */
        NC_EMAXNAME = -53,	    /* NC_MAX_NAME exceeded */
        NC_EUNLIMIT = -54,	    /* NC_UNLIMITED size already in use */
        NC_ENORECVARS = -55,	/* nc_rec op when there are no record vars */
        NC_ECHAR = -56,	        /* Attempt to convert between text & numbers */
        NC_EEDGE = -57,	        /* Start+count exceeds dimension bound */
        NC_ESTRIDE = -58,	    /* Illegal stride */
        NC_EBADNAME = -59,	    /* Attribute or variable name contains illegal characters */
        /* N.B. following must match value in ncx.h */
        NC_ERANGE = -60,	    /* Math result not representable */
        NC_ENOMEM = -61,	    /* Memory allocation malloc, failure */

        NC_EVARSIZE = -62,      /* One or more variable sizes violate format constraints */
        NC_EDIMSIZE = -63,      /* Invalid dimension size */
        NC_ETRUNC = -64,        /* File likely truncated or possibly corrupted */
    }
}
