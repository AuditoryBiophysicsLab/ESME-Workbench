using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace NetCDF
{
    public class NetCDF
    {
        private const int MaxStringLength = 100;
        #region Protected member variables
        protected int ncid;
        protected string filename;
        #endregion
        #region Enumerated types

        #endregion

        #region DLL entry point declarations and safe wrappers where needed
        // http://www.unidata.ucar.edu/downloads/netcdf/netcdf-3_6_1/index.jsp v3.6.1 precompiled for windows
        const string dllnetcdf = @"netcdf.dll";

        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        // omode = 0: open with read-only access
        private static unsafe extern NcResult nc_open(string path, int omode, int* ncidp);
        protected static NcResult NC_open(string path, int omode, out int ncid)
        {
            unsafe
            {
                // open the provided netCDF file
                fixed (int* ncidp = &ncid)
                    return nc_open(path, omode, ncidp);
            }
        }

        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_inq(int ncid, int* ndimsp, int* nvarsp, int* nattsp, int* unlimdimidp);
        protected static NcResult NC_inq(int ncid, out int ndims, out int nvars, out int natts, out int unlimdimid)
        {
            unsafe
            {
                fixed (int* ndimsp = &ndims, nvarsp = &nvars, nattsp = &natts, unlimdimidp = &unlimdimid)
                    return nc_inq(ncid, ndimsp, nvarsp, nattsp, unlimdimidp);
            }
        }

        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_inq_ndims(int ncid, int* ndimsp);
        protected static NcResult NC_inq_ndims(int ncid, out int ndims)
        {
            unsafe
            {
                fixed (int* ndimsp = &ndims)
                    return nc_inq_ndims(ncid, ndimsp);
            }
        }

        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_inq_nvars(int ncid, int* nvarsp);
        protected static NcResult NC_inq_nvars(int ncid, out int nvars)
        {
            unsafe
            {
                fixed (int* nvarsp = &nvars)
                    return nc_inq_nvars(ncid, nvarsp);
            }
        }

        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_inq_natts(int ncid, int* nattsp);
        protected static NcResult NC_inq_natts(int ncid, out int natts)
        {
            unsafe
            {
                fixed (int* nattsp = &natts)
                    return nc_inq_natts(ncid, nattsp);
            }
        }

        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_inq_unlimdim(int ncid, int* unlimdimidp);
        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_inq_format(int ncid, int* formatp);
        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_inq_dimid(int ncid, [MarshalAs(UnmanagedType.LPStr)] StringBuilder name, int* idp);
        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_inq_dim(int ncid, int dimid, [MarshalAs(UnmanagedType.LPStr)] StringBuilder dimname, int* lenp);
        protected static NcResult NC_inq_dim(int ncid, int dimid, out StringBuilder dimname, out int len)
        {
            dimname = new StringBuilder(MaxStringLength);
            unsafe
            {
                fixed (int* lenp = &len)
                    return nc_inq_dim(ncid, dimid, dimname, lenp);
            }
        }

        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult NC_inq_dimname(int ncid, int dimid, [MarshalAs(UnmanagedType.LPStr)] out StringBuilder dimname);

        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_inq_dimlen(int ncid, int dimid, int* dimlenp);
        protected static NcResult NC_inq_dimlen(int ncid, int dimid, ref int dimlen)
        {
            unsafe
            {
                fixed (int* dimlenp = &dimlen)
                    return nc_inq_dimlen(ncid, dimid, dimlenp);
            }
        }

        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_inq_attname(int ncid, int varid, int attnum, [MarshalAs(UnmanagedType.LPStr)] StringBuilder attname);
        protected static NcResult NC_inq_attname(int ncid, int varid, int attnum, out StringBuilder attname)
        {
            attname = new StringBuilder(MaxStringLength);
            return nc_inq_attname(ncid, varid, attnum, attname);
        }

        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_inq_att(int ncid, int varid, [MarshalAs(UnmanagedType.LPStr)] string attname, NcType* xtypep, int* lenp);
        protected static NcResult NC_inq_att(int ncid, int varid, string attname, out NcType xtype, out int len)
        {
            unsafe
            {
                fixed (NcType* xtypep = &xtype)
                {
                    fixed (int* lenp = &len)
                        return nc_inq_att(ncid, varid, attname, xtypep, lenp);
                }
            }
        }
        protected static NcResult NC_inq_global_att(int ncid, string attname, out NcType xtype, out int len)
        {
            return NC_inq_att(ncid, -1, attname, out xtype, out len);
        }

        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_inq_atttype(int ncid, int varid, [MarshalAs(UnmanagedType.LPStr)] StringBuilder attname, NcType* xtypep);
        protected static NcResult NC_inq_atttype(int ncid, int varid, out StringBuilder attname, out NcType xtype)
        {
            attname = new StringBuilder(MaxStringLength);
            unsafe
            {
                fixed (NcType* xtypep = &xtype)
                    return nc_inq_atttype(ncid, varid, attname, xtypep);
            }
        }
        protected static NcResult NC_inq_global_atttype(int ncid, out StringBuilder attname, out NcType xtype)
        {
            return NC_inq_atttype(ncid, -1, out attname, out xtype);
        }

        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_inq_attlen(int ncid, int varid, [MarshalAs(UnmanagedType.LPStr)] StringBuilder name, int* lenp);
        protected static NcResult NC_inq_attlen(int ncid, int varid, out StringBuilder attname, out int len)
        {
            attname = new StringBuilder(MaxStringLength);
            unsafe
            {
                fixed (int* lenp = &len)
                    return nc_inq_attlen(ncid, varid, attname, lenp);
            }
        }
        protected static NcResult NC_inq_global_attlen(int ncid, out StringBuilder attname, out int len)
        {
            return NC_inq_attlen(ncid, -1, out attname, out len);
        }

        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_inq_attid(int ncid, int attid, [MarshalAs(UnmanagedType.LPStr)] StringBuilder name, int* attnump);

        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_inq_dimid(int ncid, [MarshalAs(UnmanagedType.LPStr)] string dimname, int* dimidp);
        protected static NcResult NC_inq_dimid(int ncid, string dimname, out int dimid)
        {
            unsafe
            {
                fixed (int* dimidp = &dimid)
                    return nc_inq_dimid(ncid, dimname, dimidp);
            }
        }
        
        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_inq_var(int ncid, int varid, [MarshalAs(UnmanagedType.LPStr)] StringBuilder varname, NcType* xtypep, int* ndimsp, int* dimidsp, int* nattsp);
        protected static NcResult NC_inq_var(int ncid, int varid, out StringBuilder varname, out NcType xtype, out int ndims, ref int[] dimids, out int natts)
        {
            varname = new StringBuilder(MaxStringLength);
            unsafe
            {
                fixed (NcType* xtypep = &xtype)
                    fixed (int* ndimsp = &ndims, dimidsp = &dimids[0], nattsp = &natts)
                        return nc_inq_var(ncid, varid, varname, xtypep, ndimsp, dimidsp, nattsp);
            }
        }

        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_inq_vartype(int ncid, int varid, NcType* xtypep);
        protected static NcResult NC_inq_vartype(int ncid, int varid, out NcType xtype)
        {
            unsafe
            {
                fixed (NcType* xtypep = &xtype)
                    return nc_inq_vartype(ncid, varid, xtypep);
            }
        }

        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_inq_varid(int ncid, char *name, int *varidp);
        protected static NcResult NC_inq_varid(int ncid, string varname, out int varid)
        {
            unsafe
            {
                fixed (char* varnamep = varname)
                    fixed (int* varidp = &varid)
                        return nc_inq_varid(ncid, varnamep, varidp);
            }
        }

        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_inq_varname(int ncid, int varid, char *name);
        protected static NcResult NC_inq_varname(int ncid, int varid, ref string varname)
        {
            unsafe
            {
                fixed (char* varnamep = varname)
                    return nc_inq_varname(ncid, varid, varnamep);
            }
        }

        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_inq_varndims(int ncid, int varid, int *ndimsp);
        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_inq_vardimid(int ncid, int varid, int *dimidsp);
        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_inq_varnatts(int ncid, int varid, int *nattsp);

        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        protected static extern NcResult nc_get_vara_short(int ncid, int varid, uint[] start, uint[] count, short[] sp);
        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        protected static extern NcResult nc_get_var_double(int ncid, int varid, double[] dp);
        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        protected static extern NcResult nc_get_var_float(int ncid, int varid, float[] dp);
        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        protected static extern NcResult nc_get_var_int(int ncid, int varid, int[] dp);
        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        protected static extern NcResult nc_get_var_short(int ncid, int varid, short[] dp);
        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        protected static extern NcResult nc_get_var_char(int ncid, int varid, int[] dp);
        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        protected static extern NcResult nc_get_var_byte(int ncid, int varid, byte[] dp);
        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        protected static extern NcResult nc_get_var_char(int ncid, int varid, char[] dp);
        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_get_att_text(int ncid, int varid, [MarshalAs(UnmanagedType.LPStr)] string attname, [MarshalAs(UnmanagedType.LPStr)] StringBuilder tp);
        protected static NcResult NC_get_att_text(int ncid, int varid, string attname, ref StringBuilder value)
        {
            return nc_get_att_text(ncid, varid, attname, value);
        }
        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_get_att_uchar(int ncid, int varid, [MarshalAs(UnmanagedType.LPStr)] string name, byte* up);
        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_get_att_schar(int ncid, int varid, [MarshalAs(UnmanagedType.LPStr)] string name, [MarshalAs(UnmanagedType.LPStr)] StringBuilder cp);
        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_get_att_short(int ncid, int varid, [MarshalAs(UnmanagedType.LPStr)] string name, short* sp);
        protected static NcResult NC_get_att_short(int ncid, int varid, string attname, ref short[] ip)
        {
            unsafe
            {
                fixed (short* valp = ip)
                    return nc_get_att_short(ncid, varid, attname, valp);
            }
        }

        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_get_att_int(int ncid, int varid, [MarshalAs(UnmanagedType.LPStr)] string name, int* ip);
        protected static NcResult NC_get_att_int(int ncid, int varid, string attname, ref int[] ip)
        {
            unsafe
            {
                fixed (int* valp = ip)
                    return nc_get_att_int(ncid, varid, attname, valp);
            }
        }
        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_get_att_long(int ncid, int varid, [MarshalAs(UnmanagedType.LPStr)] string name, long* lp);
        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_get_att_float(int ncid, int varid, [MarshalAs(UnmanagedType.LPStr)] string name, float* fp);
        protected static NcResult NC_get_att_float(int ncid, int varid, string attname, ref float[] ip)
        {
            unsafe
            {
                fixed (float* valp = ip)
                    return nc_get_att_float(ncid, varid, attname, valp);
            }
        }

        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_get_att_double(int ncid, int varid, [MarshalAs(UnmanagedType.LPStr)] string name, double* dp);
        protected static NcResult NC_get_att_double(int ncid, int varid, string attname, ref double[] ip)
        {
            unsafe
            {
                fixed (double* valp = ip)
                    return nc_get_att_double(ncid, varid, attname, valp);
            }
        }

        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_get_att_ubyte(int ncid, int varid, [MarshalAs(UnmanagedType.LPStr)] string name, byte* ip);
        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_get_att_ushort(int ncid, int varid, [MarshalAs(UnmanagedType.LPStr)] string name, ushort* ip);
        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_get_att_uint(int ncid, int varid, [MarshalAs(UnmanagedType.LPStr)] string name, uint* ip);
        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_get_att_longlong(int ncid, int varid, [MarshalAs(UnmanagedType.LPStr)] string name, long* ip);
        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_get_att_ulonglong(int ncid, int varid, [MarshalAs(UnmanagedType.LPStr)] string name, ulong* ip);
        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_get_att_string(int ncid, int varid, [MarshalAs(UnmanagedType.LPStr)] string name, [MarshalAs(UnmanagedType.LPStr)] StringBuilder ip);
        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        private static unsafe extern NcResult nc_get_att(int ncid, int varid, [MarshalAs(UnmanagedType.LPStr)] string name, void* ip);
        [DllImport(dllnetcdf, CharSet = CharSet.Ansi)]
        protected static extern NcResult NC_close(int ncid);
        #endregion

        #region Public Constructors
        public NetCDF(int NcID)
        {
            ncid = NcID;
        }

        public NetCDF()
        {
        }
        #endregion

        #region Public Properties
        public int NcID { get { return ncid; } }
        #endregion

    }
}
