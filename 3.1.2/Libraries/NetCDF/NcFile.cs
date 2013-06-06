using System;

namespace NetCDF
{
    public class NcFile : NetCdf
    {
        readonly int _ndims;
        readonly int _nvars;
        readonly int _natts;
        readonly int _unlimdimid;
        readonly NcDimList _dims;
        readonly NcVarList _vars;
        readonly NcAttList _atts;

#region Constructors
        public NcFile(string filename)
        {
            if (filename == null) throw new ArgumentNullException("filename");
            int i, j;

            base.filename = filename;
            var status = NcOpen(base.filename, 0, out Ncid);
            if (status != NcResult.NC_NOERR)
                throw new ApplicationException(String.Format("Error constructing NcFile object for file \"{0}\".  NC_open returned {1}", filename, status.ToString()));

            status = NcInq(Ncid, out _ndims, out _nvars, out _natts, out _unlimdimid);
            if (status != NcResult.NC_NOERR)
                throw new ApplicationException(String.Format("Error constructing NcFile object for file \"{0}\".  NC_inq returned {1}", filename, status.ToString()));

            _atts = new NcAttList(_natts);
            for (j = 0; j < _natts; j++)
                _atts.Add(NcComponent.GetAttribute(Ncid, j));

            _dims = new NcDimList(_ndims);
            // Get the list of dimensions
            for (i = 0; i < _ndims; i++)
                _dims.Add(NcComponent.GetDimension(Ncid, i, (_unlimdimid == i)));

            _vars = new NcVarList(_nvars);
            // Get the list of variables
            for (i = 0; i < _nvars; i++)
                _vars.Add(NcComponent.GetVariable(Ncid, i, _dims));
        }
        #endregion

        public void LoadAllData()
        {
            foreach (var cur in _vars)
                cur.ReadData();
        }

        public NcResult CheckYerself(int varID, int x, int y, out short[] results)
        {
            var start = new uint[3];
            var count = new uint[3];

            start[0] = 0;
            start[2] = (uint)x;
            start[1] = (uint)y;
            count[0] = (uint)Variables[varID].Dimensions["depth"].Size;
            count[1] = count[2] = 1;

            results = new short[Variables[varID].Dimensions["depth"].Size];

            return nc_get_vara_short(NcID, varID, start, count, results);
        }

        public int DimensionCount { get { return _ndims; } }
        public int VariableCount { get { return _nvars; } }
        public int AttributeCount { get { return _natts; } }
        public NcDimList Dimensions { get { return _dims; } }
        public NcVarList Variables { get { return _vars; } }
        public NcAttList Attributes { get { return _atts; } }
        public string Filename { get { return filename; } }
    }
}
