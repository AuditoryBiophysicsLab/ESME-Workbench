using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace ESME.Environment
{
    internal class DataArraySourceInfo
    {
        public BinaryReader Stream { get; set; }
        public DataArray_DEPRECATED DataArray { get; set; }
        public uint StartColumn { get; set; }
        public uint StartRow { get; set; }
        public uint RowCount { get; set; }
    }
}
