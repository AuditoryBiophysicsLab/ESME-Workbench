using ESME.Metadata;

namespace ESME.Environment.Descriptors
{
    public class NAEMOBathymetryDescriptor : NAEMODescriptor<Bathymetry, NAEMOBathymetryMetadata>
    {
        #region public OverlayFile Data { get; set; }

        public override Bathymetry Data
        {
            get { return _data ?? (_data = Bathymetry.FromYXZ(DataFilename,-1)); }
            internal set { _data = value; }
        }
        Bathymetry _data;

        #endregion
    }
}