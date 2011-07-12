using ESME.Metadata;
using ESME.Overlay;

namespace ESME.Environment.Descriptors
{
    public class NAEMOOverlayDescriptor : NAEMODescriptor<OverlayFile, NAEMOOverlayMetadata>
    {
        #region public OverlayFile Data { get; set; }

        public override OverlayFile Data
        {
            get { return _data ?? (_data = new OverlayFile(DataFilename)); }
            internal set { _data = value; }
        }
        OverlayFile _data;

        #endregion
    }
}