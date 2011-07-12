using ESME.Metadata;
using ESME.TransmissionLoss.CASS;

namespace ESME.Environment.Descriptors
{
    public class NAEMOEnvironmentDescriptor : NAEMODescriptor<NAEMOEnvironmentFile, NAEMOEnvironmentMetadata>
    {
        #region public OverlayFile Data { get; set; }

        public override NAEMOEnvironmentFile Data
        {
            get { return _data ?? (_data = NAEMOEnvironmentFile.Load(DataFilename)); }
            internal set { _data = value; }
        }
        NAEMOEnvironmentFile _data;

        #endregion
    }

}