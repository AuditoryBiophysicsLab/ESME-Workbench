using System.Xml.Serialization;
using ESME.Model;

namespace ESME.TransmissionLoss
{
    public class TransmissionLossJob : IHasIDField
    {
        public SoundSource SoundSource { get; set; }

        public string Name { get; set; }

        public string Metadata { get; set; }

        /// <summary>
        ///   the maximum depth to which resulting TL field will be kept, in meters. Usually less than max bathymetric depth.
        /// </summary>
        public int MaxDepth { get; set; }

        public string Filename { get; set; }


        [XmlIgnore]
        public TransmissionLossField TransmissionLossField { get; set; }

        #region IHasIDField Members

        [XmlElement("TransmissionLossJobID")]
        public ulong IDField { get; set; }

        #endregion
    }
}