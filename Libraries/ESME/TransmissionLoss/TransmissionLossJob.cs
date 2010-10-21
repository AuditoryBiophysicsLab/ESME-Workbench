using System.Xml.Serialization;
using ESME.Model;

namespace ESME.TransmissionLoss
{
    public class TransmissionLossJob : IHasIDField
    {
        [XmlIgnore]
        public AnalysisPoint AnalysisPoint { get; set; }

        public string Name { get; set; }
        public string Metadata { get; set; }

        public AcousticProperties AcousticProperties { get; set; }

        /// <summary>
        ///   transmission loss radius, in meters.
        /// </summary>
        public int Radius { get; set; }

        /// <summary>
        ///   the maximum depth to which resulting TL field will be kept, in meters. Usually less than max bathymetric depth.
        /// </summary>
        public int MaxDepth { get; set; }

        public bool IsCalculated { get; set; }

        public string Filename { get; set; }

        [XmlIgnore]
        public TransmissionLossField TransmissionLossField { get; set; }

        #region IHasIDField Members

        [XmlElement("TransmissionLossJobID")]
        public int IDField { get; set; }

        #endregion
    }
}