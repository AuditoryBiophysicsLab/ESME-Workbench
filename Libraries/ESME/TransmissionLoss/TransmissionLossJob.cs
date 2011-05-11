using System;
using System.Xml.Serialization;

namespace ESME.TransmissionLoss
{
    public class TransmissionLossJob : IEquatable<TransmissionLossJob>
    {
        public SoundSource SoundSource { get; set; }

        public string Name { get; set; }

        public string Metadata { get; set; }

        /// <summary>
        ///   the maximum depth to which resulting TL field will be kept, in meters. Usually less than max bathymetric depth.
        /// </summary>
        public int MaxDepth { get; set; }

        public string Filename { get; set; }

        /// <summary>
        ///   The presumptively-unique analysis point ID.
        /// </summary>
        public string AnalysisPointID { get; set; }

        [XmlIgnore]
        public TransmissionLossField TransmissionLossField { get; set; }

        public bool Equals(TransmissionLossJob other) { return SoundSource.Equals(other.SoundSource) && MaxDepth.Equals(other.MaxDepth) && AnalysisPointID.Equals(other.AnalysisPointID); }
    }
}