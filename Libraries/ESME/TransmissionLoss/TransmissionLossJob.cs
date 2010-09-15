using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.Serialization;
using ESME.Model;

namespace ESME.TransmissionLoss
{
    public class TransmissionLossJob: IHasIDField
    {
        public NewAnalysisPoint NewAnalysisPoint { get; set; }
        public AcousticProperties AcousticProperties { get; set; }
        /// <summary>
        /// transmission loss radius, in meters.
        /// </summary>
        public int Radius { get; set; }
        /// <summary>
        /// the maximum depth to which resulting TL field will be kept, in meters. Usually less than max bathymetric depth.
        /// </summary>
        public int MaxDepth { get; set; }

        #region IHasIDField Members

        [XmlElement("TransmissionLossJobID")]
        public int IDField { get; set; }

        #endregion
        

    }
}
