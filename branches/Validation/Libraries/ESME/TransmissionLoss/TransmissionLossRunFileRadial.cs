using System;
using System.Text;
using System.Xml.Serialization;

namespace ESME.TransmissionLoss
{
    public class TransmissionLossRunFileRadial
    {
        public string Base64EncodedConfiguration { get; set; }
        public float BearingFromSourceDegrees { get; set; }

        [XmlIgnore]
        public string Configuration
        {
            set { Base64EncodedConfiguration = ToBase64(value); }
            get { return FromBase64(Base64EncodedConfiguration); }
        }

        protected static string ToBase64(string sourceData) { return Convert.ToBase64String(Encoding.ASCII.GetBytes(sourceData)); }

        protected static string FromBase64(string encodedData) { return Encoding.ASCII.GetString(Convert.FromBase64String(encodedData)); }
    }
}
