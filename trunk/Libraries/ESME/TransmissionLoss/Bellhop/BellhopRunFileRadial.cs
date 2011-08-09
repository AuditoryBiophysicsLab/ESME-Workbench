using System.Xml.Serialization;

namespace ESME.TransmissionLoss.Bellhop
{
    public class BellhopRunFileRadial : TransmissionLossRunFileRadial
    {
        public string Base64EncodedBottomProfile
        {
            get { return ToBase64(BottomProfile); }
            set { BottomProfile = FromBase64(value); }
        }

        public string Base64EncodedTopReflectionCoefficient
        {
            get { return ToBase64(TopReflectionCoefficient); }
            set { TopReflectionCoefficient = FromBase64(value); }
        }

        [XmlIgnore]
        public string BottomProfile { get; set; }

        [XmlIgnore]
        public string TopReflectionCoefficient { get; set; }
    }
}