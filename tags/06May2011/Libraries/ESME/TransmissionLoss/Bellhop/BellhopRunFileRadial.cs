using System.Xml.Serialization;

namespace ESME.TransmissionLoss.Bellhop
{
    public class BellhopRunFileRadial : TransmissionLossRunFileRadial
    {
        public string Base64EncodedBottomProfile { get; set; }

        [XmlIgnore]
        public string BottomProfile
        {
            set { Base64EncodedBottomProfile = ToBase64(value); }
            get { return FromBase64(Base64EncodedBottomProfile); }
        }
    }
}