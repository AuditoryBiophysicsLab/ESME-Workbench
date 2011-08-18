using System.Xml.Serialization;

namespace ESME.TransmissionLoss.Bellhop
{
    public class BellhopRunFileRadial : TransmissionLossRunFileRadial
    {
        [XmlIgnore]
        public string BottomProfile { get; set; }

        [XmlIgnore]
        public double[,] TopReflectionCoefficient { get; set; }
    }
}