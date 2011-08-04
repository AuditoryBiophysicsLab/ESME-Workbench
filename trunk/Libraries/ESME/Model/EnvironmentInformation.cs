using ESME.Environment;

namespace ESME.Model
{
    public class EnvironmentInformation
    {
        public Sediment Sediment { get; set; }
        public SoundSpeedField SoundSpeedField { get; set; }
        public Bathymetry Bathymetry { get; set; }
        public Wind Wind { get; set; }

        public EnvironmentInformation()
        {
            Sediment = null;
            SoundSpeedField = null;
            Bathymetry = null;
        }
    }
}
