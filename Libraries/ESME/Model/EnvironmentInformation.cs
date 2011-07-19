using ESME.Environment;

namespace ESME.Model
{
    public class EnvironmentInformation
    {
        public string LocationName { get; set; }
        public string SoundSpeedFieldName { get; set; }

        public Sediment Sediment { get; set; }
        public SoundSpeedField SoundSpeedField { get; set; }
        public Bathymetry Bathymetry { get; set; }
        public EnvironmentData<WindSample> WindSpeed { get; set; }

        public EnvironmentInformation(string locationName, string soundSpeedFieldName)
            : this()
        {
            LocationName = locationName;
            SoundSpeedFieldName = soundSpeedFieldName;
        }

        public EnvironmentInformation()
        {
            LocationName = SoundSpeedFieldName = null;
            Sediment = null;
            SoundSpeedField = null;
            Bathymetry = null;
        }
    }
}
