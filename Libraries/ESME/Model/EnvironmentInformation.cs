using ESME.Environment;

namespace ESME.Model
{
    public class EnvironmentInformation
    {
        public string LocationName { get; set; }
        public string SoundSpeedFieldName { get; set; }

        public EnvironmentData<SedimentSample> Sediment { get; set; }
        public SoundSpeedField SoundSpeedField { get; set; }
        public Environment2DData Bathymetry { get; set; }
        public Environment2DData WindSpeed { get; set; }

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
