using System.IO;
using HRC.Navigation;

namespace ESME.Environment.NAVO
{
    class EnvironmentImporterNew
    {
    }

    public class GDEMImporter
    {
        public static void Import(string variableName, NAVOTimePeriod timePeriod, GeoRect geoRect, string destinationFilename)
        {
            if (!Directory.Exists(Path.GetDirectoryName(destinationFilename))) Directory.CreateDirectory(Path.GetDirectoryName(destinationFilename));

            var dataField = GDEM.ReadFile(GDEM.FindTemperatureFile(timePeriod), variableName, timePeriod, geoRect);
            var data = new SoundSpeed();
            data.SoundSpeedFields.Add(dataField);
            data.Serialize(destinationFilename);
        }
    }
}
