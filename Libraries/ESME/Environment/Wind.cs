using HRC.Navigation;

namespace ESME.Environment
{
    public class Wind
    {
        public EnvironmentData<EarthCoordinate<float>> Samples { get; private set; }

        public Wind()
        {
            Samples = new EnvironmentData<EarthCoordinate<float>>();
        }

        public static Wind Load(string filename)
        {
            return new Wind { Samples = EnvironmentData<EarthCoordinate<float>>.Load(filename, null) };
        }

        public void Save(string filename)
        {
            Samples.Save(filename, null);
        }
    }
}
