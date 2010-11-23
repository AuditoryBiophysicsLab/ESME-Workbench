using System;
using System.IO;
using ESME.Model;
using ESME.TransmissionLoss.Bellhop;
using HRC.Navigation;

namespace ESME.TransmissionLoss.CASS
{
    public class CassRunFile : IHasIDField
    {
        EnvironmentInformation _environment;
        string _timePeriod;

        CassRunFile() { }

        public static CassRunFile Load(string filename) { throw new NotImplementedException("This application does not currently support the loading of CASS runfiles"); }

        public void Save(string path)
        {
            WriteBathymetryFile(Path.Combine(path, "bathy.dat"));
            WriteEnvironmentFile(Path.Combine(path, "env_" + _timePeriod + ".dat"));
        }

        void WriteBathymetryFile(string bathymetryFileName)
        {
            using (var bathyFile = new StreamWriter(bathymetryFileName, false))
            {
                bathyFile.WriteLine("BOTTOM DEPTH TABLE");
                bathyFile.WriteLine("DEG       DEG       M         ");
                for (var lat = _environment.Bathymetry.Latitudes.Length - 1; lat >= 0; lat--) for (var lon = 0; lon < _environment.Bathymetry.Longitudes.Length; lon++) bathyFile.WriteLine("{0,-10:0.0000}{1,-10:0.0000}{2,-10:0.0000}", _environment.Bathymetry.Latitudes[lat], _environment.Bathymetry.Longitudes[lon], -_environment.Bathymetry.Values[lat, lon]);
                bathyFile.WriteLine("EOT");
            }
        }

        void WriteEnvironmentFile(string environmentFileName)
        {
            using (var envFile = new StreamWriter(environmentFileName, false))
            {
                envFile.WriteLine("SOUND SPEED MODEL = TABLE");
                envFile.WriteLine("RESET ENVIRONMENT NUMBER");
                envFile.WriteLine();
                for (var lat = _environment.Bathymetry.Latitudes.Length - 1; lat >= 0; lat--)
                    for (var lon = 0; lon < _environment.Bathymetry.Longitudes.Length; lon++)
                    {
                        var location = new EarthCoordinate(_environment.Bathymetry.Latitudes[lat], _environment.Bathymetry.Longitudes[lon]);
                        var ssp = _environment.SoundSpeedField[location];
                        if (ssp.SoundSpeeds.Length == 0) continue;

                        envFile.WriteLine("ENVIRONMENT LATITUDE  = {0:0.0###} DEG", location.Latitude_degrees);
                        envFile.WriteLine("ENVIRONMENT LONGITUDE = {0:0.0###} DEG", location.Longitude_degrees);
                        envFile.WriteLine("OCEAN SOUND SPEED TABLE");
                        envFile.WriteLine("M         M/S       ");
                        for (var dep = 0; dep < ssp.Depths.Length; dep++)
                            if (!float.IsNaN(ssp.SoundSpeeds[dep])) envFile.WriteLine("{0,-10:0.000}{1,-10:0.000}", _environment.SoundSpeedField.DeepestSSP.Depths[dep], ssp.SoundSpeeds[dep]);
                            else break;
                        envFile.WriteLine("EOT");
                        envFile.WriteLine("BOTTOM REFLECTION COEFFICIENT MODEL   = RAYLEIGH");
                        envFile.WriteLine("SAND");
                        envFile.WriteLine("WIND SPEED                            = 10.0 KNOTS");
                        envFile.WriteLine();
                    }
            }
        }

        public static CassRunFile Create(TransmissionLossJob transmissionLossJob, EnvironmentInformation environmentInformation, TransmissionLossSettings transmissionLossSettings, string timePeriod)
        {
            return new CassRunFile
                   {
                       _environment = environmentInformation,
                       _timePeriod = timePeriod,
                   };
        }

        public ulong IDField { get; set; }
    }
}