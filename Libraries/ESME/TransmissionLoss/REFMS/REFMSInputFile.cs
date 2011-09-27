using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.NEMO;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.TransmissionLoss.REFMS
{
#if false
    public class REFMSInputFile
    {
        public double BottomShearWaveSpeed { get; set; }
        public int SourceCount { get; private set; }
        public EarthCoordinate SVPLocation { get; private set; }
        public NAVOTimePeriod TimePeriod { get; private set; }
        public NemoMode NemoMode { get; private set; }
        public string OutputPath { get; private set; }
        public List<ExplosivePoint> ExplosivePoints { get; private set; }
        
        public REFMSInputFile(string outputPath, NemoScenario scenario, NemoMode mode, Geo svpLocation)
        {
            ExplosivePoints = new List<ExplosivePoint>();
            OutputPath = outputPath;
            NemoMode = mode;
            SVPLocation = new EarthCoordinate(svpLocation);
            NAVOTimePeriod timePeriod;
            if (Enum.TryParse(scenario.TimeFrame, true, out timePeriod)) TimePeriod = timePeriod;
            else throw new ApplicationException("Error converting " + scenario.TimeFrame + " to a known time period");
            SourceCount = 0;
            foreach (var curPlatform in scenario.Platforms) 
                foreach (var curSource in curPlatform.Sources) 
                    if (curSource.Type.ToLower() == "explosive") SourceCount++;
        }

        static readonly Random Random = new Random();

        string BaseFilename
        {
            get
            {
                var step1 = NemoMode.PSMId.Replace(' ', '_').Replace(':', '+').Replace('|', '~');
                return string.Format("{0}_{1}{2:0.#}", step1, SVPFilename, _explosivePoint.ExplosionDepth);
            }
        }

        string SVPFilename
        {
            get
            {
                var northSouth = SVPLocation.Latitude >= 0 ? "N" : "S";
                var eastWest = SVPLocation.Longitude >= 0 ? "E" : "W";
                var randomInt = Random.Next(99);
                return string.Format("LOC_{0}{1}_{2}{3}_{4}-{5}", DegreesMinutes(SVPLocation.Latitude), northSouth, DegreesMinutes(SVPLocation.Longitude), eastWest, randomInt, TimePeriod);
            }
        }

        static string DegreesMinutes(double itude)
        {
            var degrees = (int)(Math.Abs(itude));
            var fraction = ((int)((Math.Abs(itude) - degrees) * 100)) / 100.0;
            var minutes = (int)(fraction * 60.0);
            return string.Format("{0}{1}", degrees, minutes);
        }

        double ShearWaveVelocity
        {
            get
            {
                double bSwSpeed;
                var vSedKM = (BottomLossData.RATIOD * _svpFile.Layers.Last().SoundVelocity) / 1000.0;

                if (vSedKM < 1.555)
                {
                    //if (vSedKM < 1.512)
                    //    warn("Sediment Compression Velocity outside of algorithm range.");
                    bSwSpeed = (3.884 * vSedKM - 5.757) * 1000.0;
                }
                else if (vSedKM < 1.650)
                    bSwSpeed = (1.137 * vSedKM - 1.485) * 1000.0;
                else if (vSedKM < 2.150)
                    bSwSpeed = (0.991 - 1.136 * vSedKM + 0.47 * vSedKM * vSedKM) * 1000.0;
                else
                    bSwSpeed = (0.78 * vSedKM - 0.962) * 1000.0;
                return bSwSpeed;
            }
        }
    }
#endif
}
