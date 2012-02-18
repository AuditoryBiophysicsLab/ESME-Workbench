using System;
using System.IO;
using ESME.Model;
using ESME.Environment;

namespace ESME.TransmissionLoss.Bellhop
{
    public static class Bellhop
    {
        //public static string GenerateBellhopRunInfo(SoundSource soundSource, bool SurfaceReflection, 
        //    bool UseVerticalBeamforming, bool GenerateArrivalsFile, double DepthCellSize_meters, 
        //    SedimentProperties Sediment, BottomProfile bottomProfile, SoundSpeedProfileProperties SSP, int NumRangeCells, int NumDepthCells, int NumBeams)
        public static string GetRadialConfiguration(TransmissionLossJob transmissionLossJob, SoundSpeedProfile<SoundSpeedSample> ssp, SedimentType sediment, float maxCalculationDepthMeters, int rangeCellCount, int depthCellCount, bool useSurfaceReflection, bool useVerticalBeamforming, bool generateArrivalsFile, int beamCount)
        {
            using (var sw = new StringWriter())
            {
                sw.WriteLine("'TL'");
                sw.WriteLine("{0},", transmissionLossJob.SoundSource.AcousticProperties.Frequency);
                sw.WriteLine("1"); // was NMEDIA in gui_genbellhopenv.m
                sw.WriteLine(useSurfaceReflection ? "'CFLT'," : "'CVLT',");

                if (depthCellCount < 5) throw new BathymetryTooShallowException("Error: Maximum depth of transect (" + maxCalculationDepthMeters + " meters) less than minimum required for transmission loss calculations.\nPlease choose a different location for this transect.");

                sw.WriteLine("0, 0.0, {0}", ssp.Data[ssp.Data.Count - 1].Depth);

                // If SSP is shallower than the bathymetry then extrapolate an SSP entry for the deepest part of the water
                //if (SSP.DepthVector[SSP.DepthVector.Length - 1] < RealBottomDepth_Meters)
                //    SoundSpeedProfile = ExtrapolateSSP(SoundSpeedProfile, RealBottomDepth_Meters);

                foreach (var soundSpeedSample in ssp.Data)
                    sw.WriteLine("{0} {1} 0.0 1.0 0.0 0.0", soundSpeedSample.Depth, soundSpeedSample.SoundSpeed);

                //sw.WriteLine("'V*', 0.0"); // A = Acoustic halfspace, * = read bathymetry file 'BTYFIL', 0.0 = bottom roughness (currently ignored)
                sw.WriteLine("'A*' 0.0"); // A = Acoustic halfspace, * = read bathymetry file 'BTYFIL', 0.0 = bottom roughness (currently ignored)
                sw.WriteLine("{0} {1} {2} {3} {4} {5} /", maxCalculationDepthMeters, sediment.CompressionWaveSpeed, sediment.ShearWaveSpeed, sediment.Density, sediment.LossParameter, 0);
                // Source and Receiver Depths and Ranges
                sw.WriteLine("1"); // Number of Source Depths
                sw.WriteLine("{0} /", transmissionLossJob.SoundSource.AcousticProperties.SourceDepth); // source depth
                sw.WriteLine("{0}", depthCellCount); // Number of Receiver Depths
                sw.WriteLine("0.0 {0} /", maxCalculationDepthMeters);
                sw.WriteLine("{0}", rangeCellCount); // Number of receiver ranges
                sw.WriteLine("0.0 {0} /", transmissionLossJob.SoundSource.Radius/1000.0);

                if (generateArrivalsFile) sw.WriteLine("'AB'");  // aB
                else sw.WriteLine(useVerticalBeamforming ? "'IG*'" : "'I'");
                // if useVerticalBeamforming is true, then SBPFIL must be present (Source Beam Pattern file)
                sw.WriteLine("{0}", beamCount); // Number of beams
                //sw.WriteLine("0"); // Number of beams
                var verticalHalfAngle = transmissionLossJob.SoundSource.AcousticProperties.VerticalBeamWidth / 2;
                var angle1 = transmissionLossJob.SoundSource.AcousticProperties.DepressionElevationAngle - verticalHalfAngle;
                var angle2 = transmissionLossJob.SoundSource.AcousticProperties.DepressionElevationAngle + verticalHalfAngle;
                sw.WriteLine("{0} {1} /", angle1, angle2); // Beam fan half-angles (negative angles are toward the surface
                //sw.WriteLine("-60.00 60.00 /"); // Beam fan half-angles (negative angles are toward the surface
                //sw.WriteLine("{0:F} {1:F} {2:F} ! step zbox(meters) rbox(km)", experiment.TransmissionLossSettings.DepthCellSize, RealBottomDepth_Meters + 100, (bottomProfile.Length / 1000.0) * 1.01);
                sw.WriteLine("0.0 {0} {1}", ssp.Data[ssp.Data.Count - 1].Depth * 2, (transmissionLossJob.SoundSource.Radius / 1000.0) * 1.01);
                return sw.ToString();
            }
        }

#if true
        /// <summary>
        /// Generate sea surface reflection coefficients for a given wind speed
        /// </summary>
        /// <param name="windSpeed">Wind speed, in meters per second.  Sample height must be 10 meters.</param>
        /// <param name="frequency">Frequency of the sound source, in Hertz.</param>
        /// <param name="startAngle">Lowest desired grazing angle, in degrees</param>
        /// <param name="endAngle">Highest desired grazing angle, in degrees</param>
        /// <param name="angleStep">Increment between calculated coefficients, in degrees</param>
        /// <returns></returns>
        public static double[,] GenerateReflectionCoefficients(float windSpeed, float frequency, double startAngle = 0, double endAngle = 90.0, double angleStep = 1.0)
        {
            //if (double.IsNaN(windSpeed) || (windSpeed < 0) || (windSpeed > 13)) throw new ArgumentException("Valid values are 0 - 13", "windSpeed");
            //if (double.IsNaN(frequency) || (frequency < 0) || (frequency > 4000)) throw new ArgumentException("Valid values are 0 - 4000", "frequency");
            //if ((windSpeed > 5) && (frequency > 1000)) throw new ArgumentException("Frequency values under 1000 require windSpeed values under 5");
            //if ((frequency < 1000) && (windSpeed < 5)) { }

            frequency /= 1000;  // Frequency is expressed in kHz in the formula

            var sampleCount = (int)((endAngle - startAngle) / angleStep) + 1;

            var result = new double[sampleCount, 3];

            var f32 = Math.Pow(frequency, 3.0 / 2.0);
            var wind4 = Math.Pow(windSpeed / 10, 4.0);
            var eta = 3.4 * f32 * wind4;
            var angle = startAngle;
            for (var angleIndex = 0; angleIndex < sampleCount; angleIndex++)
            {
                result[angleIndex, 0] = angle;
                result[angleIndex, 1] = Math.Exp(-eta * Math.Sin(angle * (Math.PI / 180.0)));
                result[angleIndex, 2] = 180;
                angle += angleStep;
            }
            return result;
        }
#endif

        public static void DrawWind(float windSpeed, float frequency)
        {
            // reference for surface loss code: US Navy
            // Software Test Description for the Surface Loss Model (SRFLOS), OAML-STD-37, Jan 1992
            // Software Requirements Specification for the Surface Loss Model (SRFLOS), OAML-SRS-37, Jan 1992

            var elevenDb = Math.Pow(10, -11 / 20d);
            var waveHeight = 0.0186 * Math.Pow(windSpeed, 2.0); // significant wave height [ft]
            var waveHeightRMS = waveHeight / 4; // rms wave height [ft]
            const double soundSpeed = 1500;
            var waveLength = (soundSpeed / frequency) * 3.2808; // wave length [ft]

            var xData = new double[91];
            var yData = new double[91];

            for (var i = 0; i < 91; i++)
            {
                var radians = (i / 180.0) * Math.PI; // convert degrees to radians
                var step1 = Math.Pow(2 * Math.PI * waveHeightRMS * Math.Sin(radians) / waveLength, 2);
                var step2 = Math.Max(Math.Exp(-2 *step1) * Bessel(2 * step1), elevenDb);

                // This looks like it's the grazing angle, in degrees
                xData[i] = i;

                // This looks like it's the attenuation, in dB
                yData[i] = Math.Max(-20 * Math.Log10(step2), 0);

                // Scott: The question is how do I get a magnitude and phase out of this, for Bellhop
            }
        }

        static double Bessel(double x)
        {
            var ax = Math.Abs(x);

            if (ax < 8.0)
            {
                var y = x * x;
                var ans1 = 57568490574.0 +
                           y *
                           (-13362590354.0 +
                            y * (651619640.7 + y * (-11214424.18 + y * (77392.33017 + y * (-184.9052456)))));
                var ans2 = 57568490411.0 +
                           y * (1029532985.0 + y * (9494680.718 + y * (59272.64853 + y * (267.8532712 + y * 1.0))));
                return ans1 / ans2;

            }
            else
            {
                var z = 8.0 / ax;
                var y = z * z;
                var xx = ax - 0.785398164;
                var ans1 = 1.0 +
                           y * (-0.1098628627e-2 + y * (0.2734510407e-4 + y * (-0.2073370639e-5 + y * 0.2093887211e-6)));
                var ans2 = -0.1562499995e-1 +
                           y * (0.1430488765e-3 + y * (-0.6911147651e-5 + y * (0.7621095161e-6 - y * 0.934935152e-7)));

                return Math.Sqrt(0.636619772 / ax) * (Math.Cos(xx) * ans1 - z * Math.Sin(xx) * ans2);
            }
        }
    }
}