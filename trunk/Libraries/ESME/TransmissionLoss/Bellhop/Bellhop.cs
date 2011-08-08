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
        public static string GetRadialConfiguration(TransmissionLossJob transmissionLossJob, SoundSpeedProfile ssp, SedimentType sediment, float maxCalculationDepthMeters, int rangeCellCount, int depthCellCount, bool useSurfaceReflection, bool useVerticalBeamforming, bool generateArrivalsFile, int beamCount)
        {
            double angle1,
                   angle2;

            using (var sw = new StringWriter())
            {
                sw.WriteLine("'TL'");
                sw.WriteLine("{0:F},", transmissionLossJob.SoundSource.AcousticProperties.HighFrequency);
                sw.WriteLine("1,"); // was NMEDIA in gui_genbellhopenv.m
                sw.WriteLine(useSurfaceReflection ? "'CFMT'," : "'CVMT',");

                if (depthCellCount < 5) throw new BathymetryTooShallowException("Error: Maximum depth of transect (" + maxCalculationDepthMeters + " meters) less than minimum required for transmission loss calculations.\nPlease choose a different location for this transect.");

                sw.WriteLine("{0}, 0.0, {1:F}, !NMESH SIGMA Z(NSSP)", rangeCellCount, maxCalculationDepthMeters);

                // If SSP is shallower than the bathymetry then extrapolate an SSP entry for the deepest part of the water
                //if (SSP.DepthVector[SSP.DepthVector.Length - 1] < RealBottomDepth_Meters)
                //    SoundSpeedProfile = ExtrapolateSSP(SoundSpeedProfile, RealBottomDepth_Meters);

                foreach (var depthValuePair in ssp.Data)
                    sw.WriteLine("{0:F} {1:F} /", depthValuePair.Depth, depthValuePair.Value);

                sw.WriteLine("'A*', 0.0"); // A = Acoustic halfspace, * = read bathymetry file 'BTYFIL', 0.0 = bottom roughness (currently ignored)
                sw.WriteLine("{0:F} {1:F} {2:F} {3:F} {4:F} {5:F} /", maxCalculationDepthMeters, sediment.CompressionWaveSpeed, 0, sediment.Density, sediment.CompressionWaveCoefficient, 0);
                // Source and Receiver Depths and Ranges
                sw.WriteLine("1    !NSD"); // Number of Source Depths
                sw.WriteLine("  {0:F} / ! source_depth", Math.Max(1, transmissionLossJob.SoundSource.AcousticProperties.SourceDepth)); // source depth
                sw.WriteLine("{0}   ! NRD", depthCellCount); // Number of Receiver Depths
                sw.WriteLine("  0.0 {0:F} /  ! surface_depth (0.0) to max_depth (in meters)", maxCalculationDepthMeters);
                sw.WriteLine("{0}  ! NRR", rangeCellCount); // Number of receiver ranges
                sw.WriteLine("  0.0 {0:F} /  ! start_range (0.0) to max_range (in km)", transmissionLossJob.SoundSource.Radius/1000.0);
                if (generateArrivalsFile) sw.WriteLine("'aB'");
                else
                {
                    sw.WriteLine(useVerticalBeamforming ? "'IG*'" : "'I'");
                }
                sw.WriteLine("{0}", beamCount); // Number of beams
                angle1 = transmissionLossJob.SoundSource.AcousticProperties.DepressionElevationAngle - (transmissionLossJob.SoundSource.AcousticProperties.VerticalBeamWidth / 2);
                angle2 = transmissionLossJob.SoundSource.AcousticProperties.DepressionElevationAngle + (transmissionLossJob.SoundSource.AcousticProperties.VerticalBeamWidth / 2);
                sw.WriteLine(angle1.ToString("###0.00") + " " + angle2.ToString("###0.00") + " /"); // Beam fan half-angles (negative angles are toward the surface
                //sw.WriteLine("-60.00 60.00 /"); // Beam fan half-angles (negative angles are toward the surface
                //sw.WriteLine("{0:F} {1:F} {2:F} ! step zbox(meters) rbox(km)", experiment.TransmissionLossSettings.DepthCellSize, RealBottomDepth_Meters + 100, (bottomProfile.Length / 1000.0) * 1.01);
                sw.WriteLine("0 {0:F} {1:F} ! step zbox(meters) rbox(km)", maxCalculationDepthMeters + 100, (transmissionLossJob.SoundSource.Radius / 1000.0) * 1.01);
                return sw.ToString();
            }
        }
    }
}