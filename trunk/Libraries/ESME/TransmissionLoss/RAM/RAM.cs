using System;
using System.IO;
using ESME.Model;

namespace ESME.TransmissionLoss.RAM
{
    public class Ram
    {
        /// <summary>
        /// RAM Runfile format:
        /// TITLE
        /// FREQ ZS ZR
        /// RMAX DR NDR
        /// ZMAX DZ NDZ ZMPLT
        /// CO NPADE NS RS
        /// %First we write out the bathymetry and range (in a loop)
        /// RB(ind),ZB(ind)
        /// %ends with -1, -1
        /// 
        /// Where:
        ///%       ram.freq         ==>  Frequency;
        ///%       ram.zs           ==>  source depth (m)
        ///%       ram.zr           ==>  depth(m) for tl.line output file 
        ///%       ram.dr           ==>  range step size (m)
        ///%       ram.dz           ==>  depth step size (m)
        ///%       ram.CO           ==>  refrence sound speed (m/s)
        ///%       ram.bathy        ==>  bathymetry [range (km), depth (m)]
        ///%       ram.ssp.wat      ==>  Water sound speed profile [depth (m), sound speed (m/s)]
        ///%       ram.bottom.props ==>  Bottom properties 
        ///%       ram.bottom_type  ==>  Bottom type (Specify atten units)
        ///%       ram.KrOpt        ==>  Kraken options (for top BC and attenuation; only attn used)
        ///% OPTIONAL INPUTS:
        ///% NPADE = number of terms in rational approximation
        ///% NS = number of stability constraints, 
        ///% RS = max range of stability constraints   
        /// 
        /// TITLE = 'RAM_GEO_MAT'; % title for ramgeo.in header
        /// FREQ = ram.freq; % source frequency (Hz)
        /// ZS = ram.zs; % source depth (m)
        /// ZR = ram.zr; % depth step (m); 1/10 lambda suggested for RAM
        /// DR = ram.dr; % range step (m); 5xDZ suggested for RAM
        /// NDR = 1; % range decimation factor for output files tl.grid and ramgeo.trf
        /// DZ = ram.dz;  % lambda/10 suggested
        /// NDZ = 1; % depth decimation factor for output files tl.grid and ramgeo.trf
        /// CO = ram.CO; %Reference sound speed (m/s)
        /// RB = ram.bathy(:,1)*1000; % range (m) corresponding to bathymetry depths, ZB
        /// ZB = ram.bathy(:,2); % depth (m) of bathymetry at ranges, RB
        /// ZMPLT = max(ZB) + DZ; % maximum depth (m) in output files tl.grid and ramgeo.trf
        /// RMAX = max(RB); % maximum range (m)
        /// </summary>
        /// <param name="transmissionLossJob"></param>
        /// <param name="ssp"></param>
        /// <param name="sediment"></param>
        /// <param name="maxCalculationDepthMeters"></param>
        /// <param name="rangeCellCount"></param>
        /// <param name="depthCellCount"></param>
        /// <param name="useSurfaceReflection"></param>
        /// <param name="useVerticalBeamforming"></param>
        /// <param name="generateArrivalsFile"></param>
        /// <param name="beamCount"></param>
        /// <returns></returns>
        public static string GetRadialConfiguration(TransmissionLossJob transmissionLossJob, SoundSpeedProfile ssp, SedimentType sediment, float maxCalculationDepthMeters, int rangeCellCount, int depthCellCount, bool useSurfaceReflection, bool useVerticalBeamforming, bool generateArrivalsFile, int beamCount)
        {
            double angle1,
                   angle2;

            
            using (var sw = new StringWriter())
            {
                sw.WriteLine("RAM\n");
                sw.WriteLine("{0:F} {1:F} {2:F}", transmissionLossJob.AcousticProperties.HighFrequency, transmissionLossJob.AcousticProperties.SourceDepth, transmissionLossJob);
                sw.WriteLine("1,"); // was NMEDIA in gui_genbellhopenv.m
                sw.WriteLine(useSurfaceReflection ? "'CFMT'," : "'CVMT',");

                if (depthCellCount < 5) throw new BathymetryTooShallowException("Error: Maximum depth of transect (" + maxCalculationDepthMeters + " meters) less than minimum required for transmission loss calculations.\nPlease choose a different location for this transect.");

                sw.WriteLine("{0}, 0.0, {1:F}, !NMESH SIGMA Z(NSSP)", rangeCellCount, maxCalculationDepthMeters);

                // If SSP is shallower than the bathymetry then extrapolate an SSP entry for the deepest part of the water
                //if (SSP.DepthVector[SSP.DepthVector.Length - 1] < RealBottomDepth_Meters)
                //    SoundSpeedProfile = ExtrapolateSSP(SoundSpeedProfile, RealBottomDepth_Meters);

                double depth2;
                var depth1 = depth2 = ssp.Depths[0];
                double speed2;
                var speed1 = speed2 = ssp.SoundSpeeds[0];
                for (var i = 0; i < ssp.Depths.Length; i++)
                {
                    if (ssp.Depths[i] < maxCalculationDepthMeters)
                    {
                        depth1 = depth2;
                        depth2 = ssp.Depths[i];
                        speed1 = speed2;
                        speed2 = ssp.SoundSpeeds[i];
                        sw.WriteLine("{0:F} {1:F} /", ssp.Depths[i], ssp.SoundSpeeds[i]);
                    }
                    else
                    {
                        break;
                    }
                }
                //sw.WriteLine("{0:F} {1:F} /", maxCalculationDepthMeters, ExtrapolateSSP(depth1, speed1, depth2, speed2, maxCalculationDepthMeters));
                sw.WriteLine("'A*', 0.0"); // A = Acoustic halfspace, * = read bathymetry file 'BTYFIL', 0.0 = bottom roughness (currently ignored)
                sw.WriteLine("{0:F} {1:F} {2:F} {3:F} {4:F} {5:F} /", maxCalculationDepthMeters, sediment.CompressionWaveSpeed_metersSec, sediment.ShearWaveSpeed_metersSec, sediment.Density_gramsCC, sediment.CompressionWaveCoefficient, sediment.ShearWaveCoefficient);
                // Source and Receiver Depths and Ranges
                sw.WriteLine("1    !NSD"); // Number of Source Depths
                sw.WriteLine("  {0:F} / ! source_depth", Math.Max(1, transmissionLossJob.AcousticProperties.SourceDepth)); // source depth
                sw.WriteLine("{0}   ! NRD", depthCellCount); // Number of Receiver Depths
                sw.WriteLine("  0.0 {0:F} /  ! surface_depth (0.0) to max_depth (in meters)", maxCalculationDepthMeters);
                sw.WriteLine("{0}  ! NRR", rangeCellCount); // Number of receiver ranges
                sw.WriteLine("  0.0 {0:F} /  ! start_range (0.0) to max_range (in km)", transmissionLossJob.Radius / 1000.0);
                if (generateArrivalsFile) sw.WriteLine("'aB'");
                else
                {
                    sw.WriteLine(useVerticalBeamforming ? "'IG*'" : "'I'");
                }
                sw.WriteLine("{0}", beamCount); // Number of beams
                angle1 = transmissionLossJob.AcousticProperties.DepressionElevationAngle - (transmissionLossJob.AcousticProperties.VerticalBeamWidth / 2);
                angle2 = transmissionLossJob.AcousticProperties.DepressionElevationAngle + (transmissionLossJob.AcousticProperties.VerticalBeamWidth / 2);
                sw.WriteLine(angle1.ToString("###0.00") + " " + angle2.ToString("###0.00") + " /"); // Beam fan half-angles (negative angles are toward the surface
                //sw.WriteLine("-60.00 60.00 /"); // Beam fan half-angles (negative angles are toward the surface
                //sw.WriteLine("{0:F} {1:F} {2:F} ! step zbox(meters) rbox(km)", experiment.TransmissionLossSettings.DepthCellSize, RealBottomDepth_Meters + 100, (bottomProfile.Length / 1000.0) * 1.01);
                sw.WriteLine("0 {0:F} {1:F} ! step zbox(meters) rbox(km)", maxCalculationDepthMeters + 100, (transmissionLossJob.Radius / 1000.0) * 1.01);
                return sw.ToString();
            }
        }
    }
}
