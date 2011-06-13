using System;
using System.IO;
using ESME.Environment;
using ESME.Model;
using ESME.TransmissionLoss.Bellhop;

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
        ///<param name="bottomProfile"></param>
        ///<param name="sediment"></param>
        /// <param name="maxCalculationDepthMeters"></param>
        /// <param name="rangeCellCount"></param>
        /// <param name="depthCellCount"></param>
        /// <returns></returns>
        public static string GetRadialConfiguration(TransmissionLossJob transmissionLossJob, SoundSpeedProfile ssp, BottomProfile bottomProfile, SedimentType sediment, float maxCalculationDepthMeters, int rangeCellCount, int depthCellCount)
        {
            const float c0 = 1500f;
            var lambda = c0 / transmissionLossJob.SoundSource.AcousticProperties.HighFrequency;
            var dz = lambda / 10;
            var dr = dz * 5f;
            var zmplt = bottomProfile.MaxDepth + dz;
            var rangeStep = transmissionLossJob.SoundSource.Radius / rangeCellCount;
            using (var sw = new StringWriter())
            {
                sw.WriteLine("RAM_GEO_MAT");
                //fprintf(fid,'%4.4f %4.4f %4.4f\n', [FREQ ZS ZR]);
                sw.WriteLine("{0:F4} {1:F4} {2:F4}", transmissionLossJob.SoundSource.AcousticProperties.HighFrequency, transmissionLossJob.SoundSource.AcousticProperties.SourceDepth, dz);
                //fprintf(fid,'%4.7f %4.8f %i\n', [RMAX DR NDR]);
                sw.WriteLine("{0:F7} {1:F8} {2:D}", transmissionLossJob.SoundSource.Radius, dr, 1);
                //fprintf(fid,'%4.7f %4.7f %i %4.7f\n', [ZMAX DZ NDZ ZMPLT]);
                sw.WriteLine("{0:F7} {1:F7} {2:D} {3:F7}", maxCalculationDepthMeters, dz, 1, zmplt);
                //fprintf(fid,'%4.2f %i %i %i \n', [CO NPADE NS RS]);
                sw.WriteLine("{0:F2} {1:D} {2:D} {3:D}", c0, 6, 0, transmissionLossJob.SoundSource.Radius);
                sw.WriteLine();

                if (bottomProfile.Profile.Length != rangeCellCount) throw new IndexOutOfRangeException("Ram: Generating a radial configuration requires that the number of range cells be equal to the number of points in the bottom profile");
                for (var i = 0; i < bottomProfile.Profile.Length; i++)
                {
                    var range = rangeStep * i;
                    var depth = bottomProfile.Profile[i];
                    sw.WriteLine("{0:F4} {1:F4}", range, depth);
                }
                sw.WriteLine("-1 -1");

                // If SSP is shallower than the bathymetry then extrapolate an SSP entry for the deepest part of the water
                //if (SSP.DepthVector[SSP.DepthVector.Length - 1] < RealBottomDepth_Meters)
                //    SoundSpeedProfile = ExtrapolateSSP(SoundSpeedProfile, RealBottomDepth_Meters);
                foreach (var depthValuePair in ssp.Data)
                    sw.WriteLine("{0:F4} {1:F4} /", depthValuePair.Depth, depthValuePair.Value);
                sw.WriteLine("-1 -1");

                sw.WriteLine("{0:F4} {1:F4}", 0, sediment.CompressionWaveSpeed);
                sw.WriteLine("-1 -1");
                sw.WriteLine("{0:F4} {1:F4}", 0, sediment.Density);
                sw.WriteLine("-1 -1");
                sw.WriteLine("{0:F4} {1:F4}", 0, sediment.CompressionWaveCoefficient); // Should be compression attenuation
                sw.WriteLine("-1 -1");

                sw.WriteLine("1000000000000.0000");
                foreach (var depthValuePair in ssp.Data)
                    sw.WriteLine("{0:F4} {1:F4} /", depthValuePair.Depth, depthValuePair.Value);
                sw.WriteLine("-1 -1");

                sw.WriteLine("{0:F4} {1:F4}", 0, sediment.CompressionWaveSpeed);
                sw.WriteLine("-1 -1");
                sw.WriteLine("{0:F4} {1:F4}", 0, sediment.Density);
                sw.WriteLine("-1 -1");
                sw.WriteLine("{0:F4} {1:F4}", 0, sediment.CompressionWaveCoefficient); // Should be compression attenuation
                sw.WriteLine("-1 -1");
                //Console.WriteLine(sw.ToString());
                return sw.ToString();
            }
        }
    }
}
