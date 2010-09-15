using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ESME.Model;
using System.IO;
using ESME.Environment;

namespace ESME.TransmissionLoss.Bellhop
{
    class Bellhop
    {
#if false
        public static BellhopRunFile CreateRunFile(
           TransmissionLossField TransmissionLossField,
           EnvironmentInformation EnvironmentInformation)
        {
            int RangeCellCount = (int)Math.Round((TransmissionLossField.AnalysisPoint.FieldRadius_meters / TransmissionLossField.AnalysisPoint.RangeCellSize_meters)) + 1;
            BellhopRunFile BellhopRunFile = new BellhopRunFile
            {
                TransmissionLossJob = TransmissionLossField,
            };

            BottomProfile[] BottomProfiles = new BottomProfile[TransmissionLossField.RadialCount];
            SoundSpeedProfile[] SoundSpeedProfiles = new SoundSpeedProfile[TransmissionLossField.RadialCount];
            float MaxCalculationDepth_meters = float.MinValue;
            for (int i = 0; i < TransmissionLossField.RadialCount; i++)
            {
                Transect CurTransect = new Transect(null, TransmissionLossField.AnalysisPoint.Location, TransmissionLossField.TransmissionLossRadials[i].BearingFromSource_degrees, TransmissionLossField.AnalysisPoint.FieldRadius_meters);
                BottomProfiles[i] = new BottomProfile(RangeCellCount, CurTransect, EnvironmentInformation.Bathymetry);
                MaxCalculationDepth_meters = Math.Max((float)BottomProfiles[i].MaxDepth_Meters, MaxCalculationDepth_meters);
                SoundSpeedProfiles[i] = EnvironmentInformation.SoundSpeedField[CurTransect.MidPoint];
                //SoundSpeedProfiles[i] = 
            }
            int DepthCellCount = (int)Math.Round((MaxCalculationDepth_meters / TransmissionLossField.AnalysisPoint.DepthCellSize_meters)) + 1;
            for (int i = 0; i < TransmissionLossField.RadialCount; i++)
            {
                string BellhopConfig;
                BellhopConfig = Bellhop.GetRadialConfiguration(
                    TransmissionLossField, SoundSpeedProfiles[i],
                    EnvironmentInformation.Sediment, MaxCalculationDepth_meters,
                    RangeCellCount, DepthCellCount, false, false, false, 1500);
                BellhopRunFile.BellhopRadials.Add(new BellhopRadial
                {
                    BearingFromSource_degrees = TransmissionLossField.TransmissionLossRadials[i].BearingFromSource_degrees,
                    Configuration = BellhopConfig,
                    BottomProfile = BottomProfiles[i].ToBellhopString(),
                });
            }
            return BellhopRunFile;
        } 
#endif
        //public static string GenerateBellhopRunInfo(SoundSource soundSource, bool SurfaceReflection, 
        //    bool UseVerticalBeamforming, bool GenerateArrivalsFile, double DepthCellSize_meters, 
        //    SedimentProperties Sediment, BottomProfile bottomProfile, SoundSpeedProfileProperties SSP, int NumRangeCells, int NumDepthCells, int NumBeams)
        public static string GetRadialConfiguration(
            TransmissionLossJob TransmissionLossJob,
            SoundSpeedProfile SSP, SedimentType Sediment, float MaxCalculationDepth_meters,
            int RangeCellCount, int DepthCellCount,
            bool UseSurfaceReflection, bool UseVerticalBeamforming, bool GenerateArrivalsFile, int BeamCount)
        {
            double Depth1, Depth2, Speed1, Speed2, Angle1, Angle2;

            using (StringWriter sw = new StringWriter())
            {
                sw.WriteLine("'TL'");
                sw.WriteLine("{0:F},", TransmissionLossJob.AcousticProperties.HighFrequency_Hz);
                sw.WriteLine("1,"); // was NMEDIA in gui_genbellhopenv.m
                if (UseSurfaceReflection)
                    sw.WriteLine("'CFMT',");
                else
                    sw.WriteLine("'CVMT',");

                if (DepthCellCount < 5)
                    return "Error: Maximum depth of transect (" + MaxCalculationDepth_meters + " meters) less than minimum required for transmission loss calculations.\nPlease choose a different location for this transect.";

                sw.WriteLine("{0}, 0.0, {1:F}, !NMESH SIGMA Z(NSSP)", RangeCellCount, MaxCalculationDepth_meters);

                // If SSP is shallower than the bathymetry then extrapolate an SSP entry for the deepest part of the water
                //if (SSP.DepthVector[SSP.DepthVector.Length - 1] < RealBottomDepth_Meters)
                //    SoundSpeedProfile = ExtrapolateSSP(SoundSpeedProfile, RealBottomDepth_Meters);

                Depth1 = Depth2 = SSP.Depths_meters[0];
                Speed1 = Speed2 = SSP.SoundSpeeds_metersSecond[0];
                for (int i = 0; i < SSP.Depths_meters.Length; i++)
                {
                    if (SSP.Depths_meters[i] < MaxCalculationDepth_meters)
                    {
                        Depth1 = Depth2;
                        Depth2 = SSP.Depths_meters[i];
                        Speed1 = Speed2;
                        Speed2 = SSP.SoundSpeeds_metersSecond[i];
                        sw.WriteLine("{0:F} {1:F} /", SSP.Depths_meters[i], SSP.SoundSpeeds_metersSecond[i]);
                    }
                    else
                    {
                        break;
                    }
                }
                sw.WriteLine("{0:F} {1:F} /", MaxCalculationDepth_meters, ExtrapolateSSP(Depth1, Speed1, Depth2, Speed2, MaxCalculationDepth_meters));
                sw.WriteLine("'A*', 0.0");  // A = Acoustic halfspace, * = read bathymetry file 'BTYFIL', 0.0 = bottom roughness (currently ignored)
                sw.WriteLine("{0:F} {1:F} {2:F} {3:F} {4:F} {5:F} /", MaxCalculationDepth_meters,
                    Sediment.CompressionWaveSpeed_metersSec, Sediment.ShearWaveSpeed_metersSec, Sediment.Density_gramsCC,
                    Sediment.CompressionWaveCoefficient, Sediment.ShearWaveCoefficient);
                // Source and Receiver Depths and Ranges
                sw.WriteLine("1    !NSD"); // Number of Source Depths
                sw.WriteLine("  {0:F} / ! source_depth", Math.Max(1, TransmissionLossJob.AcousticProperties.SourceDepth_meters)); // source depth
                sw.WriteLine("{0}   ! NRD", DepthCellCount); // Number of Receiver Depths
                sw.WriteLine("  0.0 {0:F} /  ! surface_depth (0.0) to max_depth (in meters)", MaxCalculationDepth_meters);
                sw.WriteLine("{0}  ! NRR", RangeCellCount); // Number of receiver ranges
                sw.WriteLine("  0.0 {0:F} /  ! start_range (0.0) to max_range (in km)", TransmissionLossJob.Radius / 1000.0);
                if (GenerateArrivalsFile)
                    sw.WriteLine("'aB'");
                else
                {
                    if (UseVerticalBeamforming)
                        sw.WriteLine("'IG*'"); // Requires SBPFIL to be present (Source Beam Pattern file)
                    else
                        sw.WriteLine("'I'"); // Incoherent TL calculation
                }
                sw.WriteLine("{0}", BeamCount);    // Number of beams
                Angle1 = TransmissionLossJob.AcousticProperties.DepressionElevationAngle_degrees - (TransmissionLossJob.AcousticProperties.VerticalBeamWidth_degrees / 2);
                Angle2 = TransmissionLossJob.AcousticProperties.DepressionElevationAngle_degrees + (TransmissionLossJob.AcousticProperties.VerticalBeamWidth_degrees / 2);
                sw.WriteLine(Angle1.ToString("###0.00") + " " + Angle2.ToString("###0.00") + " /"); // Beam fan half-angles (negative angles are toward the surface
                //sw.WriteLine("-60.00 60.00 /"); // Beam fan half-angles (negative angles are toward the surface
                //sw.WriteLine("{0:F} {1:F} {2:F} ! step zbox(meters) rbox(km)", experiment.TransmissionLossSettings.DepthCellSize, RealBottomDepth_Meters + 100, (bottomProfile.Length_Meters / 1000.0) * 1.01);
                sw.WriteLine("0 {1:F} {2:F} ! step zbox(meters) rbox(km)", (float)(MaxCalculationDepth_meters / (float)DepthCellCount), MaxCalculationDepth_meters + 100, (TransmissionLossJob.Radius / 1000.0) * 1.01);
                return sw.ToString();
            }
        }

        private static double ExtrapolateSSP(double Depth1, double Speed1, double Depth2, double Speed2, double DesiredDepth)
        {
            double DepthDelta1, DepthDelta2, SpeedDelta, DepthRatio, Slope, FinalSpeed;

            DepthDelta1 = Depth2 - Depth1;
            SpeedDelta = Speed2 - Speed1;
            Slope = SpeedDelta / DepthDelta1;
            DepthDelta2 = DesiredDepth - Depth2;
            DepthRatio = DepthDelta2 / DepthDelta1;
            FinalSpeed = (Slope * DepthDelta2) + Speed2;
            return FinalSpeed;
        }
    }
}
