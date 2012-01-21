using System;
using ESME.Data;
using ESME.Environment;
using ESME.Model;
using HRC.Utility;

namespace ESME.TransmissionLoss.Bellhop
{
    public class BellhopRunFile : TransmissionLossRunFile
    {
        public BellhopRunFile() {  }
        
        public override void Save(string fileName = null)
        {
            if (fileName == null) fileName = Filename;
            if (fileName == null) throw new FileNameFormatException("Null file name passed to BellhopRunFile.Save()");
            var serializer = new XmlSerializer<BellhopRunFile> { Data = this };
            serializer.Save(fileName, ReferencedTypes);
            Filename = fileName;
        }

        public BellhopSettings BellhopSettings { get; set; }

#if true
        public static BellhopRunFile Create(TransmissionLossJob transmissionLossJob, EnvironmentInformation environmentInformation, AppSettings appSettings)
        {
            var rangeCellCount = (int)Math.Round((transmissionLossJob.SoundSource.Radius / appSettings.BellhopSettings.RangeCellSize)) + 1;

            var bellhopRunFile = new BellhopRunFile {TransmissionLossJob = transmissionLossJob,};

            var radialCount = transmissionLossJob.SoundSource.RadialBearings.Count;
            var bottomProfiles = new BottomProfile[radialCount];
            var soundSpeedProfiles = new SoundSpeedProfile[radialCount];
            var maxCalculationDepthMeters = float.MinValue;
            for (var bearingIndex = 0; bearingIndex < radialCount; bearingIndex++)
            {
                var radialBearing = transmissionLossJob.SoundSource.RadialBearings[bearingIndex];
                var curTransect = new Transect(null, transmissionLossJob.SoundSource, radialBearing, transmissionLossJob.SoundSource.Radius);
                bottomProfiles[bearingIndex] = new BottomProfile(rangeCellCount, curTransect, environmentInformation.Bathymetry);
                maxCalculationDepthMeters = Math.Max((float)bottomProfiles[bearingIndex].MaxDepth, maxCalculationDepthMeters);
                soundSpeedProfiles[bearingIndex] = environmentInformation.SoundSpeedField.EnvironmentData.GetNearestPoint(curTransect.MidPoint);
            }

            var depthCellCount = (int)Math.Round((maxCalculationDepthMeters / appSettings.BellhopSettings.DepthCellSize)) + 1;
            for (var bearingIndex = 0; bearingIndex < radialCount; bearingIndex++)
            {
                var radialBearing = transmissionLossJob.SoundSource.RadialBearings[bearingIndex];
                var sedimentType = environmentInformation.Sediment.Samples.GetNearestPoint(transmissionLossJob.SoundSource);
                var bellhopConfig = Bellhop.GetRadialConfiguration(transmissionLossJob, soundSpeedProfiles[bearingIndex], sedimentType, maxCalculationDepthMeters, rangeCellCount, depthCellCount, false, false, false, 1500);
                bellhopRunFile.TransmissionLossRunFileRadials.Add(new BellhopRunFileRadial {BearingFromSourceDegrees = radialBearing, Configuration = bellhopConfig, BottomProfile = bottomProfiles[bearingIndex].ToBellhopString(),});
            }
            return bellhopRunFile;
        }
#endif
    }
}