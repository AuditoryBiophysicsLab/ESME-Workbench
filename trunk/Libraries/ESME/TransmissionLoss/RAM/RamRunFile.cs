using ESME.Data;
using ESME.Model;
using HRC.Utility;

namespace ESME.TransmissionLoss.RAM
{
    public class RamRunFile : TransmissionLossRunFile
    {
        public RamRunFile() {  }

        public override void Save(string fileName = null)
        {
            if (fileName == null) fileName = Filename;
            if (fileName == null) throw new FileNameFormatException("Null file name passed to RamRunFile.Save()");
            var serializer = new XmlSerializer<RamRunFile> { Data = this };
            serializer.Save(fileName, ReferencedTypes);
            Filename = fileName;
        }

        public RAMSettings RAMSettings { get; set; }

#if false
        public static RamRunFile Create(TransmissionLossJob transmissionLossJob, EnvironmentInformation environmentInformation, AppSettings appSettings)
        {
            var rangeCellCount = (int)Math.Round((transmissionLossJob.SoundSource.Radius / appSettings.BellhopSettings.RangeCellSize)) + 1;

            var ramRunFile = new RamRunFile
                             {
                                 TransmissionLossJob = transmissionLossJob,
                             };

            var radialCount = transmissionLossJob.SoundSource.RadialBearings.Count;
            var bottomProfiles = new BottomProfile[radialCount];
            var soundSpeedProfiles = new SoundSpeedProfile[radialCount];
            var maxCalculationDepthMeters = float.MinValue;
            for (var bearingIndex = 0; bearingIndex < radialCount; bearingIndex++)
            {
                var radialBearing = transmissionLossJob.SoundSource.RadialBearings[bearingIndex];
                var curTransect = new Transect(null, transmissionLossJob.SoundSource, radialBearing, transmissionLossJob.SoundSource.Radius);
                bottomProfiles[bearingIndex] = new BottomProfile(rangeCellCount, curTransect, environmentInformation.Bathymetry);
                maxCalculationDepthMeters = Math.Max((float) bottomProfiles[bearingIndex].MaxDepth, maxCalculationDepthMeters);
                soundSpeedProfiles[bearingIndex] = environmentInformation.SoundSpeedField.EnvironmentData[curTransect.MidPoint];
            }

            var depthCellCount = (int)Math.Round((maxCalculationDepthMeters / appSettings.BellhopSettings.DepthCellSize)) + 1;
            for (var bearingIndex = 0; bearingIndex < radialCount; bearingIndex++)
            {
                var radialBearing = transmissionLossJob.SoundSource.RadialBearings[bearingIndex];
                var sedimentType = environmentInformation.Sediment.Samples[transmissionLossJob.SoundSource];
                var ramConfig = Ram.GetRadialConfiguration(transmissionLossJob, soundSpeedProfiles[bearingIndex], bottomProfiles[bearingIndex], sedimentType, maxCalculationDepthMeters, rangeCellCount, depthCellCount);
                ramRunFile.TransmissionLossRunFileRadials.Add(new RamRunFileRadial
                                                              {
                                                                  BearingFromSourceDegrees = radialBearing,
                                                                  Configuration = ramConfig,
                                                              });
            }
            return ramRunFile;
        }
#endif
    }
}