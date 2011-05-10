using System;
using System.IO;
using System.Text;
using System.Xml;
using System.Xml.Serialization;
using ESME.Data;
using ESME.Environment;
using ESME.Model;
using ESME.TransmissionLoss.Bellhop;

namespace ESME.TransmissionLoss.RAM
{
    public class RamRunFile : TransmissionLossRunFile
    {
        #region Load and Save

        public new static RamRunFile Load(string filename)
        {
            var fileReader = new StreamReader(filename);
            var file = fileReader.ReadToEnd();
            fileReader.Close();
            var runFile = Deserialize(file);
            runFile.OriginalFilename = file;
            return runFile;
        }

        public override void Save(string path)
        {
            var fileWriter = new StreamWriter(Path.Combine(path, Filename + ".ramgeo"), false);
            fileWriter.Write(Serialize());
            fileWriter.Close();
        }

        #endregion

        #region Serialize/Deserialize

        public string Serialize()
        {
            var ms = new MemoryStream();
            var serializer = new XmlSerializer(GetType(), ReferencedTypes);
            var settings = new XmlWriterSettings
                           {
                               Encoding = Encoding.UTF8,
                               Indent = true,
                           };
            var writer = XmlWriter.Create(ms, settings);

            serializer.Serialize(writer, this);
            return Encoding.UTF8.GetString(ms.ToArray());
        }

        public static RamRunFile Deserialize(string xmlString)
        {
            var reader = new StringReader(xmlString);
            var serializer = new XmlSerializer(typeof (RamRunFile), ReferencedTypes);
            var runfile = (RamRunFile) serializer.Deserialize(reader);
            return runfile;
        }

        #endregion

        [XmlIgnore]
        public string OriginalFilename { get; private set; }

        /// <summary>
        /// </summary>
        /// <param name = "transmissionLossJob"></param>
        /// <param name = "environmentInformation"></param>
        /// <param name="appSettings"></param>
        /// <returns></returns>
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
                soundSpeedProfiles[bearingIndex] = environmentInformation.SoundSpeedField[curTransect.MidPoint];
            }

            var depthCellCount = (int)Math.Round((maxCalculationDepthMeters / appSettings.BellhopSettings.DepthCellSize)) + 1;
            for (var bearingIndex = 0; bearingIndex < radialCount; bearingIndex++)
            {
                var radialBearing = transmissionLossJob.SoundSource.RadialBearings[bearingIndex];
                var sedimentType = environmentInformation.Sediment[transmissionLossJob.SoundSource];
                var ramConfig = Ram.GetRadialConfiguration(transmissionLossJob, soundSpeedProfiles[bearingIndex], bottomProfiles[bearingIndex], sedimentType, maxCalculationDepthMeters, rangeCellCount, depthCellCount);
                ramRunFile.TransmissionLossRunFileRadials.Add(new RamRunFileRadial
                                                              {
                                                                  BearingFromSourceDegrees = radialBearing,
                                                                  Configuration = ramConfig,
                                                              });
            }
            ramRunFile.IDField = transmissionLossJob.IDField;
            return ramRunFile;
        }
    }
}