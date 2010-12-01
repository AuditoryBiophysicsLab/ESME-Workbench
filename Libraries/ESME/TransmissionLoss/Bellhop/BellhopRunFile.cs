using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Xml;
using System.Xml.Schema;
using System.Xml.Serialization;
using ESME.Environment;
using ESME.Model;

namespace ESME.TransmissionLoss.Bellhop
{
    public class BellhopRunFile : TransmissionLossRunFile
    {
        #region Load and Save

        public new static BellhopRunFile Load(string filename)
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
            var fileWriter = new StreamWriter(RandomFileName(path, ".bellhop"), false);
            fileWriter.Write(Serialize());
            fileWriter.Close();
        }

        #endregion

        #region Serialize/Deserialize

        public string Serialize()
        {
            var ms = new MemoryStream();
            var serializer = new XmlSerializer(GetType(), ReferencedTypes);
            var settings = new XmlWriterSettings {Encoding = Encoding.UTF8, Indent = true,};
            var writer = XmlWriter.Create(ms, settings);

            serializer.Serialize(writer, this);
            return Encoding.UTF8.GetString(ms.ToArray());
        }

        public static BellhopRunFile Deserialize(string xmlString)
        {
            var reader = new StringReader(xmlString);
            var serializer = new XmlSerializer(typeof(BellhopRunFile), ReferencedTypes);
            var runfile = (BellhopRunFile) serializer.Deserialize(reader);
            return runfile;
        }

        #endregion

        [XmlIgnore]
        public string OriginalFilename { get; private set; }

        public static BellhopRunFile Create(TransmissionLossJob transmissionLossJob, EnvironmentInformation environmentInformation, TransmissionLossSettings transmissionLossSettings)
        {
            var rangeCellCount = (int) Math.Round((transmissionLossJob.Radius / transmissionLossSettings.RangeCellSize)) + 1;

            var bellhopRunFile = new BellhopRunFile {TransmissionLossJob = transmissionLossJob,};

            var bottomProfiles = new BottomProfile[transmissionLossJob.AnalysisPoint.RadialCount];
            var soundSpeedProfiles = new SoundSpeedProfile[transmissionLossJob.AnalysisPoint.RadialCount];
            var bearings = new float[transmissionLossJob.AnalysisPoint.RadialCount];
            var maxCalculationDepthMeters = float.MinValue;
            var bearingStep = 360.0f / transmissionLossJob.AnalysisPoint.RadialCount;
            for (var i = 0; i < transmissionLossJob.AnalysisPoint.RadialCount; i++)
            {
                bearings[i] = bearingStep * i + transmissionLossJob.AnalysisPoint.RadialBearing;
                var curTransect = new Transect(null, transmissionLossJob.AnalysisPoint.EarthCoordinate, bearings[i], transmissionLossJob.Radius);
                bottomProfiles[i] = new BottomProfile(rangeCellCount, curTransect, environmentInformation.Bathymetry);
                maxCalculationDepthMeters = Math.Max((float) bottomProfiles[i].MaxDepth, maxCalculationDepthMeters);
                soundSpeedProfiles[i] = environmentInformation.SoundSpeedField[curTransect.MidPoint];
            }

            var depthCellCount = (int) Math.Round((maxCalculationDepthMeters / transmissionLossSettings.DepthCellSize)) + 1;
            for (var i = 0; i < transmissionLossJob.AnalysisPoint.RadialCount; i++)
            {
                var bellhopConfig = Bellhop.GetRadialConfiguration(transmissionLossJob, soundSpeedProfiles[i], environmentInformation.Sediment, maxCalculationDepthMeters, rangeCellCount, depthCellCount, false, false, false, 1500);
                bellhopRunFile.TransmissionLossRunFileRadials.Add(new BellhopRunFileRadial {BearingFromSourceDegrees = bearings[i], Configuration = bellhopConfig, BottomProfile = bottomProfiles[i].ToBellhopString(),});
            }
            bellhopRunFile.IDField = transmissionLossJob.IDField;
            return bellhopRunFile;
        }
    }
}