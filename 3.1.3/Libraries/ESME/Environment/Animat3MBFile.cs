using System;
using System.Diagnostics;
using System.IO;
using System.Text;
using ESME.Scenarios;
using HRC.Navigation;
using FileFormatException = ESME.Model.FileFormatException;

namespace ESME.Environment
{
    [Serializable]
    public class Animat3MBFile : Animat
    {
        #region public properties {get; internal set;}
        public string MbFileIdentifier { get; internal set; }
        public uint MbLibSuperVersion { get; internal set; }
        public uint MbLibSubVersion { get; internal set; }
        public uint MbSpeciesSuperVersion { get; internal set; }
        public uint MbSpeciesSubVersion { get; internal set; }
        public uint MbOutputSuperVersion { get; internal set; }
        public uint MbOutputSubVersion { get; internal set; }
        public uint NumberOfSpecies { get; internal set; }
        public uint Duration { get; internal set; }
        public uint SavedStateCount { get; internal set; }
        public uint StartClockTime { get; internal set; }
        #region public uint OutputConfiguration { get; internal set; }
        uint _outputConfiguration;
        public uint OutputConfiguration
        {
            get { return _outputConfiguration; }
            internal set
            {
                _outputConfiguration = value;
                for (var i = 0; i < _bits.Length; i++)
                {
                    _bits[i] = (_outputConfiguration & (1 << i)) != 0;
                }
                if (!_bits[31])
                    throw new FileFormatException(
                        "The animat states in this 3MB file are ordered by animat not by time. Mysterious, and forbidden.");

            }
        }
        readonly bool[] _bits = new bool[32];
        #endregion
        public int SizeOfSizeT { get; internal set; }
        public uint MiscParams { get; internal set; }
        public uint SeedValue { get; internal set; }
        public bool AcousticSourceLimitsOutput { get; internal set; }
        public uint NumAcousticSourceTypes { get; internal set; }
        public uint NumAcousticSources { get; internal set; }
        public bool IntervalLimitedFileOutputEnabled { get; internal set; }
        public uint IntervalLimitedFileOutputStart { get; internal set; }
        public uint IntervalLimitedFileOutputValue { get; internal set; }
        public uint ActualPopulation { get; internal set; }
        public float InnerBoxDensity { get; internal set; }
        public float OuterBoxDensity { get; internal set; }
        public uint TotalAnimatsOutsideTrack { get; internal set; }
        public float ActualAnimatsOutsideTrack { get; internal set; }
        public uint TotalAnimatsInsideTrack { get; internal set; }
        public float ActualAnimatsInsideTrack { get; internal set; }
        public ulong ScenarioParams { get; internal set; }
        public ulong BathymetryMap { get; internal set; }
        public ulong SalinityMap { get; internal set; }
        public ulong TemperatureMap { get; internal set; }
        public ulong PostAnalysis { get; internal set; }
        public ulong SpeciesDescription { get; internal set; }
        public ulong AnimatToSpeciesAssociation { get; internal set; }
        public ulong AnimatsState { get; internal set; }
        public ulong AcousticExposure { get; internal set; }
        public dynamic ScenarioParamsSize { get; internal set; }
        public dynamic BathyMapSize { get; internal set; }
        public dynamic SalintyMapSize { get; internal set; }
        public dynamic TemperatureMapSize { get; internal set; }
        public dynamic PostAnalysisSize { get; internal set; }
        public dynamic SpeciesDescriptionSize { get; internal set; }
        public dynamic AnimatToSpeciesAssociationSize { get; internal set; }
        public dynamic AnimatStateSize { get; internal set; }
        public dynamic AcousticExposureSize { get; internal set; }
        public ulong TotalBinaryFileOutputSize { get; internal set; }
        public ulong TotalAnimatBinaryFileOutputSize { get; internal set; }
        public ulong TotalAcousticExposureBinaryOutputSize { get; internal set; }
        #endregion

        /// <summary>
        /// the header load procedure terminates once we have determined the species name, output configuration, and total number of animats -- it's all we need to display points on a map.
        /// </summary>
        /// <param name="species"> </param>
        /// <param name="fileName">the full path to the .3mb file</param>
        /// <returns></returns>
        public static new Animat3MBFile Load(ScenarioSpecies species, string fileName)
        {
            if (Path.GetExtension(fileName) != ".3mb") throw new FileFormatException("only 3MB files are supported.");
            var result = new Animat3MBFile();
            using (var reader = new BinaryReader(new FileStream(fileName, FileMode.Open, FileAccess.Read)))
            {
                #region MbFileIdentifier
                result.MbFileIdentifier = Encoding.Default.GetString(reader.ReadBytes(16)).TrimEnd('\0');
                if (result.MbFileIdentifier != "3MBBinaryOutput") throw new FileFormatException("Invalid 3MB file.");
                #endregion
                result.MbLibSuperVersion = reader.ReadUInt32();
                result.MbLibSubVersion = reader.ReadUInt32();
                result.MbSpeciesSuperVersion = reader.ReadUInt32();
                result.MbSpeciesSubVersion = reader.ReadUInt32();
                result.MbOutputSuperVersion = reader.ReadUInt32();
                result.MbOutputSubVersion = reader.ReadUInt32();
                #region NumberOfSpecies
                result.NumberOfSpecies = reader.ReadUInt32();
                if (result.NumberOfSpecies > 1) throw new FileFormatException(string.Format("The 3MB file {0} associated with this species contains more than one species!", Path.GetFileName(fileName)));
                #endregion
                result.TotalAnimats = reader.ReadInt32();
                result.Duration = reader.ReadUInt32();
                result.SavedStateCount = reader.ReadUInt32();
                result.StartClockTime = reader.ReadUInt32();
                result.OutputConfiguration = reader.ReadUInt32();
                result.SizeOfSizeT = reader.ReadInt32();
                result.MiscParams = reader.ReadUInt32();
                result.SeedValue = reader.ReadUInt32();
                result.AcousticSourceLimitsOutput = reader.ReadUInt32() != 0; //returns bool!
                result.NumAcousticSourceTypes = reader.ReadUInt32();
                result.NumAcousticSources = reader.ReadUInt32();
                result.IntervalLimitedFileOutputEnabled = reader.ReadUInt32() != 0;
                result.IntervalLimitedFileOutputStart = reader.ReadUInt32();
                result.IntervalLimitedFileOutputValue = reader.ReadUInt32();
                reader.BaseStream.Seek(28, SeekOrigin.Current);
                #region result.LatinName
                var buf = reader.ReadBytes(32);
                var end = false;
                for (var i = 0; i < buf.Length; i++)
                {
                    if (buf[i] == 0) end = true;
                    if (end) buf[i] = 0;
                }
                var latinName = Encoding.Default.GetString(buf).TrimEnd('\0');
                if (latinName != species.LatinName) throw new FileFormatException(string.Format("{0}: expected species name {1}, got {2}", fileName, species.LatinName, latinName));
                #endregion
                result.ActualPopulation = reader.ReadUInt32();
                result.InnerBoxDensity = reader.ReadSingle();
                result.OuterBoxDensity = reader.ReadSingle();
                result.TotalAnimatsOutsideTrack = reader.ReadUInt32();
                result.ActualAnimatsOutsideTrack = reader.ReadSingle();
                result.TotalAnimatsInsideTrack = reader.ReadUInt32();
                result.ActualAnimatsInsideTrack = reader.ReadSingle();
                reader.BaseStream.Seek(4, SeekOrigin.Current);
                reader.BaseStream.Seek(128, SeekOrigin.Current);
                result.ScenarioParams = reader.ReadUInt64();
                result.BathymetryMap = reader.ReadUInt64();
                result.SalinityMap = reader.ReadUInt64();
                result.TemperatureMap = reader.ReadUInt64();
                result.PostAnalysis = reader.ReadUInt64();
                result.SpeciesDescription = reader.ReadUInt64();
                result.AnimatToSpeciesAssociation = reader.ReadUInt64();
                result.AnimatsState = reader.ReadUInt64();
                result.AcousticExposure = reader.ReadUInt64();
                reader.BaseStream.Seek(8, SeekOrigin.Current);
                if (result.SizeOfSizeT != 4 && result.SizeOfSizeT != 8) throw new FileFormatException("Unknown architecture.");
                if (result.SizeOfSizeT == 4)
                {
                    result.ScenarioParamsSize = reader.ReadUInt32();
                    result.BathyMapSize = reader.ReadUInt32();
                    result.SalintyMapSize = reader.ReadUInt32();
                    result.TemperatureMapSize = reader.ReadUInt32();
                    result.PostAnalysisSize = reader.ReadUInt32();
                    result.SpeciesDescriptionSize = reader.ReadUInt32();
                    result.AnimatToSpeciesAssociationSize = reader.ReadUInt32();
                    result.AnimatStateSize = reader.ReadUInt32();
                    result.AcousticExposureSize = reader.ReadUInt32();
                    reader.BaseStream.Seek(12, SeekOrigin.Current);
                }
                if (result.SizeOfSizeT == 8)
                {
                    result.ScenarioParamsSize = reader.ReadUInt64();
                    result.BathyMapSize = reader.ReadUInt64();
                    result.SalintyMapSize = reader.ReadUInt64();
                    result.TemperatureMapSize = reader.ReadUInt64();
                    result.PostAnalysisSize = reader.ReadUInt64();
                    result.SpeciesDescriptionSize = reader.ReadUInt64();
                    result.AnimatToSpeciesAssociationSize = reader.ReadUInt64();
                    result.AnimatStateSize = reader.ReadUInt64();
                    result.AcousticExposureSize = reader.ReadUInt64();
                    reader.BaseStream.Seek(8, SeekOrigin.Current);
                }
                result.TotalBinaryFileOutputSize = reader.ReadUInt64();
                result.TotalAnimatBinaryFileOutputSize = reader.ReadUInt64();
                result.TotalAcousticExposureBinaryOutputSize = reader.ReadUInt64();
                reader.BaseStream.Seek(8, SeekOrigin.Current);

                result.ReadStartPositions(reader); //
            }
            return result;
        }
        /// <summary>
        /// Populates AnimatStartPoints with the starting locations of every animat in the file.
        /// </summary>
        /// <param name="reader"></param>
        private void ReadStartPositions(BinaryReader reader)
        {
            //skip to where animat state data is
            reader.BaseStream.Seek((long)AnimatsState, SeekOrigin.Begin);
            Locations = new AnimatEnvironmentData<Geo<float>>();
            //uint datasize = GetSize(OutputConfiguration);   
            for (var i = 0; i < TotalAnimats; i++) Locations.Add(ReadAnimat(reader));
        }
        private Geo<float> ReadAnimat(BinaryReader reader)
        {
            var lat = float.MinValue;
            var lon = float.MinValue;
            if (_bits[5])
            {
                var id = reader.ReadSingle();
                Debug.WriteLine("id: {0}", id);
            }
            if (_bits[6])
            {
                var time = reader.ReadSingle();
                Debug.WriteLine("time: {0}", time);
            }
            if (_bits[7])
            {
                lat = reader.ReadSingle();
                Debug.WriteLine("lat: {0}", lat);
                lon = reader.ReadSingle();
                Debug.WriteLine("lon: {0}", lon);
            }
            if (_bits[8])
            {
                Debug.WriteLine("depth: {0}", reader.ReadSingle());
            }
            if (_bits[9])
            {
                Debug.WriteLine("bearing: {0}", reader.ReadSingle());
            }
            if (_bits[10])
            {
                Debug.WriteLine("dive rate: {0}", reader.ReadSingle());
            }
            if (_bits[11])
            {
                Debug.WriteLine("travel rate: {0}", reader.ReadSingle());
            }
            if (_bits[12])
            {
                Debug.WriteLine("acoustic exposure, cuum: {0}", reader.ReadSingle());
            }
            if (_bits[13])
            {
                Debug.WriteLine("acoustic exposure, inst.: {0}", reader.ReadSingle());
            }
            if (_bits[14])
            {
                Debug.WriteLine("acoustic source rel angle: {0}", reader.ReadSingle());
            }
            if (_bits[15])
            {
                Debug.WriteLine("ae timing averting: {0}", reader.ReadUInt32());
            }
            if (_bits[16])
            {
                Debug.WriteLine("bathy depth: {0}", reader.ReadSingle());
            }
            if (_bits[17])
            {
                Debug.WriteLine("salinity: {0}", reader.ReadSingle());
            }
            if (_bits[18])
            {
                Debug.WriteLine("temperature: {0}", reader.ReadSingle());
            }
            if (_bits[19])
            {
                Debug.WriteLine("target depth: {0}", reader.ReadSingle());
            }
            if (_bits[20])
            {
                Debug.WriteLine("packed data: {0:x}", reader.ReadUInt32());
            }
            if (_bits[21])
            {
                Debug.WriteLine("calculated depth: {0}", reader.ReadSingle());
            }
            return new Geo<float>(lat, lon, 0);
        }
#if false
    /// <summary>
    /// takes OutputConfiguration and determines the size of each animat's packed data.
    /// </summary>
    /// <param name="bitmap"></param>
    /// <returns></returns>
        private static uint GetSize(uint bitmap)
        {
            uint size = 0;

            for (int i = 5; i < 22; i++)
            {
                if ((bitmap & (1 << i)) != 0)
                {
                    size += 4;
                }
            }

            if ((bitmap & (1 << 7)) != 0)
            {
                size += 4;
            }

            return size;
        } 
#endif
    }
}