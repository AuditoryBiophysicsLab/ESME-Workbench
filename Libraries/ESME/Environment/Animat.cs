using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Text;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using ESME.Scenarios;
using HRC.Navigation;
using FileFormatException = ESME.Model.FileFormatException;

namespace ESME.Environment
{
    public class Animat : EnvironmentDataSetBase
    {
        public Animat() { Locations = new EnvironmentData<Geo<float>>(); }
        public ScenarioSpecies ScenarioSpecies { get; set; }
        public EnvironmentData<Geo<float>> Locations { get; protected set; }
        public int TotalAnimats { get; internal set; }
        static readonly Random Random = new Random();
        public static Animat Load(ScenarioSpecies species, string fileName)
        {
            var extension = Path.GetExtension(fileName);
            if (extension != null)
                switch (extension.ToLower())
                {
                    case ".3mb":
                        return MMMB.Load(species, fileName);
                    case ".ddb":
                        return DDB.Load(species, fileName);
                    case ".ani":
                        using (var stream = new FileStream(fileName, FileMode.Open, FileAccess.Read, FileShare.Read))
                        using (var reader = new BinaryReader(stream))
                        {
                            var header = reader.ReadBytes(4);
                            if (header[0] != 'a' && header[1] != 'n' && header[2] != 'i' && header[3] != 'm') throw new FileFormatException(string.Format("{0} is not a valid .ani file", fileName));
                            //var latinName = reader.ReadString();
                            //if (latinName != species.LatinName) throw new FileFormatException(string.Format("{0}: expected species name {1}, got {2}", fileName, species.LatinName, latinName));
                            var locationCount = reader.ReadInt32();
                            var result = new Animat();
                            for (var i = 0; i < locationCount; i++) result.Locations.Add(new Geo<float>(reader.ReadDouble(), reader.ReadDouble(), reader.ReadSingle()));
                            return result;
                        }

                    default:
                        throw new FileFormatException(string.Format("Unable to load animat locations.  Unrecognized file type: \"{0}\"", extension));
                }
            throw new Exception("specified file does not exist!");
        }

        public static Animat Seed(ScenarioSpecies species, double density, GeoRect geoRect, Bathymetry bathymetry)
        {
            var bounds = new GeoArray(geoRect.NorthWest, geoRect.NorthEast, geoRect.SouthEast, geoRect.SouthWest, geoRect.NorthWest);
            var result = new Animat { ScenarioSpecies = species };
            var radius = Planet.wgs84_earthEquatorialRadiusMeters_D / 1000;
            var area = bounds.Area * radius * radius;
            //Debug.WriteLine("Area: {0}",area);
            var population = (int)Math.Floor(area * density);
            for (var i = 0; i < population; i++)
            {
                var location = bounds.RandomLocationWithinPerimeter();
                var depth = bathymetry.Samples.GetNearestPoint(location).Data;
                if (depth < 0) result.Locations.Add(new Geo<float>(location.Latitude, location.Longitude, (float)(depth * Random.NextDouble())));
            }
            return result;
        }

        public async static Task<Animat> SeedAsync(ScenarioSpecies species, double density, GeoRect geoRect, Bathymetry bathymetry)
        {
            var bounds = new GeoArray(geoRect.NorthWest, geoRect.NorthEast, geoRect.SouthEast, geoRect.SouthWest, geoRect.NorthWest);
            var result = new Animat { ScenarioSpecies = species };
            var radius = Planet.wgs84_earthEquatorialRadiusMeters_D / 1000;
            var area = bounds.Area * radius * radius;
            //Debug.WriteLine("Area: {0}",area);
            var transformManyBlock = new TransformManyBlock<int, Geo<float>>(count =>
            {
                var geos = new List<Geo<float>>();
                for (var i = 0; i < count; i++)
                {
                    var location = bounds.RandomLocationWithinPerimeter();
                    var depth = bathymetry.Samples.GetNearestPointAsync(location).Result.Data;
                    if (depth < 0) geos.Add(new Geo<float>(location.Latitude, location.Longitude, (float)(depth * Random.NextDouble())));
                }
                return geos;
            }, new ExecutionDataflowBlockOptions
            {
                TaskScheduler = TaskScheduler.Default,
                BoundedCapacity = -1,
                MaxDegreeOfParallelism = -1,
            });
            var bufferBlock = new BufferBlock<Geo<float>>();
            transformManyBlock.LinkTo(bufferBlock);
            var population = (int)Math.Round(area * density);
            const int blockSize = 100;
            while (population > 0)
            {
                transformManyBlock.Post(population > blockSize ? blockSize : population);
                population -= blockSize;
            }
            transformManyBlock.Complete();
            await transformManyBlock.Completion;
            IList<Geo<float>> animatGeos;
            if (bufferBlock.TryReceiveAll(out animatGeos))
                result.Locations.AddRange(animatGeos);
            return result;
        }

        public override void Save(string filename)
        {
            using (var stream = new FileStream(filename, FileMode.Create, FileAccess.Write, FileShare.None))
            using (var writer = new BinaryWriter(stream))
            {
                writer.Write(new[] { 'a', 'n', 'i', 'm' });
                //writer.Write(ScenarioSpecies.LatinName);
                writer.Write(Locations.Count);
                foreach (var sample in Locations)
                {
                    writer.Write(sample.Latitude);
                    writer.Write(sample.Longitude);
                    writer.Write(sample.Data);
                }
            }
        }
    }

    [Serializable]
    public class DDB : Animat
    {
        #region public properties {get; internal set;}
        //not all of these are ever set; the header load procedure terminates once we have determined the species name, output configuration, and total number of animats -- it's all we need.

        public string SpeciesFilePath { get; internal set; }
        public int MbLibSuperVersion { get; internal set; }
        public int MbLibSubVersion { get; internal set; }
        public int MbSbSuperVersion { get; internal set; }
        public int MbSbSubVersion { get; internal set; }
        public int MbOutSuperVersion { get; internal set; }
        public int MbOutSubVersion { get; internal set; }
        public int MbNumSpecies { get; internal set; }
        public int MbIterationCount { get; internal set; }
        public int MbSavedStateCount { get; internal set; }
        public int MbStartClockTime { get; internal set; }
        public int MbOutputConfiguration { get; internal set; }
        public int MbSizeData { get; internal set; }
        public int MbSpeciesGroup { get; internal set; }
        public int MbMscParams { get; internal set; }
        public int MbSeedValue { get; internal set; }
        public int MbAcsticSourceLimitsOutput { get; internal set; }
        public int MbNumAcsticSrcTypes { get; internal set; }
        public int MbNumAcsticSrcs { get; internal set; }
        public long MbScenarioParams { get; internal set; }
        public long MbBathymetryMap { get; internal set; }
        public long MbSalinityMap { get; internal set; }
        public long MbTemperatureMap { get; internal set; }
        public long MbPostAnalisys { get; internal set; }
        public long MbSpeciesDescription { get; internal set; }
        public long MbAnimatToSpeciesAssociation { get; internal set; }
        public long MbAnimatsState { get; internal set; }
        public long MbAcousticExposure { get; internal set; }
        public dynamic MbScenarioParamsSize { get; internal set; }
        public dynamic MbBathyMapSize { get; internal set; }
        public dynamic MbSalinityMapSize { get; internal set; }
        public dynamic MbTemperatureMapSize { get; internal set; }
        public dynamic MbPostAnalisysSize { get; internal set; }
        public dynamic MbSpeciesDescriptionSize { get; internal set; }
        public dynamic MbAnimatSpeciesAssSize { get; internal set; }
        public dynamic MbAnimatStateSize { get; internal set; }
        public dynamic MbAcousticExposureSize { get; internal set; }
        public int SimAreaTotal { get; internal set; }
        public float SimAreaPopulation { get; internal set; }
        public int TrackAreaTotal { get; internal set; }
        public float TrackAreaPopulation { get; internal set; }
        public int ActualMammalPopulation { get; internal set; }


        #endregion

        /// <summary>
        /// the header load procedure terminates once we have determined the species name, output configuration, and total number of animats -- it's all we need to display points on a map.
        /// </summary>
        /// <param name="species"> </param>
        /// <param name="fileName">the full path to the .ddb file</param>
        /// <returns></returns>
        public new static DDB Load(ScenarioSpecies species, string fileName)
        {
            if (Path.GetExtension(fileName) != ".ddb") throw new FileFormatException("only ddb files are supported.");
            var result = new DDB();
            using (var reader = new BinaryReader(new FileStream(fileName, FileMode.Open, FileAccess.Read)))
            {
                //nuwc tests for header if 3mb here. (16 byte check)
                result.MbLibSuperVersion = reader.ReadInt32();
                result.MbLibSubVersion = reader.ReadInt32();

                if (result.MbLibSuperVersion > 4)
                {
                    result.MbSbSuperVersion = reader.ReadInt32();
                    result.MbSbSubVersion = reader.ReadInt32();
                    result.MbOutSuperVersion = reader.ReadInt32();
                    result.MbOutSubVersion = reader.ReadInt32();
                }
                else
                {
                    result.MbSbSuperVersion = 0;
                    result.MbSbSubVersion = 0;
                    result.MbOutSuperVersion = 0;
                    result.MbOutSubVersion = 0;
                }

                result.MbNumSpecies = reader.ReadInt32();
                result.TotalAnimats = reader.ReadInt32();
                result.MbIterationCount = reader.ReadInt32();
                result.MbSavedStateCount = reader.ReadInt32();
                result.MbStartClockTime = reader.ReadInt32();
                result.MbOutputConfiguration = reader.ReadInt32();
                result.MbSizeData = reader.ReadInt32();

                if (result.MbLibSuperVersion > 4)
                {
                    result.MbSpeciesGroup = 0;
                    result.MbMscParams = reader.ReadInt32();
                    result.MbSeedValue = reader.ReadInt32();
                    result.MbAcsticSourceLimitsOutput = reader.ReadInt32();
                    result.MbNumAcsticSrcTypes = reader.ReadInt32();
                    result.MbNumAcsticSrcs = reader.ReadInt32();
                }
                else
                {
                    result.MbSpeciesGroup = reader.ReadInt32();
                    result.MbMscParams = 0;
                    result.MbSeedValue = 0;
                    result.MbAcsticSourceLimitsOutput = 0;
                    result.MbNumAcsticSrcTypes = 0;
                    result.MbNumAcsticSrcs = 0;
                }

                // case for special .smb readers with pop and species name
                // logger.trace("Checking for super version 4 and subversion greater than 200");

                if (result.MbLibSuperVersion == 4 && result.MbLibSubVersion > 200)
                {
                    //logger.trace("version > 4.200 ( real value: " + getVersionString() + ")");

                    //  this.population = reader.ReadInt32();
                    result.ActualMammalPopulation = reader.ReadInt32();
                    var latinName = Encoding.ASCII.GetString(reader.ReadBytes(32)).TrimEnd('\0').Trim();
                    if (latinName != species.LatinName) throw new FileFormatException(string.Format("{0}: expected species name {1}, got {2}", fileName, species.LatinName, latinName));
                    //result.LatinName = reader.ReadString().TrimEnd('\0').Trim();

                    //  this.speciesName = new String(specName).trim();

                    // SpeciesEntry e = SpeciesTable.lookupEntryByScientific(speciesName);

                    //   if (e != null)
                    //  {
                    //       this.speciesCode = e.getKey();
                    //    }
                    //  else
                    //{
                    //    logger.warn("species table lookup failed: " + speciesName);
                    //}
                }
                else
                {
                    //          logger.trace("version <= 4.200. actual " + getVersionString() + ". no spec name or pop");

                    //if we can't get the population and name, it's not a valid .ddb file.  Choke and die.
                    throw new System.IO.FileFormatException("");
                }

#if false

    // Internal File Pointers To Various Regions

                result.MbScenarioParams = reader.ReadInt64();
                result.MbBathymetryMap = reader.ReadInt64();
                result.MbSalinityMap = reader.ReadInt64();
                result.MbTemperatureMap = reader.ReadInt64();
                result.MbPostAnalisys = reader.ReadInt64();
                result.MbSpeciesDescription = reader.ReadInt64();
                result.MbAnimatToSpeciesAssociation = reader.ReadInt64();
                result.MbAnimatsState = reader.ReadInt64();
                result.MbAcousticExposure = reader.ReadInt64();
                try
                {
                    reader.ReadBytes(8);
                }
                catch
                {
                    throw new System.IO.FileFormatException("");
                }

                // Storage Bytes of Each Region

                // this is in case the file was written by a 64 bit system.  4 = 32
                // bit, 8 = 64 bit.
                if (result.MbSizeData == 4)
                {
                    result.MbScenarioParamsSize = reader.ReadInt32();
                    result.MbBathyMapSize = reader.ReadInt32();
                    result.MbSalinityMapSize = reader.ReadInt32();
                    result.MbTemperatureMapSize = reader.ReadInt32();
                    result.MbPostAnalisysSize = reader.ReadInt32();
                    result.MbSpeciesDescriptionSize = reader.ReadInt32();
                    result.MbAnimatSpeciesAssSize = reader.ReadInt32();
                    result.MbAnimatStateSize = reader.ReadInt32();
                    result.MbAcousticExposureSize = reader.ReadInt32();
                    byte[] bytes = reader.ReadBytes(12);
                    if (bytes.Length != 12) throw new ApplicationException("loadFromFile: skip 12 failed");
                }
                else
                {
                    result.MbScenarioParamsSize = reader.ReadInt64();
                    result.MbBathyMapSize = reader.ReadInt64();
                    result.MbSalinityMapSize = reader.ReadInt64();
                    result.MbTemperatureMapSize = reader.ReadInt64();
                    result.MbPostAnalisysSize = reader.ReadInt64();
                    result.MbSpeciesDescriptionSize = reader.ReadInt64();
                    result.MbAnimatSpeciesAssSize = reader.ReadInt64();
                    result.MbAnimatStateSize = reader.ReadInt64();
                    result.MbAcousticExposureSize = reader.ReadInt64();

                    try
                    {
                        reader.ReadBytes(8);
                    }
                    catch
                    {
                        throw new System.IO.FileFormatException("");
                    }
                }
                if (result.MbLibSuperVersion == 4 && result.MbLibSubVersion > 300)
                {
                    try
                    {
                        reader.ReadBytes(16);
                    }
                    catch
                    {
                        throw new System.IO.FileFormatException("");
                    }

                    result.SimAreaTotal = reader.ReadInt32();
                    result.SimAreaPopulation = reader.ReadSingle();
                    result.TrackAreaTotal = reader.ReadInt32();
                    result.TrackAreaPopulation = reader.ReadSingle();

                    this.population = (int)(result.SimAreaPopulation + result.TrackAreaPopulation);
                    this.totalAnimats = result.SimAreaTotal + result.TrackAreaTotal;

             //       logger.trace("total population overridden by inbox / outbox based on file version");
                }
                else
                {
                    try
                    {
                        reader.ReadBytes(32);
                    }
                    catch
                    {
                        throw new System.IO.FileFormatException("");
                    }
                    // Species Groups.  form a list for viewing.

                    groupList.clear();

                    var speGroupName = new byte[32];

                    for (int i = 0; i < 8; i++)
                    {
                        var name = reader.ReadBytes(32).ToString();

                        float lvlAphys = reader.ReadSingle();
                        float lvlBphys = reader.ReadSingle();
                        float lvlBBeh = reader.ReadSingle();

                        try
                        {
                            reader.ReadBytes(36);
                        }
                        catch
                        {
                            throw new System.IO.FileFormatException("");
                        }

                        if (speGroupName[0] > 0)
                        {
                            var builder = new StringBuilder();
                            builder.Append(speGroupName.ToString().Trim());
                            builder.Append(", ");
                            builder.Append(lvlAphys);
                            builder.Append(", ");
                            builder.Append(lvlBphys);
                            builder.Append(", ");
                            builder.Append(lvlBBeh);

                            groupList.add(builder.ToString());


                        }
                    }

                    // update for population
                    if (population == 0)
                    {
                        population = MbTotalAnimats;
                    }

                    // if we made it here, the definition is valid

                    result.SpeciesFilePath = fileName;

                //logger.info("Loaded " + this.speciesName + ", file version " + getVersionString() + ", with " +
                //            MbIterationCount + " iteration step");
#endif

                result.ReadStartPositions(reader); //
            }
            return result;
        }

        /// <summary>
        /// jumps the .ddb header, and then populates AnimatStartPoints with the starting locations of every animat in the file.
        /// </summary>
        /// <param name="reader"></param>
        void ReadStartPositions(BinaryReader reader)
        {
            reader.BaseStream.Seek(1040, SeekOrigin.Begin); // skip the .ddb header
            Locations = new EnvironmentData<Geo<float>>();
            var datasize = GetSize(MbOutputConfiguration);
            for (var i = 0; i < TotalAnimats; i++)
            {
                //var id = reader.ReadSingle();
                //var time = reader.ReadSingle();
                reader.BaseStream.Seek(8, SeekOrigin.Current); //skip id and time
                var lat = reader.ReadSingle();
                var lon = reader.ReadSingle();
                //AnimatStartPoints.Add(new Geo(reader.ReadSingle(), reader.ReadSingle()));
                //adds lat and lon.
                //reader.BaseStream.Seek(8 + datasize, SeekOrigin.Current);

                var depth = reader.ReadSingle();
                Locations.Add(new Geo<float>(lat, lon, depth));
                reader.BaseStream.Seek(4 + datasize, SeekOrigin.Current);
                // skip packedData, and the data itself.
                //var packedData = reader.ReadSingle();
                //reader.BaseStream.Seek(datasize, SeekOrigin.Current);

                // reader.BaseStream.Seek(GetSize(MbOutputConfiguration), SeekOrigin.Current);
                //skip its data because we don't care.
            }
        }

        /// <summary>
        /// takes MbOutputConfiguration and determines the size of each animat's packed data.
        /// </summary>
        /// <param name="bitmap"></param>
        /// <returns></returns>
        static int GetSize(int bitmap)
        {
            var size = 0;

            for (var i = 5; i < 22; i++)
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
    }

    [Serializable]
    public class MMMB : Animat
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
        public static new MMMB Load(ScenarioSpecies species, string fileName)
        {
            if (Path.GetExtension(fileName) != ".3mb") throw new FileFormatException("only 3MB files are supported.");
            var result = new MMMB();
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
            Locations = new EnvironmentData<Geo<float>>();
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