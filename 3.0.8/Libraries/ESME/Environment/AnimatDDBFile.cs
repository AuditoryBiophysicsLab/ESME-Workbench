using System;
using System.IO;
using System.Text;
using ESME.Scenarios;
using HRC.Navigation;
using FileFormatException = ESME.Model.FileFormatException;

namespace ESME.Environment
{
    [Serializable]
    public class AnimatDDBFile : Animat
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
        public new static AnimatDDBFile Load(ScenarioSpecies species, string fileName)
        {
            if (Path.GetExtension(fileName) != ".ddb") throw new FileFormatException("only ddb files are supported.");
            var result = new AnimatDDBFile();
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
            Locations = new AnimatEnvironmentData<Geo<float>>();
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
}