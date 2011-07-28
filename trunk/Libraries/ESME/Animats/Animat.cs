using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading;
using System.Xml.Linq;
using System.Xml.Serialization;
using ESME.Model;
using HRC.Navigation;
using mbs;
using FileFormatException = ESME.Model.FileFormatException;

namespace ESME.Animats
{
    public class Animat
    {
        [XmlIgnore] public SourceRecieverLevelBins[] LevelBins;
        [XmlIgnore] private double _soundPressureLevel;

        #region Public Properties

        [XmlElement]
        public EarthCoordinate3D Location { get; set; }

        [XmlElement]
        public ulong SpeciesID { get; set; }

        [XmlIgnore]
        internal int AnimatID { get; set; }

        [XmlIgnore]
        public string SpeciesName { get; set; }

        [XmlIgnore]
        public static Animat Empty
        {
            get { return new Animat(); }
        }

        [XmlIgnore]
        public double SoundPressureLevel
        {
            get { return _soundPressureLevel; }
            set
            {
                _soundPressureLevel = value;
                MaxSoundPressureLevel = Math.Max(_soundPressureLevel, MaxSoundPressureLevel);
            }
        }

        [XmlIgnore]
        public double MaxSoundPressureLevel { get; internal set; }

        [XmlIgnore]
        public Species Species { get; internal set; }

        #endregion

        #region constructors

        public Animat(EarthCoordinate3D location, string speciesName)
            : this()
        {
            Location = location;
            SpeciesName = speciesName;
        }

        public Animat(EarthCoordinate location, string speciesName)
            : this()
        {
            Location = new EarthCoordinate3D(location);
            SpeciesName = speciesName;
        }

        public Animat()
        {
            Location = null;
            SpeciesName = null;
            AnimatID = 0;
            _soundPressureLevel = 0.0;
            MaxSoundPressureLevel = 0.0;
        }

        public Animat(mbsPosition position, Species species)
        {
            Location = new EarthCoordinate3D(position.latitude, position.longitude, -position.depth);
            Species = species;
        }

        public Animat(Animat source)
            : this()
        {
            Location = source.Location;
            SpeciesName = source.SpeciesName;
        }

        #endregion

        #region public methods

        public void RecordExposure(string modeName, int sourceID, float receieveLevel)
        {
            LevelBins[sourceID].AddExposure(receieveLevel, modeName);
            //Species.LevelBins[sourceID].AddExposure(receieveLevel, modeName);
        }

        public void SummarizeExposureToSpeciesBins()
        {
            if (LevelBins == null) return;
            for (int source = 0; source < LevelBins.Length; source++)
            {
                for (int bin = LevelBins[source].Bins.Length - 1; bin >= 0; bin--)
                {
                    if (LevelBins[source].Bins[bin] > 0)
                    {
                        if (string.IsNullOrEmpty(Species.LevelBins[source].ModeName))
                            Species.LevelBins[source].ModeName = LevelBins[source].ModeName;
                        Species.LevelBins[source].Bins[bin]++;
                        break;
                    }
                }
            }
        }

        public void CreateLevelBins(int sourceCount, float lowReceiveLevel, float binWidth, int binCount)
        {
            Species.CreateLevelBins(sourceCount, lowReceiveLevel, binWidth, binCount);
            if (LevelBins != null) return;
            LevelBins = new SourceRecieverLevelBins[sourceCount];
            for (int i = 0; i < sourceCount; i++)
                LevelBins[i] = new SourceRecieverLevelBins(lowReceiveLevel, binWidth, binCount);
        }

        public void AddAnimatRecord(XElement sourceElement)
        {
            var animat = new XElement("Animat");
            animat.Add(new XElement("AnimatID", AnimatID));
            var sources = new XElement("Sources");
            for (int sourceID = 0; sourceID < LevelBins.Length; sourceID++)
            {
                var source = new XElement("Source");
                source.Add(new XElement("SourceID", sourceID));
                LevelBins[sourceID].AddExposureBins(source);
                sources.Add(source);
            }
            animat.Add(sources);
            sourceElement.Add(animat);
        }

        #endregion
    }

    public class AnimatList : List<Animat>
    {
        private readonly C3mbs _mmmbs = new C3mbs();

        public AnimatList(SpeciesList speciesList)
        {
            SpeciesList = speciesList;
        }

        /// <summary>
        /// Creates an AnimatList from an (animat) Scenario File (*.sce), resulting from a 3MB.exe run.
        /// </summary>
        /// <param name="animatScenarioFile">path to the .sce file.</param>
        public AnimatList(string animatScenarioFile)
        {
            mbsRESULT mbsResult;
            mbsCONFIG config = _mmmbs.GetConfiguration();

            //load the .sce file
            if (mbsRESULT.OK != (mbsResult = _mmmbs.LoadScenario(animatScenarioFile)))
                throw new AnimatMMBSException("LoadScenario Error:" + _mmmbs.MbsResultToString(mbsResult));
            //make sure we're in durationless mode.
            config.durationLess = true;
            _mmmbs.SetConfiguration(config);

            //get species count
            int speciesCount = _mmmbs.GetSpeciesCount();

            //make a species list.
            SpeciesList = new SpeciesList();
            for (int i = 0; i < speciesCount; i++)
            {
                SpeciesList.Add(new Species
                                    {
                                        SpeciesName = _mmmbs.GetSpeciesDisplayTitle(i).Filter(c => !char.IsControl(c)),
                                        ReferenceCount = _mmmbs.GetIndivdualCount(i),
                                    });
            }
            //set up the position array from the values in the .sce file (not the ones in animatList, which doesn't exist yet..)
            int animatCount = _mmmbs.GetAnimatCount();
            var posArray = new mbsPosition[animatCount];

            //initialize the run, and wait until it's fully initialized.
            if (mbsRESULT.OK != (mbsResult = _mmmbs.InitializeRun()))
                throw new AnimatMMBSException("InitializeRun Error:" + _mmmbs.MbsResultToString(mbsResult));
            while ((_mmmbs.GetRunState()) == mbsRUNSTATE.INITIALIZING)
            {
                //wait until initializing is done.
                Thread.Sleep(1);
            }

            //get the initial positions of every animat
            if (mbsRESULT.OK != (mbsResult = _mmmbs.GetAnimatCoordinates(posArray)))
                throw new AnimatMMBSException("Error Fetching Initial Animat Coordinates: " +
                                              _mmmbs.MbsResultToString(mbsResult));

            int speciesIndex = 0;
            Species curSpecies = SpeciesList[speciesIndex];
            int nextSpeciesAnimatIndex = curSpecies.ReferenceCount;
            //add animats to each species. 
            int curAnimatCount = 0;
            for (int i = 0; i < posArray.Length; i++)
            {
                if (i >= nextSpeciesAnimatIndex)
                {
                    curSpecies.ReferenceCount -= curAnimatCount;
                    curAnimatCount = 0;
                    curSpecies = SpeciesList[++speciesIndex];
                    nextSpeciesAnimatIndex += curSpecies.ReferenceCount;
                }
                mbsPosition mbsPosition = posArray[i];
                Add(new Animat(mbsPosition, curSpecies)
                        {
                            SpeciesName = curSpecies.SpeciesName,
                        });
                curAnimatCount++;
            }
            curSpecies.ReferenceCount -= curAnimatCount;
        }

        [XmlIgnore]
        public SpeciesList SpeciesList { get; set; }

        public void AddAnimatList(XElement rootElement)
        {
            var animats = new XElement("Animats");
            foreach (Animat a in this) a.AddAnimatRecord(animats);
            rootElement.Add(animats);
        }

        public new void Add(Animat animat)
        {
            animat.Species = SpeciesList[animat.SpeciesName];
            animat.SpeciesID = SpeciesList.AddReference(animat.SpeciesName);
            base.Add(animat);
            Renumber();
        }

        public new void Remove(Animat animat)
        {
            SpeciesList.RemoveReference(animat.SpeciesName);
            base.Remove(animat);
            Renumber();
        }

        public new void RemoveAt(int index)
        {
            SpeciesList.RemoveReference(this[index].SpeciesName);
            RemoveAt(index);
            Renumber();
        }

        public new void Clear()
        {
            SpeciesList.Clear();
            Clear();
        }

        private void Renumber()
        {
            for (int i = 0; i < this.Count(); i++) this[i].AnimatID = i;
        }

        private new void AddRange(IEnumerable<Animat> collection)
        {
            throw new NotImplementedException();
        }

        private new void RemoveRange(int index, int count)
        {
            throw new NotImplementedException();
        }

        private new int RemoveAll(Predicate<Animat> match)
        {
            throw new NotImplementedException();
        }
    }

    public class DDB
    {
        #region public properties {get; internal set;}
        //not all of these are ever set; the header load procedure terminates once we have determined the species name, output configuration, and total number of animats -- it's all we need.
        public string Filename { get; internal set; }
        public string SpeciesFilePath { get; internal set; }
        public int MbLibSuperVersion { get; internal set; }
        public int MbLibSubVersion { get; internal set; }
        public int MbSbSuperVersion { get; internal set; }
        public int MbSbSubVersion { get; internal set; }
        public int MbOutSuperVersion { get; internal set; }
        public int MbOutSubVersion { get; internal set; }
        public int MbNumSpecies { get; internal set; }
        public int MbTotalAnimats { get; internal set; }
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
        public string LatinName { get; internal set; }
        public List<EarthCoordinate> AnimatStartPoints { get; internal set; }

        #endregion
        /// <summary>
        /// the header load procedure terminates once we have determined the species name, output configuration, and total number of animats -- it's all we need to display points on a map.
        /// </summary>
        /// <param name="fileName">the full path to the .ddb file</param>
        /// <returns></returns>
        public static DDB Load(string fileName)
        {
            if(Path.GetExtension(fileName) != ".ddb") throw new FileFormatException("only ddb files are supported.");
            var result = new DDB();
            result.Filename = fileName;
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
                result.MbTotalAnimats = reader.ReadInt32();
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
                    result.LatinName = reader.ReadString().TrimEnd('\0').Trim();

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
        private void ReadStartPositions(BinaryReader reader)
        {
            reader.BaseStream.Seek(1040, SeekOrigin.Begin); // skip the .ddb header
            AnimatStartPoints = new List<EarthCoordinate>();
            int datasize = GetSize(MbOutputConfiguration);
            for (int i = 0; i < MbTotalAnimats; i++)
            {
                //var id = reader.ReadSingle();
                //var time = reader.ReadSingle();
                reader.BaseStream.Seek(8, SeekOrigin.Current); //skip id and time
                float lat = reader.ReadSingle();
                float lon = reader.ReadSingle();
                AnimatStartPoints.Add(new EarthCoordinate(lat, lon));
                //AnimatStartPoints.Add(new EarthCoordinate(reader.ReadSingle(), reader.ReadSingle()));
                //adds lat and lon.
                reader.BaseStream.Seek(8 + datasize, SeekOrigin.Current);
                    // skip depth, packedData, and the data itself.

                //var depth = reader.ReadSingle();
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
        private static int GetSize(int bitmap)
        {
            int size = 0;

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
    }

    public class MMMB
    {
        #region public properties {get; internal set;}
        //not all of these are ever set; the header load procedure terminates once we have determined the species name, output configuration, and total number of animats -- it's all we need.
        public string Filename { get; internal set; }
        public string SpeciesFilePath { get; internal set; }
        public int MbLibSuperVersion { get; internal set; }
        public int MbLibSubVersion { get; internal set; }
        public int MbSbSuperVersion { get; internal set; }
        public int MbSbSubVersion { get; internal set; }
        public int MbOutSuperVersion { get; internal set; }
        public int MbOutSubVersion { get; internal set; }
        public int MbNumSpecies { get; internal set; }
        public int MbTotalAnimats { get; internal set; }
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
        public string LatinName { get; internal set; }
        public List<EarthCoordinate> AnimatStartPoints { get; internal set; }

        #endregion
        /// <summary>
        /// the header load procedure terminates once we have determined the species name, output configuration, and total number of animats -- it's all we need to display points on a map.
        /// </summary>
        /// <param name="fileName">the full path to the .3mb file</param>
        /// <returns></returns>
        public static MMMB Load(string fileName)
        {
            if (Path.GetExtension(fileName) != ".3mb") throw new FileFormatException("only 3mb files are supported.");
            var result = new MMMB();
            result.Filename = fileName;
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
                result.MbTotalAnimats = reader.ReadInt32();
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
                    result.LatinName = reader.ReadString().TrimEnd('\0').Trim();

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
        private void ReadStartPositions(BinaryReader reader)
        {
            reader.BaseStream.Seek(1040, SeekOrigin.Begin); // skip the .ddb header
            AnimatStartPoints = new List<EarthCoordinate>();
            int datasize = GetSize(MbOutputConfiguration);
            for (int i = 0; i < MbTotalAnimats; i++)
            {
                //var id = reader.ReadSingle();
                //var time = reader.ReadSingle();
                reader.BaseStream.Seek(8, SeekOrigin.Current); //skip id and time
                float lat = reader.ReadSingle();
                float lon = reader.ReadSingle();
                AnimatStartPoints.Add(new EarthCoordinate(lat, lon));
                //AnimatStartPoints.Add(new EarthCoordinate(reader.ReadSingle(), reader.ReadSingle()));
                //adds lat and lon.
                reader.BaseStream.Seek(8 + datasize, SeekOrigin.Current);
                // skip depth, packedData, and the data itself.

                //var depth = reader.ReadSingle();
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
        private static int GetSize(int bitmap)
        {
            int size = 0;

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
    }
}