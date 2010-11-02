using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Xml.Serialization;
using mbs;

namespace ESME.Model
{
    public class Species : IEquatable<Species>, IHasIDField
    {
        #region Public Properties

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
        public int Index { get; set; }

        public string SpeciesName { get; set; }

        #endregion

        [XmlIgnore] public SourceRecieverLevelBins[] LevelBins;

        #region public methods
        public override string ToString()
        {
            return SpeciesName;
        }

        public void CreateLevelBins(int sourceCount, float lowReceiveLevel, float binWidth, int binCount)
        {
            if (LevelBins != null) return;
            LevelBins = new SourceRecieverLevelBins[sourceCount];
            for (int i = 0; i < sourceCount; i++) LevelBins[i] = new SourceRecieverLevelBins(lowReceiveLevel, binWidth, binCount);
        }

        

        public void RecordExposure(int sourceID, float receieveLevel) { LevelBins[sourceID].AddExposure(receieveLevel); }

        
        #endregion

        internal Species(string speciesFilename)
        {
            Initialize(speciesFilename);
            SpeciesName = Path.GetFileNameWithoutExtension(speciesFilename);
            Filename = speciesFilename;
        }

        internal Species()
        {
        }

        internal int ReferenceCount { get; set; }
        internal string Filename { get; set; }

        #region IEquatable<Species> Members

        bool IEquatable<Species>.Equals(Species that)
        {
            return Filename == that.Filename;
        }

        #endregion

        #region IHasIDField Members

        [XmlElement("SpeciesID")]
        public ulong IDField { get; set; }

        #endregion

        #region private data members

        private readonly C3mbs _mmmbs = new C3mbs();
        [XmlIgnore] private double _soundPressureLevel;

        #endregion

        private static void Initialize(string speciesFilename)
        {
            if (!File.Exists(speciesFilename))
                throw new FileNotFoundException("Species(" + speciesFilename + "): File not found");
        }

        internal void AddToSimulationEnvironment(C3mbs simulationEnvironment)
        {
            mbsRESULT result = simulationEnvironment.AddSpecies(Filename);
            if (mbsRESULT.OK != result)
            {
                //System.Diagnostics.Debug.WriteLine("AddSpecies FAILED: " + C3mbs.MbsResultToString(result));
                throw new ApplicationException("C3mbs::AddSpecies(" + Filename + ") FATAL error " +
                                               _mmmbs.MbsResultToString(result));
            }
        }

        
    }

    public class SpeciesDeletedEventArgs : ItemDeletedEventArgs<Species>
    {
    }

    public class SpeciesList : UniqueAutoIncrementList<Species>
    {
        public SpeciesList(string speciesDirectory)
        {
            string[] files = Directory.GetFiles(speciesDirectory, "*.spe");
            foreach (string file in files) Add(new Species(file));
        }

        public SpeciesList()
        {
        }

        public IEnumerable<Species> ReferencedSpecies
        {
            get
            {
                IEnumerable<Species> result = from Species s in this
                                              where s.ReferenceCount > 0
                                              select s;
                return result;
            }
        }

        public Species this[string speciesName]
        {
            get { return Find(s => s.SpeciesName == speciesName); }
        }

        public string[] SpeciesNames
        {
            get
            {
                IEnumerable<string> result = from s in this
                                             select s.SpeciesName;
                return result.ToArray();
            }
        }


        private new void Add(Species species)
        {
            base.Add(species);
        }

        private new void Remove(Species species)
        {
            base.Remove(species);
        }

        private new void AddRange(IEnumerable<Species> collection)
        {
        }

        private new void RemoveRange(int index, int count)
        {
        }

        private new int RemoveAll(Predicate<Species> match)
        {
            return 0;
        }

        private new void Clear()
        {
        }

        internal ulong AddReference(string speciesName)
        {
            Species result = Find(s => s.SpeciesName == speciesName);
            if (result == null)
                throw new SpeciesNotFoundException("SpeciesList.Add: Requested species \"" + speciesName +
                                                   "\" was not found");
            result.ReferenceCount++;
            return result.IDField;
        }

        public void RemoveReference(string speciesName)
        {
            Species result = Find(s => s.SpeciesName == speciesName);
            if (result == null)
                throw new SpeciesNotFoundException("SpeciesList.Add: Requested species \"" + speciesName +
                                                   "\" was not found");
            if (result.ReferenceCount > 0) result.ReferenceCount--;
        }

        public void ClearReferences()
        {
            foreach (Species s in this) s.ReferenceCount = 0;
        }
    }
}