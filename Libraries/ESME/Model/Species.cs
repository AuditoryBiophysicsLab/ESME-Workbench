using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Xml.Serialization;
using ESME.SimulationAnalysis;
using ESME.Simulator;

namespace ESME.Model
{
    public class Species : IEquatable<Species>, IHasIDField
    {
        [XmlIgnore]
        public HistogramBins[] LevelBins;

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

        #region public methods

        public override string ToString()
        {
            return SpeciesName;
        }

        public void CreateLevelBins(int sourceCount, float lowReceiveLevel, float binWidth, int binCount)
        {
            if (LevelBins != null) return;
            LevelBins = new HistogramBins[sourceCount];
            for (int i = 0; i < sourceCount; i++)
                LevelBins[i] = new HistogramBins(lowReceiveLevel, binWidth, binCount);
        }

        #endregion

        #region internal methods and properties

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

        #endregion

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

        [XmlIgnore]
        private double _soundPressureLevel;

        #endregion

        private static void Initialize(string speciesFilename)
        {
            if (!File.Exists(speciesFilename))
                throw new FileNotFoundException("Species(" + speciesFilename + "): File not found");
        }
    }

    public class SpeciesDeletedEventArgs : ItemDeletedEventArgs<Species>
    {
    }

    public class SpeciesList : UniqueAutoIncrementList<Species>
    {
        #region Public Methods
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
        #endregion

        #region Private and Internal Methods
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
        #endregion
    }
}