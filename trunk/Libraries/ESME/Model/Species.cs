using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using mbs;
using System.Xml.Serialization;

namespace ESME.Model
{
    public class Species : IEquatable<Species>, IHasIDField
    {
        internal int ReferenceCount { get; set; }
        internal string Filename { get; set; }
        public string SpeciesName { get; set; }
        [XmlElement("SpeciesID")]
        public int IDField { get; set; }
        [XmlIgnore]
        public int Index { get; set; }

        internal Species(string SpeciesFilename)
        {
            Initialize(SpeciesFilename);
            SpeciesName = Path.GetFileNameWithoutExtension(SpeciesFilename);
            Filename = SpeciesFilename;
        }

        private Species() { }

        private void Initialize(string SpeciesFilename)
        {
            if (!File.Exists(SpeciesFilename))
                throw new FileNotFoundException("Species(" + SpeciesFilename + "): File not found");
        }

        internal void AddToSimulationEnvironment(C3mbs SimulationEnvironment)
        {
            mbsRESULT result;
            result = SimulationEnvironment.AddSpecies(Filename);
            if (mbsRESULT.OK != result)
            {
                //System.Diagnostics.Debug.WriteLine("AddSpecies FAILED: " + C3mbs.MbsResultToString(result));
                throw new ApplicationException("C3mbs::AddSpecies(" + Filename + ") FATAL error " + C3mbs.MbsResultToString(result));
            }
        }

        public override string ToString()
        {
            return SpeciesName;
        }

        bool IEquatable<Species>.Equals(Species that)
        {
            if (this.Filename == that.Filename)
                return true;
            return false;
        }

    }

    public class SpeciesDeletedEventArgs : ItemDeletedEventArgs<Species> { }

    public class SpeciesList : UniqueAutoIncrementList<Species>
    {
        private string SpeciesDirectory;

        private SpeciesList() 
        {
        }

        public SpeciesList(string SpeciesDirectory)
        {
            this.SpeciesDirectory = SpeciesDirectory;
            string[] files = Directory.GetFiles(SpeciesDirectory, "*.spe");
            foreach (string file in files)
                this.Add(new Species(file));
        }
        public IEnumerable<Species> ReferencedSpecies
        {
            get
            {
                var result = from Species s in this
                             where s.ReferenceCount > 0
                             select s;
                return result;
            }

        }


        private new void Add(Species Species) { base.Add(Species); }
        private new void Remove(Species Species) { base.Remove(Species); }
        private new void AddRange(IEnumerable<Species> Collection) { }
        private new void RemoveRange(int Index, int Count) { }
        private new int RemoveAll(Predicate<Species> match) { return 0; }
        private new void Clear() { }

        internal int AddReference(string SpeciesName)
        {
            Species result = this.Find(s => s.SpeciesName == SpeciesName);
            if (result == null)
                throw new SpeciesNotFoundException("SpeciesList.Add: Requested species \"" + SpeciesName + "\" was not found");
            result.ReferenceCount++;
            return result.IDField;
        }

        public void RemoveReference(string SpeciesName)
        {
            Species result = this.Find(s => s.SpeciesName == SpeciesName);
            if (result == null)
                throw new SpeciesNotFoundException("SpeciesList.Add: Requested species \"" + SpeciesName + "\" was not found");
            if (result.ReferenceCount > 0)
                result.ReferenceCount--;
        }

        public void ClearReferences()
        {
            foreach (Species s in this)
                s.ReferenceCount = 0;
        }

        public Species this[string SpeciesName]
        {
            get
            {
                return this.Find(s => s.SpeciesName == SpeciesName);
            }
        }

        public string[] SpeciesNames
        {
            get
            {
                var result = from s in this
                             select s.SpeciesName;
                return result.ToArray();
            }
        }
    }
}
