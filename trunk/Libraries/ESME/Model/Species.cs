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
        internal int ReferenceCount { get; set; }
        internal string Filename { get; set; }
        public string SpeciesName { get; set; }

        [XmlElement("SpeciesID")]
        public int IDField { get; set; }

        [XmlIgnore]
        public int Index { get; set; }

        internal Species(string speciesFilename)
        {
            Initialize(speciesFilename);
            SpeciesName = Path.GetFileNameWithoutExtension(speciesFilename);
            Filename = speciesFilename;
        }

        static void Initialize(string speciesFilename) { if (!File.Exists(speciesFilename)) throw new FileNotFoundException("Species(" + speciesFilename + "): File not found"); }

        internal void AddToSimulationEnvironment(C3mbs simulationEnvironment)
        {
            var result = simulationEnvironment.AddSpecies(Filename);
            if (mbsRESULT.OK != result)
            {
                //System.Diagnostics.Debug.WriteLine("AddSpecies FAILED: " + C3mbs.MbsResultToString(result));
                throw new ApplicationException("C3mbs::AddSpecies(" + Filename + ") FATAL error " + _mmmbs.MbsResultToString(result));
            }
        }

        public override string ToString() { return SpeciesName; }

        bool IEquatable<Species>.Equals(Species that)
        {
            return Filename == that.Filename;
        }

        readonly C3mbs _mmmbs = new C3mbs();

    }

    public class SpeciesDeletedEventArgs : ItemDeletedEventArgs<Species> {}

    public class SpeciesList : UniqueAutoIncrementList<Species>
    {
        public SpeciesList(string speciesDirectory)
        {
            var files = Directory.GetFiles(speciesDirectory, "*.spe");
            foreach (var file in files) Add(new Species(file));
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


        new void Add(Species species) { base.Add(species); }
        new void Remove(Species species) { base.Remove(species); }
        new void AddRange(IEnumerable<Species> collection) { }
        new void RemoveRange(int index, int count) { }
        new int RemoveAll(Predicate<Species> match) { return 0; }
        new void Clear() { }

        internal int AddReference(string speciesName)
        {
            var result = Find(s => s.SpeciesName == speciesName);
            if (result == null) throw new SpeciesNotFoundException("SpeciesList.Add: Requested species \"" + speciesName + "\" was not found");
            result.ReferenceCount++;
            return result.IDField;
        }

        public void RemoveReference(string speciesName)
        {
            var result = Find(s => s.SpeciesName == speciesName);
            if (result == null) throw new SpeciesNotFoundException("SpeciesList.Add: Requested species \"" + speciesName + "\" was not found");
            if (result.ReferenceCount > 0) result.ReferenceCount--;
        }

        public void ClearReferences() { foreach (var s in this) s.ReferenceCount = 0; }

        public Species this[string speciesName]
        {
            get { return Find(s => s.SpeciesName == speciesName); }
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