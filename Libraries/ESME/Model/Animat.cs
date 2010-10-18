using System;
using System.Collections.Generic;
using System.Linq;
using System.Xml.Linq;
using System.Xml.Serialization;
using HRC.Navigation;

namespace ESME.Model
{
    public class Animat
    {
        [XmlElement]
        public EarthCoordinate3D Location { get; set; }

        [XmlElement]
        public int SpeciesID { get; set; }

        [XmlIgnore] double _soundPressureLevel;

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

        [XmlIgnore] public SourceRecieverLevelBins[] LevelBins;

        public Animat(EarthCoordinate3D location, string speciesName) : this()
        {
            Location = location;
            SpeciesName = speciesName;
        }

        public Animat(EarthCoordinate location, string speciesName) : this()
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

        public Animat(Animat source) : this()
        {
            Location = source.Location;
            SpeciesName = source.SpeciesName;
        }

        public void RecordExposure(int sourceID, float receieveLevel) { LevelBins[sourceID].AddExposure(receieveLevel); }

        public void CreateLevelBins(int sourceCount, float lowReceiveLevel, float binWidth, int binCount)
        {
            LevelBins = new SourceRecieverLevelBins[sourceCount];
            for (var i = 0; i < sourceCount; i++) LevelBins[i] = new SourceRecieverLevelBins(lowReceiveLevel, binWidth, binCount);
        }

        public void AddAnimatRecord(XElement sourceElement)
        {
            var animat = new XElement("Animat");
            animat.Add(new XElement("AnimatID", AnimatID));
            var sources = new XElement("Sources");
            for (var sourceID = 0; sourceID < LevelBins.Length; sourceID++)
            {
                var source = new XElement("Source");
                source.Add(new XElement("SourceID", sourceID));
                LevelBins[sourceID].AddExposureBins(source);
                sources.Add(source);
            }
            animat.Add(sources);
            sourceElement.Add(animat);
        }
    }

    public class AnimatList : List<Animat>
    {
        [XmlIgnore]
        public SpeciesList SpeciesList { get; set; }

        readonly EventHandler<ItemDeletedEventArgs<Species>> _itemDeletedHandler;

        AnimatList() { _itemDeletedHandler = new EventHandler<ItemDeletedEventArgs<Species>>(SpeciesList_ItemDeleted); }

        public AnimatList(SpeciesList speciesList) { SpeciesList = speciesList; }

        public void Initialize(SpeciesList speciesList)
        {
            if (speciesList == null) throw new PropertyNotInitializedException("AnimatList.Initialize(): SpeciesList must be set before calling this method");
            SpeciesList = speciesList;
            SpeciesList.ItemDeleted -= _itemDeletedHandler;
            SpeciesList.ItemDeleted += _itemDeletedHandler;
            foreach (var a in this.Where(a => a != null)) 
                a.Species = SpeciesList.Find(x => x.IDField == a.SpeciesID);
        }

        void SpeciesList_ItemDeleted(object sender, ItemDeletedEventArgs<Species> e) { RemoveAll(a => a.SpeciesID == e.Item.IDField); }

        public void AddAnimatList(XElement rootElement)
        {
            var animats = new XElement("Animats");
            foreach (var a in this) a.AddAnimatRecord(animats);
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

        void Renumber() { for (var i = 0; i < this.Count(); i++) this[i].AnimatID = i; }

        new void AddRange(IEnumerable<Animat> collection) { throw new NotImplementedException(); }
        new void RemoveRange(int index, int count) { throw new NotImplementedException(); }
        new int RemoveAll(Predicate<Animat> match) { throw new NotImplementedException(); }
    }
}