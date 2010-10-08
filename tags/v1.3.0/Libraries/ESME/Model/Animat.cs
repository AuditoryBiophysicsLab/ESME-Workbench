using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using HRC.Navigation;
using System.Xml.Serialization;
using System.Xml.Linq;
using System.Windows;
using ESME.Model;
using ESME.AnimatLocationFile;


namespace ESME.Model
{
    public class Animat
    {
        [XmlElement]
        public EarthCoordinate3D Location { get; set; }
        [XmlElement]
        public int SpeciesID { get; set; }

        [XmlIgnore]
        private double curSPL = 0.0;
        [XmlIgnore]
        internal int AnimatID { get; set; }
        [XmlIgnore]
        public string SpeciesName { get; set; }
        [XmlIgnore]
        public static Animat Empty { get { return new Animat(); } }
        [XmlIgnore]
        public double SPL
        {
            get { return curSPL; }
            set
            {
                curSPL = value;
                MaxSPL = Math.Max(curSPL, MaxSPL);
            }
        }
        [XmlIgnore]
        public double MaxSPL { get; internal set; }
        [XmlIgnore]
        public Species Species { get; internal set; }

        [XmlIgnore]
        public SourceRecieverLevelBins[] LevelBins;

        public Animat(EarthCoordinate3D Location, string SpeciesName)
            : this()
        {
            this.Location = Location;
            this.SpeciesName = SpeciesName;
        }

        public Animat(EarthCoordinate Location, string SpeciesName)
            : this()
        {
            this.Location = new EarthCoordinate3D(Location);
            this.SpeciesName = SpeciesName;
        }

        public Animat()
        {
            Location = null;
            SpeciesName = null;
            AnimatID = 0;
            curSPL = 0.0;
            MaxSPL = 0.0;
        }

        public Animat(Animat Source)
            : this()
        {
            Location = Source.Location;
            SpeciesName = Source.SpeciesName;
        }

        public void RecordExposure(int SourceID, float ReceieveLevel_dBSPL)
        {
            LevelBins[SourceID].AddExposure(ReceieveLevel_dBSPL);
        }

        public void CreateLevelBins(int SourceCount, float LowReceiveLevel_dBSPL, float BinWidth_dBSPL, int BinCount)
        {
            LevelBins = new SourceRecieverLevelBins[SourceCount];
            for (int i = 0; i < SourceCount; i++)
                LevelBins[i] = new SourceRecieverLevelBins(LowReceiveLevel_dBSPL, BinWidth_dBSPL, BinCount);
        }

        public void AddAnimatRecord(XElement SourceElement)
        {
            XElement Animat = new XElement("Animat");
            Animat.Add(new XElement("AnimatID", AnimatID));
            XElement Sources = new XElement("Sources");
            for (int sourceID = 0; sourceID < LevelBins.Length; sourceID++)
            {
                XElement Source = new XElement("Source");
                Source.Add(new XElement("SourceID", sourceID));
                LevelBins[sourceID].AddExposureBins(Source);
                Sources.Add(Source);
            }
            Animat.Add(Sources);
            SourceElement.Add(Animat);
        }

    }

    public class AnimatList : List<Animat>
    {
        [XmlIgnore]
        public SpeciesList SpeciesList { get; set; }
        private EventHandler<ItemDeletedEventArgs<Species>> ItemDeletedHandler;

        private AnimatList()
        {
            ItemDeletedHandler = new EventHandler<ItemDeletedEventArgs<Species>>(SpeciesList_ItemDeleted);
        }

        public AnimatList(SpeciesList SpeciesList)
        {
            this.SpeciesList = SpeciesList;
        }

        public void Initialize(SpeciesList SpeciesList)
        {
            if (SpeciesList == null)
                throw new PropertyNotInitializedException("AnimatList.Initialize(): SpeciesList must be set before calling this method");
            this.SpeciesList = SpeciesList;
            this.SpeciesList.ItemDeleted -= ItemDeletedHandler;
            this.SpeciesList.ItemDeleted += ItemDeletedHandler;
            foreach (var a in this)
                a.Species = this.SpeciesList.Find(x => x.IDField == a.SpeciesID);
        }

        void SpeciesList_ItemDeleted(object sender, ItemDeletedEventArgs<Species> e)
        {
            this.RemoveAll(a => a.SpeciesID == e.Item.IDField);
        }

        public void AddAnimatList(XElement RootElement)
        {
            XElement Animats = new XElement("Animats");
            foreach (Animat a in this)
                a.AddAnimatRecord(Animats);
            RootElement.Add(Animats);
        }

        public new void Add(Animat Animat)
        {
            Animat.Species = SpeciesList[Animat.SpeciesName];
            Animat.SpeciesID = SpeciesList.AddReference(Animat.SpeciesName);
            base.Add(Animat);
            Renumber();
        }

        public new void Remove(Animat Animat)
        {
            SpeciesList.RemoveReference(Animat.SpeciesName);
            base.Remove(Animat);
            Renumber();
        }

        public new void RemoveAt(int Index)
        {
            SpeciesList.RemoveReference(this[Index].SpeciesName);
            this.RemoveAt(Index);
            Renumber();
        }

        public new void Clear()
        {
            SpeciesList.Clear();
            this.Clear();
        }

        private void Renumber()
        {
            for (int i = 0; i < this.Count(); i++)
                this[i].AnimatID = i;
        }

        private new void AddRange(IEnumerable<Animat> Collection) { }
        private new void RemoveRange(int Index, int Count) { }
        private new int RemoveAll(Predicate<Animat> match) { return 0; }
    }
}
