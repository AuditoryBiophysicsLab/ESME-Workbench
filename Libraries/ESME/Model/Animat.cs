using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Xml.Linq;
using System.Xml.Serialization;
using HRC.Navigation;
using mbs;


namespace ESME.Model
{
    public class Animat
    {
        [XmlIgnore]
        public SourceRecieverLevelBins[] LevelBins;
        [XmlIgnore]
        double _soundPressureLevel;

        #region Public Properties
        [XmlElement]
        public EarthCoordinate3D Location { get; set; }

        [XmlElement]
        public int SpeciesID { get; set; }

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

        public Animat(mbsPosition position, Species species)
        {
            Location = new EarthCoordinate3D(position.latitude, position.longitude, -position.depth);
            Species = species;
        }

        public Animat(Animat source) : this()
        {
            Location = source.Location;
            SpeciesName = source.SpeciesName;
        }
        #endregion

        #region public methods

        public void RecordExposure(int sourceID, float receieveLevel) { LevelBins[sourceID].AddExposure(receieveLevel); }

        public void CreateLevelBins(int sourceCount, float lowReceiveLevel, float binWidth, int binCount)
        {
            LevelBins = new SourceRecieverLevelBins[sourceCount];
            for (int i = 0; i < sourceCount; i++) LevelBins[i] = new SourceRecieverLevelBins(lowReceiveLevel, binWidth, binCount);
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
        readonly EventHandler<ItemDeletedEventArgs<Species>> _itemDeletedHandler;
        readonly C3mbs _mmmbs = new C3mbs();

        AnimatList() { _itemDeletedHandler = new EventHandler<ItemDeletedEventArgs<Species>>(SpeciesList_ItemDeleted); }

        public AnimatList(SpeciesList speciesList) { SpeciesList = speciesList; }

        /// <summary>
        /// Creates an AnimatList from an (animat) Scenario File (*.sce), resulting from a 3MB.exe run.
        /// </summary>
        /// <param name="animatScenarioFile">path to the .sce file.</param>
        public AnimatList(string animatScenarioFile)
        {
            mbsRESULT mbsResult;
            mbsCONFIG config = _mmmbs.GetConfiguration();
            mbsRUNSTATE runState;

            //load the .sce file
            if (mbsRESULT.OK != (mbsResult = _mmmbs.LoadScenario(animatScenarioFile))) throw new AnimatMMBSException("LoadScenario Error:" + _mmmbs.MbsResultToString(mbsResult));
            //make sure we're in durationless mode.
            config.durationLess = true;
            _mmmbs.SetConfiguration(config);
           
            //get species count
            int speciesCount = _mmmbs.GetSpeciesCount();
            
            //make a species list.
            SpeciesList = new SpeciesList();
            for (var i = 0; i < speciesCount; i++)
            {
                SpeciesList.Add(new Species
                                {
                                    SpeciesName = _mmmbs.GetSpeciesDisplayTitle(i).Filter(c=>!char.IsControl(c)),
                                    ReferenceCount = _mmmbs.GetIndivdualCount(i),
                                });
            }
            //set up the position array from the values in the .sce file (not the ones in animatList, which doesn't exist yet..)
            int animatCount = _mmmbs.GetAnimatCount();
            var posArray = new mbsPosition[animatCount];

            //initialize the run, and wait until it's fully initialized.
            if (mbsRESULT.OK != (mbsResult = _mmmbs.InitializeRun())) throw new AnimatMMBSException("InitializeRun Error:" + _mmmbs.MbsResultToString(mbsResult));
            while ((runState = _mmmbs.GetRunState()) == mbsRUNSTATE.INITIALIZING)
            {
                //wait until initializing is done.
                Thread.Sleep(1);
            }
            //bump the positions once, otherwise depths aren't set. 
            if (mbsRESULT.OK != (mbsResult = _mmmbs.RunScenarioNumIterations(1))) throw new AnimatMMBSException("RunScenario Error:" + _mmmbs.MbsResultToString(mbsResult));

            //get the initial positions of every animat
            if (mbsRESULT.OK != (mbsResult = _mmmbs.GetAnimatCoordinates(posArray))) throw new AnimatMMBSException("Error Fetching Initial Animat Coordinates: " + _mmmbs.MbsResultToString(mbsResult));

            int speciesIndex = 0;
            Species curSpecies = SpeciesList[speciesIndex];
            int nextSpeciesAnimatIndex = curSpecies.ReferenceCount;
            //add animats to each species. 
            for (int i = 0; i < posArray.Length; i++)
            {
                if (i >= nextSpeciesAnimatIndex)
                {
                    curSpecies.ReferenceCount -= nextSpeciesAnimatIndex;
                    curSpecies = SpeciesList[++speciesIndex];
                    nextSpeciesAnimatIndex += curSpecies.ReferenceCount;
                }
                mbsPosition mbsPosition = posArray[i];
                Add(new Animat(mbsPosition, curSpecies){SpeciesName = curSpecies.SpeciesName,});
            }
            curSpecies.ReferenceCount -= nextSpeciesAnimatIndex;
        }

        [XmlIgnore]
        public SpeciesList SpeciesList { get; set; }

        void Initialize(SpeciesList speciesList)
        {
            if (speciesList == null) throw new PropertyNotInitializedException("AnimatList.Initialize(): SpeciesList must be set before calling this method");
            SpeciesList = speciesList;
            SpeciesList.ItemDeleted -= _itemDeletedHandler;
            SpeciesList.ItemDeleted += _itemDeletedHandler;
            foreach (Animat a in this.Where(a => a != null)) a.Species = SpeciesList.Find(x => x.IDField == a.SpeciesID);
        }

        void SpeciesList_ItemDeleted(object sender, ItemDeletedEventArgs<Species> e) { RemoveAll(a => a.SpeciesID == e.Item.IDField); }

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

        void Renumber() { for (int i = 0; i < this.Count(); i++) this[i].AnimatID = i; }

        new void AddRange(IEnumerable<Animat> collection) { throw new NotImplementedException(); }
        new void RemoveRange(int index, int count) { throw new NotImplementedException(); }
        new int RemoveAll(Predicate<Animat> match) { throw new NotImplementedException(); }
    }
}