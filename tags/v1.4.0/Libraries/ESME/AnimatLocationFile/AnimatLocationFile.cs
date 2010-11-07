using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows.Media;
using ESME.Model;
using ESME.Overlay;
using HRC.Navigation;
using mbs;

namespace ESME.AnimatLocationFile
{
#if false
    internal class tester
    {
        private tester()
        {
            AnimatLocationFile file = AnimatLocationFile.Open("file.animats");
            IEnumerable<EarthCoordinate3D[]> iterator = file["generic_odontocete"];
            foreach (var posArray in iterator)
            {
                foreach (EarthCoordinate3D pos in posArray)
                {
                }
            }
        }
    } 
#endif

    public class AnimatLocationFile
    {
        #region Static factories

        /// <summary>
        /// creates a new animat location file.  Opens said file exclusively, with write access.  Will not allow duplicate file names to be created.
        /// </summary>
        /// <param name="fileName"></param>
        /// <param name="timeStepLength"></param>
        /// <param name="simulationDuration"></param>
        /// <param name="animatList"></param>
        /// <returns> animatlocationfile object.</returns>
        public static AnimatLocationFile Create(string fileName, TimeSpan timeStepLength, TimeSpan simulationDuration,
                                                AnimatList animatList)
        {
            if (animatList.Count() == 0)
                throw new ApplicationException(
                    "AnimatLocationFile::Create : at least one species must be present in speciesDescriptor");

            var result = new AnimatLocationFile();

            foreach (Species s in animatList.SpeciesList.ReferencedSpecies)
                result.SpeciesDescriptors.Add(new SpeciesDescriptor { Species = s });

            result.Duration = simulationDuration;
            result._timeStepLength = timeStepLength;
            result._totalAnimatCount = animatList.Count();
            result._totalTimeRecords = (int)(simulationDuration.TotalSeconds / timeStepLength.TotalSeconds);
            // number of animats times number of earthcoordinate3ds in the simulation.  the cast is ok because never noninteger seconds.
            result._writer =
                new BinaryWriter(File.Open(fileName, FileMode.CreateNew, FileAccess.Write, FileShare.ReadWrite));
            result.WriteHeader();
            return result;
        }

        /// <summary>
        /// opens a preexisting animat location file, and reads the header into the created object.
        /// </summary>
        /// <param name="fileName"></param>
        /// <returns>an animat location file object.</returns>
        public static AnimatLocationFile Open(string fileName)
        {
            var result = new AnimatLocationFile
                             {
                                 _reader =
                                     new BinaryReader(File.Open(fileName, FileMode.Open, FileAccess.Read, FileShare.Read))
                             };
            result.ReadHeader();
            return result;
        }

        /// <summary>
        /// closes the animat location file object.
        /// </summary>
        public void Close()
        {
            if (_writer != null && _writer.BaseStream.CanWrite)
            {
                _writer.Close();
                _writer = null;
            }
            if (_reader != null && _reader.BaseStream.CanWrite)
            {
                _reader.Close();
                _reader = null;
            }
        }

        #endregion

        #region Public utility methods and properties
        /// <summary>
        /// //todo!  
        /// </summary>
        public TimeSpan Duration { get; private set; }

        /// <summary>
        /// finds and reads the positions of all animats from all species at a given time index.  The nth index is the n+1th timesteplength position in the file.
        /// </summary>
        /// <param name="timeIndex"></param>
        /// <returns>array of Earthcoordinate3Ds from all animats at a given time</returns>      
        public EarthCoordinate3D[] this[long timeIndex]
        {
            get
            {
                CheckCanRead();
                if (timeIndex >= _totalTimeRecords)
                    throw new IndexOutOfRangeException("Index exceeds animat bounds");
                //calculate file position
                long timeRecordLength = _totalAnimatCount * EarthCoordinate3D.Size;
                long filePosition = (timeRecordLength * timeIndex) + _sizeOfHeader;
                //skip to that position
                _reader.BaseStream.Position = filePosition;
                //go get it.
                var retval = new EarthCoordinate3D[_totalAnimatCount];
                for (int i = 0; i < _totalAnimatCount; i++)
                    retval[i] = new EarthCoordinate3D(_reader.ReadDouble(), _reader.ReadDouble(), _reader.ReadDouble());
                return retval;
            }
        }

        /// <summary>
        /// finds and reads the positions of all animats from all species at a given simulated time. Accounts for simulator timestep size.
        /// </summary>
        /// <param name="time"></param>
        /// <returns>array of Earthcoordinate3Ds from all animats at a given time</returns>
        public EarthCoordinate3D[] this[TimeSpan time]
        {
            get { return this[time.Ticks / _timeStepLength.Ticks]; }
        }

        /// <summary>
        /// Get position information about all animats which are members of the specified species across all time indices
        /// </summary>
        /// <param name="speciesName">The name of the species</param>
        /// <returns>Enumerable list of EarthCoordinate3D[], intended for use in a foreach statement</returns>
        public IEnumerable<EarthCoordinate3D[]> this[string speciesName]
        {
            get
            {
                CheckCanRead();
                if ((SpeciesDescriptors.Find(x => x.SpeciesName == speciesName)) == null)
                    throw new AnimatLocationFileException("AnimatLocationFile:this[speciesname];species requested has zero animats");
                long speciesStartIndex = 0;
                long speciesEndIndex = 0;
                int speciesCount = 0;

                foreach (SpeciesDescriptor species in SpeciesDescriptors)
                {
                    if (species.SpeciesName == speciesName)
                    {
                        speciesEndIndex = speciesStartIndex + species.AnimatCount;
                        speciesCount = species.AnimatCount;
                        break;
                    }
                    speciesStartIndex += species.AnimatCount;
                }
                long startOffset = speciesStartIndex * EarthCoordinate3D.Size;
                long endOffset = (_totalAnimatCount - speciesEndIndex) * EarthCoordinate3D.Size;

                // number of species in the list prior to target species * number of animats per species
                _reader.BaseStream.Position = _sizeOfHeader;

                for (long i = 0; i < _totalTimeRecords; i++)
                {
                    _reader.BaseStream.Seek(startOffset, SeekOrigin.Current);
                    var retval = new EarthCoordinate3D[speciesCount];
                    for (int j = 0; j < speciesCount; j++) //read only the given species' positions, skip the others.
                        retval[j] = new EarthCoordinate3D(_reader.ReadDouble(), _reader.ReadDouble(),
                                                          _reader.ReadDouble());
                    //...and skip through the rest of the species for this time.
                    _reader.BaseStream.Seek(endOffset, SeekOrigin.Current);
                    yield return retval;
                }
            }
        }

        public IEnumerable<EarthCoordinate3D> AnimatTrackBySpecies(string speciesName, int animatIndexInSpecies)
        {
            var thisSpecies = this[speciesName];
            return thisSpecies.Select(timelocation => timelocation[animatIndexInSpecies]);
        }

        public IEnumerable<OverlayLineSegments> GetAnimatTracks(int[] animatIDs, TimeSpan startTime, TimeSpan duration)
        {

            var trackpoints = new List<EarthCoordinate3D>[animatIDs.Length];

            foreach (var curTimeRecord in AllRecordsBetween(startTime, startTime + duration))
            {
                for (var i = 0; i < animatIDs.Length; i++)
                {
                    if (trackpoints[i] == null)
                        trackpoints[i] = new List<EarthCoordinate3D>();
                    trackpoints[i].Add(curTimeRecord[animatIDs[i]]);
                }
            }
            return trackpoints.Select(track => new OverlayLineSegments(track.ToArray(), Colors.Red, 1, LineStyle.Solid));
        }
        /// <summary>
        /// Get position information about all animats at all times.
        /// </summary>
        /// <returns>Enumerable list of EarthCoordinate3D[], indended for use in a foreach.</returns>
        public IEnumerable<EarthCoordinate3D[]> AllRecordsBetween(TimeSpan startTime, TimeSpan endTime)
        {
            CheckCanRead();
            for (var now = startTime; now < endTime; now += _timeStepLength)
                yield return this[now];
        }

        public IEnumerable<EarthCoordinate3D[]> AllRecords
        {
            get
            {
                for (long i = 0; i < _totalTimeRecords; i++)
                    yield return this[i];
            }
        }

        /// <summary>
        /// adds an array of animatlocations of type earthcoordinate3D to the current animat location file.
        /// </summary>
        /// <param name="animatLocations"></param>
        public void AddTimeRecord(EarthCoordinate3D[] animatLocations)
        {
            CheckCanWrite();

            if (animatLocations.Length != _totalAnimatCount)
                throw new IndexOutOfRangeException("AnimatLocationFile: Animat count mismatch in AddTimeRecord()");

            _totalTimeRecords++;
            _writer.BaseStream.Seek(0, SeekOrigin.End);
            foreach (EarthCoordinate3D location in animatLocations)
            {
                _writer.Write(location.Latitude_degrees);
                _writer.Write(location.Longitude_degrees);
                _writer.Write(location.Elevation_meters);
            }
        }

        /// <summary>
        /// overloads addtimerecords to deal with 3mb output directly.
        /// </summary>
        /// <param name="animatLocations"></param>
        public void AddTimeRecord(mbsPosition[] animatLocations)
        {
            CheckCanWrite();

            if (animatLocations.Length != _totalAnimatCount)
                throw new IndexOutOfRangeException("AnimatLocationFile: Animat count mismatch in AddTimeRecord()");

            _totalTimeRecords++;
            _writer.BaseStream.Seek(0, SeekOrigin.End);
            foreach (mbsPosition location in animatLocations)
            {
                _writer.Write(location.latitude);
                _writer.Write(location.longitude);
                _writer.Write(-location.depth);
            }
        }
        

        /// <summary>
        /// test code to dump basic header information to the console. 
        /// </summary>
        public override string ToString()
        {
            CheckCanRead();
            var sb = new StringBuilder();
            sb.Append(String.Format("Total number of Animats: {0}\n", _totalAnimatCount));
            sb.Append(String.Format("Total number of time records: {0}\n", _totalTimeRecords));
            sb.Append(String.Format("Simulation time resolution was {0}.\n", _timeStepLength));
            sb.Append(String.Format("Simulation Duration was {0}.\n", Duration));
            foreach (SpeciesDescriptor s in SpeciesDescriptors)
            {
                sb.Append(String.Format("Species {0} contained {1} animats\n", s.SpeciesName, s.AnimatCount));
            }
            return sb.ToString();
        }

        public List<SpeciesDescriptor> SpeciesDescriptors
        {
            get; private set;
        }

        public void TestDataSearchers()
        {
            var r = new Random((int)DateTime.Now.Ticks);

            Console.WriteLine(@"Testing AllRecords...");
            if (AllRecords.Count() == _totalTimeRecords)
                Console.WriteLine(@" AllRecords() returned the same number of elements as totalTimeRecords.  PASS.");
            else
                Console.WriteLine(@" AllRecords() returned {0} but the value of totalTimeRecords is {1}",
                                  AllRecords.Count(), _totalTimeRecords);

            Console.WriteLine(@"Testing this.speciesName accessor...");
            try
            {
                Console.WriteLine(@"--asking for a species that does not exist.");
                IEnumerable<EarthCoordinate3D[]> badspecies = this["species that doesn't exist"];
                Console.WriteLine(badspecies.Count()); //this will chuck an exception.                
            }
            catch (ApplicationException e)
            {
                Console.WriteLine(@"---" + e.Message);
                Console.WriteLine(@"---Congrats, a known-false species doesn't exist and we know about it and threw an error. PASS.");
            }

            IEnumerable<EarthCoordinate3D[]> goodspecies = this["generic_odontocete_3"];
            if (goodspecies.Count() == _totalTimeRecords)
                Console.WriteLine(@"--A known-good species was found and has the correct number of time records in it. PASS.");
            else
                Console.WriteLine(@"--a known good species was found but has {0} time records in it when it ought to have {1}. FAIL.",
                    goodspecies.Count(), _totalTimeRecords);

            Console.WriteLine(@"Testing timespan versus index this. accessors at a random index value...");

            long tmp = r.Next(0, _totalTimeRecords / (int)_timeStepLength.TotalSeconds);
            EarthCoordinate3D[] indexed = this[tmp];
            EarthCoordinate3D[] timestepped = this[TimeSpan.FromSeconds(tmp * _timeStepLength.TotalSeconds)];
            bool fail = false;
            if (indexed.Where((t, i) => !t.Equals(timestepped[i])).Any()) {
                Console.WriteLine(@"--index and timespan accessors return inconsistent records. FAIL.");
                fail = true;
            }
            if (!fail)
                Console.WriteLine(@"--index {0} equals the {1}th second value (timestep: {2}). PASS.", tmp,
                                  tmp * _timeStepLength.TotalSeconds + _timeStepLength.TotalSeconds, _timeStepLength);

#if false
            Console.WriteLine("Testing Species/TimeRange accessor with a random known species and random time range... ");


            int tmptmp = r.Next(10, (int)_totalTimeRecords);


            int rtmp = r.Next(0, SpeciesDescriptors.Count);
            string randomSpecies = SpeciesDescriptors[rtmp].SpeciesName;

            // var timespecies = TimeSpeciesRange(DateTime.Now, DateTime.Now + TimeSpan.FromSeconds(tmptmp*_timeStepLength.TotalSeconds + _timeStepLength.TotalSeconds), randomSpecies);

            var timespecies = TimeSpeciesRange(DateTime.Now, DateTime.Now + TimeSpan.FromSeconds(tmptmp), randomSpecies);

            if (timespecies.Count() == Math.Floor((tmptmp + _timeStepLength.TotalSeconds) / _timeStepLength.TotalSeconds))//Math.Ceiling(tmptmp + _timeStepLength.TotalSeconds))
                Console.WriteLine("--{0} time records were requested from species {1} and {2} were recieved.  PASS.", Math.Floor(((tmptmp + _timeStepLength.TotalSeconds) / _timeStepLength.TotalSeconds)), randomSpecies, timespecies.Count());
            else
                Console.WriteLine("--{0} time records were requested from species {1} but {2} were recieved. FAIL.", Math.Floor(((tmptmp + _timeStepLength.TotalSeconds) / _timeStepLength.TotalSeconds)), randomSpecies, timespecies.Count());

#endif
        }
        #endregion

        #region Private utility methods

        /// <summary>
        /// 
        /// </summary>
        private void WriteHeader()
        {
            CheckCanWrite();

            _writer.BaseStream.Position = 0;
            _writer.Write(Magic);
            _writer.Write(_totalAnimatCount);
            _writer.Write(_totalTimeRecords);
            _writer.Write(_timeStepLength.Ticks);
            _writer.Write(Duration.Ticks);
            _writer.Write(SpeciesDescriptors.Count());
            foreach (SpeciesDescriptor species in SpeciesDescriptors)
            {
                species.Write(_writer);
            }

            _sizeOfHeader = _writer.BaseStream.Position;
        }

        /// <summary>
        /// Reads the header of the open AnimatLocationFile. w
        /// </summary>
        private void ReadHeader()
        {
            CheckCanRead();

            _reader.BaseStream.Position = 0;
            if (_reader.ReadUInt32() != Magic)
                throw new FormatException("Attempted to read invalid animat track data");
            _totalAnimatCount = _reader.ReadInt32(); // read total animat count
            _totalTimeRecords = _reader.ReadInt32(); //read total time records
            _timeStepLength = new TimeSpan(_reader.ReadInt64());//read timesteplength. 
            Duration = new TimeSpan(_reader.ReadInt64());//read simulation duration
            var speciesCount = _reader.ReadInt32(); //read species count
            SpeciesDescriptors = new List<SpeciesDescriptor>();
            for (int i = 0; i < speciesCount; i++)
                SpeciesDescriptors.Add(new SpeciesDescriptor(_reader));

            _sizeOfHeader = _reader.BaseStream.Position;
        }

        /// <summary>
        /// 
        /// </summary>
        private void CheckCanWrite()
        {
            if ((_writer == null) || (!_writer.BaseStream.CanWrite))
                throw new ApplicationException("AnimatLocationFile: Cannot modify this file!");
        }

        /// <summary>
        /// 
        /// </summary>
        private void CheckCanRead()
        {
            if ((_reader == null) || (!_reader.BaseStream.CanRead))
                throw new ApplicationException("AnimatLocationFile: Do not have Read Access to this file!");
        }

        #endregion

        #region Private constructors

        private AnimatLocationFile()
        {
            SpeciesDescriptors = new List<SpeciesDescriptor>();
        }

        #endregion

        #region private data members
        private const UInt32 Magic = 0x6074d582;
        private BinaryReader _reader;
        private long _sizeOfHeader;
        private TimeSpan _timeStepLength;
        private int _totalAnimatCount;
        private int _totalTimeRecords;
        private BinaryWriter _writer; 
        #endregion
    }

    public class SpeciesDescriptor
    {
        public SpeciesDescriptor ()
        {
        }

        public SpeciesDescriptor(BinaryReader reader)
        {
            AnimatCount = reader.ReadInt32();
            SpeciesName = reader.ReadString();
        }

        public int AnimatCount { get; private set; }
        public string SpeciesName { get; private set; }

        public Species Species
        {
            set
            {
                AnimatCount = value.ReferenceCount;
                SpeciesName = value.SpeciesName;
            }
        }

        public void Write(BinaryWriter writer)
        {
            writer.Write(AnimatCount);
            writer.Write(SpeciesName);
        }
    }
}