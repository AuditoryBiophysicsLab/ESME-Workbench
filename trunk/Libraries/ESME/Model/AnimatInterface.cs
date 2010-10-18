using System;
using System.IO;
using System.Threading;
using ESME.Environment;
using ESME.Overlay;
using mbs;

namespace ESME.Model
{
    public class AnimatInterface
    {
        #region public properties

        public AnimatList AnimatList { get; set; }

        public TimeSpan TimeStep
        {
            get { return _timeStep; }

            set
            {
                if (value.TotalSeconds != Math.Floor(value.TotalSeconds)) throw new AnimatInterfaceConfigurationException("AnimatInterface:Timestep; Timestep must be an integral number of seconds");
                if (value.TotalSeconds == 0) throw new AnimatInterfaceConfigurationException("AnimatInterface:Timestep; Timestep must not be zero seconds long.");
                _timeStep = value;
            }
        }

        public TimeSpan SimulationDuration
        {
            get { return _simulationDuration; }
            set
            {
                if (value.TotalSeconds != Math.Floor(value.TotalSeconds)) throw new AnimatInterfaceConfigurationException("AnimatInterface:SimulationDuration; simulation duration must be an integral number of seconds in length");

                _simulationDuration = value;
            }
        }

        public string AnimatLogFilePath { get; set; }

        public Environment2DData Bathymetry { get; set; }

        #endregion

        #region public methods

        /// <summary>
        /// Invokes necessary sanity checks to determine if AnimatInterface has been properly made ready.  Brings 3MB online and ready to start calculating animat tracks.  Creates a new AnimatLocationFile.
        /// </summary>
        public void InitializeAnimatSimulator()
        {
            StartupCheck();

            #region 3mb Init

#if false
            mbsRESULT mbResult = mbsRESULT.OK;
            mbsCONFIG mbConfig;
            mbsRUNSTATE mbRunState;
            numAnimatsPerInstance = AnimatList.Count / System.Environment.ProcessorCount;//todo
            //number of threads = proc count 
            AnimatList.SpeciesList.
            // par
            for (int i = 0; i < _mmmbsArray.Length && mbsRESULT.OK == mbResult; i++)
            {
                //----------------------------------------------------------------------//
                // Instantiate a 3mb
                //-------------------//
                _mmmbsArray[i] = new C3mbs();

                //----------------------------------------------------------------------//
                // Configure the scenario
                //------------------------//
                mbConfig = _mmmbsArray[i].GetConfiguration();
                // Probably wise to set each 3mb instance to a unique randomizer seed value.
                mbConfig.seedValue = (uint)i;

                // ESME should set enabled to false and durationless to TRUE.
                mbConfig.enabled = false; // binary output enabled/disabled
                mbConfig.durationLess = true; // ESME runs 3mb in a durationless mode.
                _mmmbsArray[i].SetConfiguration(mbConfig);


                //----------------------------------------------------------------------//
                // Load bathymetry file
                //----------------------//
                //  use either: LoadBathymetryFromTextFile() or
                //              LoadFromBinFile())
                // For development and testing it is OK to instead use:
                //              _mmmbsArray[i].SetBaythyConstantDepth(-5000.0);
                mbResult = _mmmbsArray[i].LoadFromBinFile(Bathymetry.Filename);
                if (mbsRESULT.OK != mbResult) throw new AnimatInterfaceMMBSException("Bathymetry failed to load: " + _mmmbsArray[i].MbsResultToString(mbResult));


                //----------------------------------------------------------------------//
                // Add a species to the scenario
                //-------------------------------//
                //add each species
                for (int j = 0; j < AnimatList.SpeciesList.Count; j++)
                {
                    AnimatList.SpeciesList[j].Index = j;
                    mbResult = _mmmbs.AddSpecies(AnimatList.SpeciesList[j].Filename);
                    if (mbsRESULT.OK != mbResult) throw new AnimatInterfaceMMBSException("C3mbs::AddSpecies FATAL error " + _mmmbsArray[i].MbsResultToString(mbResult));
                }

                //?????????????????????????????????????????????????????????????
                // Set the scenario's duration.
                _mmmbsArray[i].SetDuration((int)_simulationDuration.TotalSeconds);

                //----------------------------------------------------------------------//
                // Add animats to the species
                //----------------------------//
                //add all the animats of each species.

                // Allocate space for this 3mb intance's animat positional data.
                _parallelPosArray[i] = new mbsPosition[numAnimatsPerInstance];

                for (int index = 0; index < AnimatList.Count; index++)
                {
                    _posArray[index] = new mbsPosition//todo
                                       {
                                           latitude = AnimatList[index].Location.Latitude_degrees,
                                           longitude = AnimatList[index].Location.Longitude_degrees,
                                           depth = -AnimatList[index].Location.Elevation_meters
                                       };

                    mbResult = _mmmbs.AddIndividualAnimat(AnimatList[index].Species.Index, _posArray[index]);
                    if (mbsRESULT.OK != mbResult) throw new AnimatInterfaceMMBSException("C3mbs::AddSpecies FATAL error " + _mmmbsArray[i].MbsResultToString(mbResult));
                }

                //----------------------------------------------------------------------//
                // Initialize the scenario.
                //----------------------------//
                mbResult = _mmmbsArray[i].InitializeRun();
                if (mbsRESULT.OK == mbResult)
                {
                    // Wait for reach 3mb instance to finish initialzing (run state wont
                    // be mbsRUNSTATE.INITIALIZING) before initializing the instance.
                    do
                    {
                        Thread.Sleep(50);
                        mbRunState = _mmmbsArray[i].GetRunState();
                    } while (mbsRUNSTATE.INITIALIZING == mbRunState);
                }
                else
                {
                    throw new AnimatInterfaceMMBSException("C3mbs::Initialize FATAL error " + _mmmbsArray[i].MbsResultToString(mbResult));
                }


            } 


#else
            //single-threaded code follows.  Known to work with (pre october 2010) 3mb DLLs, compiles on new dlls. 
            mbsRESULT result;

            _posArray = new mbsPosition[AnimatList.Count];
            _mbsBathymetry = new double[AnimatList.Count];
            _mbsSoundExposure = new double[AnimatList.Count];

            //add each species
            for (int i = 0; i < AnimatList.SpeciesList.Count; i++)
            {
                AnimatList.SpeciesList[i].Index = i;
                result = _mmmbs.AddSpecies(AnimatList.SpeciesList[i].Filename);
                if (mbsRESULT.OK != result) throw new AnimatInterfaceMMBSException("C3mbs::AddSpecies FATAL error " + _mmmbs.MbsResultToString(result));
            }
            //add all the animats of each species.
            for (int i = 0; i < AnimatList.Count; i++)
            {
                _posArray[i] = new mbsPosition
                               {
                                   latitude = AnimatList[i].Location.Latitude_degrees,
                                   longitude = AnimatList[i].Location.Longitude_degrees,
                                   depth = -AnimatList[i].Location.Elevation_meters
                               };

                result = _mmmbs.AddIndividualAnimat(AnimatList[i].Species.Index, _posArray[i]);
                if (mbsRESULT.OK != result) throw new AnimatInterfaceMMBSException("C3mbs::AddIndividualAnimat FATAL error " + _mmmbs.MbsResultToString(result));
            }
            //logs the 0th position 
            //   _animatLocationFile.AddTimeRecords(_posArray);

            _mmmbs.SetDuration((int)_simulationDuration.TotalSeconds); //is this what we really want to do?

            UpdateAnimatBathymetry();

            if (mbsRESULT.OK != (result = _mmmbs.RunScenarioNumIterations(0)))
            {
                throw new AnimatInterfaceMMBSException("C3mbs::RunScenarioNumIterations FATAL error " + _mmmbs.MbsResultToString(result));
            }

            while (_mmmbs.GetRunState() == mbsRUNSTATE.INITIALIZING)
            {
                Thread.Sleep(1);
            }
            //set each animat's starting position bathymetry data.  
#endif

            #endregion

            #region animat location file

            _animatLocationFile = AnimatLocationFile.AnimatLocationFile.Create(AnimatLogFilePath, _timeStep, _simulationDuration, AnimatList);

            #endregion

            UpdatePositions();
            _secondsRemaining = (int) _simulationDuration.TotalSeconds;
            _beenInitialized = true;
        }

        /// <summary>
        /// Intended to be called within an external loop. Increments 3mb by one timestep, calculates and logs new animat positions. 
        /// </summary>
        /// <returns>true if more timesteps are available.  false if no more positions need to be calculated. </returns>
        public bool Step()
        {
            var stepTime = (int) _timeStep.TotalSeconds;


            if (_beenInitialized != true) throw new AnimatInterfaceConfigurationException("AnimatInterface:Step; Cannot call animat simulator without initializing first.");
            if (_secondsRemaining > stepTime)
            {
                Page3MB(stepTime);
                UpdatePositions();
                UpdateAnimatSoundExposure();
                UpdateAnimatBathymetry();

                _secondsElapsed += stepTime;
                _secondsRemaining -= stepTime;
                return true;
            }
            return false;
        }

        /// <summary>
        /// shuts it down, and closes the log file.
        /// </summary>
        public void Close()
        {
            //3mb

            //animat location file
            _animatLocationFile.Close();
        }
        /// <summary>
        /// stub.  
        /// </summary>
        /// <param name="popArea"></param>
        /// <param name="numAnimats"></param>
        /// <param name="speciesName"></param>
        public void Populate(OverlayLineSegments popArea, int numAnimats, string speciesName) //todo: add current population code here and hit test. 
        { }

        /// <summary>
        /// will return a fully populated, initialized, and paused animatInterface.
        /// </summary>
        /// <param name="animatScenarioFile"></param>
        /// <param name="mmmbsOutputDirectory"></param>
        /// <returns></returns>
       // public AnimatInterface Create(string animatScenarioFile, string mmmbsOutputDirectory)
      //  {
            
       // }
        #endregion

        #region private methods

        /// <summary>
        /// sleeps 1ms, then runs 3mb once.  chucks exceptions and aborts 3mb if bad stuff happens.
        /// </summary>
        /// <param name="stepTime"></param>
        void Page3MB(int stepTime)
        {
            mbsRESULT result = _mmmbs.RunScenarioNumIterations(stepTime);
            if (result != mbsRESULT.OK)
            {
                _mmmbs.AbortRun();
                throw new AnimatInterfaceMMBSException("C3mbs::RunScenarioNumIterations FATAL error: " + _mmmbs.MbsResultToString(result));
            }
            do
            {
                Thread.Sleep(1);
            } while (mbsRUNSTATE.RUNNING == _mmmbs.GetRunState());
        }

        /// <summary>
        /// Sanity checking.  Makes sure species, animats, and sim time exist and are nonzero.
        /// </summary>
        void StartupCheck()
        {
            //check species list, animat list, & for null length

            if (AnimatList.SpeciesList.Count == 0) throw new AnimatInterfaceConfigurationException("AnimatInterface:StartupCheck; One or more species must be present in the species list.");
            if (AnimatList.Count == 0) throw new AnimatInterfaceConfigurationException("AnimatInterface:StartupCheck; One or more Animats must be present in the animat list.");
            if (_simulationDuration.TotalSeconds == 0) throw new AnimatInterfaceConfigurationException("AnimatInterface:StartupCheck; cannot run simulator for zero time");
            if (_timeStep.TotalSeconds == 0) throw new AnimatInterfaceConfigurationException("AnimatInterface:StartupCheck; Cannot run simulator at zero time update intervals");
            if (Directory.Exists(Path.GetDirectoryName(AnimatLogFilePath))) throw new AnimatInterfaceConfigurationException("AnimatInterface: Animat Log File Path is not set.");
            if (Bathymetry == null) throw new AnimatInterfaceConfigurationException("");
        }

        void UpdateAnimatSoundExposure()
        {
            mbsRESULT result;

            const double curSPL = 0.0;
            const double lat = 0.0;
            const double lon = 0.0;

            for (int i = 0; i < AnimatList.Count; i++) _mbsSoundExposure[i] = curSPL;

            if (mbsRESULT.OK != (result = _mmmbs.SetAnimatAcousticExposure(lat, lon, _mbsSoundExposure)))
            {
                _mmmbs.AbortRun();
                throw new AnimatInterfaceMMBSException("C3mbs::SetAnimatAcousticExposure FATAL error " + _mmmbs.MbsResultToString(result));
            }
        }


        /// <summary>
        /// must be run after page3MB to make any sense.  Requests current animat state from 3mb, dumps this into a new time record in a preexisting animatlocationfile. 
        /// </summary>
        void UpdatePositions()
        {
            mbsRESULT result;

            if (mbsRESULT.OK != (result = _mmmbs.GetAnimatCoordinates(_posArray)))
            {
                _mmmbs.AbortRun(); // kills the thread.
                throw new AnimatInterfaceMMBSException("C3mbs::GetAnimatCoordinates FATAL error " + _mmmbs.MbsResultToString(result));
            }

            _animatLocationFile.AddTimeRecord(_posArray);
        }

        /// <summary>
        /// updates bathymetry
        /// </summary>
        void UpdateAnimatBathymetry()
        {
            Animat curAnimat;
            mbsRESULT result;

            for (int i = 0; i < AnimatList.Count; i++)
            {
                curAnimat = AnimatList[i];
                // Make sure the animat is still contained in the current bathymetry dataset, and if so get the depth at the animat's current position
                float bathymetryDepthMeters;
                bool animatIsWithinBathymetry = Bathymetry.Lookup(curAnimat.Location, out bathymetryDepthMeters);

                if (animatIsWithinBathymetry)
                {
                    // Update the depth at the animat's current position
                    _mbsBathymetry[i] = bathymetryDepthMeters;
                }
            } // for (animats)
            if (mbsRESULT.OK != (result = _mmmbs.SetAnimatBathymetry(_mbsBathymetry)))
            {
                //System.Diagnostics.Debug.WriteLine("SetAnimatBathymetry FAILED: " + C3mbs.MbsResultToString(result));
                _mmmbs.AbortRun(); // kills the thread.
                throw new AnimatInterfaceMMBSException("C3mbs::SetAnimatBathymetry FATAL error " + _mmmbs.MbsResultToString(result));
            }
        }

        #endregion

        #region private data members

        readonly C3mbs _mmmbs = new C3mbs();
        readonly C3mbs[] _mmmbsArray = new C3mbs[System.Environment.ProcessorCount];//todo
        AnimatLocationFile.AnimatLocationFile _animatLocationFile;
        bool _beenInitialized;
        double[] _mbsBathymetry;
        double[] _mbsSoundExposure;
        mbsPosition[][] _parallelPosArray;
        mbsPosition[] _posArray;

        int _secondsElapsed,
            _secondsRemaining;

        TimeSpan _simulationDuration = TimeSpan.Zero;
        TimeSpan _timeStep = TimeSpan.Zero;

        int numAnimatsPerInstance;

        #endregion
    }
}