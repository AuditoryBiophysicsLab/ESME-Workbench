using System;
using System.Threading;
using ESME.Environment;
using ESME.Overlay;
using mbs;

namespace ESME.Model
{
    public class AnimatInterface
    {
        #region public variables

        public AnimatList AnimatList { get; set; }

        public TimeSpan TimeStep
        {
            get { return _timeStep; }

            set
            {
                if (value.TotalSeconds != Math.Floor(value.TotalSeconds))
                    throw new ApplicationException("AnimatInterface:Timestep; Timestep must be an integral number of seconds");
                if (value.TotalSeconds == 0)
                    throw new ApplicationException("AnimatInterface:Timestep; Timestep must not be zero seconds long.");
                _timeStep = value;
            }
        }

        public TimeSpan SimulationDuration
        {
            get { return _simulationDuration; }
            set
            {
                if (value.TotalSeconds != Math.Floor(value.TotalSeconds))
                    throw new ApplicationException("AnimatInterface:SimulationDuration; simulation duration must be an integral number of seconds in length");

                _simulationDuration = value;
            }
        }

        public string AnimatLogFilePath { get; set; }
        public Environment2DData Environment2DData { get; set; }

        #endregion

        #region public methods

        /// <summary>
        /// Invokes necessary sanity checks to determine if AnimatInterface has been properly made ready.  Brings 3MB online and ready to start calculating animat tracks.  Creates a new AnimatLocationFile.
        /// </summary>
        public void InitializeAnimatSimulator()
        {
            StartupCheck();

            #region 3mb

            mbsRESULT result;
            //add each species
            _posArray = new mbsPosition[AnimatList.Count];
            _mbsBathymetry = new double[AnimatList.Count];
            _mbsSoundExposure = new double[AnimatList.Count];

            for (int i = 0; i < AnimatList.SpeciesList.Count; i++)
            {
                AnimatList.SpeciesList[i].Index = i;
                result = _mmmbs.AddSpecies(AnimatList.SpeciesList[i].Filename);
                if (mbsRESULT.OK != result)
                    throw new ApplicationException("C3mbs::AddSpecies FATAL error " + C3mbs.MbsResultToString(result));
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
                if (mbsRESULT.OK != result)
                    throw new ApplicationException("C3mbs::AddIndividualAnimat FATAL error " +
                                                   C3mbs.MbsResultToString(result));
            }
            //logs the 0th position 
            //   _animatLocationFile.AddTimeRecords(_posArray);

            _mmmbs.SetDuration((int) _simulationDuration.TotalSeconds); //is this what we really want to do?

            UpdateAnimatBathymetry();

            if (mbsRESULT.OK != (result = _mmmbs.RunScenarioNumIterations(0)))
            {
                throw new ApplicationException("C3mbs::RunScenarioNumIterations FATAL error " +
                                               C3mbs.MbsResultToString(result));
            }

            while (_mmmbs.GetRunState() == mbsRUNSTATE.INITIALIZING)
            {
                Thread.Sleep(1);
            }
            //set each animat's starting position bathymetry data.

            #endregion

            #region animat location file

            _animatLocationFile = AnimatLocationFile.AnimatLocationFile.Create(AnimatLogFilePath, _timeStep,
                                                                               _simulationDuration, AnimatList);

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


            if (_beenInitialized != true)
                throw new ApplicationException("AnimatInterface:Step; Cannot call animat simulator without initializing first.");
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


            //old step code
#if false
		             mbsRESULT result;
                        Animat curAnimat;

                    
                        if (mbsRESULT.OK != (result = _mmmbs.RunScenarioNumIterations(SecondsSonarOn)))
                        {
                            //System.Diagnostics.Debug.WriteLine("RunScenarioNumIterations(1) FAILED: " + C3mbs.MbsResultToString(result));
                            _mmmbs.AbortRun();
                            throw new ApplicationException("C3mbs::RunScenarioNumIterations FATAL error: " + C3mbs.MbsResultToString(result));
                        
                        }
                        do
                        {
                            
                            Thread.Sleep(1);
                            //Application.DoEvents();
                            
                        } while (mbsRUNSTATE.RUNNING == _mmmbs.GetRunState());

                        simulationSecondsElapsed += SecondsSonarOn;
                        simulationSecondsRemaining -= SecondsSonarOn;

                        if (mbsRESULT.OK != (result = _mmmbs.RunScenarioNumIterations(SecondsSonarOff)))
                        {
                            //System.Diagnostics.Debug.WriteLine("RunScenarioNumIterations(1) FAILED: " + C3mbs.MbsResultToString(result));
                            _mmmbs.AbortRun();
                            throw new ApplicationException("C3mbs::RunScenarioNumIterations FATAL error: " + C3mbs.MbsResultToString(result));
                            
                        }
                        do
                        {
                            //System.Diagnostics.Debug.WriteLine(mmmbs.GetRunState().ToString());
                            Thread.Sleep(1);
                            //Application.DoEvents();
                        } while (mbsRUNSTATE.RUNNING == _mmmbs.GetRunState());

                        simulationSecondsElapsed += SecondsSonarOff;
                        simulationSecondsRemaining -= SecondsSonarOff;
                        
                        
                        }

#endif
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

        public void Populate(OverlayLineSegments popArea, int numAnimats, string speciesName)//todo: add current population code here and hit test. 
        {
            
        }

        #endregion

        #region private methods

        /// <summary>
        /// sleeps 1ms, then runs 3mb once.  chucks exceptions and aborts 3mb if bad stuff happens.
        /// </summary>
        /// <param name="stepTime"></param>
        private void Page3MB(int stepTime)
        {
            mbsRESULT result = _mmmbs.RunScenarioNumIterations(stepTime);
            if (result != mbsRESULT.OK)
            {
                _mmmbs.AbortRun();
                throw new ApplicationException("C3mbs::RunScenarioNumIterations FATAL error: " +
                                               C3mbs.MbsResultToString(result));
            }
            do
            {
                Thread.Sleep(1);
            } while (mbsRUNSTATE.RUNNING == _mmmbs.GetRunState());
        }

        /// <summary>
        /// Sanity checking.  Makes sure species, animats, and sim time exist and are nonzero.
        /// </summary>
        private void StartupCheck()
        {
            //check species list, animat list, & for null length

            if (AnimatList.SpeciesList.Count == 0)
                throw new ApplicationException("AnimatInterface:StartupCheck; One or more species must be present in the species list.");
            if (AnimatList.Count == 0)
                throw new ApplicationException("AnimatInterface:StartupCheck; One or more Animats must be present in the animat list.");
            if (_simulationDuration.TotalSeconds == 0)
                throw new ApplicationException("AnimatInterface:StartupCheck; cannot run simulator for zero time");
            if(_timeStep.TotalSeconds == 0)
                throw new ApplicationException("AnimatInterface:StartupCheck; Cannot run simulator at zero time update intervals");
        }

        private void UpdateAnimatSoundExposure()
        {
            mbsRESULT result;

            const double curSPL = 0.0;
            const double lat = 0.0;
            const double lon = 0.0;

            for (int i = 0; i < AnimatList.Count; i++)
                _mbsSoundExposure[i] = curSPL;

            if (mbsRESULT.OK != (result = _mmmbs.SetAnimatAcousticExposure(lat, lon, _mbsSoundExposure)))
            {
                _mmmbs.AbortRun();
                throw new ApplicationException("C3mbs::SetAnimatAcousticExposure FATAL error " +
                                               C3mbs.MbsResultToString(result));
            }
        }


        /// <summary>
        /// must be run after page3MB to make any sense.  Requests current animat state from 3mb, dumps this into a new time record in a preexisting animatlocationfile. 
        /// </summary>
        private void UpdatePositions()
        {
            mbsRESULT result;

            if (mbsRESULT.OK != (result = _mmmbs.GetAnimatCoordinates(_posArray)))
            {
                _mmmbs.AbortRun(); // kills the thread.
                throw new ApplicationException("C3mbs::GetAnimatCoordinates FATAL error " +
                                               C3mbs.MbsResultToString(result));
            }
            
            _animatLocationFile.AddTimeRecord(_posArray);
        }

        /// <summary>
        /// updates bathymetry
        /// </summary>
        private void UpdateAnimatBathymetry()
        {
            Animat curAnimat;
            mbsRESULT result;

            for (int i = 0; i < AnimatList.Count; i++)
            {
                curAnimat = AnimatList[i];
                // Make sure the animat is still contained in the current bathymetry dataset, and if so get the depth at the animat's current position
                float bathymetryDepthMeters;
                bool animatIsWithinBathymetry = Environment2DData.Lookup(curAnimat.Location, out bathymetryDepthMeters);

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
                throw new ApplicationException("C3mbs::SetAnimatBathymetry FATAL error " +
                                               C3mbs.MbsResultToString(result));
            }
        }

        #endregion

        #region private data members

        private readonly C3mbs _mmmbs = new C3mbs();
        private AnimatLocationFile.AnimatLocationFile _animatLocationFile;
        private bool _beenInitialized;
        private double[] _mbsBathymetry;
        private double[] _mbsSoundExposure;
        private mbsPosition[] _posArray;
        private int _secondsElapsed, _secondsRemaining;
        private TimeSpan _simulationDuration = TimeSpan.Zero;
        private TimeSpan _timeStep = TimeSpan.Zero;

        #endregion
    }
}