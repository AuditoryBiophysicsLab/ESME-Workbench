using System;
using System.Collections;
using System.Text;
using System.Diagnostics;
using mbs;  // mmmb C++ wrapper class code code and data types.


// 1111111 222222222 333333333 444444444 555555555 666666666 777777777 888888888 999999999 000000000 111 11 1122
namespace MMMBSLib
{
    // C3mbsSpeciesModel interfaces with the wrapper class so requires the "using mbs" directive.
    public class C3mbSpeciesModel
    {
        //-------------//
        // Class Fields
        //-------------//
        uint m_mbsLibSavedSuperVer;
        uint m_mbsLibSavedSubVer;
        uint m_speciesMdlSavedSuperVer;
        uint m_speciesMdlSavedSubVer;
        Boolean m_needSave = false;

        C3mbsWrapperSpeciesModel m_hereSoItWorks; // needed so the functions work
            // on 64 bit (or just Vista... or just current version of Vista... or..?).
            // Works fine without this on Windows XP.
        
        string m_shortDescription = "";
        string m_longComment = "";

        int m_groupIndex = 0; // The species group index this species belongs to.  Determined by the species index.
        int m_speciesIndex = 97; // required... // This species index this species belongs to.

        int m_year;
        int m_month;
        int m_day;
        int m_hour;
        int m_min;
        int m_sec;
        int m_id;

        CAcousticAversion m_acAvrsn = new CAcousticAversion();
        CBehaviorPodFollow m_podFollow = new CBehaviorPodFollow(); // will need to be moved;
        double m_shoreFollow = MBSDEFAULTS.SHORE_FOLLOW_DEPTH;
        double m_seedMinDepth = MBSDEFAULTS.SEED_MIN_DEPTH;
        double m_seedingDepthLimit = MBSDEFAULTS.SEED_DEPTH_LIMIT;
        Boolean m_seedingDepthLimitEnabled = MBSDEFAULTS.SEED_DEPTH_LIMIT_ENABLED;
        double m_beachingDepth = MBSDEFAULTS.BEACHING_DEPTH;
        Boolean m_aePodBreaksApart = MBSDEFAULTS.AE_POD_BREAKS_UP;
        C3mbsWrapperSpeciesModel m_wrapper = new C3mbsWrapperSpeciesModel();

        // Behaviors
        CBehaviorListMgr m_behMgr = new CBehaviorListMgr();

        //CMatrix m_initialBehavior = new CMatrix(MBSDEFAULTS.BEHAVIOR_INITIAL_MATRIX);
        //mbsBEHTRAN[] m_ib = new mbsBEHTRAN[1];

        // Initial Behavior
        CInitialBehaviorSpanMgr m_initBehSpanMgr;

        //------------------------//
        // Static Class Properties
        //------------------------//
        public int NumDefinedGroups { get { return m_wrapper.GetSpeciesGroupCount(); } }

        //---------------------//
        // Static Class Methods
        //---------------------//
        public int GetSpeciesNameListCount()
        {
            return m_wrapper.GetSpeciesNameListCount();
        }
        public string GetSpeciesLatinNameString(int Index)
        {
            return m_wrapper.GetSpeciesLatinNameString(Index);
        }
        public string GetSpeciesEnglishNameString(int Index)
        {
            return m_wrapper.GetSpeciesEnglishNameString(Index);
        }
        public string GetSpeciesGroupString(int GroupType)
        {
            return m_wrapper.GetSpeciesGroupString(GroupType);
        }


        //------------------------//
        // Member Class Properties
        //------------------------//
        public Boolean NeedSave {get {return m_needSave;}}
        public int GroupIndex { get { return m_groupIndex; }}
        public string GroupString { get { return m_wrapper.GetSpeciesGroupString(m_groupIndex); } }
        public int SpeciesIndex
        {
            get { return m_speciesIndex; }

            // The layout here must match the order in the CScenario.cpp constructor.  A
            // function that does the mapping through the wrapper interface would be better.
            set
            {
                int inputValue = CUtil.MinXOrX(0, CUtil.MaxXOrX(199, value));
                m_speciesIndex = inputValue;
                m_needSave = true;

                if(m_speciesIndex <= 29)        // ODONTOCETES (HF SPECIALIST)(index 1)
                    m_groupIndex = 1;
                else if(m_speciesIndex <= 96)   // ODONTOCETES (MF SPECIALIST) (index 2)
                    m_groupIndex = 2;
                else if(m_speciesIndex <= 115)  // MYSTICETES (LF SPECIALISTS) (index 0)
                    m_groupIndex = 0;
                else if(m_speciesIndex <= 130)  // PINNIPED (PHOCID) Subfamily PHOCINAE (index 4)
                    m_groupIndex = 4;
                else if(m_speciesIndex <= 140)  // PINNIPED (PHOCID) Subfamily MONACHINAE (index 3)
                    m_groupIndex = 3;
                else if(m_speciesIndex <= 161)  // PINNIPED (OTARRID) (index 5)
                    m_groupIndex = 5;
                else if(m_speciesIndex <= 164)  // SPECIAL CONSIDERATIONS (index 8)
                    m_groupIndex = 8;
                else if(m_speciesIndex <= 182)  // OTHER MARINE MAMMALS (index 6)
                    m_groupIndex = 6;
                else if(m_speciesIndex <= 192)  // SEA TURTLES	(index 7)
                    m_groupIndex = 7;
                else if(m_speciesIndex <= 199)  // SOUND SOURCES
                    m_groupIndex = 9;
            }
        }

        public uint mbsSavedSuperVer { get { return m_mbsLibSavedSuperVer; } }
        public uint mbsSavedSubVer { get { return m_mbsLibSavedSubVer; } }
        public uint speciesSavedSuperVer { get { return m_speciesMdlSavedSuperVer; } }
        public uint speciesSavedSubVer { get { return m_speciesMdlSavedSubVer; } }
        public uint speciesCurrentSuperVer { get { return m_wrapper.GetSpeSuperVersion(); } }
        public uint speciesCurrentSubVer { get { return m_wrapper.GetSpeSubVersion(); } }
        public int shortDescriptionMaxAllowedLenth
        {
            get { return m_wrapper.GetShortDescriptionAllowedLength(); }
        }
        public int longCommentMaxAllowedLenth
        {
            get { return m_wrapper.GetLongCommentAllowedLength(); }
        }
        public string szShortDescription
        { 
            get { return m_shortDescription;}
            set
            {
                if(value.Length != m_shortDescription.Length)
                    m_needSave = true;
                if(value.Length > shortDescriptionMaxAllowedLenth)
                    value = value.Substring(0, shortDescriptionMaxAllowedLenth-1);
                else if(value.Length == 0)
                    value = "";
                m_shortDescription = value;
            }
        }
        public string szLongComment
        {
            get { return m_longComment;}
            set
            {
                if(value.Length != m_longComment.Length)
                    m_needSave = true;
                if(value.Length > longCommentMaxAllowedLenth)
                    value = value.Substring(0, longCommentMaxAllowedLenth-1);
                else if(value.Length == 0)
                    value = "";
                m_longComment = value;
            }
        }
        public string szLatinSpeciesName
        {
            get { return m_wrapper.GetSpeciesLatinNameString(m_speciesIndex); }
        }
        public string szEnglishSpeciesName
        {
            get { return m_wrapper.GetSpeciesEnglishNameString(m_speciesIndex); }
        }
        public string szGroupSring { get { return m_wrapper.GetSpeciesGroupString(m_groupIndex); } }
        public int yearSaved { get { return m_year; } }
        public int monthSaved { get { return m_month; } }
        public int daySaved { get { return m_day; } }
        public int hourSaved { get { return m_hour; } }
        public int minSaved { get { return m_min; } }
        public int secSaved { get { return m_sec; } }
        public int semiUniqueId { get { return m_id; } }
        public string[] NameArray { get { return m_behMgr.NameArray; } }
        public CBehaviorPodFollow podFollowBehavior { get { return m_podFollow; } }
        public double shoreFollowDepth
        {
            get { return m_shoreFollow; }
            set
            {
                double inputValue = CUtil.MaxXOrX(MBSDEFAULTS.BEACHING_DEPTH, value);
                inputValue = CUtil.MinXOrX(m_seedMinDepth, inputValue);
                if(m_shoreFollow == inputValue)
                    return;

                m_shoreFollow = inputValue;
                m_needSave = true;
            }
        }
        public double seedMinDepth
        { 
            get { return m_seedMinDepth; }
            set
            {
                double inputValue = CUtil.MaxXOrX(m_shoreFollow, value);
                if(m_seedMinDepth == inputValue)
                    return;
                m_seedMinDepth = inputValue;
                m_needSave = true;
            }
        }
        public double seedingDepthLimit
        {
            get { return m_seedingDepthLimit; }
            set
            {
                double inputValue = CUtil.MaxXOrX(0, value);
                if(m_seedingDepthLimit == inputValue)
                    return;

                m_seedingDepthLimit = inputValue;
                m_needSave = true;
            }
        }
        public Boolean seedingDepthLimitEnabled
        {
            get{ return m_seedingDepthLimitEnabled; }
            set
            {
                if(m_seedingDepthLimitEnabled == value)
                    return;
                m_seedingDepthLimitEnabled = value;
                m_needSave = true;
            } 
        }
        public double beachingDepth { get { return m_beachingDepth; } }
        public Boolean podBreaksOnAE
        {
            get { return m_aePodBreaksApart; }
            set
            {
                m_needSave = true;
                m_aePodBreaksApart = value;
            }
        }

        public CInitialBehaviorSpanMgr InitBehSpanMgr
        {
            get { return m_initBehSpanMgr;}
            set { m_initBehSpanMgr = value; m_needSave = true; }
        }


/*
        public CMatrix InitialBehaviorMatrix
        {
            get { return m_initialBehavior; }
            set { m_initialBehavior = value; m_needSave = true; }
        }
*/
        public CAcousticAversion AcousticAversion
        {
            get { return m_acAvrsn; }
            set { m_needSave = true; m_acAvrsn = value; }
        }

        private Boolean BehaviorCountCheck
        {
            get
            {
                int i;
                int behMgrCnt, behaviorTransCnt;
                CSpan span;
                for(i=0; i<m_initBehSpanMgr.SpanCount; i++)
                {
                    span = m_initBehSpanMgr.GetSpan(i);
                    behaviorTransCnt = span.behaviorTransCount;
                    behMgrCnt = m_behMgr.Count;
                    if(behaviorTransCnt != m_behMgr.Count)
                        return false;
                }
                return true;
            }
        }
        public int BehaviorCount
        {
            get
            {
                Debug.Assert(BehaviorCountCheck == true);
                return m_behMgr.Count;
            }
        }

        //-------------------//
        // Class Constructors
        //-------------------//
        public C3mbSpeciesModel()
        {
            m_initBehSpanMgr = new CInitialBehaviorSpanMgr();
            m_initBehSpanMgr.AddSpan();
            m_initBehSpanMgr.SetSpan(0, 0, -3500);
            m_behMgr.AddBehavior(true);

            Debug.Assert(m_behMgr.Count == 1);
            Debug.Assert(BehaviorCountCheck == true);

        }

        //-----------------//
        // Member functions
        //-----------------//
        public void Touch() { m_needSave = true; }


        //------------------------------------//
        // Behavior Accessor Member Functions
        //------------------------------------//
        public CBehavior GetBehaviorCopy(int Index){ return m_behMgr.GetBehavior(Index).GetCopy(); }

        /* Returns the index of the added behavior */
        public int AddBehavior()
        {
            m_needSave = true;
            m_initBehSpanMgr.IncrementAllSpanTransitionBehaviorVectorElements();
            //m_initialBehavior.AddColumn(); // Update the intitial behavior matrix
            CBehavior b = m_behMgr.AddBehavior(true);
            return m_behMgr.Count-1;
        }

        public Boolean DeleteBehavior(int BehaviorIndex)
        {
            if(CUtil.CheckIndex(m_behMgr.Count, BehaviorIndex) == false) return false;
            m_needSave = true;

            // Must update the matrices before deleting the behavior for indexing reasons.
            //if(false == m_initialBehavior.DeleteColumn(BehaviorIndex+3)) { Debug.Assert(false); return false; }
            m_initBehSpanMgr.DecrementAllSpanBehaviorTransitionVectorElements(BehaviorIndex);

            // Now safe to delete the behavior
            if(false == m_behMgr.RemoveBehavior(BehaviorIndex)) { Debug.Assert(false); return false; }
            return true;
        }

        // Replaces a behavior at index.
        public Boolean SetBehavior(int Index, CBehavior Replacement)
        {
            if(true == m_behMgr.ReplaceBehavior(Index, Replacement))
            {
                m_needSave = true;
                return true;
            }
            return false;
        }


        //--------------------------//
        // File IO Member Functions
        //--------------------------//
        public mbsRESULT LoadFromBinFile(string FileName)
        {
            int i;
            int depthRange;
            mbsRESULT res = mbsRESULT.OK;
            CBehavior beh;
            mbsSPECIESMODEL speMdl = new mbsSPECIESMODEL(); // from the wrapper class project
            CSpan depthRangeMdl;

            if(mbsRESULT.OK != (res = m_wrapper.LoadFromBinFile(FileName, speMdl)))
                return res;

            m_needSave = false;


            ///////////////////////////////////////////////////////////////
            //----------------------//
            // Acoustic Aversion
            // 39 parameters
            //----------------------//
            m_acAvrsn.ascentEnabled = CUtil.IntToBoolean(speMdl.acousticAversion.ascentRateAffected);
            m_acAvrsn.ascent.coeff = speMdl.acousticAversion.ascentRate.coeff;
            m_acAvrsn.ascent.mean = speMdl.acousticAversion.ascentRate.mean;
            m_acAvrsn.ascent.std = speMdl.acousticAversion.ascentRate.std;
            m_acAvrsn.descentEnabled = CUtil.IntToBoolean(speMdl.acousticAversion.descentRateAffected);
            m_acAvrsn.descent.coeff = speMdl.acousticAversion.descentRate.coeff;
            m_acAvrsn.descent.mean = speMdl.acousticAversion.descentRate.mean;
            m_acAvrsn.descent.std = speMdl.acousticAversion.descentRate.std;
            m_acAvrsn.beachingEnabled = CUtil.IntToBoolean(speMdl.acousticAversion.beaches);
            m_beachingDepth = speMdl.acousticAversion.beachingDepth;
            //              = speMdl.acousticAversion.depth.coeff; // Not used by depth model.
            m_acAvrsn.depth.mean = speMdl.acousticAversion.depth.mean;
            m_acAvrsn.depth.std = speMdl.acousticAversion.depth.std;
            m_acAvrsn.depthEnabled = CUtil.IntToBoolean(speMdl.acousticAversion.depthAffected);
            m_acAvrsn.flatBottomDivingEnabled = CUtil.IntToBoolean(speMdl.acousticAversion.flatBottomDiveAffected);
            m_acAvrsn.flatBottomDives = CUtil.IntToBoolean(speMdl.acousticAversion.flatBottomDives);
            m_aePodBreaksApart = CUtil.IntToBoolean(speMdl.acousticAversion.podBreaksUp);
            //              = speMdl.acousticAversion.reversal.count.coeff;;// Not used by depth model.
            m_acAvrsn.reversal.meanCnt = speMdl.acousticAversion.reversal.count.mean;
            m_acAvrsn.reversal.stdCnt = speMdl.acousticAversion.reversal.count.std;
            m_acAvrsn.reversal.prob = speMdl.acousticAversion.reversal.probOfReversal;
            //              = speMdl.acousticAversion.reversal.time.coeff;-1;// Not used by depth model
            m_acAvrsn.reversal.meanTime = speMdl.acousticAversion.reversal.time.mean;
            m_acAvrsn.reversal.stdTime = speMdl.acousticAversion.reversal.time.std;
            m_acAvrsn.reverseEnabled = CUtil.IntToBoolean(speMdl.acousticAversion.reversalAffected);
            //              = speMdl.acousticAversion.surfaceInterval.coeff;-1;// Not used by depth model
            m_acAvrsn.surfaceInterval.mean = speMdl.acousticAversion.surfaceInterval.mean;
            m_acAvrsn.surfaceInterval.std = speMdl.acousticAversion.surfaceInterval.std;
            m_acAvrsn.srfIntvlEnabled = CUtil.IntToBoolean(speMdl.acousticAversion.surfaceIntervalAffected);
            m_acAvrsn.direction.arcStep = speMdl.acousticAversion.travel.arcStep;
            m_acAvrsn.direction.bias = speMdl.acousticAversion.travel.bias;
            m_acAvrsn.direction.biasDir = speMdl.acousticAversion.travel.directionOfBias;
            m_acAvrsn.direction.pert = speMdl.acousticAversion.travel.perturbation;
            m_acAvrsn.direction.coeff = speMdl.acousticAversion.travel.termCoeff;
            m_acAvrsn.directnEnabled = CUtil.IntToBoolean(speMdl.acousticAversion.travelDirectionAffected);
            m_acAvrsn.rate.coeff = speMdl.acousticAversion.travelRate.coeff;
            m_acAvrsn.rate.mean = speMdl.acousticAversion.travelRate.mean;
            m_acAvrsn.rate.std = speMdl.acousticAversion.travelRate.std;
            m_acAvrsn.rateEnabled = CUtil.IntToBoolean(speMdl.acousticAversion.travelRateAffected);


            //----------------------------------------------------------------------------------//
            // Species Description
            // 7 parameters: 3mbs lib version, species version, species group, short description,
            //				 long comment, shore following depth, and number of behaviors
            //----------------------------------------------------------------------------------//
            m_mbsLibSavedSuperVer = speMdl.mbsLibVerSuper;
            m_mbsLibSavedSubVer = speMdl.mbsLibVerSub;
            m_speciesMdlSavedSuperVer = speMdl.speVerSuper;
            m_speciesMdlSavedSubVer = speMdl.speVerSub;
            //m_mbsLibVersion = speMdl.mbsLibVer;
            //m_speciesMdlVersion = speMdl.speVer;
            m_groupIndex = speMdl.speciesGroup;
            m_shortDescription = speMdl.szSpeciesShrtDscrptn;
            m_longComment = speMdl.szSpeciesLongComment;
            m_shoreFollow = speMdl.shoreFollowDepth;
            m_seedMinDepth = speMdl.minSeedDepth;
            m_seedingDepthLimit = speMdl.deepWaterSeedingDepth;

            if(speMdl.deepWaterSeedingDepthEnabled == 0)
                m_seedingDepthLimitEnabled = false;
            else
                m_seedingDepthLimitEnabled = true;

            // Behavior count not maintained because the array stores it's own length.

            m_speciesIndex = speMdl.speciesName;

            m_year = speMdl.year;
            m_month = speMdl.month;
            m_day = speMdl.day;
            m_hour = speMdl.hour;
            m_min = speMdl.min;
            m_sec = speMdl.sec;
            m_id = speMdl.id;


            //---------------------//
            // Initial Behavior
            //---------------------//
            //m_initialBehavior = CUtil.CopyMatrix(speMdl.initialBehavior);
            m_initBehSpanMgr.DeleteAllSpans();
            Debug.Assert(m_initBehSpanMgr.SpanCount == 0);
            for(depthRange=0; depthRange<speMdl.initBehSpanCnt; depthRange++)
            {
                depthRangeMdl = m_initBehSpanMgr.AddSpan();
                depthRangeMdl.shallow = speMdl.initialBehavior[depthRange].depthSpan.shallow;
                depthRangeMdl.deep = speMdl.initialBehavior[depthRange].depthSpan.deep;
                depthRangeMdl.behTrans.matrix = CUtil.CopyMatrix(speMdl.initialBehavior[depthRange].m);
            }
            //m_ib = new mbsBEHTRAN[speMdl.initBehSpanCnt];
            
            
            
            //------------------------//
            // Behavior Allocation
            //------------------------//
            m_behMgr = new CBehaviorListMgr();

            for(i=0; i<speMdl.behaviorCount; i++)
            {
                // Add a behavior.  Set the UpdateModel param to false in order to
                // initialize the behavior and rest of model based upon loaded params.
                beh = m_behMgr.AddBehavior(false);

                //------------------//
                // Behavior Name
                //------------------//
                beh.name = speMdl.behavior[i].szName;

                //-------------------------------------//
                // Environmental Attractor Priority
                //-------------------------------------//
                if(speMdl.behavior[i].depthHasPriorityOverTemp == 1)
                    beh.envInfPriority = ENVATTRACTORPRIORITY.DIVE;
                else
                    beh.envInfPriority = ENVATTRACTORPRIORITY.TEMPERATURE;

                //----------------------------------------//
                // Depth environmental attractor
                // 8 parameters, including 2 vector models
                //----------------------------------------//
                beh.envAttrDepth.shelfEnabled = CUtil.IntToBoolean(speMdl.behavior[i].depthEnvAtt.shelfIsEnabled);
                beh.envAttrDepth.shelfDepth = speMdl.behavior[i].depthEnvAtt.shelfDepth;
                beh.envAttrDepth.shelfSlope = speMdl.behavior[i].depthEnvAtt.shelfSlope;

                beh.envAttrDepth.basinEnabled = CUtil.IntToBoolean(speMdl.behavior[i].depthEnvAtt.basinIsEnabled);
                beh.envAttrDepth.basinDepth = speMdl.behavior[i].depthEnvAtt.basinDepth;
                beh.envAttrDepth.basinSlope = speMdl.behavior[i].depthEnvAtt.basinSlope;

                beh.envAttrDepth.slopeEnabled = CUtil.IntToBoolean(speMdl.behavior[i].depthEnvAtt.slopeIsEnabled);
                beh.envAttrDepth.slopeDepth = speMdl.behavior[i].depthEnvAtt.slopeDepth;
                beh.envAttrDepth.slopeSlope = speMdl.behavior[i].depthEnvAtt.slopeSlope;
                
                beh.depthBehTrans.vector = CUtil.CopyMatrix(speMdl.behavior[i].depthEnvAttBehTrans.behavior);
                beh.depthBehTrans.element = CUtil.CopyMatrix(speMdl.behavior[i].depthEnvAttBehTrans.terminate);
                beh.depthBehTrans.meanTimeInBehMinuites = speMdl.behavior[i].depthEnvAttBehTrans.meanTimeInBeh;
                beh.depthBehTrans.slopeCoeff_goesAway = speMdl.behavior[i].depthEnvAttBehTrans.slopeCoefficient;

                //-----------------------------------------//
                // Temperature Environmental Attractor
                // 8 parameters, including 2 vector models
                //-----------------------------------------//
                beh.envAttrTemp.front = speMdl.behavior[i].tempEnvAtt.delta;
                beh.envAttrTemp.frontEnabled = CUtil.IntToBoolean(speMdl.behavior[i].tempEnvAtt.deltaIsEnabled);
                beh.envAttrTemp.warm = speMdl.behavior[i].tempEnvAtt.max;
                beh.envAttrTemp.coldEnabled = CUtil.IntToBoolean(speMdl.behavior[i].tempEnvAtt.maxIsEnabled);
                beh.envAttrTemp.cold = speMdl.behavior[i].tempEnvAtt.min;
                beh.envAttrTemp.warmEnabled = CUtil.IntToBoolean(speMdl.behavior[i].tempEnvAtt.minIsEnabled);
                beh.temperatureBehTrans.vector = CUtil.CopyMatrix(speMdl.behavior[i].tempEnvAttBehTrans.behavior);
                beh.temperatureBehTrans.element = CUtil.CopyMatrix(speMdl.behavior[i].tempEnvAttBehTrans.terminate);
                beh.temperatureBehTrans.meanTimeInBehMinuites = speMdl.behavior[i].tempEnvAttBehTrans.meanTimeInBeh;
                beh.temperatureBehTrans.slopeCoeff_goesAway = speMdl.behavior[i].tempEnvAttBehTrans.slopeCoefficient;


                //-----------------------------------------//
                // Travel Direction
                // 12 parameters, including 3 vector models
                //-----------------------------------------//
                beh.direction.type = (DIRECTIONMODELTYPE)speMdl.behavior[i].travelDirection.modelType;
                beh.direction.correlatedRndmWalk.pert = speMdl.behavior[i].travelDirection.crRndWalk.perturbation;
                beh.direction.correlatedRndmWalk.coeff = speMdl.behavior[i].travelDirection.crRndWalk.termCoeff;
                beh.direction.correlatedRndmWalkDirBias.arcStep = speMdl.behavior[i].travelDirection.crRndWalkDb.arcStep;
                beh.direction.correlatedRndmWalkDirBias.bias = speMdl.behavior[i].travelDirection.crRndWalkDb.bias;
                beh.direction.correlatedRndmWalkDirBias.biasDir = speMdl.behavior[i].travelDirection.crRndWalkDb.directionOfBias;
                beh.direction.correlatedRndmWalkDirBias.pert = speMdl.behavior[i].travelDirection.crRndWalkDb.perturbation;
                beh.direction.correlatedRndmWalkDirBias.coeff = speMdl.behavior[i].travelDirection.crRndWalkDb.termCoeff;
                beh.direction.rndmWalk.coeff = speMdl.behavior[i].travelDirection.rndWalk.termCoeff;
                beh.direction.matrix.directionVector = CUtil.CopyMatrix(speMdl.behavior[i].travelDirection.vm.direction);
                beh.direction.matrix.directionBiasMatrix = CUtil.CopyMatrix(speMdl.behavior[i].travelDirection.vm.directionalBias);
                beh.direction.matrix.term = CUtil.CopyMatrix(speMdl.behavior[i].travelDirection.vm.terminate);

                //-----------------------------------------//
                // Travel Rate
                // 10 parameters, including 3 vector models
                //-----------------------------------------//
                beh.rate.modelType = (MODELTYPE)speMdl.behavior[i].travelRate.modelType;
                beh.rate.gauss.coeff = speMdl.behavior[i].travelRate.gauss.coeff;
                beh.rate.gauss.mean = speMdl.behavior[i].travelRate.gauss.mean;
                beh.rate.gauss.std = speMdl.behavior[i].travelRate.gauss.std;
                beh.rate.randm.coeff = speMdl.behavior[i].travelRate.rnd.coeff;
                beh.rate.randm.max = speMdl.behavior[i].travelRate.rnd.max;
                beh.rate.randm.min = speMdl.behavior[i].travelRate.rnd.min;
                beh.rate.vectorMdl.step = CUtil.CopyMatrix(speMdl.behavior[i].travelRate.vm.step);
                beh.rate.vectorMdl.termination = CUtil.CopyMatrix(speMdl.behavior[i].travelRate.vm.terminate);
                beh.rate.vectorMdl.vector = CUtil.CopyMatrix(speMdl.behavior[i].travelRate.vm.vector);

                //-----------------------------------------//
                // Dive Ascent Rate
                // 10 parameters, including 3 vector models
                //-----------------------------------------//
                beh.ascent.modelType = (MODELTYPE)speMdl.behavior[i].dive.ascentRate.modelType;
                beh.ascent.gauss.coeff = speMdl.behavior[i].dive.ascentRate.gauss.coeff;
                beh.ascent.gauss.mean = speMdl.behavior[i].dive.ascentRate.gauss.mean;
                beh.ascent.gauss.std = speMdl.behavior[i].dive.ascentRate.gauss.std;
                beh.ascent.randm.coeff = speMdl.behavior[i].dive.ascentRate.rnd.coeff;
                beh.ascent.randm.max = speMdl.behavior[i].dive.ascentRate.rnd.max;
                beh.ascent.randm.min = speMdl.behavior[i].dive.ascentRate.rnd.min;
                beh.ascent.vectorMdl.step = CUtil.CopyMatrix(speMdl.behavior[i].dive.ascentRate.vm.step);
                beh.ascent.vectorMdl.termination = CUtil.CopyMatrix(speMdl.behavior[i].dive.ascentRate.vm.terminate);
                beh.ascent.vectorMdl.vector = CUtil.CopyMatrix(speMdl.behavior[i].dive.ascentRate.vm.vector);

                //-----------------------------------------//
                // Dive Descent Rate
                // 10 parameters, including 3 vector models
                //-----------------------------------------//
                beh.descent.modelType = (MODELTYPE)speMdl.behavior[i].dive.descentRate.modelType;
                beh.descent.gauss.coeff = speMdl.behavior[i].dive.descentRate.gauss.coeff;
                beh.descent.gauss.mean = speMdl.behavior[i].dive.descentRate.gauss.mean;
                beh.descent.gauss.std = speMdl.behavior[i].dive.descentRate.gauss.std;
                beh.descent.randm.coeff = speMdl.behavior[i].dive.descentRate.rnd.coeff;
                beh.descent.randm.max = speMdl.behavior[i].dive.descentRate.rnd.max;
                beh.descent.randm.min = speMdl.behavior[i].dive.descentRate.rnd.min;
                beh.descent.vectorMdl.step = CUtil.CopyMatrix(speMdl.behavior[i].dive.descentRate.vm.step);
                beh.descent.vectorMdl.termination = CUtil.CopyMatrix(speMdl.behavior[i].dive.descentRate.vm.terminate);
                beh.descent.vectorMdl.vector = CUtil.CopyMatrix(speMdl.behavior[i].dive.descentRate.vm.vector);

                //---------------------------//
                // Dive Bottom Following
                //---------------------------//
                //beh.flatBottomDivingEnabled = CUtil.IntToBoolean(speMdl.behavior[i].dive.bottomFollows);
                switch(speMdl.behavior[i].dive.bttmFollow.bttmFollowType)
                {
                    case mbsBTTMFLLW_MDL_TYPE.mbsNO_BTTMFLLWNG:
                        beh.flatBottomDive.modelType = MODELTYPE.DISABLED;
                        break;
                    case mbsBTTMFLLW_MDL_TYPE.mbsBOTTOMFOLLOWS_CURRENT_VELOCITY:
                        beh.flatBottomDive.modelType = MODELTYPE.EXTERNALLYMODELED;
                        break;
                    case mbsBTTMFLLW_MDL_TYPE.mbsBOTTOMFOLLOWS_GAUSSIAN_VELOCITY:
                        beh.flatBottomDive.modelType = MODELTYPE.GAUSSIAN;
                        break;
                    case mbsBTTMFLLW_MDL_TYPE.mbsBOTTOMFOLLOWS_UNIFORM_VELOCITY:
                        beh.flatBottomDive.modelType = MODELTYPE.RANDOM;
                        break;
                }

                beh.flatBottomDive.gauss.mean = speMdl.behavior[i].dive.bttmFollow.gauss.mean;
                beh.flatBottomDive.gauss.std = speMdl.behavior[i].dive.bttmFollow.gauss.std;
                beh.flatBottomDive.gauss.coeff = speMdl.behavior[i].dive.bttmFollow.gauss.coeff;

                beh.flatBottomDive.randm.max = speMdl.behavior[i].dive.bttmFollow.rnd.max;
                beh.flatBottomDive.randm.min = speMdl.behavior[i].dive.bttmFollow.rnd.min;
                beh.flatBottomDive.randm.coeff = speMdl.behavior[i].dive.bttmFollow.rnd.coeff;

                beh.flatBottomDive.vectorMdl.vector.a = null;
                beh.flatBottomDive.vectorMdl.step.a = 0;
                beh.flatBottomDive.vectorMdl.termination.a = 0;

                //-----------------------------------------//
                // Dive Depth
                // 9 parameters, including 2 vector models
                //-----------------------------------------//
                beh.depth.type = (MODELTYPE)speMdl.behavior[i].dive.depth.modelType;
                //      = speMdl.behavior[i].dive.depth.gauss.coeff; // not used.
                beh.depth.gauss.mean = speMdl.behavior[i].dive.depth.gauss.mean;
                beh.depth.gauss.std = speMdl.behavior[i].dive.depth.gauss.std;
                //      = speMdl.behavior[i].dive.depth.rnd.coeff; // not used
                beh.depth.randm.max = speMdl.behavior[i].dive.depth.rnd.max;
                //      = speMdl.behavior[i].dive.depth.rnd.min; // not used
                beh.depth.vectorMdl.step = CUtil.CopyMatrix(speMdl.behavior[i].dive.depth.vm.step);
                beh.depth.vectorMdl.vector = CUtil.CopyMatrix(speMdl.behavior[i].dive.depth.vm.vector);

                //-----------------------------------------//
                // Dive Reversals
                // 23 parameters, including 4 vector models
                //-----------------------------------------//
                beh.reversal.type = (MODELTYPE)speMdl.behavior[i].dive.reversal.modelType;
                beh.reversal.enabled = CUtil.IntToBoolean(speMdl.behavior[i].dive.reversal.reverses);
                //-1 = speMdl.behavior[i].dive.reversal.gauss.count.coeff; // not used.
                beh.reversal.gauss.meanCnt = speMdl.behavior[i].dive.reversal.gauss.count.mean;
                beh.reversal.gauss.stdCnt = speMdl.behavior[i].dive.reversal.gauss.count.std;
                beh.reversal.gauss.prob = speMdl.behavior[i].dive.reversal.gauss.probOfReversal;
                //-1 = speMdl.behavior[i].dive.reversal.gauss.time.coeff; // not used.
                beh.reversal.gauss.meanTime = speMdl.behavior[i].dive.reversal.gauss.time.mean;
                beh.reversal.gauss.stdTime = speMdl.behavior[i].dive.reversal.gauss.time.std;
                beh.reversal.randm.maxCnt = speMdl.behavior[i].dive.reversal.rnd.count.max;
                beh.reversal.randm.minCnt = speMdl.behavior[i].dive.reversal.rnd.count.min;
                beh.reversal.randm.prob = speMdl.behavior[i].dive.reversal.rnd.probOfReversal;
                //-1 = speMdl.behavior[i].dive.reversal.rnd.time.coeff; // not used.
                beh.reversal.randm.meanTime = speMdl.behavior[i].dive.reversal.rnd.time.mean;
                beh.reversal.randm.stdTime = speMdl.behavior[i].dive.reversal.rnd.time.std;
                beh.reversal.reversalDiveRateType = (REVERSAL_DIVE_RATE_TYPE)(speMdl.behavior[i].dive.reversal.diveRateType);
                //beh.reversal.diveRateEnabled = CUtil.IntToBoolean(speMdl.behavior[i].dive.reversal.hasaIndependentDiveRate);

                beh.reversal.diveRate.coeff = speMdl.behavior[i].dive.reversal.diveRate.coeff;
                beh.reversal.diveRate.mean = speMdl.behavior[i].dive.reversal.diveRate.mean;
                beh.reversal.diveRate.std = speMdl.behavior[i].dive.reversal.diveRate.std;

                beh.reversal.ascentRate.coeff = speMdl.behavior[i].dive.reversal.ascentRate.coeff;
                beh.reversal.ascentRate.mean = speMdl.behavior[i].dive.reversal.ascentRate.mean;
                beh.reversal.ascentRate.std = speMdl.behavior[i].dive.reversal.ascentRate.std;


                beh.reversal.vector.countVector = CUtil.CopyMatrix(speMdl.behavior[i].dive.reversal.vm.count);
                beh.reversal.vector.probabilityElement = CUtil.CopyMatrix(speMdl.behavior[i].dive.reversal.vm.probOfReversal);
                beh.reversal.vector.durationVector = CUtil.CopyMatrix(speMdl.behavior[i].dive.reversal.vm.time);
                beh.reversal.vector.durationStepElement =  CUtil.CopyMatrix(speMdl.behavior[i].dive.reversal.vm.timeStep);

                //-----------------------------------------//
                // Dive Surface interval
                // 6 parameters, including 4 vector models
                //-----------------------------------------//
                //-1 = speMdl.behavior[i].dive.srfInv.gauss.coeff; // not used.
                beh.surfaceInterval.gauss.mean = speMdl.behavior[i].dive.srfInv.gauss.mean;
                beh.surfaceInterval.gauss.std = speMdl.behavior[i].dive.srfInv.gauss.std;
                beh.surfaceInterval.type = (SRFINVMODELTYPE)speMdl.behavior[i].dive.srfInv.modelType;
                beh.surfaceInterval.vectorMdl.step = CUtil.CopyMatrix(speMdl.behavior[i].dive.srfInv.vm.step);
                beh.surfaceInterval.vectorMdl.vector = CUtil.CopyMatrix(speMdl.behavior[i].dive.srfInv.vm.vector);

                //-----------------------------------------//
                // Behavior Transition
                // 2 parameters, including 2 vector models
                //-----------------------------------------//
                switch(speMdl.behavior[i].behTransTermFormula)
                {
                    case mbsBehTermFormula.T50_K:
                        beh.BehaviorTerminationModel = BEHTRANS_TERM_MODEL.T50_K_TERM;
                        break;
                    case mbsBehTermFormula.GAUSSIAN:
                        beh.BehaviorTerminationModel = BEHTRANS_TERM_MODEL.GAUSSIAN_TERM;
                        break;
                }
                //nrmlTrans
                beh.SpanManager.DeleteAllSpans();
                Debug.Assert(beh.SpanManager.SpanCount == 0);
                for(depthRange=0; depthRange<speMdl.behavior[i].nrmlBehTransCnt; depthRange++)
                {
                    depthRangeMdl = beh.SpanManager.AddSpan(/*m_behMgr.Count*/);
                    depthRangeMdl.shallow = speMdl.behavior[i].nrmlBehTrans[depthRange].depthSpan.shallow;
                    depthRangeMdl.deep = speMdl.behavior[i].nrmlBehTrans[depthRange].depthSpan.deep;
                    depthRangeMdl.behTrans.matrix = CUtil.CopyMatrix(speMdl.behavior[i].nrmlBehTrans[depthRange].m);
                }

            }
            ///////////////////////////////////////////////////////////////
            return mbsRESULT.OK;
        }


        public void SaveToBinFile(string FileName, bool TextFileInstead)
        {
            int i, j;
            CBehavior srcBeh;
            CSpan behTransRef;
            mbsSPECIESMODEL speMdl = new mbsSPECIESMODEL(); // from the wrapper class project
            m_needSave = false;
            

            ///////////////////////////////////////////////////////////////
            //----------------------//
            // Acoustic Aversion
            // 39 parameters
            //----------------------//
            speMdl.acousticAversion.ascentRateAffected = CUtil.BooleanToInt(m_acAvrsn.ascentEnabled);
            speMdl.acousticAversion.ascentRate.coeff = m_acAvrsn.ascent.coeff;
            speMdl.acousticAversion.ascentRate.mean = m_acAvrsn.ascent.mean;
            speMdl.acousticAversion.ascentRate.std = m_acAvrsn.ascent.std;
            speMdl.acousticAversion.descentRateAffected = CUtil.BooleanToInt(m_acAvrsn.descentEnabled);
            speMdl.acousticAversion.descentRate.coeff = m_acAvrsn.descent.coeff;
            speMdl.acousticAversion.descentRate.mean = m_acAvrsn.descent.mean;
            speMdl.acousticAversion.descentRate.std = m_acAvrsn.descent.std;
            speMdl.acousticAversion.beaches = CUtil.BooleanToInt(m_acAvrsn.beachingEnabled);
            speMdl.acousticAversion.beachingDepth = m_beachingDepth;
            speMdl.acousticAversion.depth.coeff = -1; // Not used by depth model.
            speMdl.acousticAversion.depth.mean = m_acAvrsn.depth.mean;
            speMdl.acousticAversion.depth.std = m_acAvrsn.depth.std;
            speMdl.acousticAversion.depthAffected = CUtil.BooleanToInt(m_acAvrsn.depthEnabled);
            speMdl.acousticAversion.flatBottomDiveAffected = CUtil.BooleanToInt(m_acAvrsn.flatBottomDivingEnabled);
            speMdl.acousticAversion.flatBottomDives = CUtil.BooleanToInt(m_acAvrsn.flatBottomDives);
            speMdl.acousticAversion.podBreaksUp = CUtil.BooleanToInt(m_aePodBreaksApart);
            speMdl.acousticAversion.reversal.count.coeff = -1;// Not used by depth model.
            speMdl.acousticAversion.reversal.count.mean = m_acAvrsn.reversal.meanCnt;
            speMdl.acousticAversion.reversal.count.std = m_acAvrsn.reversal.stdCnt;
            speMdl.acousticAversion.reversal.probOfReversal = m_acAvrsn.reversal.prob;
            speMdl.acousticAversion.reversal.time.coeff = -1;// Not used by depth model
            speMdl.acousticAversion.reversal.time.mean = m_acAvrsn.reversal.meanTime;
            speMdl.acousticAversion.reversal.time.std = m_acAvrsn.reversal.stdTime;
            speMdl.acousticAversion.reversalAffected = CUtil.BooleanToInt(m_acAvrsn.reverseEnabled);
            speMdl.acousticAversion.surfaceInterval.coeff = -1;// Not used by depth model
            speMdl.acousticAversion.surfaceInterval.mean = m_acAvrsn.surfaceInterval.mean;
            speMdl.acousticAversion.surfaceInterval.std = m_acAvrsn.surfaceInterval.std;
            speMdl.acousticAversion.surfaceIntervalAffected = CUtil.BooleanToInt(m_acAvrsn.srfIntvlEnabled);
            speMdl.acousticAversion.travel.arcStep = m_acAvrsn.direction.arcStep;
            speMdl.acousticAversion.travel.bias = m_acAvrsn.direction.bias;
            speMdl.acousticAversion.travel.directionOfBias = m_acAvrsn.direction.biasDir;
            speMdl.acousticAversion.travel.perturbation = m_acAvrsn.direction.pert;
            speMdl.acousticAversion.travel.termCoeff = m_acAvrsn.direction.coeff;
            speMdl.acousticAversion.travelDirectionAffected = CUtil.BooleanToInt(m_acAvrsn.directnEnabled);
            speMdl.acousticAversion.travelRate.coeff = m_acAvrsn.rate.coeff;
            speMdl.acousticAversion.travelRate.mean = m_acAvrsn.rate.mean;
            speMdl.acousticAversion.travelRate.std = m_acAvrsn.rate.std;
            speMdl.acousticAversion.travelRateAffected = CUtil.BooleanToInt(m_acAvrsn.rateEnabled);


            //----------------------------------------------------------------------------------//
            // Species Description
            // 7 parameters: 3mbs lib version, species version, species group, short description,
            //				 long comment, shore following depth, and number of behaviors
            //----------------------------------------------------------------------------------//
            speMdl.speciesGroup = m_groupIndex;
            speMdl.szSpeciesShrtDscrptn = m_shortDescription;
            speMdl.szSpeciesLongComment = m_longComment;
            speMdl.shoreFollowDepth = shoreFollowDepth;
            speMdl.minSeedDepth = seedMinDepth;
            speMdl.deepWaterSeedingDepth = m_seedingDepthLimit;

            if(m_seedingDepthLimitEnabled == false)
                speMdl.deepWaterSeedingDepthEnabled = 0;
            else
                speMdl.deepWaterSeedingDepthEnabled = 1;

            speMdl.behaviorCount = m_behMgr.Count;

            speMdl.speciesName = m_speciesIndex;


            // These can only be loaded in, not saved, and get reset at the end of this function.
            //speMdl.year = m_year;
            //speMdl.month = m_month;
            //speMdl.day = m_day;
            //speMdl.sec = m_sec;
            //speMdl.id = m_id;


            //---------------------//
            // Initial Behavior
            //---------------------//
            //speMdl.initialBehavior = CUtil.CopyMatrix(m_initialBehavior);
            speMdl.initBehSpanCnt = m_initBehSpanMgr.SpanCount;
            speMdl.initialBehavior = new mbsBEHTRAN[speMdl.initBehSpanCnt];
            for(j=0; j<speMdl.initBehSpanCnt; j++)
            {
                behTransRef = m_initBehSpanMgr.GetSpan(j);
                speMdl.initialBehavior[j].depthSpan.shallow = behTransRef.shallow;
                speMdl.initialBehavior[j].depthSpan.deep = behTransRef.deep;
                speMdl.initialBehavior[j].m = CUtil.CopyMatrix(behTransRef.behTrans.matrix);
            }

            //---------------------------------------------------------------//
            // Behavior Allocation
            // 2 parameters including the dyanically allocated behavior array
            //---------------------------------------------------------------//
            speMdl.behavior = new mbsNRMLBEHMDL[speMdl.behaviorCount];
            for(i=0; i<speMdl.behaviorCount; i++)
            {
                // keep the lines short.
                srcBeh = m_behMgr.GetBehavior(i);

                //------------------//
                // Behavior Name
                //------------------//
                speMdl.behavior[i].szName = srcBeh.name;

                //-------------------------------------//
                // Environmental Attractor Priority
                //-------------------------------------//
                if(srcBeh.envInfPriority == ENVATTRACTORPRIORITY.DIVE)
                    speMdl.behavior[i].depthHasPriorityOverTemp = 1;
                else
                    speMdl.behavior[i].depthHasPriorityOverTemp = 0;


                //----------------------------------------//
                // Depth environmental attractor
                // 8 parameters, including 2 vector models
                //----------------------------------------//
                speMdl.behavior[i].depthEnvAtt.shelfIsEnabled = CUtil.BooleanToInt(srcBeh.envAttrDepth.shelfEnabled);
                speMdl.behavior[i].depthEnvAtt.shelfDepth = srcBeh.envAttrDepth.shelfDepth;
                speMdl.behavior[i].depthEnvAtt.shelfSlope = srcBeh.envAttrDepth.shelfSlope;

                speMdl.behavior[i].depthEnvAtt.basinIsEnabled = CUtil.BooleanToInt(srcBeh.envAttrDepth.basinEnabled);
                speMdl.behavior[i].depthEnvAtt.basinDepth = srcBeh.envAttrDepth.basinDepth;
                speMdl.behavior[i].depthEnvAtt.basinSlope = srcBeh.envAttrDepth.basinSlope;

                speMdl.behavior[i].depthEnvAtt.slopeIsEnabled = CUtil.BooleanToInt(srcBeh.envAttrDepth.slopeEnabled);
                speMdl.behavior[i].depthEnvAtt.slopeDepth = srcBeh.envAttrDepth.slopeDepth;
                speMdl.behavior[i].depthEnvAtt.slopeSlope = srcBeh.envAttrDepth.slopeSlope;

                speMdl.behavior[i].depthEnvAttBehTrans.behavior = CUtil.CopyMatrix(srcBeh.depthBehTrans.vector);
                speMdl.behavior[i].depthEnvAttBehTrans.terminate = CUtil.CopyMatrix(srcBeh.depthBehTrans.element);

                speMdl.behavior[i].depthEnvAttBehTrans.meanTimeInBeh = srcBeh.depthBehTrans.meanTimeInBehMinuites;
                speMdl.behavior[i].depthEnvAttBehTrans.slopeCoefficient = srcBeh.depthBehTrans.slopeCoeff_goesAway;


                //-----------------------------------------//
                // Temperature Environmental Attractor
                // 8 parameters, including 2 vector models
                //-----------------------------------------//
                speMdl.behavior[i].tempEnvAtt.delta = srcBeh.envAttrTemp.front;
                speMdl.behavior[i].tempEnvAtt.deltaIsEnabled = CUtil.BooleanToInt(srcBeh.envAttrTemp.frontEnabled);
                speMdl.behavior[i].tempEnvAtt.max = srcBeh.envAttrTemp.warm;
                speMdl.behavior[i].tempEnvAtt.maxIsEnabled = CUtil.BooleanToInt(srcBeh.envAttrTemp.coldEnabled);
                speMdl.behavior[i].tempEnvAtt.min = srcBeh.envAttrTemp.cold;
                speMdl.behavior[i].tempEnvAtt.minIsEnabled = CUtil.BooleanToInt(srcBeh.envAttrTemp.warmEnabled);
                speMdl.behavior[i].tempEnvAttBehTrans.behavior = CUtil.CopyMatrix(srcBeh.temperatureBehTrans.vector);
                speMdl.behavior[i].tempEnvAttBehTrans.terminate = CUtil.CopyMatrix(srcBeh.temperatureBehTrans.element);

                speMdl.behavior[i].tempEnvAttBehTrans.meanTimeInBeh = srcBeh.temperatureBehTrans.meanTimeInBehMinuites;
                speMdl.behavior[i].tempEnvAttBehTrans.slopeCoefficient = srcBeh.temperatureBehTrans.slopeCoeff_goesAway;


                //-----------------------------------------//
                // Travel Direction
                // 12 parameters, including 3 vector models
                //-----------------------------------------//
                speMdl.behavior[i].travelDirection.modelType = (mbsDIRECTIONAL_MODEL_TYPE)srcBeh.direction.type;
                speMdl.behavior[i].travelDirection.crRndWalk.perturbation = srcBeh.direction.correlatedRndmWalk.pert;
                speMdl.behavior[i].travelDirection.crRndWalk.termCoeff = srcBeh.direction.correlatedRndmWalk.coeff;
                speMdl.behavior[i].travelDirection.crRndWalkDb.arcStep = srcBeh.direction.correlatedRndmWalkDirBias.arcStep;
                speMdl.behavior[i].travelDirection.crRndWalkDb.bias = srcBeh.direction.correlatedRndmWalkDirBias.bias;
                speMdl.behavior[i].travelDirection.crRndWalkDb.directionOfBias = srcBeh.direction.correlatedRndmWalkDirBias.biasDir;
                speMdl.behavior[i].travelDirection.crRndWalkDb.perturbation = srcBeh.direction.correlatedRndmWalkDirBias.pert;
                speMdl.behavior[i].travelDirection.crRndWalkDb.termCoeff = srcBeh.direction.correlatedRndmWalkDirBias.coeff;
                speMdl.behavior[i].travelDirection.rndWalk.termCoeff = srcBeh.direction.rndmWalk.coeff;
                speMdl.behavior[i].travelDirection.vm.direction = CUtil.CopyMatrix(srcBeh.direction.matrix.directionVector);
                speMdl.behavior[i].travelDirection.vm.directionalBias = CUtil.CopyMatrix(srcBeh.direction.matrix.directionBiasMatrix);
                speMdl.behavior[i].travelDirection.vm.terminate = CUtil.CopyMatrix(srcBeh.direction.matrix.term);

                //-----------------------------------------//
                // Travel Rate
                // 10 parameters, including 3 vector models
                //-----------------------------------------//
                speMdl.behavior[i].travelRate.modelType = (mbsSTANDARD_MODEL_TYPE)srcBeh.rate.modelType;
                speMdl.behavior[i].travelRate.gauss.coeff = srcBeh.rate.gauss.coeff;
                speMdl.behavior[i].travelRate.gauss.mean = srcBeh.rate.gauss.mean;
                speMdl.behavior[i].travelRate.gauss.std = srcBeh.rate.gauss.std;
                speMdl.behavior[i].travelRate.rnd.coeff = srcBeh.rate.randm.coeff;
                speMdl.behavior[i].travelRate.rnd.max = srcBeh.rate.randm.max;
                speMdl.behavior[i].travelRate.rnd.min = srcBeh.rate.randm.min;
                speMdl.behavior[i].travelRate.vm.step = CUtil.CopyMatrix(srcBeh.rate.vectorMdl.step);
                speMdl.behavior[i].travelRate.vm.terminate = CUtil.CopyMatrix(srcBeh.rate.vectorMdl.termination);
                speMdl.behavior[i].travelRate.vm.vector = CUtil.CopyMatrix(srcBeh.rate.vectorMdl.vector);

                //-----------------------------------------//
                // Dive Ascent Rate
                // 10 parameters, including 3 vector models
                //-----------------------------------------//
                speMdl.behavior[i].dive.ascentRate.modelType = (mbsSTANDARD_MODEL_TYPE)srcBeh.ascent.modelType;
                speMdl.behavior[i].dive.ascentRate.gauss.coeff = srcBeh.ascent.gauss.coeff;
                speMdl.behavior[i].dive.ascentRate.gauss.mean = srcBeh.ascent.gauss.mean;
                speMdl.behavior[i].dive.ascentRate.gauss.std = srcBeh.ascent.gauss.std;
                speMdl.behavior[i].dive.ascentRate.rnd.coeff = srcBeh.ascent.randm.coeff;
                speMdl.behavior[i].dive.ascentRate.rnd.max = srcBeh.ascent.randm.max;
                speMdl.behavior[i].dive.ascentRate.rnd.min = srcBeh.ascent.randm.min;
                speMdl.behavior[i].dive.ascentRate.vm.step = CUtil.CopyMatrix(srcBeh.ascent.vectorMdl.step);
                speMdl.behavior[i].dive.ascentRate.vm.terminate = CUtil.CopyMatrix(srcBeh.ascent.vectorMdl.termination);
                speMdl.behavior[i].dive.ascentRate.vm.vector = CUtil.CopyMatrix(srcBeh.ascent.vectorMdl.vector);

                //-----------------------------------------//
                // Dive Descent Rate
                // 10 parameters, including 3 vector models
                //-----------------------------------------//
                speMdl.behavior[i].dive.descentRate.modelType = (mbsSTANDARD_MODEL_TYPE)srcBeh.descent.modelType;
                speMdl.behavior[i].dive.descentRate.gauss.coeff = srcBeh.descent.gauss.coeff;
                speMdl.behavior[i].dive.descentRate.gauss.mean = srcBeh.descent.gauss.mean;
                speMdl.behavior[i].dive.descentRate.gauss.std = srcBeh.descent.gauss.std;
                speMdl.behavior[i].dive.descentRate.rnd.coeff = srcBeh.descent.randm.coeff;
                speMdl.behavior[i].dive.descentRate.rnd.max = srcBeh.descent.randm.max;
                speMdl.behavior[i].dive.descentRate.rnd.min = srcBeh.descent.randm.min;
                speMdl.behavior[i].dive.descentRate.vm.step = CUtil.CopyMatrix(srcBeh.descent.vectorMdl.step);
                speMdl.behavior[i].dive.descentRate.vm.terminate = CUtil.CopyMatrix(srcBeh.descent.vectorMdl.termination);
                speMdl.behavior[i].dive.descentRate.vm.vector = CUtil.CopyMatrix(srcBeh.descent.vectorMdl.vector);

                //---------------------------//
                // Dive Bottom Following
                //---------------------------//
                //speMdl.behavior[i].dive.bottomFollows = CUtil.BooleanToInt(beh.flatBottomDivingEnabled);
                switch(srcBeh.flatBottomDive.modelType)
                {
                    case MODELTYPE.DISABLED:
                        speMdl.behavior[i].dive.bttmFollow.bttmFollowType = mbsBTTMFLLW_MDL_TYPE.mbsNO_BTTMFLLWNG;
                        break;
                    case MODELTYPE.EXTERNALLYMODELED:
                        speMdl.behavior[i].dive.bttmFollow.bttmFollowType = 
                        mbsBTTMFLLW_MDL_TYPE.mbsBOTTOMFOLLOWS_CURRENT_VELOCITY;
                        break;
                    case MODELTYPE.GAUSSIAN:
                        speMdl.behavior[i].dive.bttmFollow.bttmFollowType = 
                        mbsBTTMFLLW_MDL_TYPE.mbsBOTTOMFOLLOWS_GAUSSIAN_VELOCITY;
                        break;
                    case MODELTYPE.RANDOM:
                        speMdl.behavior[i].dive.bttmFollow.bttmFollowType = 
                        mbsBTTMFLLW_MDL_TYPE.mbsBOTTOMFOLLOWS_UNIFORM_VELOCITY;
                        break;
                    case MODELTYPE.MATRIX:
                        Debug.Assert(false); // coding error.
                        break;
                }

                speMdl.behavior[i].dive.bttmFollow.gauss.mean = srcBeh.flatBottomDive.gauss.mean;
                speMdl.behavior[i].dive.bttmFollow.gauss.std = srcBeh.flatBottomDive.gauss.std;
                speMdl.behavior[i].dive.bttmFollow.gauss.coeff = srcBeh.flatBottomDive.gauss.coeff;

                speMdl.behavior[i].dive.bttmFollow.rnd.max = srcBeh.flatBottomDive.randm.max;
                speMdl.behavior[i].dive.bttmFollow.rnd.min = srcBeh.flatBottomDive.randm.min;
                speMdl.behavior[i].dive.bttmFollow.rnd.coeff = srcBeh.flatBottomDive.randm.coeff;


                //-----------------------------------------//
                // Dive Depth
                // 9 parameters, including 2 vector models
                //-----------------------------------------//
                speMdl.behavior[i].dive.depth.modelType = (mbsSTANDARD_MODEL_TYPE)srcBeh.depth.type;
                speMdl.behavior[i].dive.depth.gauss.coeff = -1; // not used.
                speMdl.behavior[i].dive.depth.gauss.mean = srcBeh.depth.gauss.mean;
                speMdl.behavior[i].dive.depth.gauss.std = srcBeh.depth.gauss.std;
                speMdl.behavior[i].dive.depth.rnd.coeff = -1; // not used
                speMdl.behavior[i].dive.depth.rnd.max = srcBeh.depth.randm.max;
                speMdl.behavior[i].dive.depth.rnd.min = 0; // not used
                speMdl.behavior[i].dive.depth.vm.step = CUtil.CopyMatrix(srcBeh.depth.vectorMdl.step);
                speMdl.behavior[i].dive.depth.vm.vector = CUtil.CopyMatrix(srcBeh.depth.vectorMdl.vector);

                //-----------------------------------------//
                // Dive Reversals
                // 23 parameters, including 4 vector models
                //-----------------------------------------//
                speMdl.behavior[i].dive.reversal.modelType = (mbsSTANDARD_MODEL_TYPE)srcBeh.reversal.type;
                speMdl.behavior[i].dive.reversal.reverses = CUtil.BooleanToInt(srcBeh.reversal.enabled);
                speMdl.behavior[i].dive.reversal.gauss.count.coeff = -1; // not used.
                speMdl.behavior[i].dive.reversal.gauss.count.mean = srcBeh.reversal.gauss.meanCnt;
                speMdl.behavior[i].dive.reversal.gauss.count.std = srcBeh.reversal.gauss.stdCnt;
                speMdl.behavior[i].dive.reversal.gauss.probOfReversal = srcBeh.reversal.gauss.prob;
                speMdl.behavior[i].dive.reversal.gauss.time.coeff = -1; // not used.
                speMdl.behavior[i].dive.reversal.gauss.time.mean = srcBeh.reversal.gauss.meanTime;
                speMdl.behavior[i].dive.reversal.gauss.time.std = srcBeh.reversal.gauss.stdTime;
                speMdl.behavior[i].dive.reversal.rnd.count.max = srcBeh.reversal.randm.maxCnt;
                speMdl.behavior[i].dive.reversal.rnd.count.min = srcBeh.reversal.randm.minCnt;
                speMdl.behavior[i].dive.reversal.rnd.probOfReversal = srcBeh.reversal.randm.prob;
                speMdl.behavior[i].dive.reversal.rnd.time.coeff = -1; // not used.
                speMdl.behavior[i].dive.reversal.rnd.time.mean = srcBeh.reversal.randm.meanTime;
                speMdl.behavior[i].dive.reversal.rnd.time.std = srcBeh.reversal.randm.stdTime;
                //speMdl.behavior[i].dive.reversal.hasaIndependentDiveRate = CUtil.BooleanToInt(beh.reversal.diveRateEnabled);
                speMdl.behavior[i].dive.reversal.diveRateType = (mbsREVERSAL_DIVE_RATE_TYPE)srcBeh.reversal.reversalDiveRateType;
                speMdl.behavior[i].dive.reversal.diveRate.coeff = srcBeh.reversal.diveRate.coeff;
                speMdl.behavior[i].dive.reversal.diveRate.mean = srcBeh.reversal.diveRate.mean;
                speMdl.behavior[i].dive.reversal.diveRate.std = srcBeh.reversal.diveRate.std;

                speMdl.behavior[i].dive.reversal.ascentRate.coeff = srcBeh.reversal.ascentRate.coeff;
                speMdl.behavior[i].dive.reversal.ascentRate.mean = srcBeh.reversal.ascentRate.mean;
                speMdl.behavior[i].dive.reversal.ascentRate.std = srcBeh.reversal.ascentRate.std;

                speMdl.behavior[i].dive.reversal.vm.count = CUtil.CopyMatrix(srcBeh.reversal.vector.countVector);
                speMdl.behavior[i].dive.reversal.vm.probOfReversal = CUtil.CopyMatrix(srcBeh.reversal.vector.probabilityElement);
                speMdl.behavior[i].dive.reversal.vm.time = CUtil.CopyMatrix(srcBeh.reversal.vector.durationVector);
                speMdl.behavior[i].dive.reversal.vm.timeStep = CUtil.CopyMatrix(srcBeh.reversal.vector.durationStepElement);

                //-----------------------------------------//
                // Dive Surface interval
                // 6 parameters, including 4 vector models
                //-----------------------------------------//
                speMdl.behavior[i].dive.srfInv.gauss.coeff = -1; // not used.
                speMdl.behavior[i].dive.srfInv.gauss.mean = srcBeh.surfaceInterval.gauss.mean;
                speMdl.behavior[i].dive.srfInv.gauss.std = srcBeh.surfaceInterval.gauss.std;
                speMdl.behavior[i].dive.srfInv.modelType = (mbsSTANDARD_MODEL_TYPE)srcBeh.surfaceInterval.type;
                speMdl.behavior[i].dive.srfInv.vm.step = CUtil.CopyMatrix(srcBeh.surfaceInterval.vectorMdl.step);
                speMdl.behavior[i].dive.srfInv.vm.vector = CUtil.CopyMatrix(srcBeh.surfaceInterval.vectorMdl.vector);

                //-----------------------------------------//
                // Behavior Transition
                // 2 parameters, including 2 vector models
                //-----------------------------------------//
                speMdl.behavior[i].nrmlBehTransCnt = srcBeh.SpanManager.SpanCount;
                speMdl.behavior[i].nrmlBehTrans = new mbsBEHTRAN[speMdl.behavior[i].nrmlBehTransCnt];

                switch(srcBeh.BehaviorTerminationModel)
                {
                    case BEHTRANS_TERM_MODEL.T50_K_TERM:
                        speMdl.behavior[i].behTransTermFormula = mbsBehTermFormula.T50_K;
                        break;
                    case BEHTRANS_TERM_MODEL.GAUSSIAN_TERM:
                        speMdl.behavior[i].behTransTermFormula = mbsBehTermFormula.GAUSSIAN;
                        break;
                }
                //srcBeh.BehaviorTerminationModel;
                //speMdl.behavior[i].
                for(j=0; j<speMdl.behavior[i].nrmlBehTransCnt; j++)
                {
                    behTransRef = srcBeh.SpanManager.GetSpan(j);
                    speMdl.behavior[i].nrmlBehTrans[j].depthSpan.shallow = behTransRef.shallow;
                    speMdl.behavior[i].nrmlBehTrans[j].depthSpan.deep = behTransRef.deep;
                    speMdl.behavior[i].nrmlBehTrans[j].m = CUtil.CopyMatrix(behTransRef.behTrans.matrix);
                }
            }

            ///////////////////////////////////////////////////////////////
            if(TextFileInstead == true)
                m_wrapper.ModelToTextFile(FileName, speMdl);
            else
                m_wrapper.SaveToBinFile(FileName, speMdl);

            // Update fields for year, month, day, hour, min, sec, and semiunique ID resulting from the save.
            m_mbsLibSavedSuperVer = speMdl.mbsLibVerSuper;
            m_mbsLibSavedSubVer = speMdl.mbsLibVerSub;
            m_speciesMdlSavedSuperVer = speMdl.speVerSuper;
            m_speciesMdlSavedSubVer = speMdl.speVerSub;

            m_year = speMdl.year;
            m_month = speMdl.month;
            m_day = speMdl.day;
            m_hour = speMdl.hour;
            m_min = speMdl.min;
            m_sec = speMdl.sec;
            m_id = speMdl.id;
        }
    }
}
