using System;
using System.Collections.Generic;
using System.Collections;
using System.Text;
using System.Diagnostics;

namespace MMMBSLib
{
    public class CBehavior
    {
        //------------------//
        // Member Variables
        //------------------//
        string m_name = " "; // Behavior Name

        // The array manager holding spans of behavior transitions by depth for this behavior.
        CBehaviorTransitionSpanMgr m_spanMgr;
        BEHTRANS_TERM_MODEL m_behTermMdl; // Behavior termination model.

        // Dive Parameters
        //Boolean m_fBttmDvEnabled; // remove this
        CRateParameters m_flatBottomDive = new CRateParameters();
        CRateParameters m_ascent = new CRateParameters();
        CRateParameters m_descent = new CRateParameters();
        CDepthParameters m_depth = new CDepthParameters();
        CReversalParameters m_reverse = new CReversalParameters();
        CSurfaceIntervalParams m_surfintrvl = new CSurfaceIntervalParams();

        // Travel Parameters
        CDirectionalParameters m_direction = new CDirectionalParameters();
        CRateParameters m_rate = new CRateParameters();

        // Environmental Attractors
        ENVATTRACTORPRIORITY m_envInfPriority;
        CEnvAttDepthMdl m_envAttDirDepth = new CEnvAttDepthMdl();
        CEnvAttTemperatureMdl m_envAttDirTemp = new CEnvAttTemperatureMdl();
        CEnvAttrBehTrans m_depthEnvAttctrBehTrans = new CEnvAttrBehTrans();
        CEnvAttrBehTrans m_tempEnvAttctrBehTrans = new CEnvAttrBehTrans();
       
        //-----------------//
        // Class Properties
        //-----------------//
        public string name { get { return m_name; } set { m_name = value; } }
        public CBehaviorTransitionSpanMgr SpanManager { get { return m_spanMgr; } set { m_spanMgr = value; } }
        public BEHTRANS_TERM_MODEL BehaviorTerminationModel { get { return m_behTermMdl; } set { m_behTermMdl = value; } }

        // Behavior Transition
        public CEnvAttrBehTrans depthBehTrans { get { return m_depthEnvAttctrBehTrans; } }
        public CEnvAttrBehTrans temperatureBehTrans { get { return m_tempEnvAttctrBehTrans; } }

        // Dive Parameter Properties
        public CRateParameters flatBottomDive { get { return m_flatBottomDive; } set { m_flatBottomDive = value; } }
        public CRateParameters ascent { get { return m_ascent; } }
        public CRateParameters descent { get { return m_descent; } }
        public CDepthParameters depth { get { return m_depth; } }
        public CReversalParameters reversal { get { return m_reverse; } }
        public CSurfaceIntervalParams surfaceInterval { get { return m_surfintrvl; } }

        // Travel Parameter Properties
        public CDirectionalParameters direction { get { return m_direction; } }
        public CRateParameters rate { get { return m_rate; } }

        // Environmental Attractor Properties
        public ENVATTRACTORPRIORITY envInfPriority
        { get { return m_envInfPriority; } 
          set { m_envInfPriority = value; } 
        }
        public CEnvAttDepthMdl envAttrDepth { get { return m_envAttDirDepth; } }
        public CEnvAttTemperatureMdl envAttrTemp { get { return m_envAttDirTemp; } }

        //------------------//
        // Class Constructor
        //------------------//
        //public CBehavior() { m_spanMgr = new CSpanListMgr(); }

        /* The Current Behavior Count passed in must include this behavior being added*/
        public CBehavior()
        {
            //Debug.Assert(CurrentBehaviorCnt >= 1);
            // Normal behavior transition.
            m_spanMgr = new CBehaviorTransitionSpanMgr();
            m_spanMgr.AddSpan();
            m_behTermMdl = BEHTRANS_TERM_MODEL.T50_K_TERM;

            // Flat Bottom Dive
            m_flatBottomDive.modelType = MODELTYPE.DISABLED;
            m_flatBottomDive.gauss.mean = MBSDEFAULTS.ASCENT_GAUSS_MEAN;
            m_flatBottomDive.gauss.std = MBSDEFAULTS.ASCENT_GAUSS_STD;
            m_flatBottomDive.gauss.coeff = MBSDEFAULTS.ASCENT_GAUSS_COEFF;
            m_flatBottomDive.randm.max = MBSDEFAULTS.ASCENT_RANDOM_MAX;
            m_flatBottomDive.randm.min = MBSDEFAULTS.ASCENT_RANDOM_MIN;
            m_flatBottomDive.randm.coeff = MBSDEFAULTS.ASCENT_RANDOM_COEFF;
            m_flatBottomDive.vectorMdl.vector.a = null;
            m_flatBottomDive.vectorMdl.step.a = 0;
            m_flatBottomDive.vectorMdl.termination.a = 0;

            // Diving Ascent
            m_ascent.modelType = MBSDEFAULTS.ASCENT_MODEL_TYPE;
            m_ascent.gauss.mean = MBSDEFAULTS.ASCENT_GAUSS_MEAN;
            m_ascent.gauss.std = MBSDEFAULTS.ASCENT_GAUSS_STD;
            m_ascent.gauss.coeff = MBSDEFAULTS.ASCENT_GAUSS_COEFF;
            m_ascent.randm.max = MBSDEFAULTS.ASCENT_RANDOM_MAX;
            m_ascent.randm.min = MBSDEFAULTS.ASCENT_RANDOM_MIN;
            m_ascent.randm.coeff = MBSDEFAULTS.ASCENT_RANDOM_COEFF;
            m_ascent.vectorMdl.vector.a = MBSDEFAULTS.ASCENT_RATE_VECTOR;
            m_ascent.vectorMdl.step.a = MBSDEFAULTS.ASCENT_RATE_STEP;
            m_ascent.vectorMdl.termination.a = MBSDEFAULTS.ASCENT_RATE_TERMINATE;

            // Diving Descent
            m_descent.modelType = MBSDEFAULTS.DESCENT_MODEL_TYPE;
            m_descent.gauss.mean = MBSDEFAULTS.DESCENT_GAUSS_MEAN;
            m_descent.gauss.std = MBSDEFAULTS.DESCENT_GAUSS_STD;
            m_descent.gauss.coeff = MBSDEFAULTS.DESCENT_GAUSS_COEFF;
            m_descent.randm.max = MBSDEFAULTS.DESCENT_RANDOM_MAX;
            m_descent.randm.min = MBSDEFAULTS.DESCENT_RANDOM_MIN;
            m_descent.randm.coeff = MBSDEFAULTS.DESCENT_RANDOM_COEFF;
            m_descent.vectorMdl.vector.a = MBSDEFAULTS.DESCENT_RATE_VECTOR;
            m_descent.vectorMdl.step.a = MBSDEFAULTS.DESCENT_RATE_STEP;
            m_descent.vectorMdl.termination.a = MBSDEFAULTS.DESCENT_RATE_TERMINATE;

            // Diving Depth
            m_depth.type = MBSDEFAULTS.DEPTH_MODEL_TYPE;
            m_depth.gauss.mean = MBSDEFAULTS.DEPTH_GAUSS_MEAN;
            m_depth.gauss.std = MBSDEFAULTS.DEPTH_GAUSS_STD;
            m_depth.randm.max = MBSDEFAULTS.DEPTH_RANDOM_MAX;
            m_depth.vectorMdl.vector.a = MBSDEFAULTS.DEPTH_VECTOR;
            m_depth.vectorMdl.step.a = MBSDEFAULTS.DEPTH_STEP;

            // Diving Reversals
            m_reverse.type = MBSDEFAULTS.REVERSALS_MODEL_TYPE;
            m_reverse.enabled = MBSDEFAULTS.REVERSALS_ENABLED;
            m_reverse.reversalDiveRateType = MBSDEFAULTS.REVERSALS_DIVE_RATE_TYPE;
            m_reverse.diveRate.mean = MBSDEFAULTS.REVERSALS_DIVE_RATE_GAUSS_MEAN;
            m_reverse.diveRate.std = MBSDEFAULTS.REVERSALS_DIVE_RATE_GAUSS_STD;
            m_reverse.diveRate.coeff = MBSDEFAULTS.REVERSALS_DIVE_RATE_GAUSS_TERMCOEFF;
            m_reverse.gauss.meanCnt = MBSDEFAULTS.REVERSALS_GAUSS_MEAN_COUNT;
            m_reverse.gauss.stdCnt = MBSDEFAULTS.REVERSALS_GAUSS_STD_COUNT;
            m_reverse.gauss.prob = MBSDEFAULTS.REVERSALS_GAUSS_PROBABILITY;
            m_reverse.gauss.meanTime = MBSDEFAULTS.REVERSALS_GAUSS_MEAN_TIME;
            m_reverse.gauss.stdTime = MBSDEFAULTS.REVERSALS_GAUSS_STD_TIME;
            m_reverse.randm.maxCnt = MBSDEFAULTS.REVERSALS_RANDOM_MAX_COUNT;
            m_reverse.randm.minCnt = MBSDEFAULTS.REVERSALS_RANDOM_MIN_COUNT;
            m_reverse.randm.prob = MBSDEFAULTS.REVERSALS_RANDOM_PROBABILITY;
            m_reverse.randm.meanTime = MBSDEFAULTS.REVERSALS_RANDOM_MEAN_TIME;
            m_reverse.randm.stdTime = MBSDEFAULTS.REVERSALS_RANDOM_STD_TIME;

            m_reverse.vector.probabilityElement.a = MBSDEFAULTS.REVERSALS_PROBABILITY0;
            m_reverse.vector.countVector.a = MBSDEFAULTS.REVERSALS_COUNT_VECTOR0;
            m_reverse.vector.durationVector.a = MBSDEFAULTS.REVERSALS_TIME_VECTOR0;
            m_reverse.vector.durationStepElement.a = MBSDEFAULTS.REVERSALS_TIME_STEP;

            // Diving Surface Interval
            m_surfintrvl.type = MBSDEFAULTS.SURFACE_INTERVAL_MODEL_TYPE;
            m_surfintrvl.gauss.mean = MBSDEFAULTS.SURFACE_INTERVAL_GAUSS_MEAN;
            m_surfintrvl.gauss.std = MBSDEFAULTS.SURFACE_INTERVAL_GAUSS_STD;
            m_surfintrvl.vectorMdl.vector.a = MBSDEFAULTS.SURFACE_INTERVAL_VECTOR0;
            m_surfintrvl.vectorMdl.step.a = MBSDEFAULTS.SURFACE_INTERVAL_STEP;

            // Travel Direction
            m_direction.type = MBSDEFAULTS.DIRECTION_MODEL_TYPE;
            m_direction.rndmWalk.coeff = MBSDEFAULTS.RANDOM_WALK_COEFFICENT;
            m_direction.correlatedRndmWalk.pert = MBSDEFAULTS.CORRELATED_RANDOM_WALK_PERTURBATION;
            m_direction.correlatedRndmWalk.coeff = MBSDEFAULTS.CORRELATED_RANDOM_WALK_COEFFICENT;
            m_direction.correlatedRndmWalkDirBias.pert = MBSDEFAULTS.CORRELATED_RANDOM_WALK_DIR_BIAS_PERTURBATION;
            m_direction.correlatedRndmWalkDirBias.biasDir = MBSDEFAULTS.CORRELATED_RANDOM_WALK_DIR_BIAS_DIRECTION_OF_BIAS;
            m_direction.correlatedRndmWalkDirBias.bias = MBSDEFAULTS.CORRELATED_RANDOM_WALK_DIR_BIAS_VALUE_OF_BIAS;
            m_direction.correlatedRndmWalkDirBias.arcStep = MBSDEFAULTS.CORRELATED_RANDOM_WALK_DIR_BIAS_ARC_STEP;
            m_direction.correlatedRndmWalkDirBias.coeff = MBSDEFAULTS.CORRELATED_RANDOM_WALK_DIR_BIAS_COEFFICENT;
            m_direction.matrix.SetDefaultModel();

            // Travel Rate
            m_rate.modelType = MBSDEFAULTS.RATE_MODEL_TYPE;
            m_rate.gauss.mean = MBSDEFAULTS.TRAVEL_RATE_GAUSS_MEAN;
            m_rate.gauss.std = MBSDEFAULTS.TRAVEL_RATE_GAUSS_STD;
            m_rate.gauss.coeff = MBSDEFAULTS.TRAVEL_RATE_GAUSS_COEFF;
            m_rate.randm.max = MBSDEFAULTS.TRAVEL_RATE_RANDOM_MAX;
            m_rate.randm.min = MBSDEFAULTS.TRAVEL_RATE_RANDOM_MIN;
            m_rate.randm.coeff = MBSDEFAULTS.TRAVEL_RATE_RANDOM_COEFF;
            m_rate.vectorMdl.vector.a = MBSDEFAULTS.TRAVEL_RATE_VECTOR0;
            m_rate.vectorMdl.step.a = MBSDEFAULTS.TRAVEL_RATE_STEP;
            m_rate.vectorMdl.termination.a = MBSDEFAULTS.TRAVEL_RATE_TERMINATE;

            // General Environmental Attractor
            m_envInfPriority = MBSDEFAULTS.ENVINF_PRIORITY; ;
            m_envAttDirDepth.slopeEnabled = MBSDEFAULTS.ENVINFL_DEPTH_SHALLOW_ENABLED;
            m_envAttDirDepth.basinEnabled = MBSDEFAULTS.ENVINFL_DEPTH_DEEP_ENABLED;
            m_envAttDirDepth.shelfEnabled = MBSDEFAULTS.ENVINFL_DEPTH_SHELF_ENABLED;

            m_envAttDirDepth.shelfDepth = MBSDEFAULTS.ENVINFL_DEPTH_SHELF_BATHYDEPTH_METERS;
            m_envAttDirDepth.basinDepth = MBSDEFAULTS.ENVINFL_DEPTH_BASIN_BATHYDEPTH_METERS;
            m_envAttDirDepth.slopeDepth = MBSDEFAULTS.ENVINFL_DEPTH_SLOPE_BATHYDEPTH_METERS;

            m_envAttDirDepth.shelfSlope = MBSDEFAULTS.ENVINFL_DEPTH_SHELF_BATHYSLOPE_DEGREES;
            m_envAttDirDepth.basinSlope = MBSDEFAULTS.ENVINFL_DEPTH_BASIN_BATHYSLOPE_DEGREES;
            m_envAttDirDepth.slopeSlope = MBSDEFAULTS.ENVINFL_DEPTH_SLOPE_BATHYSLOPE_DEGREES;


            m_envAttDirTemp.coldEnabled = MBSDEFAULTS.ENVINFL_TEMP_COLD_ENABLED;
            m_envAttDirTemp.warmEnabled = MBSDEFAULTS.ENVINFL_TEMP_WARM_ENABLED;
            m_envAttDirTemp.frontEnabled = MBSDEFAULTS.ENVINFL_TEMP_FRONT_ENABLED;
            m_envAttDirTemp.cold = MBSDEFAULTS.ENVINFL_TEMP_COLD;
            m_envAttDirTemp.warm = MBSDEFAULTS.ENVINFL_TEMP_WARM;
            m_envAttDirTemp.front = MBSDEFAULTS.ENVINFL_TEMP_SHELF;

            // Depth Environmental Attractor Behavior Transition
            m_depthEnvAttctrBehTrans.vector.a = new double[1];
            m_depthEnvAttctrBehTrans.vector.a[0] = 1;
            m_depthEnvAttctrBehTrans.element.a = MBSDEFAULTS.BEHAVIOR_TRANS_TERMINATION_COEFF;
            m_depthEnvAttctrBehTrans.meanTimeInBehMinuites = 60;
            m_depthEnvAttctrBehTrans.slopeCoeff_goesAway = 7;

            // Temperature environmental attractor behavior transition
            m_tempEnvAttctrBehTrans.vector.a = new double[1];
            m_tempEnvAttctrBehTrans.vector.a[0] = 1;
            m_tempEnvAttctrBehTrans.element.a = MBSDEFAULTS.BEHAVIOR_TRANS_TERMINATION_COEFF;
            m_tempEnvAttctrBehTrans.meanTimeInBehMinuites = 60;
            m_tempEnvAttctrBehTrans.slopeCoeff_goesAway = 7;
        }

        //--------------//
        // Class Methods
        //--------------//

        public CBehavior GetCopy()
        {
            int i;
            CSpan span;
            CBehavior c = new CBehavior();

            c.m_name = m_name;

            //--------------------//
            // Behavior transition
            //--------------------//
            // Delete any spans in the newly instantiated CBehavior instance
            c.m_spanMgr.DeleteAllSpans();

            // Copy the behavior spans from this behavor to the copy.
            for(i=0; i<m_spanMgr.SpanCount && null != (span = m_spanMgr.GetSpan(i)); i++)
            {
                c.SpanManager.AddSpan(span);
            }
            c.m_behTermMdl = m_behTermMdl;
              
            c.m_depthEnvAttctrBehTrans = m_depthEnvAttctrBehTrans.GetCopy();
            c.m_tempEnvAttctrBehTrans = m_tempEnvAttctrBehTrans.GetCopy();

            // Dive Parameters
            //c.m_fBttmDvEnabled = m_fBttmDvEnabled;
            c.m_flatBottomDive = m_flatBottomDive.GetCopy();
            c.m_ascent = m_ascent.GetCopy();
            c.m_descent = m_descent.GetCopy();
            c.m_depth = m_depth.GetCopy();
            c.m_reverse = m_reverse.GetCopy();
            c.m_surfintrvl = m_surfintrvl.GetCopy();

            // Travel Parameters
            c.m_direction = m_direction.GetCopy();
            c.m_rate = m_rate.GetCopy();

            // Environmental Attractors
            c.m_envInfPriority = m_envInfPriority;
            c.m_envAttDirDepth = m_envAttDirDepth.GetCopy();
            c.m_envAttDirTemp = m_envAttDirTemp.GetCopy();

            return c;
        }

        public int IncrementTransitionBehaviorVectors()
        {
            double[] d;
            int newArrayLength;

            //m_spanMgr.IncrementAllSpanTransitionBehaviorVectorElements();

            // Depth env attractor behavior transition array
            newArrayLength = m_depthEnvAttctrBehTrans.vector.columnCount + 1;
            d = new double[newArrayLength];
            m_depthEnvAttctrBehTrans.vector.a.CopyTo(d, 0);
            d[newArrayLength-1] = 1.0;
            m_depthEnvAttctrBehTrans.vector.a = d;

            // Temperature env attractor behavior transition array
            newArrayLength = m_tempEnvAttctrBehTrans.vector.columnCount + 1;
            d = new double[newArrayLength];
            m_tempEnvAttctrBehTrans.vector.a.CopyTo(d, 0);
            d[newArrayLength-1] = 1.0;
            m_tempEnvAttctrBehTrans.vector.a = d;

            return newArrayLength;
        }

        public Boolean DecrementEnvironmentalTransitionBehaviorVectorElement(int Column)
        {
            double[] d;
            int arrLen;
            int di, si; // Destination and source indicies.

            //if(false == m_spanMgr.DecrementAllSpanBehaviorTransitionVectorElements(Index))
              //  return false;

            // Depth env attractor behavior transition array
            arrLen = m_depthEnvAttctrBehTrans.vector.columnCount;
            d = new double[arrLen-1];
            for(di=0,si=0; di<d.Length && si<arrLen && di<Column; di++,si++)
                d[di] = m_depthEnvAttctrBehTrans.vector.a[si];
            di++; // skip over index being deleted.
            for(; di<d.Length && si<arrLen; di++, si++)
                d[di] = m_depthEnvAttctrBehTrans.vector.a[si];
            m_depthEnvAttctrBehTrans.vector.a = d;


            // Temperature env attractor behavior transition array
            arrLen = m_tempEnvAttctrBehTrans.vector.columnCount;
            d = new double[arrLen-1];
            for(di=0, si=0; di<d.Length && si<arrLen && di<Column; di++, si++)
                d[di] = m_tempEnvAttctrBehTrans.vector.a[si];
            di++; // skip over index being deleted.
            for(; di<d.Length && si<arrLen; di++, si++)
                d[di] = m_tempEnvAttctrBehTrans.vector.a[si];
            m_tempEnvAttctrBehTrans.vector.a = d;
            return true;
        }
    }



    // Previous discussion with Dorian... needed to disable flat bottom diving while in acoustic exposure.  Look into.
    public class CAcousticAversion
    {
        //-----------------//
        // Member Variables
        //-----------------//

        // Acoustic Exposure Affects...
        Boolean m_ascentEnabled;
        Boolean m_descentEnabled;
        Boolean m_depthEnabled;
        Boolean m_reverseEnabled;
        Boolean m_srfIntvlEnabled;
        Boolean m_directnEnabled;
        Boolean m_rateEnabled;
        Boolean m_beachEnabled;
        Boolean m_fBttmEnabled; // IF ae affects flat bottom diving

        // Dive Parameter Fields
        Boolean m_fbttmDives; // The ae effect if it is affected by ae.
        CParamsRateGaussn m_ascent = new CParamsRateGaussn();
        CParamsRateGaussn m_descent = new CParamsRateGaussn();
        CParamsDepthGaussn m_depth = new CParamsDepthGaussn();
        CParamsReversalGaussn m_reverse = new CParamsReversalGaussn();
        CParamsSurfaceIntervalGaussn m_surfintrvl = new CParamsSurfaceIntervalGaussn();

        // Travel Parameter Fields
        CParamsDirectionCRndmWalkDirBias m_direction = new CParamsDirectionCRndmWalkDirBias();
        CParamsRateGaussn m_rate = new CParamsRateGaussn();

        // Dive Parameter Properties
        public Boolean flatBottomDives { get { return m_fbttmDives; } set { m_fbttmDives = value; } }
        public CParamsRateGaussn ascent { get { return m_ascent; } }
        public CParamsRateGaussn descent { get { return m_descent; } }
        public CParamsDepthGaussn depth { get { return m_depth; } }
        public CParamsReversalGaussn reversal { get { return m_reverse; } }
        public CParamsSurfaceIntervalGaussn surfaceInterval { get { return m_surfintrvl; } }

        // Travel Parameter Properties
        public CParamsDirectionCRndmWalkDirBias direction { get { return m_direction; } }
        public CParamsRateGaussn rate { get { return m_rate; } }
        
        // Acoustic Exposure Affects...
        public Boolean flatBottomDivingEnabled { get { return m_fBttmEnabled; } set { m_fBttmEnabled = value; } }
        public Boolean ascentEnabled { get { return m_ascentEnabled; } set { m_ascentEnabled = value; } }
        public Boolean descentEnabled { get { return m_descentEnabled; } set { m_descentEnabled = value; } }
        public Boolean depthEnabled { get { return m_depthEnabled; } set { m_depthEnabled = value; } }
        public Boolean reverseEnabled { get { return m_reverseEnabled; } set { m_reverseEnabled = value; } }
        public Boolean srfIntvlEnabled { get { return m_srfIntvlEnabled; } set { m_srfIntvlEnabled = value; } }
        public Boolean directnEnabled { get { return m_directnEnabled; } set { m_directnEnabled = value; } }
        public Boolean rateEnabled { get { return m_rateEnabled; } set { m_rateEnabled = value; } }
        
        //------------------//
        // Class Constructor
        //------------------//
        public CAcousticAversion() { SetToDefault(); }

        //-----------------//
        // Class Properties
        //-----------------//
        public Boolean beachingEnabled { get { return m_beachEnabled; } set { m_beachEnabled = value; } }

        public CAcousticAversion GetCopy()
        {
            CAcousticAversion c = new CAcousticAversion();

            c.SetToDefault();

            c.m_ascentEnabled = m_ascentEnabled;
            c.m_descentEnabled = m_descentEnabled;
            c.m_depthEnabled = m_depthEnabled;
            c.m_reverseEnabled = m_reverseEnabled;
            c.m_srfIntvlEnabled = m_srfIntvlEnabled;
            c.m_directnEnabled = m_directnEnabled;
            c.m_rateEnabled = m_rateEnabled;
            c.beachingEnabled = m_beachEnabled;
            c.m_fBttmEnabled = m_fBttmEnabled;

            // Dive Parameter Fields
            c.m_fbttmDives = m_fbttmDives;

            c.m_ascent.mean = m_ascent.mean;
            c.m_ascent.std = m_ascent.std;
            c.m_ascent.coeff = m_ascent.coeff;
            c.m_descent.mean = m_descent.mean;

            c.m_descent.std = m_descent.std ;
            c.m_descent.coeff = m_descent.coeff;

            c.m_depth.mean = m_depth.mean;
            c.m_depth.std = m_depth.std;

            c.m_reverse.meanCnt = m_reverse.meanCnt;
            c.m_reverse.stdCnt = m_reverse.stdCnt;
            c.m_reverse.meanTime = m_reverse.meanTime;
            c.m_reverse.stdTime = m_reverse.stdTime;
            c.m_reverse.prob = m_reverse.prob;

            c.m_surfintrvl.mean = m_surfintrvl.mean;
            c.m_surfintrvl.std = m_surfintrvl.std;

            c.m_direction.pert = m_direction.pert;
            c.m_direction.bias = m_direction.bias;
            c.m_direction.arcStep = m_direction.arcStep;
            c.m_direction.coeff = m_direction.coeff;

            c.m_rate.mean = m_rate.mean;
            c.m_rate.std = m_rate.std;
            c.m_rate.coeff = m_rate.coeff;

            return c;
        }

        //--------------//
        // Class Methods
        //--------------//

        void SetToDefault()
        {
//            m_aeTypeB = MBSDEFAULTS.AETYPEB;
  //          m_aeTypeA = MBSDEFAULTS.AETYPEA;
//            m_levelBDose = MBSDEFAULTS.LEVELBDOSE;
//            m_levelADose = MBSDEFAULTS.LEVELADOSE;

            // Activation, deactivation
//            m_actThreshB = MBSDEFAULTS.AE_ACTIVATION_THRESH;
//            m_actThreshA = MBSDEFAULTS.AE_ACTIVATION_THRESH;
//            m_deactThreshB = MBSDEFAULTS.AE_DEACTIVATE_THRESH;
//            m_deactThreshA = MBSDEFAULTS.AE_DEACTIVATE_THRESH;
//            m_sndPressUnitB = MBSDEFAULTS.AE_SND_PRESS_UNITB;
//            m_sndPressUnitA = MBSDEFAULTS.AE_SND_PRESS_UNITA;

            // Decay function
//            m_decayFncParamsB.decayFunctionType = MBSDEFAULTS.AE_DECAY_FNC;

              // Acoustic Exposure Affects...
            m_ascentEnabled = MBSDEFAULTS.AE_ASCENT_ENABLED;
            m_descentEnabled = MBSDEFAULTS.AE_DESCENT_ENABLED;
            m_depthEnabled = MBSDEFAULTS.AE_DEPTH_ENABLED;
            m_reverseEnabled = MBSDEFAULTS.AE_REVERSALS_ENABLED;
            m_srfIntvlEnabled = MBSDEFAULTS.AE_SURFINTVL_ENABLED;
            m_directnEnabled = MBSDEFAULTS.AE_DIRECTIN_ENABLED;
            m_rateEnabled = MBSDEFAULTS.AE_RATE_ENABLED;
            m_beachEnabled = MBSDEFAULTS.AE_BEACHING_ENABLED;
            m_fBttmEnabled = MBSDEFAULTS.AE_FLATBOTTMDIVEAFFECT_ENABLED;

            // Dive Parameter Fields
            m_fbttmDives = MBSDEFAULTS.AE_FLAT_BOTTOM_DIVES;

            m_ascent.mean = MBSDEFAULTS.AE_DIVE_ASCENT_MEAN;
            m_ascent.std = MBSDEFAULTS.AE_DIVE_ASCENT_STD;
            m_ascent.coeff = MBSDEFAULTS.AE_DIVE_ASCENT_COEFFICIENT;
            m_descent.mean = MBSDEFAULTS.AE_DIVE_DESCENT_MEAN;

            m_descent.std = MBSDEFAULTS.AE_DIVE_DESCENT_STD;
            m_descent.coeff = MBSDEFAULTS.AE_DIVE_DESCENT_COEFFICIENT;

            m_depth.mean = MBSDEFAULTS.AE_DIVE_DEPTH_MEAN;
            m_depth.std = MBSDEFAULTS.AE_DIVE_DEPTH_STD;

            m_reverse.meanCnt = MBSDEFAULTS.AE_DIVE_REVERSAL_COUNT_MEAN;
            m_reverse.stdCnt = MBSDEFAULTS.AE_DIVE_REVERSAL_COUNT_STD;
            m_reverse.meanTime = MBSDEFAULTS.AE_DIVE_REVERSAL_TIME_MEAN;
            m_reverse.stdTime = MBSDEFAULTS.AE_DIVE_REVERSAL_TIME_STD;
            m_reverse.prob = MBSDEFAULTS.AE_DIVE_REVERSAL_PROBABLILITY;

            m_surfintrvl.mean = MBSDEFAULTS.AE_DIVE_SURFINTVL_MEAN;
            m_surfintrvl.std = MBSDEFAULTS.AE_DIVE_SURFINTVL_STD;

            m_direction.pert = MBSDEFAULTS.AE_TRAVEL_CORRELATED_RANDOM_WALK_DIR_BIAS_PERTURBATION;
            m_direction.bias = MBSDEFAULTS.AE_TRAVEL_CORRELATED_RANDOM_WALK_DIR_BIAS_VALUE_OF_BIAS;
            m_direction.arcStep = MBSDEFAULTS.AE_TRAVEL_CORRELATED_RANDOM_WALK_DIR_BIAS_ARC_STEP;
            m_direction.coeff = MBSDEFAULTS.AE_TRAVEL_CORRELATED_RANDOM_WALK_DIR_BIAS_COEFFICENT;

            m_rate.mean = MBSDEFAULTS.AE_TRAVEL_RATE_MEAN;
            m_rate.std = MBSDEFAULTS.AE_TRAVEL_RATE_STD;
            m_rate.coeff = MBSDEFAULTS.AE_TRAVEL_RATE_COEFFICIENT;

        }
    }

    public class CBehaviorStimulusBase
    {
        protected CParamsDirectionCRndmWalkDirBias m_direction = new CParamsDirectionCRndmWalkDirBias();
        protected Boolean m_enabled;
        protected double m_actVal;
        public Boolean enabled { get { return m_enabled; } set { m_enabled = value; } }
    }

    public class CBehaviorPodFollow: CBehaviorStimulusBase
    {
        PODTYPE m_podType;
        public double focalDistance { get { return m_actVal; } set { m_actVal = CUtil.Min0OrKeep(value); } }
        public PODTYPE podType { get { return m_podType; } set { m_podType = value; } }
    }

    public class CEnvAttBaseMdl
    {
        protected CParamsDirectionCRndmWalkDirBias m_direction = new CParamsDirectionCRndmWalkDirBias();
        protected Boolean m_minEnable, m_maxEnable, m_deltaEnable;
        protected double m_fValA, m_fValB, m_fValC;
        public Boolean enabled { get { return m_minEnable || m_maxEnable || m_deltaEnable; } }
        public CEnvAttBaseMdl GetBaseCopy()
        {
            CEnvAttBaseMdl c = new CEnvAttBaseMdl();
            c.m_direction.GetCopy();
            c.m_fValA = m_fValA;
            c.m_fValB = m_fValB;
            c.m_fValC = m_fValC;

            c.m_minEnable = m_minEnable;
            c.m_maxEnable = m_maxEnable;
            c.m_deltaEnable = m_deltaEnable;
            return c;
        }
    }


    public class CEnvAttTemperatureMdl: CEnvAttBaseMdl
    {
        public Boolean coldEnabled { get { return m_minEnable; } set { m_minEnable = value; } }
        public Boolean warmEnabled { get { return m_maxEnable; } set { m_maxEnable = value; } }
        public Boolean frontEnabled { get { return m_deltaEnable; } set { m_deltaEnable = value; } }
        public double cold { get { return m_fValA; } set { m_fValA = value; } }
        public double warm { get { return m_fValB; } set { m_fValB = value; } }
        public double front { get { return m_fValC; } set { m_fValC = value; } }
        public CEnvAttTemperatureMdl GetCopy()
        {
            CEnvAttTemperatureMdl c = new CEnvAttTemperatureMdl();
            c.m_direction.GetCopy();
            c.m_fValA = m_fValA;
            c.m_fValB = m_fValB;
            c.m_fValC = m_fValC;

            c.m_minEnable = m_minEnable;
            c.m_maxEnable = m_maxEnable;
            c.m_deltaEnable = m_deltaEnable;
            return c;
        }
    }

    public class CEnvAttDepthMdl
    {
        // Any changes to this class need to be handled in the GetCopy() function or
        // you'll waste time debugging...
        private Boolean m_shelfEnabled;
        private double m_shelfDepth;
        private double m_shelfSlope;

        private Boolean m_basinEnabled;
        private double m_basinDepth;
        private double m_basinSlope;

        private Boolean m_slopeEnabled;
        private double m_slopeDepth;
        private double m_slopeSlope;

        private CParamsDirectionCRndmWalkDirBias m_direction = new CParamsDirectionCRndmWalkDirBias();

        public Boolean enabled { get { return m_shelfEnabled || m_basinEnabled || m_slopeEnabled; } }
        public Boolean shelfEnabled { get { return m_shelfEnabled; } set { m_shelfEnabled = value; } }
        public Boolean basinEnabled { get { return m_basinEnabled; } set { m_basinEnabled = value; } }
        public Boolean slopeEnabled { get { return m_slopeEnabled; } set { m_slopeEnabled = value; } }

        public double shelfDepth { get { return m_shelfDepth; } set { m_shelfDepth = value; } }
        public double basinDepth { get { return m_basinDepth; } set { m_basinDepth = value; } }
        public double slopeDepth { get { return m_slopeDepth; } set { m_slopeDepth = value; } }

        public double shelfSlope { get { return m_shelfSlope; } set { m_shelfSlope = value; } }
        public double basinSlope { get { return m_basinSlope; } set { m_basinSlope = value; } }
        public double slopeSlope { get { return m_slopeSlope; } set { m_slopeSlope = value; } }

        public CEnvAttDepthMdl GetCopy()
        {
            CEnvAttDepthMdl c = new CEnvAttDepthMdl();
            c.m_direction.GetCopy();

            c.m_shelfEnabled = m_shelfEnabled;
            c.m_shelfDepth = m_shelfDepth;
            c.m_shelfSlope = m_shelfSlope;

            c.m_basinEnabled = m_basinEnabled;
            c.m_basinDepth = m_basinDepth;
            c.m_basinSlope = m_basinSlope;

            c.m_slopeEnabled = m_slopeEnabled;
            c.m_slopeDepth = m_slopeDepth;
            c.m_slopeSlope = m_slopeSlope;

            return c;
        }
    }
}

