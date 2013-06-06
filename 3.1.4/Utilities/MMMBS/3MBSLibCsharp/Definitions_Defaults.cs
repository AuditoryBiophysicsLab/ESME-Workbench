using System;
using System.Collections.Generic;
using System.Text;

namespace MMMBSLib
{
    public class MBSDEFAULTS
    {
        //----------------------//
        // (1) Acoustic Aversion
        // 47 parameters
        //----------------------//
        // Pod Reaction
        public static Boolean AE_POD_BREAKS_UP = false;
        // Thresholds
        public static double AE_ACTIVATION_THRESH = 0.0;
        public static double AE_DEACTIVATE_THRESH = 0.0;
        public static SOUNDPRESSUREUNIT AE_SND_PRESS_UNITB = SOUNDPRESSUREUNIT.MICRO_PA;
        public static SOUNDPRESSUREUNIT AE_SND_PRESS_UNITA = SOUNDPRESSUREUNIT.MICRO_PA;

        public static AETYPE AETYPEB = AETYPE.DOSE;
        public static AETYPE AETYPEA = AETYPE.THRESHOLD;
        public static DOSERESPONSE LEVELBDOSE = DOSERESPONSE.ODONTOCETE;
        public static DOSERESPONSE LEVELADOSE = DOSERESPONSE.ODONTOCETE;

        public static DECAYFUNCTIONS AE_DECAY_FNC = DECAYFUNCTIONS.DISABLED;

        // Movement Enables/Disables
        public static Boolean AE_FLATBOTTMDIVEAFFECT_ENABLED = true; // if AE affects flat bottom diving
        public static Boolean AE_ASCENT_ENABLED = false;
        public static Boolean AE_DESCENT_ENABLED = false;
        public static Boolean AE_DEPTH_ENABLED = false;
        public static Boolean AE_REVERSALS_ENABLED = false;
        public static Boolean AE_SURFINTVL_ENABLED = false;
        public static Boolean AE_DIRECTIN_ENABLED = false;
        public static Boolean AE_RATE_ENABLED = false;
        public static Boolean AE_BEACHING_ENABLED = false;

        // Movement Params Dive
        public static Boolean AE_FLAT_BOTTOM_DIVES = false; // the effect on flat bottom diving
        public static double AE_DIVE_ASCENT_MEAN = 20.0;
        public static double AE_DIVE_ASCENT_STD = 5.0;
        public static double AE_DIVE_ASCENT_COEFFICIENT = 0.2;

        public static double AE_DIVE_DESCENT_MEAN = 20.0;
        public static double AE_DIVE_DESCENT_STD = 5.0;
        public static double AE_DIVE_DESCENT_COEFFICIENT = 0.2;

        public static double AE_DIVE_DEPTH_MEAN = 20.0;
        public static double AE_DIVE_DEPTH_STD = 10.0;

        public static double AE_DIVE_REVERSAL_COUNT_MEAN = 10.0;
        public static double AE_DIVE_REVERSAL_COUNT_STD = 1.0;
        public static double AE_DIVE_REVERSAL_TIME_MEAN = 20.0;
        public static double AE_DIVE_REVERSAL_TIME_STD = 3.2;
        public static double AE_DIVE_REVERSAL_PROBABLILITY = 0.2;

        public static double AE_DIVE_SURFINTVL_MEAN = 50.0;
        public static double AE_DIVE_SURFINTVL_STD = 2.0;

        // Movement Params Travel
        public static double AE_TRAVEL_CORRELATED_RANDOM_WALK_DIR_BIAS_PERTURBATION = 30.0;
        //public static double CORRELATED_RANDOM_WALK_DIR_BIAS_DIRECTION_OF_BIAS;
        public static double AE_TRAVEL_CORRELATED_RANDOM_WALK_DIR_BIAS_VALUE_OF_BIAS = .02;
        public static double AE_TRAVEL_CORRELATED_RANDOM_WALK_DIR_BIAS_ARC_STEP = 5.0;
        public static double AE_TRAVEL_CORRELATED_RANDOM_WALK_DIR_BIAS_COEFFICENT = 0.2;

        public static double AE_TRAVEL_RATE_MEAN = 20.0;
        public static double AE_TRAVEL_RATE_STD = 5.0;
        public static double AE_TRAVEL_RATE_COEFFICIENT = 0.2;



        //---------------//
        // Species Params
        //---------------//
        public static double BEACHING_DEPTH = -2.000000000;
        public static double SHORE_FOLLOW_DEPTH = -10.0000000;
        public static double SEED_MIN_DEPTH = -10.0000000;
        public static double SEED_DEPTH_LIMIT = -500000.0;
        public static Boolean SEED_DEPTH_LIMIT_ENABLED = false;

        // ------------------------//
        // Animat Behavior Modeling
        // ------------------------//
        public static double BEHAVIOR_TRANS_MEANTIME_IN_BEHAVIOR = 60.0;
        public static double BEHAVIOR_SLOPE_COEFF = 7.0;
        public static double BEHAVIOR_TRANS_TERMINATION_COEFF = 0.2;

        public static double[][] BEHAVIOR_INITIAL_TRANSLATION_MATRIX
        {
            get
            {
                double[][] v = {
                    new double[] { 0.00, 24.00, 0.00, 1.00, BEHAVIOR_TRANS_MEANTIME_IN_BEHAVIOR, BEHAVIOR_SLOPE_COEFF }
                };
                return v;
            }
        }

        public static double[][] BEHAVIOR_INITIAL_MATRIX
        {
            get
            {
                double[][] v = {new double[] { 0.00,  24.00, 0.00, 1.00}};
                return v;
            }
        }


        //------------------------------------------------------------------------------//
        // Travel Direction
        //-----------------//
        public static DIRECTIONMODELTYPE DIRECTION_MODEL_TYPE = DIRECTIONMODELTYPE.RANDOM_WALK;

        public static double RANDOM_WALK_COEFFICENT = 0.2;

        public static double CORRELATED_RANDOM_WALK_PERTURBATION = 10.0;
        public static double CORRELATED_RANDOM_WALK_COEFFICENT = 0.2;

        public static double CORRELATED_RANDOM_WALK_DIR_BIAS_PERTURBATION = 10.0;
        public static double CORRELATED_RANDOM_WALK_DIR_BIAS_DIRECTION_OF_BIAS = 0.0;
        public static double CORRELATED_RANDOM_WALK_DIR_BIAS_VALUE_OF_BIAS = 0.02;
        public static double CORRELATED_RANDOM_WALK_DIR_BIAS_ARC_STEP = 5.0;
        public static double CORRELATED_RANDOM_WALK_DIR_BIAS_COEFFICENT = 0.2;

        public static double[] DIRECTIONAL_PROB_VECTOR0
        {
            get
            {
                double[] v = { 0.050, 0.039, 0.035, 0.025, 0.025, 0.025, 0.043, 0.093, 0.188, 0.147, 0.091, 0.039, 0.023, 0.025, 0.033, 0.027, 0.037, 0.054 };
                return v;
            }
        }

        public static double[] DIRECTIONAL_PROB_VECTOR1
        {
            get
            {
                double[] v = {0.060, 0.048, 0.036, 0.012, 0.024, 0.000, 0.048, 0.107, 0.190, 0.155, 0.083, 0.024, 0.000, 0.012, 0.000, 0.024, 0.048, 0.131};
                return v;
            }
        }
        public static double[] DIRECTIONAL_PROB_VECTOR2
        {
            get
            {
                double[] v = {0.072, 0.048, 0.019, 0.039, 0.034, 0.043, 0.039, 0.116, 0.193, 0.179, 0.039, 0.024, 0.024, 0.014, 0.034, 0.019, 0.029, 0.034};
                return v;
            }
        }
        public static double[] DIRECTIONAL_PROB_VECTOR3
        {
            get
            {
                double[] v = {0.065, 0.042, 0.032, 0.028, 0.034, 0.037, 0.040, 0.087, 0.185, 0.141, 0.085, 0.046, 0.026, 0.020, 0.033, 0.022, 0.025, 0.052};
                return v;
            }
        }
        public static double[] DIRECTIONAL_PROB_VECTOR4
        {
            get
            {
                double[] v = {0.028, 0.012, 0.008, 0.009, 0.010, 0.015, 0.024, 0.079, 0.361, 0.315, 0.059, 0.019, 0.012, 0.006, 0.007, 0.006, 0.009, 0.023};
                return v;
            }
        }


        public static double[][] DIRECTIONAL_PROB_BIAS
        {
            get
            {
                double[][] v = {
               //new double[] {0.000, 0.001, 0.010, 0.100, 1.000, 1.001, 1.010, 1.100, 1.101, 1.110, 1.111, 11.057, 0.027, 0.019, 0.015, 0.020, 0.020, 0.041},
               new double[] {0.006, 0.002, 0.001, 0.000, 0.000, 0.002, 0.002, 0.003, 0.018, 0.555, 0.213, 0.057, 0.027, 0.019, 0.015, 0.020, 0.020, 0.041},
               new double[] {0.019, 0.015, 0.015, 0.010, 0.017, 0.002, 0.017, 0.021, 0.011, 0.179, 0.253, 0.177, 0.067, 0.055, 0.036, 0.040, 0.032, 0.034},
               new double[] {0.023, 0.027, 0.050, 0.057, 0.031, 0.050, 0.034, 0.008, 0.019, 0.061, 0.084, 0.191, 0.130, 0.061, 0.065, 0.046, 0.027, 0.038},
               new double[] {0.016, 0.026, 0.062, 0.104, 0.062, 0.067, 0.057, 0.036, 0.010, 0.031, 0.047, 0.093, 0.104, 0.124, 0.062, 0.052, 0.036, 0.010},
               new double[] {0.044, 0.044, 0.006, 0.088, 0.127, 0.061, 0.039, 0.039, 0.022, 0.028, 0.022, 0.094, 0.066, 0.122, 0.083, 0.039, 0.061, 0.017},
               new double[] {0.045, 0.037, 0.037, 0.066, 0.066, 0.091, 0.120, 0.070, 0.041, 0.050, 0.054, 0.054, 0.079, 0.017, 0.050, 0.058, 0.033, 0.033},
               new double[] {0.024, 0.027, 0.027, 0.024, 0.041, 0.058, 0.150, 0.167, 0.109, 0.046, 0.070, 0.053, 0.027, 0.022, 0.027, 0.039, 0.051, 0.041},
               new double[] {0.035, 0.016, 0.014, 0.014, 0.017, 0.021, 0.065, 0.188, 0.318, 0.112, 0.079, 0.013, 0.010, 0.017, 0.012, 0.013, 0.026, 0.028},
               new double[] {0.019, 0.009, 0.012, 0.007, 0.006, 0.009, 0.020, 0.079, 0.377, 0.335, 0.051, 0.018, 0.009, 0.007, 0.004, 0.006, 0.011, 0.021},
               new double[] {0.026, 0.010, 0.010, 0.007, 0.008, 0.011, 0.025, 0.058, 0.158, 0.399, 0.139, 0.049, 0.009, 0.014, 0.013, 0.013, 0.023, 0.028},
               new double[] {0.021, 0.049, 0.040, 0.019, 0.021, 0.021, 0.051, 0.055, 0.070, 0.205, 0.171, 0.122, 0.038, 0.021, 0.015, 0.019, 0.034, 0.030},
               new double[] {0.032, 0.020, 0.032, 0.040, 0.032, 0.056, 0.069, 0.036, 0.044, 0.069, 0.089, 0.149, 0.093, 0.056, 0.044, 0.052, 0.065, 0.020},
               new double[] {0.015, 0.025, 0.066, 0.091, 0.107, 0.071, 0.071, 0.061, 0.051, 0.056, 0.015, 0.041, 0.081, 0.056, 0.076, 0.056, 0.030, 0.030},
               new double[] {0.017, 0.017, 0.045, 0.034, 0.085, 0.074, 0.114, 0.063, 0.051, 0.040, 0.045, 0.085, 0.063, 0.074, 0.085, 0.045, 0.040, 0.023},
               new double[] {0.038, 0.023, 0.057, 0.076, 0.095, 0.099, 0.122, 0.092, 0.046, 0.057, 0.046, 0.034, 0.042, 0.050, 0.065, 0.031, 0.015, 0.011},
               new double[] {0.030, 0.030, 0.030, 0.030, 0.018, 0.071, 0.129, 0.204, 0.162, 0.059, 0.061, 0.032, 0.028, 0.016, 0.018, 0.030, 0.024, 0.030},
               new double[] {0.026, 0.019, 0.018, 0.012, 0.015, 0.018, 0.049, 0.178, 0.380, 0.138, 0.052, 0.022, 0.013, 0.007, 0.005, 0.011, 0.017, 0.019},
               new double[] {0.018, 0.011, 0.005, 0.005, 0.006, 0.008, 0.022, 0.077, 0.393, 0.332, 0.061, 0.017, 0.007, 0.005, 0.005, 0.007, 0.006, 0.014}};
                return v;
            }
        }

        public static double DIRECTIONAL_TERMINATE = 0.2;
        //------------------------------------------------------------------------------//


        //------------------------------------------------------------------------------//
        // Travel Rate
        //------------//
        public static MODELTYPE RATE_MODEL_TYPE = MODELTYPE.GAUSSIAN;
        public static double TRAVEL_RATE_GAUSS_MEAN = 2.0;
        public static double TRAVEL_RATE_GAUSS_STD = 0.5;
        public static double TRAVEL_RATE_GAUSS_COEFF = 0.2;
        public static double TRAVEL_RATE_RANDOM_MAX = 0.0;
        public static double TRAVEL_RATE_RANDOM_MIN = 0.0;
        public static double TRAVEL_RATE_RANDOM_COEFF = 0.0;
        public static double[] TRAVEL_RATE_VECTOR0
        {
            get
            {
                double[] v = { 0.00, 0.263, 0.580, 0.765, 0.890, 0.937, 0.961, 0.979, 0.991, 0.993, 0.996, 1.000 };
                return v;
            }
        }

        public static double[] TRAVEL_RATE_VECTOR1
        {
            get
            {
                double[] v = { 0.00, 0.286, 0.592, 0.806, 0.888, 0.949, 0.949, 0.969, 0.980, 0.980, 0.980, 1.000 };
                return v;
            }
        }
        public static double[] TRAVEL_RATE_VECTOR2
        {
            get
            {
                double[] v = { 0.00, 0.516, 0.820, 0.916, 0.956, 0.980, 0.992, 1.000, 1.000, 1.000, 1.000, 1.000 };
                return v;
            }
        }
        public static double[] TRAVEL_RATE_VECTOR3
        {
            get
            {
                double[] v = { 0.00, 0.409, 0.738, 0.893, 0.951, 0.976, 0.986, 0.993, 0.995, 0.996, 0.998, 1.000 };
                return v;
            }
        }
        public static double[] TRAVEL_RATE_VECTOR4
        {
            get
            {
                double[] v = { 0.00, 0.108, 0.459, 0.789, 0.912, 0.963, 0.981, 0.992, 0.996, 0.998, 0.998, 1.000};
                return v;
            }
        }

        public static double TRAVEL_RATE_STEP = 0.5;
        public static double TRAVEL_RATE_TERMINATE = 0.2;
        //------------------------------------------------------------------------------//


        //------------------------------------------------------------------------------//
        // Flat Bottom Diving / Bottom Following
        //--------------------------------------//
        public static Boolean FLAT_BOTTOM_DIVE_ENABLE = false;
        //------------------------------------------------------------------------------//

        //------------------------------------------------------------------------------//
        // Ascent Rate Model
        //------------------//
        public static MODELTYPE ASCENT_MODEL_TYPE = MODELTYPE.GAUSSIAN;
        public static double ASCENT_GAUSS_MEAN = 2.0;
        public static double ASCENT_GAUSS_STD = 0.5;
        public static double ASCENT_GAUSS_COEFF = 0.2;
        public static double ASCENT_RANDOM_MAX = 0.0;
        public static double ASCENT_RANDOM_MIN = 0.0;
        public static double ASCENT_RANDOM_COEFF = 0.0;
        public static double[] ASCENT_RATE_VECTOR
        {
            get { double[] v = { 0.00, 0.15, 0.30, 0.45, 0.55, 0.85, 0.95, 1.00 }; return v; }
        }

        public static double ASCENT_RATE_STEP = 0.50;
        public static double ASCENT_RATE_TERMINATE = 0.2;

        //------------------------------------------------------------------------------//

        //------------------------------------------------------------------------------//
        // Descent Rate Model
        //-------------------//
        public static MODELTYPE DESCENT_MODEL_TYPE = MODELTYPE.GAUSSIAN;
        public static double DESCENT_GAUSS_MEAN = 2.0;
        public static double DESCENT_GAUSS_STD = 0.5;
        public static double DESCENT_GAUSS_COEFF = 0.2;
        public static double DESCENT_RANDOM_MAX = 0.0;
        public static double DESCENT_RANDOM_MIN = 0.0;
        public static double DESCENT_RANDOM_COEFF = 0.0;

        public static double[] DESCENT_RATE_VECTOR
        {
            get { double[] v = { 0.00, 0.15, 0.30, 0.45, 0.55, 0.85, 0.95, 1.00 }; return v; }
        }

        public static double DESCENT_RATE_STEP = 0.50;
        public static double DESCENT_RATE_TERMINATE = 0.2;
        //------------------------------------------------------------------------------//

        //------------------------------------------------------------------------------//
        // Depth Model
        //------------//
        public static MODELTYPE DEPTH_MODEL_TYPE = MODELTYPE.GAUSSIAN;
        public static double DEPTH_GAUSS_MEAN = 500.0;
        public static double DEPTH_GAUSS_STD = 50.0;
        public static double DEPTH_RANDOM_MAX = 0.0;

        public static double[] DEPTH_VECTOR
        {
            get { double[] v = { 0.00, 0.10, 0.25, 0.5, 0.75, 1.00 }; return v; }
        }

        public static double DEPTH_STEP = 50;
        //------------------------------------------------------------------------------//

        //------------------------------------------------------------------------------//
        // Reversal Model
        //---------------//
        public static MODELTYPE REVERSALS_MODEL_TYPE = MODELTYPE.GAUSSIAN;
        public static Boolean REVERSALS_ENABLED = false;
        //public static Boolean REVERSALS_DIVE_RATE_ENABLED = false;
        public static REVERSAL_DIVE_RATE_TYPE REVERSALS_DIVE_RATE_TYPE = REVERSAL_DIVE_RATE_TYPE.NO_INDEPENDENT;
        public static double REVERSALS_DIVE_RATE_GAUSS_MEAN = 5.0;
        public static double REVERSALS_DIVE_RATE_GAUSS_STD = 2.0;
        public static double REVERSALS_DIVE_RATE_GAUSS_TERMCOEFF = 0.2;
        public static double REVERSALS_GAUSS_MEAN_COUNT = 0.0;
        public static double REVERSALS_GAUSS_STD_COUNT = 0.0;
        public static double REVERSALS_GAUSS_PROBABILITY = 0.0;
        public static double REVERSALS_GAUSS_MEAN_TIME = 0.0;
        public static double REVERSALS_GAUSS_STD_TIME = 0.0;
        public static int REVERSALS_RANDOM_MAX_COUNT = 0;
        public static int REVERSALS_RANDOM_MIN_COUNT = 0;
        public static double REVERSALS_RANDOM_PROBABILITY = 0.0;
        public static double REVERSALS_RANDOM_MEAN_TIME = 0.0;
        public static double REVERSALS_RANDOM_STD_TIME = 0.0;

        public static double REVERSALS_PROBABILITY0 = 0.2;
        public static double REVERSALS_PROBABILITY1 = 0.5;
        public static double REVERSALS_PROBABILITY2 = 0.3;
        public static double REVERSALS_PROBABILITY3 = 0.5;
        public static double REVERSALS_PROBABILITY4 = 0.8;

        public static double[] REVERSALS_COUNT_VECTOR0
        {
            get { double[] v = { 0.00, 0.20, 0.40, 0.6, 1.000 }; return v; }
        }

        public static double[] REVERSALS_COUNT_VECTOR1
        {
            get { double[] v = { 0.00, 0.40, 0.60, 0.8, 1.000 }; return v; }
        }
        public static double[] REVERSALS_COUNT_VECTOR2
        {
            get { double[] v = { 0.00, 0.15, 0.30, 0.45, 1.00 }; return v; }
        }
        public static double[] REVERSALS_COUNT_VECTOR3
        {
            get { double[] v = { 0.00, 0.30, 0.60, 0.9, 1.000 }; return v; }
        }
        public static double[] REVERSALS_COUNT_VECTOR4
        {
            get { double[] v = { 0.00, 0.45, 0.65, 0.85, 1.00 }; return v; }
        }
               
        public static double[] REVERSALS_TIME_VECTOR0
        {
            get { double[] v = { 0.00, 0.20, 0.40, 0.60, 1.00 }; return v; }
        }

        public static double[] REVERSALS_TIME_VECTOR1
        {
            get { double[] v = { 0.00, 0.40, 0.60, 0.80, 1.00 }; return v; }
        }
        public static double[] REVERSALS_TIME_VECTOR2
        {
            get { double[] v = { 0.00, 0.15, 0.30, 0.45, 1.00 }; return v; }
        }
        public static double[] REVERSALS_TIME_VECTOR3
        {
            get { double[] v = { 0.00, 0.30, 0.60, 0.90, 1.00 }; return v; }
        }
        public static double[] REVERSALS_TIME_VECTOR4
        {
            get { double[] v = { 0.00, 0.45, 0.65, 0.85, 1.00 }; return v; }
        }

        public static double REVERSALS_TIME_STEP = 0.2;
        //------------------------------------------------------------------------------//

        //------------------------------------------------------------------------------//
        // Surface Intervals
        //------------------//
        public static SRFINVMODELTYPE SURFACE_INTERVAL_MODEL_TYPE = SRFINVMODELTYPE.GAUSSIAN;
        public static double SURFACE_INTERVAL_GAUSS_MEAN = 120.0;
        public static double SURFACE_INTERVAL_GAUSS_STD = 30.0;

        public static double[] SURFACE_INTERVAL_VECTOR0
        {
            get { double[] v = { 0.00, 0.25, 0.45, 0.60, 0.80, 1.00 }; return v; }
        }

        public static double[] SURFACE_INTERVAL_VECTOR1
        {
            get { double[] v = { 0.00, 0.20, 0.30, 0.60, 0.80, 1.00 }; return v; }
        }

        public static double[] SURFACE_INTERVAL_VECTOR2
        {
            get { double[] v = { 0.00, 0.30, 0.35, 0.45, 0.80, 1.00 }; return v; }
        }

        public static double[] SURFACE_INTERVAL_VECTOR3
        {
            get { double[] v = { 0.00, 0.50, 0.60, 0.70, 0.80, 1.00 }; return v; }
        }
        public static double[] SURFACE_INTERVAL_VECTOR4
        {
            get { double[] v = { 0.00, 0.10, 0.25, 0.55, 0.90, 1.00 }; return v; }
        }

        public static double SURFACE_INTERVAL_STEP = 1.0;
        //------------------------------------------------------------------------------//


        //--------------------------//
        // Environmental Influcences
        //--------------------------//
        // Priority
        public static ENVATTRACTORPRIORITY ENVINF_PRIORITY = ENVATTRACTORPRIORITY.DIVE;
        // Depth
        public static Boolean ENVINFL_DEPTH_SHALLOW_ENABLED = false;
        public static Boolean ENVINFL_DEPTH_DEEP_ENABLED = false;
        public static Boolean ENVINFL_DEPTH_SHELF_ENABLED = false;

        public static double ENVINFL_DEPTH_SHELF_BATHYDEPTH_METERS = -550; // negative for below surface.
        public static double ENVINFL_DEPTH_BASIN_BATHYDEPTH_METERS = -550; // negative for below surface.
        public static double ENVINFL_DEPTH_SLOPE_BATHYDEPTH_METERS = -150; // negative for below surface.

        public static double ENVINFL_DEPTH_SHELF_BATHYSLOPE_DEGREES = 1.0; // values less than this
        public static double ENVINFL_DEPTH_BASIN_BATHYSLOPE_DEGREES = 1.0; // values less than this
        public static double ENVINFL_DEPTH_SLOPE_BATHYSLOPE_DEGREES = 1.0; // values greater than this

        // Temperature, Celsius
        public static Boolean ENVINFL_TEMP_COLD_ENABLED = false;
        public static Boolean ENVINFL_TEMP_WARM_ENABLED = false;
        public static Boolean ENVINFL_TEMP_FRONT_ENABLED = false;

        public static double ENVINFL_TEMP_COLD = 20;
        public static double ENVINFL_TEMP_WARM = 70;
        public static double ENVINFL_TEMP_SHELF = 30;



        public enum BITMAPDISPLAYTYPE { INITIAL_BEHAVIOR, BEHAVIOR_TRANSITION }
    }
}