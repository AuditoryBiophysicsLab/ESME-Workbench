#ifndef MMMBSDATATYPES_SCENARIOEXECUTION_H
#define MMMBSDATATYPES_SCENARIOEXECUTION_H
#include <windows.h>


// All activities the animat may be in at any given moment.
enum ACTIVITY
{
	IN_SURFACE_INTERVAL,
	BOTTOM_FOLLOWING,
	ASCENDING,
	DESCENDING,
	ASCENDING_REVERSAL,
	DESCENDING_REVERSAL,
	BEACHED,
};
enum PODLEADERTYPE
{
	ANIMAT = 0,
	CENTROID = 1,
};

typedef struct Coordinate
{
	double lon; // x
	double lat; // y
}COORDINATE;

// Defines setup to limit output by iterations.
typedef struct OutputIntervalLimit
{
	DWORD start;
	DWORD interval;
	BOOL enabled;
}OUTPTINTRVLLMT;


typedef struct FollowState
{
	// Status
	BOOL isActive;
	int timeLapsed;
	double maxFocalDistance; // s/b in species modeling, but is set during setup, so is placed here.
	double distanceTo;
	double desiredHeading;// the heading this stimulus wants the animat to go
	COORDINATE focalCoord;
}FOLLOWSTATE;

typedef struct ShoreFollowState
{
	BOOL isActive;
	double adjustedHeading;// the heading this stimulus wants the animat to go
}SHOREFOLLOWSTATE;

typedef struct EnvironmentalAttractorSettings
{
	double distanceTo;
	double angleTo;
	double desiredHeading;// the heading this stimulus wants the animat to go
	BOOL isActive;
	COORDINATE coordTo;
	double value;
	int timeLapsed;
}ENVATTRACTORSTATE;


// Surface interval information.
typedef struct SurfaceIntervalState
{
	int timeLapsed;	// start time of SI
	int setDuration;	// interval duration
} SRFINTRVLSTATE;

// Animat reveral information.
typedef struct ReversalState
{
	int remainingQty;	// number of reversals to engage
	int timeLapsed;		// time that the reversal starts
	int legDuration;		// time spent in a reversed mode
}REVERSALSTATE;

typedef struct RateState
{
	double rate; // meters per second.
	int timeLapsed; // time of engagement.
	double endDice; // random number for determining time of dive speed change.
}RATESTATE;

// Animat rates, used for both horizontal and diving.
typedef struct DiveState
{
	SRFINTRVLSTATE surfInterval;
	RATESTATE rate;
	double targetDepth;
	double projectedDepth;
	REVERSALSTATE reversal;
	ACTIVITY activity;
}DIVESTATE;

//-----------------------------//
// Animat behavior information.
//------------------------------//

/* TRANSITNSTATE: One of many structs that maintains information about animat state.
Maintains the time (in seconds) the animat has spent in its current normal behavior
(not behavior modifiers such as acoustic aversion and shore following) and a value used
in determining when that behavior comes to an end.*/ 
typedef struct BehaviorTransitionState
{
	int timeLapsed;	// time spent in current behavior, used for T50_K_TERM model types but maintained for both.
	int timeRemaining; // Used only for GAUSSIAN_TERM model behavior termination model types.
	double endDice;	// random number for determining time of behavior change, used for T50_K_TERM model types
}TRANSITNSTATE;

// The horizontal direction the animat is facing and related information.
typedef struct DirectionState
{
	double bearing;		// direction of animal movmement relative to true north
	int timeLapsed;	// time a particular direction is engaged
	double endDice;	// random number for determining time of direction change
}DIRECTIONSTATE;

typedef struct SubModelState
{
	TRANSITNSTATE behavior;
	DIRECTIONSTATE travelDirection;
	RATESTATE travelRate;
	DIVESTATE dive;
}SUBMDLSTATE;


typedef struct AcousticAversionState
{
	// Stimulus
	COORDINATE actualSrcCoord;
	//COORDINATE responseCoord;
	double actualSrcInstantValue;
	double cumulativeValue;
	//double responseValue;
	double responseSrcAngle;
	double desiredHeading;

	// Book Keeping
	// Status
	double risk;
	double maxInstant;
	BOOL isSet; // set by external function.  Being set does not mean acoustic aversion is active.
	BOOL isActive; // Once acoustic respons is activated it remains activated
	//BOOL isPermanentlyActive;
	int timeLapsed;
	BOOL lvlAPhysFlag; // Flag indicating if the source is currently exceeding threshold.
	int lvlAPhysFlagCnt; // Number of times threshold has been exceeded (not duration of it).
	BOOL lvlBPhysFlag; // Flag indicating if the source is currently exceeding threshold.
	int lvlBPhysFlagCnt; // Number of times threshold has been exceeded (not duration of it).


	BOOL lvlBBehFlag; //Flag indicating if the source is currently exceeding threshold.
	int lvlBBehFlagCnt; //Number of times threshold has been exceeded (not duration of it).
}ACSTCAVRSN_STATE;

typedef struct AnimatOffScreenInformation
{
	BOOL offScreen; // Indicates the animat went off map
	BOOL lvlAPhysFlagPrev; // Flag indicating if the source is currently exceeding threshold.
	BOOL lvlBPhysFlagPrev; // Flag indicating if the source is currently exceeding threshold.
	BOOL lvlBBehFlagPrev; // Flag indicating if the source triggred a risk value 
	double lvlBBehRiskValue; // The risk value.
	double actualSrcInstantValue;
	double cumulativeValue;
}OFFSCREEN_INF;

typedef struct FLOAT_XY
{
	double x;
	double y;
}FLOAT_XY; // used in planar geometry distance calculations.

typedef struct INT_XY
{
	int x;
	int y;
}INT_XY;

typedef struct BathyValue
{
	double depth;
	double slope;
	double slopeHeading;
}BATHYVALUE;

typedef struct AnimatState
{
	// Clock
	UINT simClock;
	UINT16 behState;		// behavioral state of the marine mammal at time
	INT16 nextBehState;    // the next behavioral state of the marine mammal at time
	BOOL behTransActive;    // indicates a next behavior state has been determined.
	//COORDINATE initialCoord;
	BOOL beached;

	double setDiveRate; // set by the models
	double setTravelRate;// set by the models
	double setHeading;// set by the models

	// Coordinate, depth, heading, rates, ... at begining of iteration.
	COORDINATE coord;
	FLOAT_XY deltaXY;
	double depth;

	double bathyDepth;
	double bathySlope;
	double bathySlopeHeading;

	// Normal Behaviors
	SUBMDLSTATE submdl;

	// Stimuli Behaviors
	FOLLOWSTATE podFollowFocal; // lat, lon, focal distance
	SHOREFOLLOWSTATE shoreFollow;
	ACSTCAVRSN_STATE acstcExp;
	//ACSTCAVRSN_STATE acstcExpA;
	ENVATTRACTORSTATE tempEnvAtt;
	ENVATTRACTORSTATE depthEnvAtt;
	OFFSCREEN_INF offScreenInf;
}ANIMATSTATE;


#endif //MMMBSDATATYPES_SCENARIOEXECUTION_H