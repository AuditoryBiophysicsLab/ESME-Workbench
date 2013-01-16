
#if !defined(AFX_3MBS_H__D078736E_FF6B_414E_995E_B535F4D31989__INCLUDED_)
#define AFX_3MBS_H__D078736E_FF6B_414E_995E_B535F4D31989__INCLUDED_


#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#define WIN32_LEAN_AND_MEAN		// Exclude rarely-used stuff from Windows headers

// Windows Header Files:
#include <windows.h>

// C RunTime Header Files
#include <stdlib.h>
#include <malloc.h>
#include <memory.h>
#include <tchar.h>
#include <stdio.h>
#include <time.h>
#include <sys/types.h>
#include <sys/timeb.h>
//#include <winuser.h.>
#include <commctrl.h>

#include "resource.h"
#include "datatypes.h" // mbs class library
#include "3MBSLib.h" // mbs functions library

#include "Scenario.h"
#include "datatypes.h"
#include "EnvironmentData.h"
#include "FileExtracter.h"


//-------------------------------------------------------------------------------------//
// User window messages
//---------------------//
#define WM_SEED_MOUSE_MOVE				WM_APP +  5
#define WM_USER_CANCEL					WM_APP +  6
#define WM_SEED_COORDINATE				WM_APP +  7
#define WM_UPDATE_PROGRESS_BAR			WM_APP +  8
#define WM_UPDATE_ENV_ANIMATS			WM_APP +  9
#define WM_UPDATE_PROGRESS_MESSAGE		WM_APP + 10
#define WM_REFRESH_ENV_BITMAP			WM_APP + 11
#define WM_UPDATE_GUI					WM_APP + 12
#define WM_CAPTURE_MOUSE_DOWN			WM_APP + 13
#define	WM_ADD_LIST_BOX_MESSAGE			WM_APP + 14
#define WM_3MBS_LOAD_SCENARIO			WM_APP + 15
#define WM_HANDLE_CMDLINE_ARG			WM_APP + 16
#define WM_REDRAW_BITMAP_THREAD			WM_APP + 17
#define WM_UPDATE_BITMAP				WM_APP + 18
#define WM_SHUTDOWN_THREAD				WM_APP + 19
#define WM_3MBS_EXTRACT_BIN_TO_TEXT		WM_APP + 20
#define WM_UPDATE_TOGGLED_INF			WM_APP + 21
#define WM_ZOOMBITMAP					WM_APP + 22
#define WM_ZOOMOUTBITMAP				WM_APP + 23
#define WM_SEED_MOUSE_MOVE_DEPTHSCALE	WM_APP + 24
#define WM_SEED_MOUSE_MOVE_SLOPESCALE	WM_APP + 25
#define WM_3MB_LOAD_BIN_INTO_MEMORY     WM_APP + 26
#define WM_3MB_REDRAWALL                WM_APP + 27
#define WM_3MB_CREATEBITMAPS            WM_APP + 28
#define WM_INITIALIZE_BATHYMAP_SETUP      WM_APP + 29
#define WM_3MB_RUN_SEARCH_QUERY_THREAD  WM_APP + 30
#define WM_3MB_TOGGLE_PLAYBACK_RUN      WM_APP + 31
#define WM_3MB_TOGGLE_SLOPEHEADING		WM_APP + 32
#define WM_3MB_RESET					WM_APP + 33
#define WM_3MB_CHECKSTATUS				WM_APP + 34
//#define WM_3MB_FREE_RESOURCES			WM_APP + 35
//--------------------------------------------------------------------------------------//


//--------------------------------------------------------------------------------------//
// Configuration
//--------------------------------------------------------------------------------------//
// Bathy bit map information
#define NUMCOLORS_256 256

#define DATA_PT_INDEX 251
#define ACOUSTICANIMATINDEX 252
#define ANIMAT_COLOR_INDEX 253
#define ANIMAT_HIGHLIGHT_COLOR_INDEX 254
#define LAND_COLOR_INDEX 255

#define SLOPE_BITMAP_BLACK_INDEX 255
#define SLOPE_BITMAP_NUMCOLORS_255 NUMCOLORS_256 - 1

#define NUMCOLORS_BATHY NUMCOLORS_256 - 5
#define COLORINDEXSTART_BATHY 0



#define BATHYBITMAPWINDOWCLASSNAME "BathyBitmapWindow"
#define BATHYBITMAP_X 14 // Bathymetry map X location
#define BATHYBITMAP_Y 20 // Bathymetry map Y location
#define BATHYBITMAP_INITIAL_WIDTH   500 // Initial width of the bathymetry bit map, including black box boarder
#define BATHYBITMAP_INITIAL_HEIGHT  500 // Initial height of the bathymetry bit map, including black box boarder

#define SCALEBITMAPWINDOWCLASSNAME "ScaleBitmapWindow"
#define SLOPEBITMAPWINDOWCLASSNAME "SlopeBitmapWindow"
#define SCALEBITMAP_INITIAL_X   BATHYBITMAP_X + BATHYBITMAP_INITIAL_WIDTH + 25 // this will have to be more dynamic and based on the final size of the BATHYBITMAPWINDOWCLASSNAME window.
#define SCALEBITMAP_INITIAL_Y   BATHYBITMAP_Y
#define SCALEBITMAP_WIDTH   18  // The (fixed and permanent) width of the scale bit map
#define SCALEBITMAP_INITIAL_HEIGHT  BATHYBITMAP_INITIAL_HEIGHT
#define COMMONCONTROLSPACING 5
#define BITMAPBORDERPIXELS 2

#define POLYCORRDARRAYLEN 100
#define ENDPOLYGONCLOSEBYDIST 5.0
#define MAX_SEED_AT_ONCE 100000
#define MAX_NUMBER_ANIAMTS_IN_LIST_BOX 40
#define LAND_DEPTH 0
#define SLIDER_UPPER_RANGE_MIN_EXTENSION 3
#define SLIDER_LOWER_RANGE_MIN_EXTENSION 4
const int REFRESHTICKS = 100;



//#define OUTPUT_FILE_FILTER "Output Files (*.3mb)\0*.3mb\0"
//#define OUTPUT_FILE_DEFAULT_EXTENSION "3mb"
const TCHAR SZSHAPEFILTER[] = TEXT("Shape Files (*.shp)\0*.shp\0");
const TCHAR SZSHAPEDEFEXT[] = TEXT("shp");


const TCHAR SZ3MBBINOUTFILTER[] = TEXT("MB Binary Output Files (*.3mb)\0*.3mb\0");
const TCHAR SZ3MBBINOUTDEFEXT[] = TEXT("mbs");

const TCHAR SZSCEFILTER[] = TEXT("Scenario Files (*.SCE)\0*.sce\0");
const TCHAR SZSCEDEFEXT[] = TEXT("sce") ;

const TCHAR SZ_BATHY_FILTER[] = TEXT("Bathymetry Files (*.BTH);Text Files (.TXT)\0*.bth;*.txt\0"); 
const TCHAR SZ_BATHY_DEFEXT[] = TEXT("bth\0") ;

const TCHAR szSaltFilter[] = TEXT("Salinity Files (*.SLT)\0*.slt\0"); 
const TCHAR szSaltDefExt[] = TEXT("slt") ;

const TCHAR szTemperatureFilter[] = TEXT("Temperature Files (*.TEM)\0*.tem\0"); 
const TCHAR szTemperatureDefExt[] = TEXT("tem") ;

const TCHAR SZSPEFILTER[] = TEXT ("Species Models (*.spe)\0*.spe\0"); 
const TCHAR SZSPEDEFEXT[] = TEXT ("spe") ;

const TCHAR SZ_CSV_FILTER[] = TEXT ("Specific Iteration CSV Files (*.csv)\0*.csv\0"); 
const TCHAR SZ_CSV_DEFEXT[] = TEXT ("csv") ;

const TCHAR szTextFilter[] = TEXT("Text Files (*.txt)\0*.txt\0");
const TCHAR szTextDefExt[] = TEXT("txt");

const TCHAR szEnvFilter[] = TEXT("All Environment Files (*.TEM;*.BTH;*.SLT)\0*.tem;*.bth;*.slt\0Temperature(*.TEM)\0*.tem\0Bathymetry(*.BTH)\0*.bth\0Salinity(*.SLT)\0*.slt\0"); 
const TCHAR szEnvDefExt[] = TEXT("env") ;

const TCHAR szSrcFilter[] = TEXT ("Sound Source Files (*.src)\0*.src\0"); 
const TCHAR szSrcDefExt[] = TEXT ("src") ;

//--------------------------------------------------------------------------------------//


typedef struct LatLonMinMax
{
	double xMin; // lat min
	double xMax; // lat min
	double yMin; // lon min
	double yMax; // lon min
}LATLONMINMAX;
//--------------------------------------------------------------------------------------//
// Data Structures
//--------------------------------------------------------------------------------------//
typedef struct WindowProperties
{
	//HWND hwnd; // window 
	int x; // X location
	int y; // Y location
	int width; // width
	int height; // height
}WNDPROP;


typedef struct MBSSPECIESVECTORMDLPROCPARAM
{
	int modelType;
	CSpeciesModel *pSpeciesModel;
} MBSSPECIESVECTORMDLPROCPARAM;

LRESULT CALLBACK SpeciesVectorModelProc(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam);


enum EDIT_MODE
{
	MAKE_NEW,
	OPEN_EXISTING_FILE,
	MODIFY_EXISTING
};


typedef struct TextOutput
{
	BOOL enabled; // If TRUE, text files for each animat and stats are outputted.
	BOOL splitTextOutput;	 // Specifies splitting text output file into multiple file (TRUE) or one (FALSE) (change name to splitTextFiles to match ESME script).
	int	iterationsPerFile;	 // If 'splitTextOutput' is TRUE, specifies num iterations per file to split into. (change name to iterationsPerSplitTextFile to match esme script).
}TEXTOUTPUT;

typedef struct ScenarioConfigProcWindowParams
{
	CScenario *sce;
	TEXTOUTPUT *texConfig;
	TCHAR *szOutputFolder;
	BOOL *outputFolderSet;
	DWORD outputFolderBufferSize;
}SCECONFIGWINPARAMS;

typedef struct FileInfo
{
	BOOL bNeedSave;
	BOOL bNamed;
	TCHAR szFileName[BUFFERED_MAX_PATH]; // path and name of the file.
	TCHAR szTitleName[BUFFERED_MAX_PATH]; // name of the file only
}FILE_INFO;

//**************************************************************//
// Tagged for removal //

typedef struct SPECIESSUMMARY
{
	int animatCount;
	int podCount;
	int individualCount;
}SPECIESSUMMARY;

typedef struct ThreadInf
{
	HANDLE hdl;
	DWORD id;
	BOOL exit;
	BOOL running;
	BOOL noRepaint;
}THREAD_INF;

typedef struct ThreadOpenFileInf
{
	HANDLE hdl;
	DWORD id;
	BOOL exit;
	BOOL running;
	FILE_INFO fileInf;
}THREAD_OPENFILE_INF;

typedef struct UpdateThreadParam
{
	THREAD_INF threadInf;
	HWND   hWnd;
}UPDATETHREADINF;

//**************************************************************//
typedef struct UpdateWindowMsgThreadInf
{
	//THREAD_OPENFILE_INF openFileThreadInf;
	THREAD_INF threadInf;
	//HWND updateWindow; // this goes away and is repleaced with the global handle to the bathymetry bitmap window.
	DWORD updatePeriod; // in milliseconds for GetTickCount().
	UINT updateMessage;
	WPARAM wParam; // wParam, if any
	LPARAM lParam; // lParam, if any
	BOOL pause; // pause the updates
}UPDATEWINDOWMSGTHREADINF;


typedef struct MainDlgWinGlobalData
{
	CScenario sce; // Responsible for creating and running scenarios.
	CFileExtracter fileExtractor; // Responsible for extracting binary output into text files.
	CFileManager fileMgr; // Responsible for loading in binary files for display and animation.


	THREAD_INF sceRunThreadInf;
	THREAD_OPENFILE_INF	extractThreadInf;
	UPDATEWINDOWMSGTHREADINF displayInf; // displaying and animating output data


	THREAD_INF sceAbortThreadInf;

	BOOL		 bNeedSave;
	HWND		 hwin;
	DWORD		 binStorageBytesReq;
	DWORD		 txtStorageBytesReq;

	// ESME interface vars
	BOOL		 bMatlabDetected;

	BOOL		 autoRunEnabled;
	LPSTR		 lpCmdLineScenarioFile;
	TEXTOUTPUT	 textOutputConfig;

	TCHAR szOutputFolder[MAX_PATH];
	BOOL outputFolderSet;

}GLOBALDATA;


enum ACTIVE_LB
{
	NONE_LB,
	SPECIES_LB,
	INDIVIDUAL_LB,
	POD_LB,
	MEMBER_LB,
	INTERMEDIATE_LB,
};

typedef struct ActiveIndex
{
	int spe;
	int ind;
	int pod;
	int pdm;
}ACTIVE_LISTINDEX;


typedef struct ActiveListBox
{
	ACTIVE_LB lstbox;
	ACTIVE_LISTINDEX index;
}ACTIVE_LISTBOX;

const TCHAR SZ_SEEDMETHOD[2][8][SIZE_64] = 
{
	{"coding error",
	 "Single Animat At Each Click",
	 "N Animats Randomly Distributed About Click",
	 "N Animats Randomly Distributed Within Box",
	 "N Animats Randomly Distributed Over Entire Map",
	 "N Animats Randomly Distributed Within Polygon",
	 "Distribute Animats Randomly By Density"},

	{"coding error",
	"Single Pod At Each Click",
	"N Pods Randomly Distributed About Click", 
	"N Pods Randomly Distributed Within Box", 
	"N Pods Randomly Distributed Over Entire Map",
	"N Pods Randomly Distributed Within Polygon",
	"Distribute N Pods Randomly, Populate By Density"}
};

enum RADIO_SEEDTYPE
{
	ADD_NONE = 0,
	ADD_INDIVIDUAL = 1,
	ADD_POD = 2,
	ADD_POD_MEMBER = 3,
};

// Values need to correspond to the SZ_SEEDMETHOD strings.
enum RADIO_ADDMETHOD
{
	NOT_SET = 0,
	MOUSECLICK = 1,
	N_AROUND_CLICK = 2,
	BOUNDINGBOX = 3,
	THROUGHOUT_MAP = 4,
	POLYGON = 5,
	BY_DENSITY = 6,
	SHAPE_FILE = 7,
};


typedef struct GuiState
{
	//ACTIVE_LB activeLB.lstbox;
	//RADIO_SEEDTYPE radioAddType;
	//RADIO_ADDMETHOD radioAddMethod;
}GUISTATE;

typedef struct EnvironmentMapMouseClick
{
	// Integrate struct COORD_DEPTH into this.
	// COORD_DEPTH needs to be changed to a different name since it contains bathy depth.
	// Whatever it gets changed to, it will need lat, lon, depth, temperature, 
	double lat; // Latitude;
	double lon; // Longitude
	double depth;
	double slopeAngle;
	double slopeHeading;
	double temperature;
	double salinity;

	int    index;
	COORD  coord; // struct COORD is defined in MSDN library.
} ENVMOUSECLICK;


typedef struct mbBitmapBathyInf
{
	double metersPerPixelDepth; // meters per pixel depth
	double latPerPixel;
	double lonPerPixel;
	long topBarrier;
	long bttmBarrier;
	long topBarrierRangeExtension;
	long bttmBarrierRangeExtension;

	BATHYEXTREMES dataExtremes; // Data extremes
	BATHYEXTREMES displayExtremes; // Display extremes
}BATHY_BITMAP_INF;

typedef struct
{
	BITMAPINFOHEADER bmiHeader; 
    RGBQUAD          bmiColors[NUMCOLORS_256]; 
}BATHYBITMAPINFO;
//--------------------------------------------------------------------------------------//

typedef struct mbsSeedEnvPolygonInformation
{
	int	numVertices;
	BOOL isClosed;
	COORD cArray[POLYCORRDARRAYLEN];
	ENVMINMAX minMax;
}POLYGONINF;

enum INTENSITY
{
	OFF,
	LOW,
	MED,
	HIGH,
};

typedef struct FeatureToggle 
{
	INTENSITY bathymetryDataPoints;
	BOOL slopeHeading;
	BOOL speciesStimulus;
}FEATURETOGGLE;


typedef struct DlgLayout
{
	WNDPROP bathy;		// Properties of the bathymetry bitmap window
	WNDPROP depth; // Properties of the depth scale bitmap window
	WNDPROP slope; // Properties of the slope scale bitmap window
}DLGLAYOUT;

typedef struct WindowDisplayState
{
	BOOL bMaximized; // TRUE if window is maximized, FALSE (0) if minimized.
	BOOL bZoom;		 // TRUE if window is zoomed, FALSE (0) if not.
}WINDOWDISPLAYSTATE;

enum BATHYUSAGESTATEENUM
{
	UNINITIALIZED_STATE,
	SEED_SCENARIO_STATE,
	PLAYBACK_STATE,
};

enum BATHYUSAGETYPENUM
{
	SEED_SCENARIO_TYPE,
	PLAYBACK_TYPE,
};

typedef BATHYUSAGETYPENUM BATHYUSETYPE;
typedef BATHYUSAGESTATEENUM BATHYUSESTATE;

// Used by seeding dialog and bitmap processes.  Declared in the Seeding Dialog process
// and passed into the seeding bitmap window as a parameter.
typedef struct BathymetryBitmapWinParam
{
	BATHY_BITMAP_INF bitmapInf;
	GUISTATE		 state;			  
	//GUISTATE		 prevState;
	int				 addAmount;
	int				 initialPodSize;
	double			 PoissonDistribution;
	double			 AveDist;
	double			 StdDevDist;
	double			 focalDist;
	BOOL			 boundingBoxPresent;
	PODLEADERTYPE	 podLeaderType;
	double			 seedDensityKm;
	POLYGONINF		 poly;
	BOOL			 soundSourcePresent;

	DLGLAYOUT		 *layout;
	SCENARIOUTPUTFILEINF *playbackInf;
	WINDOWDISPLAYSTATE *pWindowDisplayState;

	BATHYUSESTATE	 usageState;
	BATHYUSETYPE	 usageType;

	BOOL bBathymetry; // show bathymetry.
	BOOL bSlopeHeading; // show slop heading
	BOOL bDataPoints; // show data points
	BOOL bAnimats; // show animats

	//BOOL bShowTrack;
	PLAYENUM playState;
	int nPlaybackRate; // Initialize to 0, 2^0 = 1.

	BOOL shapeDataLoaded;

}BATHYBITMAP_WIN_PARAM; // Passed into the bathymetry bitmap and the scaling bitmaps.

typedef struct ScalingInformation
{
	//double yMax; // shallowest/minimum slope value for this map (no need for this, both assumed zero).
	//double yMin; // deepest bathymetry/slope value for this map.
	double yMaxDisplay; // Most shallow/minimum slope to display, top of the scale
	double yMinDisplay; // Most deep/ max slope to display, top of the scale
	BATHYEXTREMES bathyExtremes;
}SCALEINGINF;


// This struct goes away.
typedef struct PlaybackBitmapGlobalData
{
	BATHY_BITMAP_INF bitmapInf;
	HWND			 hwndParent;
	double			 surfarea;
	POLYGONINF		 poly;
	SCENARIOUTPUTFILEINF sceInf;
}PLAYBACKINF; // Globally used data for the playback bitmap 


typedef struct SeedingDlgWinParam
{
	BATHYUSETYPE usageType; // Bathy usage type.  Either for Seeding animats or to display outputted results of a run.
	HINSTANCE hInstance;
	HWND hWndParent;
	CScenario *pScenario;
}SEED_DLG_PARAM; // Passed into the seeding/bathymetry dialog box.

/*
typedef struct PlaybackDlgWinParam
{
	BATHYUSAGESTATEENUM usage;
	HINSTANCE hInstance;
	HWND hWndParent;
	//CScenario *pScenario;
}PLAYBACK_DLG_PARAM; // formerly passed into the playback dialog box.
*/
typedef struct BathyMapInformation
{
	double latMax;
	double latMin;
	double lonMax;
	double lonMin;
	double widthMeters;
	double heightMeters;
	double surfarea;
	ENVDATAPOINTCOUNT dataPointCount;
}BATHYMAPINF;


#endif // !defined(AFX_3MBS_H__D078736E_FF6B_414E_995E_B535F4D31989__INCLUDED_)
