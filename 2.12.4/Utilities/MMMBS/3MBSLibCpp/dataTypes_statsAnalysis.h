#ifndef DATATYPES_STATSANALYSIS_H
#define DATATYPES_STATSANALYSIS_H
#include "dataTypes_general.h"

const TCHAR TXTSTATSDATE0[] = TEXT("File Creation Date: %02d\\%02d\\%04d  %02d:%02d:%02d\n");
const TCHAR TXTSTATS0000[]  = TEXT("3mb Ver (current):  %d.%02d (built on %s at %s)\n");
const TCHAR TXTSTATS0005[]  = TEXT("3mb Ver (file):     %d.%02d\n\n");
const TCHAR TXTSTATS0100[]  = TEXT("Species Count:      %d\n");
const TCHAR TXTSTATS0200[]  = TEXT("Animat Counts\n");
const TCHAR TXTSTATS0220[]  = TEXT("      Initial:      %d\n");
const TCHAR TXTSTATS0230[]  = TEXT("    Additions:      %d (due to moving off map)\n");
const TCHAR TXTSTATS0240[]  = TEXT("        Final:      %d\n");
const TCHAR TXTSTATS0300[]  = TEXT("Start Time:         %02d:%02d:%02d\n");
const TCHAR TXTSTATS0400[]  = TEXT("Duration:           %02d:%02d:%02d\n");
const TCHAR TXTSTATS0420[]  = TEXT("Acoustic Source\n");
const TCHAR TXTSTATS0422[]  = TEXT("        Level:      %d dB\n");
const TCHAR TXTSTATS0423[]  = TEXT("        Level:      --- dB\n");
const TCHAR TXTSTATS0450[]  = TEXT("   Duty Cycle:      %.2f%c\n");
const TCHAR TXTSTATS0451[]  = TEXT("   Duty Cycle:      ---\n");
const TCHAR TXTSTATS0470[]  = TEXT("   Start Time:      %02d:%02d:%02d\n");
const TCHAR TXTSTATS0471[]  = TEXT("   Start Time:      ---\n");


const TCHAR TXTSTATS0900[]  = TEXT("----------- SPECIES MID-FREQUENCY THRESHOLD SPECIFICATIONS -------------\n\n");
const TCHAR TXTSTATS1000[]  = TEXT("                         LvlA Phys         LvlB Phys        LvlB Beh\n");
const TCHAR TXTSTATS1100[]  = TEXT("                         Threshold         Threshold       Threshold\n");
const TCHAR TXTSTATS1170[]  = TEXT("   Species Group    (dB re 1 uPa^2 s) (dB re 1 uPa^2 s) (dB re 1 uPa)\n");
const TCHAR TXTSTATS1200[]  = TEXT("------------------- ----------------- ----------------- -------------\n");
//const TCHAR TXTSTATS1300[] = TEXT("%15s %9.2f %9.2f %9.2f\n";
const TCHAR TXTSTATS1300[]  = TEXT("%19s %13.2f     %13.2f              RF\n");
const TCHAR TXTSTATS1301[]  = TEXT("%19s %13.2f     %13.2f    %13.2f\n");
const TCHAR TXTSTATS2000[]  = TEXT("\n\n\n-------------------------- SCENARIO RESULTS ---------------------------\n\n");
//const TCHAR TXTSTATS2000[] = TEXT("\n\n\nSCENARIO RESULTS\n\n";
const TCHAR TXTSTATS3000[]  = TEXT("Takes By Species\n");
const TCHAR TXTSTATS3010[]  = TEXT("              (ACSTC SRC SPECIES)\n");


const TCHAR TXTSTATS4000[]  = TEXT("              ");
const TCHAR TXTSTATS4005[]  = TEXT("File Name     ");
const TCHAR TXTSTATS4010[]  = TEXT("Species       ");
const TCHAR TXTSTATS4012[]  = TEXT("Species Group ");
const TCHAR TXTSTATS4015[]  = TEXT("Animats t0/tf ");
//const TCHAR TXTSTATS4015[] = TEXT("Animats (init)");
//const TCHAR TXTSTATS4020[] = TEXT("Animats (fin) ");
//const TCHAR TXTSTATS4810[] = TEXT("Species %2d ");
const TCHAR TXTSTATS4810[]  = TEXT("%18s "); // file title, species name
const TCHAR TXTSTATS4820[]  = TEXT("------------------ ");
const TCHAR TXTSTATS4821[]  = TEXT("Lvl B Phys    ");
const TCHAR TXTSTATS4822[]  = TEXT("Lvl B Behv    ");
const TCHAR TXTSTATS4830[]  = TEXT("Total Lvl B   ");
const TCHAR TXTSTATS4840[]  = TEXT("%18d ");
const TCHAR TXTSTATS4841[]  = TEXT("(%8.3f)%8d ");
const TCHAR TXTSTATS4842[]  = TEXT("          %8d ");

const TCHAR TXTSTATS4500[]  = TEXT("Lvl A         ");

const TCHAR TXTSTATS400F[]  = TEXT("%18.5f ");
const TCHAR TXTSTATS4600[]  = TEXT("Max Exposures\n");
const TCHAR TXTSTATS4700[]  = TEXT(" Instantaneous");
const TCHAR TXTSTATS4800[]  = TEXT("    Cumulative");
const TCHAR TXTSTATS4900[]  = TEXT("# Stranded    ");

const TCHAR TXTSTATS5000[]  = TEXT("Takes By Group\n");
const TCHAR TXTSTATS5200[]  = TEXT("%18s ");
const TCHAR TXTSTATS5300[]  = TEXT(" ---------------");
const TCHAR TXTSTATS5400[]  = TEXT("Lvl B Takes ");
const TCHAR TXTSTATS5500[]  = TEXT(" %10d     ");
const TCHAR TXTSTATS5600[]  = TEXT("Lvl A Takes ");
const TCHAR TXTSTATS5700[]  = TEXT("Risk  Takes ");


const TCHAR TXTSTATS6000[]  = TEXT("\n\n\nANIMAT TAKE TOTALS\n\n");

const TCHAR TXTSTATS6400[]  = TEXT("Lvl B Phys:  %d\n"); // new
const TCHAR TXTSTATS6500[]  = TEXT("Lvl B Behv:  %d (%.3f)\n"); // new, risk function
const TCHAR TXTSTATS7000[]  = TEXT("Lvl B Total: %d\n");
const TCHAR TXTSTATS7100[]  = TEXT("Lvl A Total: %d\n");

const TCHAR TXTSTATS7200[]  = TEXT("Total # Stranded: %d\n");


typedef struct TakeStats
{
	// This structure is recalculated (entirely) at each iteration and saved 
	// to the binary output file.
	DWORD lvlAPhysTakes; // 4  Total of all level A physical takes (which is the only type of level A take)
	DWORD lvlBPhysTakes; // 4 Total of all level B physical takes and level B behavioral takes (from the risk function)
	DWORD lvlBTakesTotal; // added
	float lvlBBehTakes; // Total of all Level B behavioral takes determind by risk function

	float maxInstant; // 4
	float maxCumulative; // 4
	DWORD numStranded; // 4
	DWORD offScreenCount;
	DWORD initialAnimatCount;

	BYTE _reserved[SIZE_28];
}TAKESTATS;

// Saved to file
typedef struct StatisticAnalysis
{
	TAKESTATS animat; // recalculated each iteration, all animats in the scenario
	TAKESTATS speGroup[NUM_SPEGROUPS_ALLOC];// recalculated each iteration, takes by group
	DWORD numSpeGroups; // the number of species groups used (less than or equal to
		// NUM_SPEGROUPS_ALLOC
	BYTE _reserved[SIZE_124];
}TAKE;

#endif