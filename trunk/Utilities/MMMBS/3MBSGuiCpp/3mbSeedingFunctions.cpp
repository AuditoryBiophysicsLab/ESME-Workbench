#include "3mbSeedingFunctions.h"
#include "3mbSceInterface.h"
#include "3mbSeedDlgLayoutConfig.h"
#include "3mbsLib.h"

extern HWND g_hwndBathy;
extern HWND g_hDlgSeed;
extern C3MBRandom g_3mbRandom;
extern C3mbStaticsLib staticLib;

// Functions kept internal to this module.
BOOL RetVal(BOOL Val);
ACTIVE_LISTBOX SetActiveListBox(ACTIVE_LISTINDEX Indices);




BOOL RetVal(BOOL Val){return Val;} // meant to simply quiet compiler warning(s)


COORD_DEPTH GetValidRndCoordWithinFocalDistance(CScenario *pScenario, double Lat, double Lon, ENVMINMAX Bathy,
												double ShrFllwVal, double MinSeedDepth, double FocalDistance)
{
	return GetValidRndCoordAboutLatLon(pScenario, Lat, Lon, Bathy, ShrFllwVal, MinSeedDepth, g_3mbRandom.myrand()*FocalDistance, 0);
}

ENVMINMAX EnvMousClickCoordToEnvMinMax(COORD ClickDown, COORD ClickUp)
{
	ENVMINMAX box;
	box.xMax = ClickUp.X;
	box.xMin = ClickDown.X;
	box.yMax = ClickUp.Y;
	box.yMin = ClickDown.Y;
	if(box.xMin > box.xMax)
	{
		box.xMin = ClickUp.X;
		box.xMax = ClickDown.X;
	}
	if(box.yMin > box.yMax)
	{
		box.yMin = ClickUp.Y;
		box.yMax = ClickDown.Y;
	}
	return box;
}

// Lat and Lon validated
BOOL AttemptToGetValidRndCoord(COORD_DEPTH *pCoordResult, CScenario *pScenario, double LatStep, double LonStep,
							   ENVMINMAX Bathy, double ShrFllwVal, double MinSeedDepth, RECT Rect)
{
	int loopCount = 0;
	int x, y;
	double bathyDepth;
	memset(pCoordResult, 0, sizeof(COORD_DEPTH));
	COORD_DEPTH c = {0};
	BOOL validDepth = FALSE;
	BATHYVALUE bathyValues;

	do
	{
		// Determine a new coordiant within the entire map.
		x = g_3mbRandom.rnd(0, Rect.right-1);
		y = g_3mbRandom.rnd(0, Rect.bottom-1);
		c.lat = Bathy.xMin + LatStep * (Rect.bottom - 1 - y);
		c.lon = Bathy.yMin + LonStep * x;
		bathyValues = pScenario->GetBathymetryDepth(c.lat, c.lon); 
		bathyDepth = bathyValues.depth;

		// Check if the coordiant is within the bathymap.
		if(staticLib.LatLonWithinBoundaries(c.lat, c.lon, Bathy) == TRUE)
		{
			// Verify the depth is valid meaning not on a beach or within the species
			// shore following depth.
			if(FALSE == (validDepth=staticLib.BathyDepthAtLatLonValid(bathyDepth, ShrFllwVal, MinSeedDepth)))
				if(loopCount++ >= 1000)
					return FALSE;
		}
	}while(validDepth == FALSE);

	// A valid coordiant found.  Copy it over and return.
	pCoordResult->lat = c.lat;
	pCoordResult->lon = c.lon;
	return TRUE;
}

BOOL AttemptToGetValidRndCoordWithinPolygon(COORD_DEPTH *pCoordResult, CScenario *pScenario, double LatStep, double LonStep,
										RECT Rect, POLYGONINF Poly, ENVMINMAX Bathy, double ShrFllwVal, double MinSeedDepth)
{
	int loopCount = 0;
	short x, y;
	memset(pCoordResult, 0, sizeof(COORD_DEPTH));
	COORD_DEPTH c = {0};
	BOOL validDepth = FALSE;
	double bathyDepth;
	BATHYVALUE bathyValues;


	do
	{
		// Determine a new coordiant within the polygon.
		x	= (short)g_3mbRandom.rnd((short)Poly.minMax.xMin, (short)Poly.minMax.xMax);
		y	= (short)g_3mbRandom.rnd((short)Poly.minMax.yMin, (short)Poly.minMax.yMax);

		if(staticLib.CoordWithinCoordPolygonBoundaries(x, y, Poly.cArray, Poly.numVertices))
		{
			c.lat = Bathy.xMin + LatStep * (Rect.bottom - 1 - y);
			c.lon = Bathy.yMin + LonStep * x;

			// Check if the coordiant is within the bathymap.
			if(staticLib.LatLonWithinBoundaries(c.lat, c.lon, Bathy) == TRUE)
			{
				// Verify the depth is valid meaning not on a beach or within the species
				// shore following depth.
				bathyDepth = (bathyValues = pScenario->GetBathymetryDepth(c.lat, c.lon)).depth;
				if(FALSE == (validDepth=staticLib.BathyDepthAtLatLonValid(bathyDepth, ShrFllwVal, MinSeedDepth)))
					if(loopCount++ >= 1000)
						return FALSE; // failed to find a valid coordinate
			}
		}
		else
		{
			if(loopCount++ >= 1000)
				return FALSE;
		}
	}while(validDepth == FALSE);

	// A valid coordiant found.  Copy it over and return.
	pCoordResult->lat = c.lat;
	pCoordResult->lon = c.lon;

	return TRUE;
}



BOOL AttemptToGetValidRndCoordWithinBox(COORD_DEPTH *pCoordResult, CScenario *pScenario, double LatStep, double LonStep,
										RECT Rect, ENVMINMAX Box, ENVMINMAX Bathy, double ShrFllwVal, double MinSeedDepth)
{
	int loopCount = 0;
	int x, y;
	memset(pCoordResult, 0, sizeof(COORD_DEPTH));
	COORD_DEPTH c = {0};
	BOOL validDepth = FALSE;
	double bathyDepth;
	BATHYVALUE bathyValue;

	do
	{
		// Determine a new coordiant within the bounding box.
		x = (int)g_3mbRandom.rnd((int)Box.xMin, (int)Box.xMax);
		y = (int)g_3mbRandom.rnd((int)Box.yMin, (int)Box.yMax);
		c.lat = Bathy.xMin + LatStep * (Rect.bottom - 1 - y);
		c.lon = Bathy.yMin + LonStep * x;

		// Check if the coordiant is within the bathymap.
		if(staticLib.LatLonWithinBoundaries(c.lat, c.lon, Bathy) == TRUE)
		{
			// Verify the depth is valid meaning not on a beach or within the species
			// shore following depth.
			bathyDepth = (bathyValue = pScenario->GetBathymetryDepth(c.lat, c.lon)).depth;
			if(FALSE == (validDepth=staticLib.BathyDepthAtLatLonValid(bathyDepth, ShrFllwVal, MinSeedDepth)))
				if(loopCount++ >= 1000)
					return FALSE; // failed to find a valid coordinate
		}
	}while(validDepth == FALSE);

	// A valid coordiant found.  Copy it over and return.
	pCoordResult->lat = c.lat;
	pCoordResult->lon = c.lon;
	return TRUE;
}




COORD_DEPTH GetValidRndCoordAboutLatLon(CScenario *pScenario, double Lat, double Lon, ENVMINMAX Bathy,
										double ShrFllwVal, double MinSeedDepth, double AveDist, double StdDist)
{
	COORD_DEPTH c;
	int loopCount = 0;
	BOOL validDepth = FALSE;
	double bathyDepth;
	BATHYVALUE bathyValue;

	do
	{
		// Determine a new coordiant near the focal point.
		c = staticLib.RandomLatLonAboutFocal(Lat, Lon, AveDist, StdDist, &g_3mbRandom);

		// Check if the coordiant is within the bathymap.
		if(staticLib.LatLonWithinBoundaries(c.lat, c.lon, Bathy) == TRUE)
		{
			// Verify the depth is valid meaning not on a beach or within the species
			// shore following depth.
			bathyDepth = (bathyValue = pScenario->GetBathymetryDepth(c.lat, c.lon)).depth;
			if(FALSE == (validDepth=staticLib.BathyDepthAtLatLonValid(bathyDepth, ShrFllwVal, MinSeedDepth)))
			{
				if(loopCount++ >= 1000)
				{
					loopCount = 0;
					AveDist /= 1.2; // reduce the ave distance
					StdDist /= 1.2; // decrease std
					Sleep(1); // don't hog processor time
				}
			}
		}
	}while(validDepth == FALSE);
	return c;
}

void AddAnimatToPod(int SpeIndex, int PodIndex, BATHYBITMAP_WIN_PARAM *pEnvBitmpGdata, CScenario *pSce)
{
	BOOL   bathyLoaded;
	BOOL   salinityLoaded;
	BOOL   tempLoaded;
	BOOL   anyEnvLoaded;
	BOOL   anySpecMdlLoaded;
	double shrFllw, minSeedDepth;
	INHABITINF inhabitantInf, inhabitantInf2;

//COORD_DEPTH		  coord;//, coord2;
	BATHYEXTREMES	   bathyExtremes;

	// Set state variables for the dialog box controls.
	bathyLoaded		 = CBitmapEnvFunctions::BathyFileLoaded();
	salinityLoaded	 = CBitmapEnvFunctions::SaltinityFileLoaded();
	tempLoaded		 = CBitmapEnvFunctions::TemperatureFileLoaded();
	anyEnvLoaded	 = bathyLoaded || salinityLoaded || tempLoaded;
	anySpecMdlLoaded = (CBitmapEnvFunctions::GetSpeciesCount() > 0);

	if(anyEnvLoaded == FALSE || anySpecMdlLoaded == FALSE || SpeIndex < 0 || PodIndex < 0 )
		return;

	bathyExtremes = CBitmapEnvFunctions::GetBathyExtremes();
	shrFllw = CBitmapEnvFunctions::GetShoreFollowingDepth(SpeIndex);
	minSeedDepth = CBitmapEnvFunctions::GetMinimumSeededingDepth(SpeIndex);

	// Get the lead animat's coordinates.  Lead animat is at index 0.
	inhabitantInf = CBitmapEnvFunctions::GetPodMemberInitialCoordinate(SpeIndex, PodIndex, 0);
	inhabitantInf2.coord = GetValidRndCoordWithinFocalDistance(CBitmapEnvFunctions::GetSceRef(),
															   inhabitantInf.coord.lat,
															   inhabitantInf.coord.lon,
															   staticLib.BathyExtremesToEnvExtemes(bathyExtremes),
															   shrFllw,
															   minSeedDepth,
															   pEnvBitmpGdata->focalDist);
	pSce->AddPodMember(SpeIndex, PodIndex, inhabitantInf2);
}


void AddAnimatsAroundEnvData(ENVMOUSECLICK *PMseClkDwn, ENVMOUSECLICK *PMseClkUp,
							 BATHYBITMAP_WIN_PARAM *pEnvBitmpGdata, CScenario *pSce)
{
			BOOL multipleSourcesAllowed = MULTISOUNDSOURCEALLOWED;

			BOOL   anyEnvLoaded;
			BOOL anySpecMdlLoaded;

			ACTIVE_LISTBOX activeLB;
			RADIO_SEEDTYPE addType;
			RADIO_ADDMETHOD addMethod;
			int	   i, j, loopCount;
			double bathyDepth, shrFllw, minSeedDepth;
			double ave, std;
			double lat,lon, latStep, lonStep;
			ENVMINMAX box;

			INHABITINF		  inhabitantInf, inhabitantInf2;
			COORD_DEPTH		  coord;//, coord2;
	static	COORD			  c; // COORD is a struct in the Visual Studio libary.
	static  LinkedList <int> PoissonDistLinkedList;
			BATHYEXTREMES	   bathyExtremes = pEnvBitmpGdata->bitmapInf.displayExtremes;
			RECT			   rect;
			int desnityAdd;
			BATHYVALUE bathyValue;

	// Set state variables for the dialog box controls.
	activeLB = GetActiveListBox();
	addType = GetSeedTypeRadioState();
	addMethod = GetAddMethodRadioState();
	anySpecMdlLoaded = (pSce->GetSpeciesCount() > 0);
	anyEnvLoaded = CBitmapEnvFunctions::AnyEnvFileLoaded();
	if(anySpecMdlLoaded == FALSE)
		_ASSERT(activeLB.lstbox == NONE_LB);
	if(anyEnvLoaded == FALSE || anySpecMdlLoaded==FALSE || activeLB.index.spe < 0 || activeLB.lstbox == NONE_LB)
		return;

	// Get the Environmental extremes
	GetClientRect(g_hwndBathy, &rect);
	latStep	= pEnvBitmpGdata->bitmapInf.latPerPixel;
	lonStep	= pEnvBitmpGdata->bitmapInf.lonPerPixel;

	switch(addType)
	{
	case ADD_INDIVIDUAL:
		switch(addMethod)
		{
		case MOUSECLICK://ADD_INDIVIDUAL
			memset(&inhabitantInf, 0, sizeof(inhabitantInf));
			shrFllw = pSce->GetShoreFollowingDepth(activeLB.index.spe);
			minSeedDepth = pSce->GetMinimumSeededingDepth(activeLB.index.spe);
			inhabitantInf.coord.lat = PMseClkDwn->lat;
			inhabitantInf.coord.lon = PMseClkDwn->lon;

			// Verify the user selected a valid location to add the individual
			bathyDepth = (bathyValue = pSce->GetBathymetryDepth(inhabitantInf.coord.lat,
																					 inhabitantInf.coord.lon)).depth;
			if(staticLib.IsAValidSeedLatLon(inhabitantInf.coord.lat,
				                  inhabitantInf.coord.lon,
								  bathyDepth,
								  staticLib.BathyExtremesToEnvExtemes(bathyExtremes),
								  shrFllw,
								  minSeedDepth) == FALSE)
			{
				break;
			}

			pSce->AddIndividual(activeLB.index.spe, inhabitantInf);
			break;

		case N_AROUND_CLICK://ADD_INDIVIDUAL
			if(pEnvBitmpGdata->addAmount < 1)
				break;

			memset(&inhabitantInf, 0, sizeof(inhabitantInf));
			lat = PMseClkDwn->lat;
			lon = PMseClkDwn->lon;
			shrFllw = pSce->GetShoreFollowingDepth(activeLB.index.spe);
			minSeedDepth = pSce->GetMinimumSeededingDepth(activeLB.index.spe);

			// Verify the user selected a valid location to add the individual
			bathyDepth = (bathyValue = pSce->GetBathymetryDepth(lat, lon)).depth;
			if(staticLib.IsAValidSeedLatLon(lat,
				                  lon,
								  bathyDepth,
								  staticLib.BathyExtremesToEnvExtemes(bathyExtremes),
								  shrFllw,
								  minSeedDepth) == FALSE)
			{
				return;
			}

			for(i=0; i<pEnvBitmpGdata->addAmount; i++)
			{
				ave = pEnvBitmpGdata->AveDist;
				std = pEnvBitmpGdata->StdDevDist;
				inhabitantInf.coord = GetValidRndCoordAboutLatLon(pSce,
																  lat,
																  lon,
																  staticLib.BathyExtremesToEnvExtemes(bathyExtremes),
																  shrFllw,
																  minSeedDepth,
																  ave,
																  std);
				pSce->AddIndividual(activeLB.index.spe, inhabitantInf);
			}
			break;

		case BOUNDINGBOX://ADD_INDIVIDUAL
			if(pEnvBitmpGdata->addAmount < 1)
				break;
			memset(&inhabitantInf, 0, sizeof(inhabitantInf));

			shrFllw = pSce->GetShoreFollowingDepth(activeLB.index.spe);
			minSeedDepth = pSce->GetMinimumSeededingDepth(activeLB.index.spe);
			box = EnvMousClickCoordToEnvMinMax(PMseClkDwn->coord, PMseClkUp->coord);

			for(i=0; i<pEnvBitmpGdata->addAmount; i++)
			{
				if(TRUE == AttemptToGetValidRndCoordWithinBox(&inhabitantInf.coord,
															  pSce,
															  latStep,
															  lonStep,
															  rect,
															  box,
															  staticLib.BathyExtremesToEnvExtemes(bathyExtremes),
															  shrFllw,
															  minSeedDepth))
					pSce->AddIndividual(activeLB.index.spe, inhabitantInf);
			}
			break;

		case POLYGON://ADD_INDIVIDUAL
			if(pEnvBitmpGdata->addAmount < 1)
				break;

			shrFllw = pSce->GetShoreFollowingDepth(activeLB.index.spe);
			minSeedDepth = pSce->GetMinimumSeededingDepth(activeLB.index.spe);
//			box = EnvMousClickCoordToEnvMinMax(PMseClkDwn->coord, PMseClkUp->coord);

			memset(&inhabitantInf, 0, sizeof(inhabitantInf));
			for(i=0; i<pEnvBitmpGdata->addAmount; i++)
			{
				if(TRUE == AttemptToGetValidRndCoordWithinPolygon(&coord,
																  pSce,
																  latStep,
																  lonStep,
																  rect,
																  pEnvBitmpGdata->poly,
																  staticLib.BathyExtremesToEnvExtemes(bathyExtremes),
																  shrFllw,
																  minSeedDepth))
				{
					inhabitantInf.coord = coord;
					pSce->AddIndividual(activeLB.index.spe, inhabitantInf);
				}
			}
			break;


		case THROUGHOUT_MAP://ADD_INDIVIDUAL
			if(pEnvBitmpGdata->addAmount < 1)
				break;
			shrFllw = pSce->GetShoreFollowingDepth(activeLB.index.spe);
			minSeedDepth = pSce->GetMinimumSeededingDepth(activeLB.index.spe);

			memset(&inhabitantInf, 0, sizeof(inhabitantInf));

			for(i=0; i<pEnvBitmpGdata->addAmount; i++)
			{
				if(TRUE == AttemptToGetValidRndCoord(&coord,
													 pSce,
													 latStep,
													 lonStep,
													 staticLib.BathyExtremesToEnvExtemes(bathyExtremes),
													 shrFllw,
													 minSeedDepth,
													 rect))
				{
					inhabitantInf.coord = coord;
					pSce->AddIndividual(activeLB.index.spe, inhabitantInf);
				}
			}
			break;
		case BY_DENSITY://ADD_INDIVIDUAL
			activeLB.index.spe = SendMessage(GetDlgItem(g_hDlgSeed, IDC_LIST_SPECIES), LB_GETCURSEL, 0, 0);
			if(multipleSourcesAllowed == FALSE && TRUE == pSce->SpeciesIsASoundSourceModel(activeLB.index.spe))
			{	
				// If already populated with a sound source, break;
				if(pSce->GetTotalSoundSourceCount() > 0)
					desnityAdd = 0;
				else
					desnityAdd = 1;
			}
			else
			{
				desnityAdd = staticLib.CalcAnimatQTYByDensity(CBitmapEnvFunctions::GetBathymtryWaterSurfaceAreaMeters(), pEnvBitmpGdata->seedDensityKm);
			}
			if(desnityAdd < 1)
				break;
			shrFllw = pSce->GetShoreFollowingDepth(activeLB.index.spe);
			minSeedDepth = pSce->GetMinimumSeededingDepth(activeLB.index.spe);

			memset(&inhabitantInf, 0, sizeof(inhabitantInf));


			for(i=0; i<desnityAdd; i++)
			{
				if(TRUE == AttemptToGetValidRndCoord(&coord,
													 pSce,
													 latStep,
													 lonStep,
													 staticLib.BathyExtremesToEnvExtemes(bathyExtremes),
													 shrFllw,
													 minSeedDepth,
													 rect))
				{
					inhabitantInf.coord = coord;
					pSce->AddIndividual(activeLB.index.spe, inhabitantInf);
				}
			}
			break;
		}
		break;

	case ADD_POD:
		// Verify the user didn't click on land if adding the lead animat at the click point.
//		if(addMethod == MOUSECLICK || addMethod == N_AROUND_CLICK)
//			if(BATHY_MIN_SEED_DEPTH < pSce->GetBathymetryDepth(PMseClkDwn->lat, PMseClkDwn->lon))
//				break;

		switch(addMethod)
		{
		case MOUSECLICK://ADD_POD
			if(pEnvBitmpGdata->addAmount < 1)
				break;
			//***********************************//
			// Adds a single pod and its members.
			//***********************************//
			shrFllw = pSce->GetShoreFollowingDepth(activeLB.index.spe);
			minSeedDepth = pSce->GetMinimumSeededingDepth(activeLB.index.spe);

			coord.lat = PMseClkDwn->lat;
			coord.lon = PMseClkDwn->lon;

			// Verify the user selected a valid location to add the pod.
			bathyDepth = (bathyValue = pSce->GetBathymetryDepth(coord.lat, coord.lon)).depth;
			if(staticLib.IsAValidSeedLatLon(coord.lat,
								  coord.lon,
								  bathyDepth,
								  staticLib.BathyExtremesToEnvExtemes(bathyExtremes),
								  shrFllw,
								  minSeedDepth) == FALSE)
			{
				break;
			}

			// Add a single pod to the currently selected species at the clicked location
			pSce->AddPod(activeLB.index.spe, pEnvBitmpGdata->podLeaderType, pEnvBitmpGdata->focalDist);
			activeLB.index.pod = pSce->GetPodCount(activeLB.index.spe)-1;

			// Add the lead animat to the pod at the clicked location
			memset(&inhabitantInf, 0, sizeof(inhabitantInf));
			inhabitantInf.coord = coord;
			pSce->AddPodMember(activeLB.index.spe, activeLB.index.pod, inhabitantInf);

			// Add the rest of the animats to this pod around the lead animat.  The
			// initialPodSize-1 is there because the lead animat counted as one of the pod
			// members.
			for(i=0; i<pEnvBitmpGdata->initialPodSize-1; i++)
			{
				inhabitantInf2.coord =
					GetValidRndCoordWithinFocalDistance(pSce,
														coord.lat,
														coord.lon,
														staticLib.BathyExtremesToEnvExtemes(bathyExtremes),
														shrFllw,
														minSeedDepth,
														pEnvBitmpGdata->focalDist);
				pSce->AddPodMember(activeLB.index.spe, activeLB.index.pod, inhabitantInf2);
			}
			break;


		case N_AROUND_CLICK://ADD_POD
			if(pEnvBitmpGdata->addAmount < 1)
				break;
			//*************************************************************//
			// Adds N pods around click and populates each pod with members
			//*************************************************************//
			lat = PMseClkDwn->lat;
			lon = PMseClkDwn->lon;
			shrFllw = pSce->GetShoreFollowingDepth(activeLB.index.spe);
			minSeedDepth = pSce->GetMinimumSeededingDepth(activeLB.index.spe);

			// Verify the user selected a valid location to add the individual
			bathyDepth = (bathyValue = pSce->GetBathymetryDepth(lat, lon)).depth;
			if(staticLib.IsAValidSeedLatLon(lat, lon, bathyDepth, staticLib.BathyExtremesToEnvExtemes(bathyExtremes), shrFllw, minSeedDepth) == FALSE)
				return;

			// First add a single pod to the currently selected species.
			pSce->AddPod(activeLB.index.spe, pEnvBitmpGdata->podLeaderType, pEnvBitmpGdata->focalDist);
			activeLB.index.pod = pSce->GetPodCount(activeLB.index.spe)-1;

			// Then add a single member to the pod just added at the clicked-on coordinates.  This
			// member will is the first in the list and the lead animat.
			memset(&inhabitantInf, 0, sizeof(inhabitantInf));
			inhabitantInf.coord.lat = coord.lat = PMseClkDwn->lat;
			inhabitantInf.coord.lon = coord.lon = PMseClkDwn->lon;
			pSce->AddPodMember(activeLB.index.spe, activeLB.index.pod, inhabitantInf);

			// Add the rest of the animats to this pod around the lead animat.  The
			// initialPodSize-1 is there because the lead animat counted as one of the pod
			// members.
			for(i=0; i<pEnvBitmpGdata->initialPodSize-1; i++)
			{
				inhabitantInf2.coord =
					GetValidRndCoordWithinFocalDistance(pSce,
														coord.lat,
														coord.lon,
														staticLib.BathyExtremesToEnvExtemes(bathyExtremes),
														shrFllw,
														minSeedDepth,
														pEnvBitmpGdata->focalDist);
				pSce->AddPodMember(activeLB.index.spe, activeLB.index.pod, inhabitantInf2);
			}


			// Add the remaining pods and their animats.
			// The addAmount-1 because the first pod was already added.
			for(i=0; i<pEnvBitmpGdata->addAmount-1; i++)
			{
				loopCount = 0;
				ave = pEnvBitmpGdata->AveDist;
				std = pEnvBitmpGdata->StdDevDist;

				// Determine the coordinates of the lead animat of this pod.
				coord = GetValidRndCoordAboutLatLon(pSce,
													lat,
													lon,
													staticLib.BathyExtremesToEnvExtemes(bathyExtremes),
													shrFllw,
													minSeedDepth,
													ave,
													std);

				// Add the pod and its lead animat.
				pSce->AddPod(activeLB.index.spe, pEnvBitmpGdata->podLeaderType, pEnvBitmpGdata->focalDist);
				activeLB.index.pod = pSce->GetPodCount(activeLB.index.spe)-1;
				pSce->AddPodMember(activeLB.index.spe, activeLB.index.pod, inhabitantInf);

				// Add the rest of the animats to this pod around the lead animat.  The
				// initialPodSize-1 is there because the lead animat counted as one of the pod
				// members.
				for(j=0; j<pEnvBitmpGdata->initialPodSize-1; j++)
				{
					inhabitantInf2.coord =
						GetValidRndCoordWithinFocalDistance(pSce,
															coord.lat,
															coord.lon,
															staticLib.BathyExtremesToEnvExtemes(bathyExtremes),
															shrFllw,
															minSeedDepth,
															pEnvBitmpGdata->focalDist);
					pSce->AddPodMember(activeLB.index.spe, activeLB.index.pod, inhabitantInf2);
				}
			}
			break;

		case BOUNDINGBOX://ADD_POD
			if(pEnvBitmpGdata->addAmount < 1)
				break;

			shrFllw = pSce->GetShoreFollowingDepth(activeLB.index.spe);
			minSeedDepth = pSce->GetMinimumSeededingDepth(activeLB.index.spe);
			box = EnvMousClickCoordToEnvMinMax(PMseClkDwn->coord, PMseClkUp->coord);

			// Add the remaining pods and their animats.
			memset(&inhabitantInf, 0, sizeof(inhabitantInf));
			for(i=0; i<pEnvBitmpGdata->addAmount; i++)
			{
				if(TRUE == AttemptToGetValidRndCoordWithinBox(&coord,
															  pSce,
															  latStep,
															  lonStep,
															  rect,
															  box,
															  staticLib.BathyExtremesToEnvExtemes(bathyExtremes),
															  shrFllw,
															  minSeedDepth))
				{
					// Add the pod and its lead animat.
					inhabitantInf.coord = coord;
					pSce->AddPod(activeLB.index.spe, pEnvBitmpGdata->podLeaderType, pEnvBitmpGdata->focalDist);
					activeLB.index.pod = pSce->GetPodCount(activeLB.index.spe)-1;
					pSce->AddPodMember(activeLB.index.spe, activeLB.index.pod, inhabitantInf);

					// Add the rest of the animats to this pod around the lead animat.  The
					// initialPodSize-1 is there because the lead animat counted as one of the pod
					// members.
					for(j=0; j<pEnvBitmpGdata->initialPodSize-1; j++)
					{
						inhabitantInf2.coord =
							GetValidRndCoordWithinFocalDistance(pSce,
																coord.lat,
																coord.lon,
																staticLib.BathyExtremesToEnvExtemes(bathyExtremes),
																shrFllw,
																minSeedDepth,
																pEnvBitmpGdata->focalDist);
						pSce->AddPodMember(activeLB.index.spe, activeLB.index.pod, inhabitantInf2);
					}
				}
			}
			break;

		case POLYGON://ADD_POD
			if(pEnvBitmpGdata->addAmount < 1)
				break;

			shrFllw = pSce->GetShoreFollowingDepth(activeLB.index.spe);
			minSeedDepth = pSce->GetMinimumSeededingDepth(activeLB.index.spe);
			//box = EnvMousClickCoordToEnvMinMax(PMseClkDwn->coord, PMseClkUp->coord);

			// Add the remaining pods and their animats.
			memset(&inhabitantInf, 0, sizeof(inhabitantInf));
			memset(&inhabitantInf2, 0, sizeof(inhabitantInf2));
			for(i=0; i<pEnvBitmpGdata->addAmount; i++)
			{
				if(TRUE == AttemptToGetValidRndCoordWithinPolygon(&coord,
																  pSce,
																  latStep,
																  lonStep,
																  rect,
																  pEnvBitmpGdata->poly,
																  staticLib.BathyExtremesToEnvExtemes(bathyExtremes),
																  shrFllw,
																  minSeedDepth))
					{
					// Add the pod and its lead animat.
					inhabitantInf.coord = coord;
					pSce->AddPod(activeLB.index.spe, pEnvBitmpGdata->podLeaderType, pEnvBitmpGdata->focalDist);
					activeLB.index.pod = pSce->GetPodCount(activeLB.index.spe)-1;
					pSce->AddPodMember(activeLB.index.spe, activeLB.index.pod, inhabitantInf);

					// Add the rest of the animats to this pod around the lead animat.  The
					// initialPodSize-1 is there because the lead animat counted as one of the pod
					// members.
					for(j=0; j<pEnvBitmpGdata->initialPodSize-1; j++)
					{
						inhabitantInf2.coord =
							GetValidRndCoordWithinFocalDistance(pSce,
																coord.lat,
																coord.lon,
																staticLib.BathyExtremesToEnvExtemes(bathyExtremes),
																shrFllw,
																minSeedDepth,
																pEnvBitmpGdata->focalDist);
						pSce->AddPodMember(activeLB.index.spe, activeLB.index.pod, inhabitantInf2);
					}
				}
			}
			break;


		case THROUGHOUT_MAP://ADD_POD
			if(pEnvBitmpGdata->addAmount < 1)
				break;
			shrFllw = pSce->GetShoreFollowingDepth(activeLB.index.spe);
			minSeedDepth = pSce->GetMinimumSeededingDepth(activeLB.index.spe);
			memset(&inhabitantInf, 0, sizeof(inhabitantInf));
			for(i=0; i<pEnvBitmpGdata->addAmount; i++)
			{
				if(TRUE == AttemptToGetValidRndCoord(&coord,
													 pSce,
													 latStep,
													 lonStep,
													 staticLib.BathyExtremesToEnvExtemes(bathyExtremes),
													 shrFllw,
													 minSeedDepth,
													 rect))
				{
					// Add the pod and its lead animat.
					inhabitantInf.coord = coord;
					pSce->AddPod(activeLB.index.spe, pEnvBitmpGdata->podLeaderType, pEnvBitmpGdata->focalDist);
					activeLB.index.pod = pSce->GetPodCount(activeLB.index.spe)-1;
					pSce->AddPodMember(activeLB.index.spe, activeLB.index.pod, inhabitantInf);

					// Add the rest of the animats to this pod around the lead animat.  The
					// initialPodSize-1 is there because the lead animat counted as one of the pod
					// members.
					for(j=0; j<pEnvBitmpGdata->initialPodSize-1; j++)
					{
						inhabitantInf2.coord = GetValidRndCoordWithinFocalDistance(pSce,
																				   coord.lat,
																				   coord.lon,
																				   staticLib.BathyExtremesToEnvExtemes(bathyExtremes),
																				   shrFllw,
																				   minSeedDepth,
																				   pEnvBitmpGdata->focalDist);
						pSce->AddPodMember(activeLB.index.spe, activeLB.index.pod, inhabitantInf2);
					}
				}
			}
			break;
		}
		break;

	case ADD_POD_MEMBER:
		switch(addMethod)
		{
		case MOUSECLICK://ADD_POD_MEMBER
			//***********************************************//
			// Adds an animat at clicked coordinates to a pod
			//***********************************************//
			memset(&inhabitantInf, 0, sizeof(inhabitantInf));
			shrFllw = pSce->GetShoreFollowingDepth(activeLB.index.spe);
			minSeedDepth = pSce->GetMinimumSeededingDepth(activeLB.index.spe);
			coord.lat = PMseClkDwn->lat;
			coord.lon = PMseClkDwn->lon;

			// Verify the user selected a valid location to add the member.
			bathyDepth = (bathyValue = pSce->GetBathymetryDepth(coord.lat, coord.lon)).depth;
			if(staticLib.IsAValidSeedLatLon(coord.lat,
								  coord.lon,
								  bathyDepth,
								  staticLib.BathyExtremesToEnvExtemes(bathyExtremes),
								  shrFllw,
								  minSeedDepth) == FALSE)
			{
				break;
			}

			// Add a single member to the pod.
			inhabitantInf.coord = coord;
			pSce->AddPodMember(activeLB.index.spe, activeLB.index.pod, inhabitantInf);
			break;

		case N_AROUND_CLICK://ADD_POD_MEMBER
			memset(&inhabitantInf, 0, sizeof(inhabitantInf));
			memset(&inhabitantInf2, 0, sizeof(inhabitantInf2));
			//**********************************************************//
			// Adds N animats at and around clicked coordinates to a pod
			//**********************************************************//
			if(pEnvBitmpGdata->addAmount < 1)
				break;

			shrFllw = pSce->GetShoreFollowingDepth(activeLB.index.spe);
			minSeedDepth = pSce->GetMinimumSeededingDepth(activeLB.index.spe);
			coord.lat = PMseClkDwn->lat;
			coord.lon = PMseClkDwn->lon;

			// Verify the user selected a valid location to add the members around.
			bathyDepth = (bathyValue = pSce->GetBathymetryDepth(coord.lat, coord.lon)).depth;
			if(staticLib.IsAValidSeedLatLon(coord.lat,
								  coord.lon,
								  bathyDepth,
								  staticLib.BathyExtremesToEnvExtemes(bathyExtremes),
								  shrFllw,
								  minSeedDepth) == FALSE)
				break;

			// Then add a single member to the pod just added at the clicked-on coordinates.
			inhabitantInf.coord = coord;
			pSce->AddPodMember(activeLB.index.spe, activeLB.index.pod, inhabitantInf);

			for(i=0; i<pEnvBitmpGdata->addAmount-1; i++)
			{
				ave = pEnvBitmpGdata->AveDist;
				std = pEnvBitmpGdata->StdDevDist;
				inhabitantInf2.coord =
					GetValidRndCoordAboutLatLon(pSce,
												coord.lat,
												coord.lon,
												staticLib.BathyExtremesToEnvExtemes(bathyExtremes),
												shrFllw,
												minSeedDepth,
												ave,
												std);
				// Add a single member to the pod.
				pSce->AddPodMember(activeLB.index.spe, activeLB.index.pod, inhabitantInf2);
			}
			break;

		case BOUNDINGBOX://ADD_POD_MEMBER
			//******************************************//
			// Adds N animats in a bounding box to a pod
			//******************************************//
			memset(&inhabitantInf, 0, sizeof(inhabitantInf));
			if(pEnvBitmpGdata->addAmount < 1)
				break;

			shrFllw = pSce->GetShoreFollowingDepth(activeLB.index.spe);
			minSeedDepth = pSce->GetMinimumSeededingDepth(activeLB.index.spe);
			box = EnvMousClickCoordToEnvMinMax(PMseClkDwn->coord, PMseClkUp->coord);

			for(i=0; i<pEnvBitmpGdata->addAmount; i++)
			{
				// Sets variable 'coord' if it finds a valid coordinate in the box.
				if(TRUE == AttemptToGetValidRndCoordWithinBox(&coord,
															  pSce,
															  latStep,
															  lonStep,
															  rect,
															  box,
															  staticLib.BathyExtremesToEnvExtemes(bathyExtremes),
															  shrFllw,
															  minSeedDepth))
				{
					inhabitantInf.coord = coord;
					pSce->AddPodMember(activeLB.index.spe, activeLB.index.pod, inhabitantInf);
				}
			}
			break;

		case POLYGON://ADD_POD_MEMBER
			//******************************************//
			// Adds N animats in a bounding box to a pod
			//******************************************//
			memset(&inhabitantInf, 0, sizeof(inhabitantInf));
			if(pEnvBitmpGdata->addAmount < 1)
				break;

			shrFllw = pSce->GetShoreFollowingDepth(activeLB.index.spe);
			minSeedDepth = pSce->GetMinimumSeededingDepth(activeLB.index.spe);
			//box = EnvMousClickCoordToEnvMinMax(PMseClkDwn->coord, PMseClkUp->coord);

			for(i=0; i<pEnvBitmpGdata->addAmount; i++)
			{
				// Sets variable 'coord' if it finds a valid coordinate in the box.
				if(TRUE == AttemptToGetValidRndCoordWithinPolygon(&coord,
																  pSce,
																  latStep,
																  lonStep,
																  rect,
																  pEnvBitmpGdata->poly,
																  staticLib.BathyExtremesToEnvExtemes(bathyExtremes),
																  shrFllw,
																  minSeedDepth))
				{
					inhabitantInf.coord = coord;
					pSce->AddPodMember(activeLB.index.spe, activeLB.index.pod, inhabitantInf);
				}
			}
			break;

		case THROUGHOUT_MAP://ADD_POD_MEMBER
			memset(&inhabitantInf, 0, sizeof(inhabitantInf));
			if(pEnvBitmpGdata->addAmount < 1)
				break;

			shrFllw = pSce->GetShoreFollowingDepth(activeLB.index.spe);
			minSeedDepth = pSce->GetMinimumSeededingDepth(activeLB.index.spe);
			for(i=0; i<pEnvBitmpGdata->addAmount; i++)
			{
				if(TRUE == AttemptToGetValidRndCoord(&coord,
													 pSce,
													 latStep,
													 lonStep,
													 staticLib.BathyExtremesToEnvExtemes(bathyExtremes),
													 shrFllw,
													 minSeedDepth,
													 rect))
				{
					inhabitantInf.coord = coord;
					pSce->AddPodMember(activeLB.index.spe, activeLB.index.pod, inhabitantInf);
				}
			}
			break;
		}
		break;
	}
}


BATHYBITMAP_WIN_PARAM GetInitializedSeedInf(SCENARIOUTPUTFILEINF *pSceOutputFileInf, BATHYUSESTATE InitialState)
{
	BATHYBITMAP_WIN_PARAM s;
	memset(&s, 0, sizeof(BATHYBITMAP_WIN_PARAM));
	//s.state.radioAddMethod = NOT_SET;
	//s.state.radioAddType = ADD_INDIVIDUAL;
	s.podLeaderType = ANIMAT;
	s.addAmount = 5;
	s.initialPodSize = 2;
	s.AveDist = 50.0f;
	s.PoissonDistribution = 100.0f;
	s.StdDevDist = 20.0f;
	s.focalDist = 20.0f;
	s.seedDensityKm = .1;
	s.bBathymetry = TRUE;
	s.bSlopeHeading = FALSE;
	s.bDataPoints = FALSE;
	s.bAnimats = TRUE;
	s.playState = STOP;
	s.nPlaybackRate = 9;
	s.playbackInf = pSceOutputFileInf;
	s.usageState = InitialState;

	if(InitialState == SEED_SCENARIO_STATE)
		s.usageType = SEED_SCENARIO_TYPE;
	else
		s.usageType = PLAYBACK_TYPE;
	return s;
}


BATHYMAPINF GetBathyMapInf(ENVMOUSECLICK *PMseClkDwn, ENVMOUSECLICK *PMseClkUp)
{
	BATHYMAPINF m = GetBathyMapInf();

	m.lonMax = PMseClkDwn->lon;
	m.lonMin = PMseClkUp->lon;

	if(PMseClkDwn->coord.X < PMseClkUp->coord.X)
	{
		m.lonMin = PMseClkDwn->lon;
		m.lonMax = PMseClkUp->lon;
	}

	m.latMin = PMseClkDwn->lat;
	m.latMax = PMseClkUp->lat;
	if(PMseClkDwn->coord.Y < PMseClkUp->coord.Y)
	{
		m.latMax = PMseClkDwn->lat;
		m.latMin = PMseClkUp->lat;
	}

	// Keep longitude (y) at the top, vary latitude (x) to get approximate width
	m.widthMeters = staticLib.MetersBetweenCoordinates(m.latMax, m.lonMin, m.latMax, m.lonMax);
	// Vary longitude (y), Keep latitude constant (x) to get approximate height.
	m.heightMeters = staticLib.MetersBetweenCoordinates(m.latMin, m.lonMin, m.latMax, m.lonMin);
	return m;
}


BATHYMAPINF GetBathyMapInf()
{
	BATHYMAPINF m = {0};
	//BATHYEXTREMES e = pSce->GetBathymetryExtremes();
	BATHYEXTREMES e = CBitmapEnvFunctions::GetBathyExtremes();

	m.latMin = e.xMin;
	m.lonMin = e.yMin;
	m.latMax = e.xMax;
	m.lonMax = e.yMax;

	// Keep longitude (y) at the top, vary latitude (x) to get approximate width
	m.widthMeters = staticLib.MetersBetweenCoordinates(m.latMax, m.lonMin, m.latMax, m.lonMax);
	// Vary longitude (y), Keep latitude constant (x) to get approximate height.
	m.heightMeters = staticLib.MetersBetweenCoordinates(m.latMin, m.lonMin, m.latMax, m.lonMin);
	// x is latitude and y longitude.
	
	m.surfarea = CBitmapEnvFunctions::GetBathymtryWaterSurfaceAreaMeters();
	m.dataPointCount = CBitmapEnvFunctions::GetDataPointCounts(FALSE);
	return m;
}

/*

enum SeedingRadioBoxText
{
}

TCHAR *SeedingMethodRadioBox[2][2][7] =
{
	{
	}
}
*/
/*
const TCHAR *aa = "cow";
const TCHAR *bb[] = {"cow", "dow"};
const TCHAR *bc[3] = {"cow", "dow"};
const TCHAR *cc[2][1] = {{"cow"}, {"dow"}};
//const TCHAR *cd[2][] = {{"cow"}, {"dow"}};
const TCHAR *ce[][1] = {{"cow"}, {"dow"}};
const TCHAR *dd[2][3] = {{"cow", "dog", "cat"}, {"mouse", "rat", "hat"}};
//const TCHAR *df[2][] = {{"cow", "dog", "cat"}, {"mouse", "rat", "hat"}};
const TCHAR *df[][3] = {{"cow", "dog", "cat"}, {"mouse", "rat", "hat"}};
*/


void UpdateEnvBitmapDlg(BATHYBITMAP_WIN_PARAM *pGdata)
{
	int i;
	BOOL bEnable;
	BOOL bVal;
	double dVal;
	double nVal;
	int nCmdShow;
	char szBuff0[SIZE_256];
	UINT wparam;

	RADIO_SEEDTYPE addType = GetSeedTypeRadioState();
	RADIO_ADDMETHOD addMethod = GetAddMethodRadioState();
	ACTIVE_LISTBOX activeLB = GetActiveListBox();
	BATHYUSESTATE usageState = pGdata->usageState;
	BATHYUSETYPE usageType = pGdata->usageType;

	//BATHYUSAGESTATEE bathyUsage = CBitmapEnvFunctions::GetUsageState();
	BOOL anyEnvLoaded = CBitmapEnvFunctions::AnyEnvFileLoaded();
	//BOOL bathyLoaded = CBitmapEnvFunctions::BathyFileLoaded();
	int numSpe = CBitmapEnvFunctions::GetSpeciesCount();
	//int numAnimats = CBitmapEnvFunctions::GetAnimatCount();
	//BOOL bSndSrcAnimatPresent = CBitmapEnvFunctions::SoundSourceIsPresent();
	BOOL bSndSrcSpePresent = FALSE;
	//BOOL bFileLoaded = TRUE;//
	BATHYMAPINF mapInf = GetBathyMapInf();
	BOOL bDebugDefined = FALSE;
	BOOL bSndSrcActive = FALSE;

	

#ifdef _DEBUG
	bDebugDefined = TRUE;
#endif

	for(i=0; i<numSpe; i++)
	{
		if(CBitmapEnvFunctions::SpeciesIsASoundSourceModel(i) == FALSE)
			continue;
		bSndSrcSpePresent = TRUE;
		break;
	}

	if(activeLB.index.spe >= 0)
		bSndSrcActive = CBitmapEnvFunctions::SpeciesIsASoundSourceModel(activeLB.index.spe);

	//----------------------------------------------------------------------------------//
	// Playback buttons
	//-----------------//
	bVal = (usageState == PLAYBACK_STATE);
	nCmdShow = SW_HIDE;
	if(bVal == TRUE)
		nCmdShow = SW_SHOW;

	bEnable = (usageState == PLAYBACK_STATE) && (anyEnvLoaded == TRUE);
		
	ShowWindow(GetDlgItem(g_hDlgSeed, IDC_VISUAL_DEC_BUTTON), nCmdShow);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_VISUAL_DEC_BUTTON), bEnable);

	switch(pGdata->playState)
	{
	case STOP:
		SetDlgItemText(g_hDlgSeed, IDC_VISUAL_PLAY_BUTTON, "Play");
		break;
	case PLAY:
		SetDlgItemText(g_hDlgSeed, IDC_VISUAL_PLAY_BUTTON, "Pause");
		break;
	case PAUSE:
		SetDlgItemText(g_hDlgSeed, IDC_VISUAL_PLAY_BUTTON, "Play");
		break;
	}

	ShowWindow(GetDlgItem(g_hDlgSeed, IDC_VISUAL_PLAY_BUTTON), nCmdShow);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_VISUAL_PLAY_BUTTON), bEnable);

	ShowWindow(GetDlgItem(g_hDlgSeed, IDC_VISUAL_STOP_BUTTON), nCmdShow);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_VISUAL_STOP_BUTTON), bEnable);

	ShowWindow(GetDlgItem(g_hDlgSeed, IDC_VISUAL_INC_BUTTON), nCmdShow);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_VISUAL_INC_BUTTON), bEnable);

	ShowWindow(GetDlgItem(g_hDlgSeed, IDC_VISUAL_RESET_BUTTON), nCmdShow);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_VISUAL_RESET_BUTTON), bEnable);

	ShowWindow(GetDlgItem(g_hDlgSeed, IDC_TRACKTOGGLE_BUTTON), nCmdShow);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_TRACKTOGGLE_BUTTON), bEnable);

	//----------------------------------------------------------------------------------//
	// Load Scenario Button (used for loading Shape File Button)
	//----------------------------------------------------------//
	bEnable = (usageType == PLAYBACK_TYPE);
	//bEnable |= (usageState == SEED_SCENARIO_STATE) && anyEnvLoaded && (addMethod == SHAPE_FILE);
	bEnable |= (usageState == SEED_SCENARIO_STATE) && anyEnvLoaded;

	nCmdShow = SW_HIDE;
	if(bEnable == TRUE)
		nCmdShow = SW_SHOW;

	//bEnable = bVal;//(usageState == SEED_SCENARIO_STATE) && (anyEnvLoaded || (usageState == PLAYBACK_STATE));

	if(usageType == PLAYBACK_TYPE)
	{
		strncpy_s(szBuff0, sizeof(szBuff0), "Load File", sizeof(szBuff0)-1);
	}
	else
	{
		strncpy_s(szBuff0, sizeof(szBuff0), "Shape File", sizeof(szBuff0)-1);
	}

	SetWindowText(GetDlgItem(g_hDlgSeed, IDC_LOAD_SCENARIO), szBuff0);
	ShowWindow(GetDlgItem(g_hDlgSeed, IDC_LOAD_SCENARIO), nCmdShow);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_LOAD_SCENARIO), bEnable);
	//----------------------------------------------------------------------------------//


	//----------------------------------------------------------------------------------//
	// "Animat Density" group box controls
	//-------------------------------------//
	bEnable = anyEnvLoaded;

	// The group box.
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_STATIC_ANIMATDENSITYGROUPBOX), bEnable);

	// Total animats static label
	if((usageState == PLAYBACK_STATE) || (bEnable == FALSE))
		strncpy_s(szBuff0, sizeof(szBuff0), "Total Animats: ---", sizeof(szBuff0)-1);
	else
		sprintf_s(szBuff0, sizeof(szBuff0), "Total Animats: %d", CBitmapEnvFunctions::GetAnimatCount());
	SetDlgItemText(g_hDlgSeed, IDC_STATIC_ANIMATPOPSIZE, (LPCTSTR)szBuff0);	
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_STATIC_ANIMATPOPSIZE), bEnable);

	// Animat density static label.
	if(usageState == PLAYBACK_STATE || bEnable == FALSE)
		strncpy_s(szBuff0, sizeof(szBuff0), "Density: --- (1/km^2)", sizeof(szBuff0)-1);
	else
		sprintf_s(szBuff0, sizeof(szBuff0), "Density: %.4f (1/km^2)", CBitmapEnvFunctions::GetAnimatDensity());
	SetDlgItemText(g_hDlgSeed, IDC_STATIC_ANIMATDENSITY, (LPCTSTR)szBuff0);	
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_STATIC_ANIMATDENSITY), bEnable);
	//----------------------------------------------------------------------------------//

	//----------------------------------------------------------------------------------//
	// "Map Information" group Box:
	//-----------------------------//
	bEnable = anyEnvLoaded;

	// The group box static control
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_STATIC_MAPGROUPBOX), bEnable);

	// Map height static label
	if(bEnable == FALSE)
		strncpy_s(szBuff0, sizeof(szBuff0), "Height: ----", sizeof(szBuff0)-1);
	else if(mapInf.heightMeters > 999.0)
		sprintf_s(szBuff0, sizeof(szBuff0), "Height: %.0f (km), %d data points", mapInf.heightMeters/1000.0, mapInf.dataPointCount.x);
	else
		sprintf_s(szBuff0, sizeof(szBuff0), "Height: %.2f (m), %d data points", mapInf.heightMeters, mapInf.dataPointCount.x);
	SetDlgItemText(g_hDlgSeed, IDC_STATIC_MAPHEIGHT, (LPCTSTR)szBuff0);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_STATIC_MAPHEIGHT), bEnable);

	// Map width static label
	if(bEnable == FALSE)
		strncpy_s(szBuff0, sizeof(szBuff0), "Width: ----", sizeof(szBuff0)-1);
	else if(mapInf.widthMeters > 999.0)
		sprintf_s(szBuff0, sizeof(szBuff0), "Width: %.0f (km), %d data points", mapInf.widthMeters/1000.0, mapInf.dataPointCount.y);
	else
		sprintf_s(szBuff0, sizeof(szBuff0), "Width: %.2f (m), %d data points", mapInf.widthMeters, mapInf.dataPointCount.y);
	SetDlgItemText(g_hDlgSeed, IDC_STATIC_MAPWIDTH, (LPCTSTR)szBuff0);	
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_STATIC_MAPWIDTH), bEnable);

	// Map total surface area static label
	dVal = CBitmapEnvFunctions::GetTotalBathySufaceAreaMeters();
	if(bEnable == FALSE)
		strncpy_s(szBuff0, sizeof(szBuff0), "Surface Area: ----", sizeof(szBuff0)-1);
	else if(dVal > 99999.0)
		sprintf_s(szBuff0, sizeof(szBuff0), "Surface Area: %.0f (km^2)", dVal/(1000*1000));
	else
		sprintf_s(szBuff0, sizeof(szBuff0), "Surface Area: %.2f (km^2)", dVal);
	SetDlgItemText(g_hDlgSeed, IDC_STATIC_MAPSURFAREA, (LPCTSTR)szBuff0);	
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_STATIC_MAPSURFAREA), bEnable);

	// Map water surface area static label
	dVal = CBitmapEnvFunctions::GetBathymtryWaterSurfaceAreaMeters();
	if(bEnable == FALSE)
		strncpy_s(szBuff0, sizeof(szBuff0), "Seedable Depths: ----", sizeof(szBuff0)-1);
	else if(dVal > 99999.0)
		sprintf_s(szBuff0, sizeof(szBuff0), "Seedable Depths: %.0f (km^2)", dVal/(1000*1000));
	else
		sprintf_s(szBuff0, sizeof(szBuff0), "Seedable Depths: %.2f (m^2)", dVal);
	SetDlgItemText(g_hDlgSeed, IDC_STATIC_MAPSURFAREA_WATER, (LPCTSTR)szBuff0);	
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_STATIC_MAPSURFAREA_WATER), bEnable);

	// Map land surface area static label
	dVal = CBitmapEnvFunctions::GetBathymetryLandSufaceAreaMeters();
	if(bEnable == FALSE)
		strncpy_s(szBuff0, sizeof(szBuff0), "Non-Seedable Depths: ----", sizeof(szBuff0)-1);
	else if(dVal > 99999.0)
		sprintf_s(szBuff0, sizeof(szBuff0), "Non-Seedable Depths: %.0f (km^2)", dVal/(1000*1000));
	else
		sprintf_s(szBuff0, sizeof(szBuff0), "Non-Seedable Depths: %.2f (m^2)", dVal);
	SetDlgItemText(g_hDlgSeed, IDC_STATIC_MAPSURFAREA_LAND, (LPCTSTR)szBuff0);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_STATIC_MAPSURFAREA_LAND), bEnable);

	// Map latitudal resolution static label
	if((bEnable == FALSE) || (mapInf.dataPointCount.x-1 == 0))
	{
		strncpy_s(szBuff0, sizeof(szBuff0), "Lat. Meters Per Sample Point: ----", sizeof(szBuff0)-1);
	}
	else
	{
		dVal = mapInf.heightMeters/(mapInf.dataPointCount.x-1);
		sprintf_s(szBuff0, sizeof(szBuff0), "Lat. Meters Per Sample Point: %d ", (int)dVal);
	}
	SetDlgItemText(g_hDlgSeed, IDC_STATIC_RESOLUTION_LAT, (LPCTSTR)szBuff0);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_STATIC_RESOLUTION_LAT), bEnable);

	// Map longitudal resolution static label
	if((bEnable == FALSE) || (mapInf.dataPointCount.y-1 == 0))
	{
		strncpy_s(szBuff0, sizeof(szBuff0), "Lon. Meters Per Sample Point: ----", sizeof(szBuff0)-1);
	}
	else
	{
		dVal = mapInf.widthMeters/(mapInf.dataPointCount.y-1);
		sprintf_s(szBuff0, sizeof(szBuff0), "Lon. Meters Per Sample Point: %d ", (int)dVal);
	}
	sprintf_s(szBuff0, sizeof(szBuff0), "Lon. Meters Per Sample Point: %d ", (int)dVal);
	SetDlgItemText(g_hDlgSeed, IDC_STATIC_RESOLUTION_LON, (LPCTSTR)szBuff0);	
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_STATIC_RESOLUTION_LON), bEnable);
	//----------------------------------------------------------------------------------//

	//-----------------------------//
	// "Display Options" group Box:
	//-----------------------------//
	bEnable = anyEnvLoaded;
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_STATIC_DISPLAYOPTSGROUPBOX), bEnable);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_BUTTON_TOGGLE_DATPTS), bEnable);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_BUTTON_TOGGLE_SLOPEREGIONS), bEnable);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_BUTTON_TOGGLE_EXTRAS), bEnable);


	//----------------------------------------------------------------------------------//
	// Populants group box:
	//----------------------------------------------------------------------------------//
	bEnable = ((usageState == SEED_SCENARIO_STATE) && (anyEnvLoaded)) || (usageType == PLAYBACK_TYPE);
	// Populant Group box.
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_STATIC_POPULANTS_GROUP_BOX), bEnable);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_STATIC_SPE_ACSRC), bEnable);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_STATIC_INDIVIDUAL), bEnable);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_STATIC_PODS), bEnable);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_STATIC_POP_LOCATION), bEnable);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_STATIC_SEEDOPTNS_GRPBOX), bEnable);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_STATIC_ADJUSTPODSIZE_GRPBOX), bEnable);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_STATIC_LEADANIMATSETTING_GRPBOX), bEnable);
	

	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_STATIC_SEEDING_METHOD), bEnable);
	
	bEnable = (usageState == SEED_SCENARIO_STATE) && (anyEnvLoaded);
	//bSndSrcAnimatPresent
	// Add acoustic source button.
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_BUTTON_ENVDLG_FIRSANIMATACSTSRCE), bEnable && !bSndSrcSpePresent);
	// Add species button.
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_BUTTON_ENVDLG_SPEC_ADD), anyEnvLoaded);
	// Delete species (and acoutics source) button
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_BUTTON_ENVDLG_SPEC_REMOVE), anyEnvLoaded && numSpe != 0);


	// Delete Individuals button
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_BUTTON_ENVDLG_INDIVIDUAL_DELETE), bEnable && (activeLB.lstbox == INDIVIDUAL_LB));
	// Delete Pod Button
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_BUTTON_ENVDLG_POD_DELETE), bEnable && (activeLB.lstbox == POD_LB));
	// Delete Pod Member Button button
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_BUTTON_ENVDLG_PODMEMBER_DELETE), bEnable && (activeLB.lstbox == MEMBER_LB));

	//----------------------------------------------------//
	// "Populants" group box: "Seed Types" group box:
	//----------------------------------------------------//
	// "Add Individuals" radio box
	bDebugDefined = FALSE;

	bEnable = (usageState == SEED_SCENARIO_STATE) && (anyEnvLoaded);
	bEnable &= (activeLB.lstbox == SPECIES_LB || (bDebugDefined && (addType == ADD_POD_MEMBER) && activeLB.lstbox == POD_LB));
	// Handle case when sound source is present and populated.
	if(activeLB.lstbox == SPECIES_LB && bDebugDefined == FALSE)
	{
		bVal = CBitmapEnvFunctions::SpeciesIsASoundSourceModel(activeLB.index.spe);
		if(bVal == TRUE)
		{
			nVal = CBitmapEnvFunctions::GetAnimatCount(activeLB.index.spe);
			_ASSERT(nVal >= 0);
			if(nVal > 0)
				bEnable = FALSE;
		}
	}
	wparam = BST_UNCHECKED;
	if((addType == ADD_INDIVIDUAL || addType == ADD_POD_MEMBER) && bEnable)
		wparam = BST_CHECKED;
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_RADIO_SEED_INDIVIDUAL), bEnable);
	SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_SEED_INDIVIDUAL), BM_SETCHECK, (WPARAM)wparam, 0);


	// "Add Pods" radio box.
	wparam = BST_UNCHECKED;
	bEnable = (usageState == SEED_SCENARIO_STATE) && (anyEnvLoaded);
	bEnable &= (activeLB.lstbox == SPECIES_LB || (bDebugDefined && (addType == ADD_POD_MEMBER) && activeLB.lstbox == POD_LB));
	bEnable &= !bSndSrcActive;
	if((addType == ADD_POD || addType == ADD_POD_MEMBER) && bEnable && !bSndSrcActive)
		wparam = BST_CHECKED;
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_RADIO_ADD_POD), bEnable);
	SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_ADD_POD), BM_SETCHECK, (WPARAM)wparam, 0);


	// "Initial Size" static control.
	bEnable &= (addType == ADD_POD);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_STATIC_ENVBITMAP_INITPODSIZE), bEnable);

	// Add Pods initial size edit box.
	szBuff0[0] = 0;
	if(bEnable)
		sprintf_s(szBuff0, sizeof(szBuff0), "%d", pGdata->initialPodSize);
	SetDlgItemText(g_hDlgSeed, IDC_EDIT_ENVBITMAP_INIT_PODSIZE, szBuff0);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_EDIT_ENVBITMAP_INIT_PODSIZE), bEnable);


	//---------------------------------------------------------//
	// "Populants" group box: "Lead Animat Settings group box:
	//---------------------------------------------------------//
	// Group Box
	bEnable = (usageState == SEED_SCENARIO_STATE) && anyEnvLoaded &&  !bSndSrcActive;
	bEnable &= (activeLB.lstbox == SPECIES_LB) && (addType == ADD_POD);

	// "Focal Distance" edit box and static control .
	szBuff0[0] = 0;
	if(bEnable)
		sprintf_s(szBuff0, sizeof(szBuff0), "%.3f", pGdata->focalDist);
	SetDlgItemText(g_hDlgSeed, IDC_EDIT_ENVBITMAP_FOCAL_DIST, szBuff0);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_EDIT_ENVBITMAP_FOCAL_DIST), bEnable);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_STATIC_ENVBITMAP_FOCAL_DIST), bEnable);

	// "Focal Animat" radio button
	wparam = BST_UNCHECKED;
	if((pGdata->podLeaderType == ANIMAT) && bEnable)
		wparam = BST_CHECKED;
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_RADIO_PODLEADTYPE_ANIMAT), bEnable);
	SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_PODLEADTYPE_ANIMAT), BM_SETCHECK, (WPARAM)wparam, 0);

	// "Centroid" radio buttons.
	wparam = BST_UNCHECKED;
	if((pGdata->podLeaderType == CENTROID) && bEnable)
		wparam = BST_CHECKED;
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_RADIO_PODLEADTYPE_CENTROID), bEnable);
	SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_PODLEADTYPE_CENTROID), BM_SETCHECK, (WPARAM)wparam, 0);

	//----------------------------------------------------//
	// "Populants" group box: "Adjust Pod Size" group box:
	//----------------------------------------------------//
	bEnable = (usageState == SEED_SCENARIO_STATE) && anyEnvLoaded && !bSndSrcActive;
	bEnable &= (activeLB.lstbox == POD_LB);// && (addType == ADD_POD);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_BUTTON_PODSIZEINC), bEnable);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_BUTTON_PODSIZEDEC), bEnable);


	bEnable = (usageState == SEED_SCENARIO_STATE) && anyEnvLoaded && bDebugDefined && (activeLB.lstbox == POD_LB) && !bSndSrcActive;
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_BUTTON_DEBUG_ADDPODMEMBER), bEnable);

	//--------------------------------------------------------------//
	// "Seeding Methods" group box within the "Populants" group box: 
	//--------------------------------------------------------------//
	// Group Box.
	bEnable = (usageState == SEED_SCENARIO_STATE) && anyEnvLoaded;

	// Radio Buttons BM_SETCHECK, BST_CHECKED
	i = (int)ADD_INDIVIDUAL-1;
	if(addType == ADD_POD)
		i = (int)ADD_POD-1;

	
	bEnable = (addType == ADD_INDIVIDUAL || addType == ADD_POD) && (activeLB.lstbox == SPECIES_LB);
	bEnable |= (bDebugDefined == TRUE && activeLB.lstbox == POD_LB && addType == ADD_POD_MEMBER);
	bEnable &= (usageState == SEED_SCENARIO_STATE) && anyEnvLoaded ;

	// Handle case when sound source is present and populated.
	if(activeLB.lstbox == SPECIES_LB && bDebugDefined == FALSE)
	{
		bVal = CBitmapEnvFunctions::SpeciesIsASoundSourceModel(activeLB.index.spe);
		if(bVal == TRUE)
		{
			nVal = CBitmapEnvFunctions::GetAnimatCount(activeLB.index.spe);
			_ASSERT(nVal >= 0);
			if(nVal > 0)
				bEnable = FALSE;
		}
	}

	// Single Animat Each Click
	wparam = BST_UNCHECKED;
	bVal = (addMethod == MOUSECLICK && bEnable);
	if(bVal == TRUE)
		wparam = BST_CHECKED;
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_RADIO_MOUSE_CLICKING), bEnable);
	SetDlgItemText(g_hDlgSeed, IDC_RADIO_MOUSE_CLICKING, SZ_SEEDMETHOD[i][MOUSECLICK]);
	SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_MOUSE_CLICKING), BM_SETCHECK, (WPARAM)wparam, 0);

	// N Animats around click.
	bEnable = (usageState == SEED_SCENARIO_STATE) && anyEnvLoaded;
	bEnable &= (addType == ADD_INDIVIDUAL || addType == ADD_POD) && (activeLB.lstbox == SPECIES_LB) && !bSndSrcActive;
	wparam = BST_UNCHECKED;
	if(bEnable && (addMethod == N_AROUND_CLICK))
		wparam = BST_CHECKED;
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_RADIO_N_ANIMATS_AROUND_CLICK), bEnable);
	SetDlgItemText(g_hDlgSeed, IDC_RADIO_N_ANIMATS_AROUND_CLICK, SZ_SEEDMETHOD[i][N_AROUND_CLICK]);
	SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_N_ANIMATS_AROUND_CLICK), BM_SETCHECK, (WPARAM)wparam, 0);

	wparam = BST_UNCHECKED;
	if(bEnable && (addMethod == BOUNDINGBOX))
		wparam = BST_CHECKED;
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_RADIO_BOUNDING_BOX), bEnable);
	SetDlgItemText(g_hDlgSeed, IDC_RADIO_BOUNDING_BOX, SZ_SEEDMETHOD[i][BOUNDINGBOX]);
	SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_BOUNDING_BOX), BM_SETCHECK, (WPARAM)wparam, 0);

	wparam = BST_UNCHECKED;
	if(bEnable && (addMethod == THROUGHOUT_MAP))
		wparam = BST_CHECKED;
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_RADIO_N_ANIMATS_OVERMAP), bEnable);
	SetDlgItemText(g_hDlgSeed, IDC_RADIO_N_ANIMATS_OVERMAP, SZ_SEEDMETHOD[i][THROUGHOUT_MAP]);
	SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_N_ANIMATS_OVERMAP), BM_SETCHECK, (WPARAM)wparam, 0);

	wparam = BST_UNCHECKED;
	if(bEnable && (addMethod == POLYGON))
		wparam = BST_CHECKED;
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_RADIO_POLYGON), bEnable);
	SetDlgItemText(g_hDlgSeed, IDC_RADIO_POLYGON, SZ_SEEDMETHOD[i][POLYGON]);
	SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_POLYGON), BM_SETCHECK, (WPARAM)wparam, 0);

	bEnable &= (addType == ADD_INDIVIDUAL);
	wparam = BST_UNCHECKED;
	if(bEnable && (addMethod == BY_DENSITY))
		wparam = BST_CHECKED;
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_RADIO_ANIMATS_BY_DENSITY), bEnable);
	SetDlgItemText(g_hDlgSeed, IDC_RADIO_ANIMATS_BY_DENSITY, SZ_SEEDMETHOD[i][BY_DENSITY]);
	SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_ANIMATS_BY_DENSITY), BM_SETCHECK, (WPARAM)wparam, 0);

	// Shape File radio button
	/*wparam = BST_UNCHECKED;
	if(bEnable && (addMethod == SHAPE_FILE))
		wparam = BST_CHECKED;
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_RADIO_LOAD_SHAPE_FILE), bEnable);
	SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_LOAD_SHAPE_FILE), BM_SETCHECK, (WPARAM)wparam, 0);*/
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_RADIO_LOAD_SHAPE_FILE), FALSE);
	SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_LOAD_SHAPE_FILE), BST_UNCHECKED, (WPARAM)wparam, 0);


	// N Animat or pod controls.
	bEnable = (usageState == SEED_SCENARIO_STATE) && anyEnvLoaded && (addType == ADD_INDIVIDUAL || addType == ADD_POD);
	bEnable &= (addMethod==N_AROUND_CLICK) || (addMethod==BOUNDINGBOX) || (addMethod==THROUGHOUT_MAP) ||
		(addMethod==POLYGON);
	szBuff0[0] = 0;
	if(bEnable)
		sprintf_s(szBuff0, sizeof(szBuff0), "%d", pGdata->addAmount);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_EDIT_NUM_ANIMATS), bEnable);
	SetDlgItemText(g_hDlgSeed, IDC_EDIT_NUM_ANIMATS, szBuff0);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_STATIC_NUMANIMATS), bEnable);


	bEnable = (usageState == SEED_SCENARIO_STATE) && anyEnvLoaded && (addType == ADD_INDIVIDUAL || addType == ADD_POD);
	bEnable &= addMethod==N_AROUND_CLICK;

	// Average distance edit and static control
	szBuff0[0] = 0;
	if(bEnable)
		sprintf_s(szBuff0, sizeof(szBuff0), "%.3f", pGdata->AveDist);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_EDIT_AVE_DIST), bEnable);
	SetDlgItemText(g_hDlgSeed, IDC_EDIT_AVE_DIST, szBuff0);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_STATIC_AVEDIST), bEnable);

	// Average distance edit and static control
	szBuff0[0] = 0;
	if(bEnable)
		sprintf_s(szBuff0, sizeof(szBuff0), "%.3f", pGdata->StdDevDist);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_EDIT_STD_DIST), bEnable);
	SetDlgItemText(g_hDlgSeed, IDC_EDIT_STD_DIST, szBuff0);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_STATIC_STDEVDIST), bEnable);


	// Distribute Animats Randomly by Density controls
	bEnable = (usageState == SEED_SCENARIO_STATE) && anyEnvLoaded && (addType == ADD_INDIVIDUAL);
	bEnable &= addMethod==BY_DENSITY;
	szBuff0[0] = 0;
	if(bEnable)
		sprintf_s(szBuff0, sizeof(szBuff0), "%.6f", pGdata->seedDensityKm);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_EDIT_DENSITY), bEnable);
	SetDlgItemText(g_hDlgSeed, IDC_EDIT_DENSITY, szBuff0);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_STATIC_DENSITY), bEnable);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_STATIC_DENSITYCALC), bEnable);


	bEnable = (usageState == SEED_SCENARIO_STATE) && anyEnvLoaded && (addType == ADD_INDIVIDUAL);
	bEnable &= (addMethod==BY_DENSITY || (addMethod==SHAPE_FILE && pGdata->soundSourcePresent));
	if(addMethod==BY_DENSITY)
		SetDlgWinProps(g_hDlgSeed, IDC_BUTTON_DENSITYDISTRIBUTE, NORMAL_DIST_BUTTON_PROP);
	else if(addMethod==SHAPE_FILE)
		SetDlgWinProps(g_hDlgSeed, IDC_BUTTON_DENSITYDISTRIBUTE, SHAPEFILE_DIST_BUTTON_PROP);
	EnableWindow(GetDlgItem(g_hDlgSeed, IDC_BUTTON_DENSITYDISTRIBUTE), bEnable);
	ShowWindow(GetDlgItem(g_hDlgSeed, IDC_BUTTON_DENSITYDISTRIBUTE), bEnable);
	//----------------------------------------------------------------------------------//
	// End of "Populants" group box
	//------------------------------//
}


typedef struct ControlStateTypeA
{
	BOOL enabled; // FALSE or TRUE
	int show;	  // normally SW_HIDE or SW_SHOW
}CONTROLSTATETYPE_A;

/*
enum ACTIVE_LB
{
	NONE_LB,
	SPECIES_LB,
	INDIVIDUAL_LB,
	POD_LB,
	MEMBER_LB
};
*/
ACTIVE_LISTBOX ClearActiveListBoxes()
{
	ACTIVE_LISTINDEX index;
	index.spe = index.ind = index.pod = index.pdm = -1;
	return SetActiveListBox(index);
}

ACTIVE_LISTBOX SetActiveListBox(ACTIVE_LISTINDEX Indices) // try not to use.  If used, needs to be worked on.
{
	// Pod member index was set.
	if(Indices.pdm > -1)
	{
		_ASSERT(Indices.spe > -1 && Indices.ind == -1 && Indices.pod > -1 && Indices.pdm > -1);
		SendMessage(GetDlgItem(g_hDlgSeed, IDC_LIST_INDIVIDUAL), LB_SETCURSEL, (WPARAM)-1, 0);
	}
	else if(Indices.pod > -1)
	{
		_ASSERT(Indices.spe > -1 && Indices.ind == -1 && Indices.pod > -1);
		SendMessage(GetDlgItem(g_hDlgSeed, IDC_LIST_INDIVIDUAL), LB_SETCURSEL, (WPARAM)-1, 0);
	}
	else if(Indices.ind > -1)
	{
		_ASSERT(Indices.spe > -1 && Indices.ind > -1 && Indices.pod == -1 && Indices.pdm == -1);
		SendMessage(GetDlgItem(g_hDlgSeed, IDC_LIST_PODS), LB_SETCURSEL, (WPARAM)-1, 0);
		SendMessage(GetDlgItem(g_hDlgSeed, IDC_LIST_SEL_POP), LB_RESETCONTENT, (WPARAM)0, 0);
	}

	if(Indices.spe == -1)
	{
		_ASSERT(Indices.spe == -1 && Indices.ind == -1 && Indices.pod == -1 && Indices.pdm == -1);
		SendMessage(GetDlgItem(g_hDlgSeed, IDC_LIST_INDIVIDUAL), LB_RESETCONTENT, (WPARAM)0, 0);
		SendMessage(GetDlgItem(g_hDlgSeed, IDC_LIST_PODS), LB_RESETCONTENT, (WPARAM)0, 0);
		SendMessage(GetDlgItem(g_hDlgSeed, IDC_LIST_SEL_POP), LB_RESETCONTENT, (WPARAM)0, 0);
	}


	SendMessage(GetDlgItem(g_hDlgSeed, IDC_LIST_SPECIES), LB_SETCURSEL, (WPARAM)Indices.spe, 0);
	SendMessage(GetDlgItem(g_hDlgSeed, IDC_LIST_INDIVIDUAL), LB_SETCURSEL, (WPARAM)Indices.ind, 0);
	SendMessage(GetDlgItem(g_hDlgSeed, IDC_LIST_PODS), LB_SETCURSEL, (WPARAM)Indices.pod, 0);
	SendMessage(GetDlgItem(g_hDlgSeed, IDC_LIST_SEL_POP), LB_SETCURSEL, (WPARAM)Indices.pdm, 0);


	return GetActiveListBox();
}



//ACTIVE_LB GetActiveListBox(int *pSpe, int *pInd, int *pPod, int *pPdm)
ACTIVE_LISTBOX GetActiveListBox()
{
	ACTIVE_LISTBOX lb;
	if(g_hDlgSeed == NULL)
	{
		lb.index.spe = lb.index.ind = lb.index.pod = lb.index.pdm = -1;
		lb.lstbox = NONE_LB;
		return lb;
	}

	lb.index.spe = SendMessage(GetDlgItem(g_hDlgSeed, IDC_LIST_SPECIES), LB_GETCURSEL, 0, 0);
	lb.index.ind = SendMessage(GetDlgItem(g_hDlgSeed, IDC_LIST_INDIVIDUAL), LB_GETCURSEL, 0, 0);
	lb.index.pod = SendMessage(GetDlgItem(g_hDlgSeed, IDC_LIST_PODS), LB_GETCURSEL, 0, 0);
	lb.index.pdm = SendMessage(GetDlgItem(g_hDlgSeed, IDC_LIST_SEL_POP), LB_GETCURSEL, 0, 0);

	if(lb.index.spe >= 0 && lb.index.ind == -1 && lb.index.pod >= 0 && lb.index.pdm >= 0)
		lb.lstbox = MEMBER_LB; // Pod member is selected.
	else if(lb.index.spe >= 0 && lb.index.ind >= 0 && lb.index.pod == -1 && lb.index.pdm == -1)
		lb.lstbox = INDIVIDUAL_LB; // Individual is selected.
	else if(lb.index.spe >= 0 && lb.index.ind == -1 && lb.index.pod >= 0 && lb.index.pdm == -1)
		lb.lstbox = POD_LB; // Pod is selected.
	else if(lb.index.spe >= 0 && lb.index.pod == -1 && lb.index.ind == -1 && lb.index.pdm == -1)
		lb.lstbox = SPECIES_LB;
	else if(lb.index.spe == -1 && lb.index.pod == -1 && lb.index.ind == -1 && lb.index.pdm == -1)
		lb.lstbox = NONE_LB;
	else 
		lb.lstbox = INTERMEDIATE_LB;
	return lb;
}

// Pod, individual, or pod member.
RADIO_SEEDTYPE GetSeedTypeRadioState()
{
	LRESULT resInd;
	LRESULT resPod;

	if(g_hDlgSeed == NULL)
		return ADD_NONE;

	resInd = SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_SEED_INDIVIDUAL), BM_GETCHECK, 0, 0);
	resPod = SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_ADD_POD), BM_GETCHECK, 0, 0);

	if(resInd == BST_CHECKED && resPod == BST_CHECKED)
		return ADD_POD_MEMBER; // allowed for debugging only
	if(resInd == BST_CHECKED)
		return ADD_INDIVIDUAL;
	if(resPod == BST_CHECKED)
		return ADD_POD;
	return ADD_NONE;
}

// Pod, individual, or pod member.
RADIO_SEEDTYPE SetSeedTypeRadioState(RADIO_SEEDTYPE SeedType)
{
	if(g_hDlgSeed == NULL)
		return ADD_NONE;

	switch(SeedType)
	{
	case ADD_INDIVIDUAL:
		SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_SEED_INDIVIDUAL), BM_SETCHECK, BST_CHECKED, 0);
		SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_ADD_POD), BM_SETCHECK, BST_UNCHECKED, 0);
		break;
	case ADD_POD:
		SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_SEED_INDIVIDUAL), BM_SETCHECK, BST_UNCHECKED, 0);
		SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_ADD_POD), BM_SETCHECK, BST_CHECKED, 0);
		break;
	case ADD_POD_MEMBER:
		SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_SEED_INDIVIDUAL), BM_SETCHECK, BST_CHECKED, 0);
		SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_ADD_POD), BM_SETCHECK, BST_CHECKED, 0);
		break;
	case ADD_NONE:
		SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_SEED_INDIVIDUAL), BM_SETCHECK, BST_UNCHECKED, 0);
		SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_ADD_POD), BM_SETCHECK, BST_UNCHECKED, 0);
		break;
	}
	_ASSERT(GetSeedTypeRadioState() == SeedType);
	return SeedType;
}

// Seeding Method: single animat, n animats about click, N randomly within box, n over
// map, n within polygon, n by density, shape file.
RADIO_ADDMETHOD SetAddMethodStateRadio(RADIO_ADDMETHOD AddMethod)
{
	int resSingle = BST_UNCHECKED;
	int resClick = BST_UNCHECKED;
	int resBox = BST_UNCHECKED;
	int resMap = BST_UNCHECKED;
	int resPlygn = BST_UNCHECKED;
	int resDensity = BST_UNCHECKED;
	int resPDF = BST_UNCHECKED;

	if(g_hDlgSeed == NULL)
		return NOT_SET;

	switch(AddMethod)
	{
	case NOT_SET:
		break;
	case SHAPE_FILE:
		resPDF = BST_CHECKED;
		break;
	case MOUSECLICK:
		resSingle = BST_CHECKED;
		break;
	case N_AROUND_CLICK:
		resClick = BST_CHECKED;
		break;
	case POLYGON:
		resPlygn = BST_CHECKED;
		break;
	case BOUNDINGBOX:
		resBox = BST_CHECKED;
		break;
	case THROUGHOUT_MAP:
		resMap = BST_CHECKED;
		break;
	case BY_DENSITY:
		resDensity = BST_CHECKED;
		break;
	}


	SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_MOUSE_CLICKING), BM_SETCHECK, resSingle, 0);
	SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_N_ANIMATS_AROUND_CLICK), BM_SETCHECK, resClick, 0);
	SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_BOUNDING_BOX), BM_SETCHECK, resBox, 0);
	SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_N_ANIMATS_OVERMAP), BM_SETCHECK, resMap , 0);
	SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_POLYGON), BM_SETCHECK, resPlygn, 0);
	SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_ANIMATS_BY_DENSITY), BM_SETCHECK, resDensity, 0);
	SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_LOAD_SHAPE_FILE), BM_SETCHECK, resPDF, 0);

	_ASSERT(AddMethod == GetAddMethodRadioState());
	return AddMethod;
}

RADIO_ADDMETHOD GetAddMethodRadioState()
{
	UINT single;
	UINT click;
	UINT box;
	UINT map;
	UINT plygn;
	UINT dens;
	UINT pdf;
	UINT chk = BST_CHECKED;
	UINT unchk = BST_UNCHECKED;

	if(g_hDlgSeed == NULL)
		return NOT_SET;

	single = SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_MOUSE_CLICKING), BM_GETCHECK, 0, 0);
	click = SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_N_ANIMATS_AROUND_CLICK), BM_GETCHECK, 0, 0);
	box = SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_BOUNDING_BOX), BM_GETCHECK, 0, 0);
	map = SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_N_ANIMATS_OVERMAP), BM_GETCHECK, 0, 0);
	plygn = SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_POLYGON), BM_GETCHECK, 0, 0);
	dens = SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_ANIMATS_BY_DENSITY), BM_GETCHECK, 0, 0);
	pdf = SendMessage(GetDlgItem(g_hDlgSeed, IDC_RADIO_LOAD_SHAPE_FILE), BM_GETCHECK, 0, 0);

	unchk = BST_UNCHECKED; // quiet compiler warning.

	if(single==chk)
	{
		_ASSERT(click==unchk && box==unchk && map==unchk && plygn==unchk && dens==unchk && pdf==unchk);
		return MOUSECLICK;
	}else if(click==chk)
	{
		_ASSERT(single==unchk && box==unchk && map==unchk && plygn==unchk && dens==unchk && pdf==unchk);
		return N_AROUND_CLICK;
	}
	else if(box==chk)
	{
		_ASSERT(single==unchk && click == unchk && map==unchk && plygn==unchk && dens==unchk && pdf==unchk);
		return BOUNDINGBOX;
	}
	else if(map==chk)
	{
		_ASSERT(single==unchk && click==unchk && box==unchk && plygn==unchk && dens==unchk && pdf==unchk);
		return THROUGHOUT_MAP;
	}
	else if(plygn==chk)
	{
		_ASSERT(single==unchk && click==unchk && box==unchk && map==unchk && dens==unchk && pdf==unchk);
		return POLYGON;
	}
	else if(dens==chk)
	{
		_ASSERT(single==unchk && click==unchk && box==unchk && map==unchk && plygn==unchk && pdf==unchk);
		return BY_DENSITY;
	}
	else if(pdf==chk)
	{
		_ASSERT(single==unchk && click==unchk && box==unchk && map==unchk && plygn==unchk && dens==unchk);
		return SHAPE_FILE;
	}
	return NOT_SET;
}