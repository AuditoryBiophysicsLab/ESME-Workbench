// BathyMaker.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <math.h>

#define APPROX_LATLON_PER_METER 0.0000090
#define RESOLUTION_METERS 10.0
#define SIDE_SIZE_METERS 1000.0
#define GRADE_FRACTION .7 // percent

#define WALLED 1

int _tmain(int argc, _TCHAR* argv[])
{
	int i, j;
	int numLoops;
	double depth;
	double percentLon, percentLat, percent;
	double increment = RESOLUTION_METERS * APPROX_LATLON_PER_METER;
	double delta = SIDE_SIZE_METERS*APPROX_LATLON_PER_METER;
	double start = -(SIDE_SIZE_METERS/2)*APPROX_LATLON_PER_METER;
	double lat, lon;
	FILE *fd;
	errno_t err;

	numLoops = (int)floor(delta/increment) + 1;

	err = fopen_s(&fd, "bathy.bth", "wt");

	depth = -5000.00;
	for(i=0; i<numLoops; i++)
	{
		lon = start + i*increment;
		for(j=0; j<numLoops; j++)
		{
			lat = start + j*increment;
			if(!WALLED)
			{
				fprintf(fd, "%8.5f %8.5f %f\n", lon, lat, depth);
			}
			else
			{

				percent = percentLon = fabs(lon)/fabs(start);
				percentLat = fabs(lat)/fabs(start);

				if(percentLat > percent)
					percent = percentLat;

				if(percent < GRADE_FRACTION)
					percent = 0;

				fprintf(fd, "%8.5f %8.5f %8.2f\n", lon, lat, depth - depth*percent);

				//printf("%8.5f %8.5f %8.2f pcntLon(%.3f) pcntLat(%.3f)\n", lon, lat, depth - depth*percent, percentLon, percentLat);
			}
		}
	}

	fclose(fd);
	return 0;
}

