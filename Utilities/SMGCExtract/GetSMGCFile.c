/*
 *  GetSMGCName.c   Get the name of a SMGC data file
 *
 *  GetSMGCName returns the name of a SMGC data file based on 
 *  a give lat and lon.  THIS IS INSTALLATION SPECIFIC.
 *  
 *
 *  12/1/98 Original code by Mike Duncan, Plannning Systems Inc.
 *  12/30/98 Added ilon to handle lon >= 180, Tracy Hall, Plannning Systems Inc.
 *
 */


#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
/* #include <unistd.h> */

FILE  *GetSMGCFile(int lat, int lon) {

   char DbPath[100];
   char LatHem[2];
   char LonHem[2];
   char tmpname[200];
   int retval;
   char *path_north;
   char *path_south;

   FILE *smgc_file;

   struct stat filestat;

   int ilon;

   path_north = getenv("SMGC_DATA_NORTH"); 
   path_south = getenv("SMGC_DATA_SOUTH"); 


/*   path_north = ".\\smgc2.0\\north\\";
   path_south = ".\\smgc2.0\\south\\";
*/


   if(lat < 0) {
	strcpy(LatHem,"s");
	strcpy(DbPath,path_south);
   }
   else {
	strcpy(LatHem,"n");
	strcpy(DbPath,path_north);
   }


   ilon = (lon >= 180) ? lon - 360 : lon;
   if(ilon < 0) strcpy(LonHem,"w");
   else strcpy(LonHem,"e");

   sprintf(tmpname,"%s%s%02d%s%03d.stt",DbPath,LatHem,abs(lat),LonHem,abs(ilon)); 

   retval = stat(tmpname,&filestat);
   if(filestat.st_size == 0) return NULL;

   smgc_file = fopen(tmpname,"rb");

   return smgc_file;

}

/* END GetSMGCFile.c */
