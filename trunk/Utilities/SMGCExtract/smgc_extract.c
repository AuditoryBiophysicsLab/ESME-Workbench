/*
 *  smgc_extract    extracts data from the SMGC v1.1 databank.
 *
 *  smgc_extract finds observations in the SMGC databank which
 *  meet specified search critera.  If an observations is not
 *  within the right time and geographic region it is ignored
 *  entirely.  If a met/ocean parameter is out of range that
 *  parameter is replaced with a missing value (-9999).
 *
 *  type "smgc_extract" w/ no arguments for more help.
 *
 *  12/28/98 Original Code by Tracy Hall, Planning Systems Inc.
 *
 *  11/21/01 Fixed up for SMGC 2.0 format changes, additional distributions
 *  and percent occurence.  Mike Duncan PSI (--md--).
 *
 *  11/25/01 Put in nicer labels for output.   --md--
 *  11/29/01 Re-structured a lot of stuff to get rid of 
 *  useless code.  --md--
 *  
 *  10/30/02 Put in byte swap routine for Sun, etc.  --th--
 *  10/30/02 Shifted label/data output to line up better.  --th--
 *
 *  01/09/03 Added if(fpin) conditional and removed statement
 *     that broke the longitude loop.  --md--
 *  01/13/03 Added fclose().  --md--
 *  01/13/03 Added -nohdr option.  --md--
 *
 *  05/18/04 Added check for floating point lat/lons entered in 
 *      western and southern hemispheres causing access of incorrect
 *      data square.  --th--
 *  
 */


/* INCLUDED FILES */
#include "smgc_extract.h"
#include <errno.h>

#define CNULL  ((char *)NULL)

double nint (double x);
void Help ( );
void swap_long (long *);

int main (int argc, char *argv[])
{
  char *text,string[100],*file;

  register int j,n,m,k,p,q,lat,lon,icnt,p2,p3;  /* loop indices */
   
  long int   pval, nxt, NBytRec, result, BinNum, Bin1, Bin2, Bin3, Count;
  float      BinVal, BinV1, BinV2, BinV3;
  long int   DatLoc[12][22], NByte[12][22];
  long int   Buf[MSTAT], ExtRan[25][2];
  unsigned long int  MVdist[40000];
  short int wrap_month = 0;       /* flag */
  short int wrap_lon = 0;         /* flag */
  short int swap_data = 0;        /* flat */

  float flat,flon;


  char **NAME;
  char **STAT;
  char **MONTH;

  int Npar;
  int Nlat;
  int Nlon;
  int Nmon;
  int *months;
  int *lats;
  int *lons;
  int ilat,ilon;

  long int lat0,lon0,latN,lonN,lonNx,lon0x;
  long int FSize,Nread;

  FILE *fpin;

  int dohdr=1;
  int donum=0;

  NAME = malloc(sizeof(char)*22*28);
  NAME[0]  = "YEAR                       ";
  NAME[1]  = "AIR/SEA TEMPERATURE DIFF.  ";
  NAME[2]  = "AIR TEMPERATURE            ";
  NAME[3]  = "CEILING HEIGHT             ";
  NAME[4]  = "RELATIVE  HUMIDITY         ";
  NAME[5]  = "SEA LEVEL PRESSSURE        ";
  NAME[6]  = "SEA SURFACE TEMPERATURE    ";
  NAME[7]  = "SWELL DIRECTION            ";
  NAME[8]  = "SWELL HEIGHT               ";
  NAME[9]  = "SWELL PERIOD               ";
  NAME[10] = "TOTAL CLOUD COVER          ";
  NAME[11] = "VISIBILITY                 ";
  NAME[12] = "WAVE DIRECTION             ";
  NAME[13] = "WAVE HEIGHT                ";
  NAME[14] = "WAVE PERIOD                ";
  NAME[15] = "WEATHER                    ";
  NAME[16] = "WIND DIRECTION             ";
  NAME[17] = "WIND SPEED                 ";
  NAME[18] = "TIME/VISIBILITY/CEILING HGT";
  NAME[19] = "TIME/WIND-DIR/WIND-SPEED   ";
  NAME[20] = "TIME/WAVE-DIR/WAVE-HEIGHT  ";
  NAME[21] = "TIME/SWELL-DIR/SWELL-HEIGHT";

  MONTH = malloc(sizeof(char)*4*12);
  MONTH[0]  = "JAN";
  MONTH[1]  = "FEB";
  MONTH[2]  = "MAR";
  MONTH[3]  = "APR";
  MONTH[4]  = "MAY";
  MONTH[5]  = "JUN";
  MONTH[6]  = "JUL";
  MONTH[7]  = "AUG";
  MONTH[8]  = "SEP";
  MONTH[9]  = "OCT";
  MONTH[10] = "NOV";
  MONTH[11] = "DEC";


  STAT = malloc(sizeof(char)*7*10);
  STAT[0] = "NUM. OBS.";
  STAT[0] = "MINIMUM  ";
  STAT[0] = "MAXIMUM  ";
  STAT[0] = "MEAN     ";
  STAT[0] = "STD. DEV.";
  STAT[0] = "MEDIAN   ";
  STAT[0] = "MODE     ";

   /* an exhaustive help message */
  if(argc <= 4)
  {
    Help();
  }

  n=1;
  Npar=0;
  while( n < argc ) 
  {
    strcpy(string,argv[n]);

    if(!strcmp(string,"-lat"))
    {
      strcpy(string,argv[++n]);
      text = strtok(string,"/");
      flat = (float)atof(text);
      if (flat < 0 && (flat - (int)flat) != 0)
        ExtRan[0][0] = (int)flat - 1;
      else
        ExtRan[0][0] = (int)flat;
      text = strtok(CNULL,"/");
      flat = (float)atof(text);
      flat = (float)atof(text);
      if (flat < 0 && (flat - (int)flat) != 0)
        ExtRan[0][1] = (int)flat - 1;
      else
        ExtRan[0][1] = (int)flat;
    }

    if(!strcmp(string,"-lon")) 
    {
      strcpy(string,argv[++n]);
      text = strtok(string,"/");
      flon = (float)atof(text);
      if (flon < 0 && (flon - (int)flon) != 0)
        ExtRan[1][0] = (int)flon - 1;
      else
        ExtRan[1][0] = (int)flon;
      text = strtok(CNULL,"/");
      flon = (float)atof(text);
      if (flon < 0 && (flon - (int)flon) != 0)
        ExtRan[1][1] = (int)flon - 1;
      else
        ExtRan[1][1] = (int)flon;
    }

    if(!strcmp(string,"-mon")) 
    {
      strcpy(string,argv[++n]);
      text = strtok(string,"/");
      ExtRan[2][0] = atoi(text);
      text = strtok(CNULL,"/");
      ExtRan[2][1] = atoi(text);
    }

    if(!strcmp(string,"-par")) 
    {
      strcpy(string,argv[++n]);
      text = strtok(string,"/");
      p = atoi(text);
      if(p>= 0 && p < 22) 
      {
        ExtRan[3+Npar][0] = p;
        text = strtok(CNULL,"/");
        pval = atoi(text);
        if(pval < 1 || pval > 2) pval = 1;
        ExtRan[3+Npar][1] = pval;
        Npar++;
      }
    }


  if(!strcmp(string,"-nohdr")) dohdr = 0;
  if(!strcmp(string,"-number")) donum = 1;


    n++;
  }
  /* print help message if no valid parameters were requested */
  if(Npar < 1)
  {
    Help();
  }

  /* check to see if we're wrapping any of these */
  if(ExtRan[2][0] > ExtRan[2][1]) wrap_month = 1;
  if(ExtRan[1][0] > ExtRan[1][1]) wrap_lon = 1;

/*****************************************************************/

  /* set corners of region to lower left corners so that we
     extract from the right 1 degree boxes */
  lat0 = ExtRan[0][0];
  latN = ExtRan[0][1];
  lon0 = ExtRan[1][0];
  lonN = ExtRan[1][1];
  lon0x = lon0;                /* store lon0 in a safe place */
  lonNx = lonN;                /* store lonN in a safe place */

  Nlat = ExtRan[0][1] - ExtRan[0][0] + 1;
  Nlon = (wrap_lon) ? (ExtRan[1][1] + 180 + 1) + (180 - ExtRan[1][0]) : ExtRan[1][1] - ExtRan[1][0] + 1;
  Nmon = (wrap_month) ? ExtRan[2][1] + (12 - ExtRan[2][0] + 1) : ExtRan[2][1]-ExtRan[2][0] + 1;


  /* lats,lons, and months for which we'll be extracting --md-- */
  months = (int *)calloc(sizeof(int),Nmon);
  lats = (int *)calloc(sizeof(int),Nlat);
  lons = (int *)calloc(sizeof(int),Nlon);

  for(n=0;n<Nmon;n++) 
  {
    months[n] = ExtRan[2][0] + n;
    if(months[n] > 12) months[n] -= 12;
  }

  for(n=0;n<Nlat;n++) {
    lats[n] = ExtRan[0][0] + n;
  }

  for(n=0;n<Nlon;n++) 
  {
    lons[n] = ExtRan[1][0] + n;
    if(lons[n] > 179) lons[n] -= 360;
  }


  for(ilat = 0; ilat < Nlat; ilat++) 
  {
    lat = lats[ilat];

    for(ilon = 0; ilon < Nlon; ilon++) 
    {
      lon = lons[ilon];

      fpin = GetSMGCFile(lat,lon);


    if(fpin == NULL) { 
      //printf("NULL:  %d\n",fpin);
     }


    else if(fpin != NULL ) {

     fread(&flat,sizeof(float),1,fpin);
     if (flat != (float)99.0)
          swap_data = 1;

      fread(&flon,sizeof(float),1,fpin);

      /* get byte offsets for all the months */
      nxt = 0;
      for (m=0; m<12; m++)
      {         /* loop over month */
        for (p=0; p<22; p++)
        {       /* loop over parameters ( there are 22 total) */
          Nread = fread(&NBytRec,PARSIZ,1,fpin);    /* read number of bytes in record */
          if (swap_data)
            swap_long(&NBytRec);
          DatLoc[m][p] = nxt+4;       /* store location/offset (in bytes) of each record */
          NByte[m][p] = NBytRec;        /* Store number of bytes in each record */
          nxt += NBytRec + 4;
          result = fseek( fpin, NBytRec, SEEK_CUR);   /* advance to next record */
        }
      }

      for(n=0;n<Npar;n++)
      {              /* loop over the parameter input arguments */

        p = ExtRan[n+3][0];
        pval = ExtRan[n+3][1];


        if(dohdr) {

          // print Lat, Lon, Month, and Parameter Type header to STDOUT 
          printf("\n Lat\t Lon\t Mon\t Parameter Name             \t");

          // print column headers 
          if (pval == 1 && p < 18) 
           printf(" NObs \t   Min  \t   Max  \t  Mean  \t Std Dev\t Median \t  Mode  \n");
          else 
          {
           if (p < 18)
             printf("  Bin \t Pct.\t  Bin \t Pct. \t  Bin \t Pct. \t  Bin \t Pct. \t  Bin \t Pct.\n");
            else if(p == 18) 
           {
             printf("  Time \tVis. \tHgt. \t Pct.\t  Time \tVis. \tHeight\t Pct.\t  Time \tVis. \tHeight\t Pct.\n");
           }
           else if (p == 19) 
           { 
             printf("  Time \tDir. \tSpeed\t Pct.\t  Time \tDir. \tSpeed \t Pct.\t  Time \tDir. \tSpeed \t Pct.\n");
           }
           else if (p == 20) { 
             printf("  Time \tDir. \tHeight\t Pct.\t Time \tDir. \tHeight\t Pct.\t  Time \tDir. \tHeight\t Pct.\n");
           }
           else if (p == 21) { 
             printf("  Time \tDir. \tHeight\t Pct.\t Time \tDir. \tHeight\t Pct.\t  Time \tDir. \tHeight\t Pct.\n");
           }
         }


          printf(" ---\t ---\t ---\t ---------------------------\t");
          if (pval == 1 && p < 18) 
            printf("------\t--------\t--------\t--------\t--------\t--------\t--------\n");
          else {
            if (p < 18)
              printf("------\t-------\t------\t-------\t------\t-------\t------\t-------\t------\t-----\n");
            else
             printf("  -----\t-----\t-----\t -----\t  -----\t-----\t-----\t -----\t  -----\t-----\t-----\t -----\n");
          }

        } //if(dohdr)

        for(k=0;k<Nmon;k++) 
        {
          m = months[k]-1;

          icnt = 0;
          NBytRec = DatLoc[m][p];
          result = fseek(fpin, NBytRec+8, SEEK_SET);
          
          NBytRec = NByte[m][p];
          if(NBytRec == 0) continue;
          
          /* print the lat, lon, month, and parameter */
          if(!donum) 
            printf(" %3d\t%4d\t %s\t %s\t",lat,lon,MONTH[m],NAME[p]);
          else 
            printf(" %3d\t%4d\t %d\t %d\t",lat,lon,m,p);

          /* read stats and distribution */
          if (p < 18) 
          {
            for(j=0;j<MSTAT;j++) Buf[j] = -9999;
            Nread = fread(Buf,PARSIZ,NBytRec/PARSIZ,fpin);

            if (pval == 1) 
            {          /* print stats output */
              if (swap_data)
                swap_long(&Buf[0]);
              printf("%6d\t",Buf[0]);
              for (j=1; j<PSTAT; j++) 
              {
                if (swap_data)
                  swap_long(&Buf[j]);
                if(Buf[j] == -9999) printf("%8d\t",Buf[j]);
                else {
                  printf("%8.1f\t",Buf[j]*Scale[p]);
                }
              }
            }
            else {            /* print distriubtion output */
              for (j=PSTAT; j<NBytRec/PARSIZ; j++) 
              {
                icnt++;
                if (swap_data)
                  swap_long(&Buf[j]);
                BinNum = (long) nint(Buf[j]/10000000);
                BinVal = (float) (Range[p][0] + BinNum*BinSize[p]);
                Count = Buf[j] - BinNum*10000000;
                printf("%6.1f\t%5.2f\t",BinVal,Count/1e4);
                if (icnt == 5 && j < NBytRec/PARSIZ-1) {
                  icnt = 0;
                  printf("\n    \t    \t    \t                            \t");
                }
              }
            }
          }
          /* read multivariate distribution */
          else 
          {
            if(p == 18) {
              p2 = 11;
              p3 = 3;
            }
            else if(p == 19)  {
              p2 = 16;
              p3 = 17;
            }
            else if(p == 20) {
              p2 = 12;
              p3 = 13;
            }
            else {
              p2 = 7;
              p3 = 8;
            }
            Nread = fread(MVdist,PARSIZ,NBytRec/PARSIZ,fpin);
            for (j=0; j<NBytRec/PARSIZ; j++) {          /* decode and print multivariate dist */
              icnt++;
              if (swap_data)
                swap_long(&MVdist[j]);
              Bin1 = (long) nint(MVdist[j]/1000000000);
              BinV1 = (float) (Bin1*6);
              Bin2 = (long) nint((MVdist[j]-Bin1*1000000000)/10000000);
              BinV2 = (float) (Range[p2][0] + Bin2*BinSize[p2]);
              Bin3 = (long) nint((MVdist[j]-Bin1*1000000000-Bin2*10000000)/100000);
              BinV3 = (float) (Range[p3][0] + Bin3*BinSize[p3]);
              Count = (long) nint(MVdist[j]-Bin1*1000000000-Bin2*10000000-Bin3*100000);
              printf("  %5.1f\t%5.1f\t%5.1f\t %5.2f ",BinV1,BinV2,BinV3,(float)Count/1e3);
              if (icnt == 3 && j < NBytRec/PARSIZ-1) {
                icnt = 0;
                printf("\n    \t    \t    \t                            \t");
              }
            }
          }
          printf("\n");
        }
      }

      fclose(fpin);

      } //   if(fpin)...

    } // ilon
  } // ilat
  return 0;
}

/* END main */



double nint (double x) {

  double frac;
  double whole;
  double y;

  frac = modf(x,&whole);
  y = (frac < 0.5) ? whole : whole + 1;
  return y;
}

void Help ( ) 
{
  printf("\nUsage:");
  printf("\n  smgc_extract -lat [latmin/latmax] -lon [lonmin/lonmax]  -mon [monmin/monmax]  -par [ndx/typ] -par [ndx/typ] .........\n");
  printf("\n");
  printf(" This program expects a min/max latitude, longitude and month and at least \n");
  printf(" one input index and type argument like that described above where ndx is\n");
  printf(" the parameter index (described below) and typ is the type output (also\n");
  printf(" described below) for that parameter.  You can have as many of these ndx/typ\n");
  printf(" input arguments as you want.  If you enter the same ndx more than once the\n");
  printf(" last one will be used. The arguments may be in any order.\n\n");
  printf(" So doing somthing like:\n\n");
  printf("\t smgc_extract -lat 23.0/33.0 -lon  -50.2/-38.5 -mon 1/3 -par 0/0\n");
  printf("\n\n");
  printf(" would get you all the statistics for year between 23.0 - 33.0 North and\n");
  printf(" 38.5 - 50.2 West, for the months of January, February, and March.\n");
  printf(" Note that even though SMGC is partitioned into 1 degree cells you can\n");
  printf(" extract over an arbitrary geographic region.\n");
  printf("\n");
  printf(" Latitude min must always be less than max.  Longitude and month\n");
  printf(" min may be greater than max since these paramaters are cyclic.\n");
  printf(" Thus if you stick in somthing such as 5/-90/-100 you're going to go\n");
  printf(" from 90 W all the way around the world to 100 W. \n");
  printf(" \n\n");
  printf(" The valid list of paramaters, and their global range is as follows:\n\n");
  printf("\n");
  printf("\t Parameter\t\tndx\tmin\tmax\n");
  printf("\t ---------\t\t---\t---\t---\n\n");
  printf("\t Year\t\t\t0\t1854\t1995\n");
  printf("\t Air/Sea dt\t\t1\t-82.00\t50.50\n");
  printf("\t Air tmp.\t\t2\t-50.00\t50.00\n");
  printf("\t Ceil. hgt.\t\t3\t0\t9\n");
  printf("\t Rel. Humid.\t\t4\t0.0\t104.40\n");
  printf("\t Sea lvl. Prs.\t\t5\t920.0\t1060.0\n");
  printf("\t Sea sfc. tmp.\t\t6\t-2.80\t40.00\n");
  printf("\t Swell dir.\t\t7\t0\t360\n");
  printf("\t Swell hgt.\t\t8\t0.0\t50.00\n");
  printf("\t Swell period.\t\t9\t0.0\t30.0\n");
  printf("\t Total Cloud\t\t10\t0\t8\n");
  printf("\t Visibility\t\t11\t1\t10\n");
  printf("\t Wave dir.\t\t12\t0\t360\n");
  printf("\t Wave height\t\t13\t0.0\t35.0\n");
  printf("\t Wave period\t\t14\t0.0\t30.0\n");
  printf("\t Weather\t\t15\t0\t99\n");
  printf("\t Wind dir.\t\t16\t0\t360\n");
  printf("\t Wind speed\t\t17\t0\t102.00\n");
  printf("\t Tim/Vis/Ceil Dist\t18\n");
  printf("\t Tim/WnDir/WnSpd Dist\t19\n");
  printf("\t Tim/WvDir/WvHgt\t20\n");
  printf("\t Tim/SwDir/SwHgt\t21\n");
  printf("\n");
  printf("\t Type Output\t\ttyp\n");
  printf("\t -----------\t\t---\n\n");
  printf("\t Statistics\t\t1\n");
  printf("\t Histogram\t\t2\n");
  printf("\t \n");
  exit(0);
} 

/***************************************************************************\
*                                                                           *
*   Module Name:        swap_long                                           *
*                                                                           *
*   Programmer:         Jan C. Depner                                       *
*                       Original code from dbdbv api routines               *
*                                                                           *
*   Date Written:       July 1992                                           *
*                                                                           *
*   Module Security                                                         *
*   Classification:     Unclassified                                        *
*                                                                           *
*   Data Security                                                           *
*   Classification:     Unknown                                             *
*                                                                           *
*   Purpose:            This function swaps bytes in a four byte long.      *
*                                                                           *
*   Inputs:             word                -   pointer to the long         *
*                                                                           *
\***************************************************************************/

void swap_long (long *word)
{
    unsigned int    temp[4];

    union
    {
        unsigned int    iword;
        long            lword;
    } eq;

    eq.lword = *word;

    temp[0] = eq.iword & 0x000000ff;

    temp[1] = (eq.iword & 0x0000ff00) >> 8;

    temp[2] = (eq.iword & 0x00ff0000) >> 16;

    temp[3] = (eq.iword & 0xff000000) >> 24;

    eq.iword = (temp[0] << 24) + (temp[1] << 16) + (temp[2] << 8) + temp[3];

    *word = eq.lword;

    return;
}

 
/* END smgc_extract.c */
