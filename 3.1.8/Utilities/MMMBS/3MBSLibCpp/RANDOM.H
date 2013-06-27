/* random.h - contains random number generator and related utilities,
   including advance_random, warmup_random, random, randomize,
   flip, and rnd */

#ifndef _RANDOM_H
#define _RANDOM_H

#include <math.h>
#include <stdlib.h>


class C3MBRandom
{
public:
	C3MBRandom();
	virtual ~C3MBRandom();
	int advance_random ();
	int flip (double prob);
	int initrandomnormaldeviate ();
	double noise (double mu, double sigma);
	int randomize ();
	double randomnormaldeviate ();
	double randomperc ();
	int rnd (int low, int high);
	double rndreal (double lo, double hi);
	int warmup_random (double random_seed);

	void  mysrand(double RandSeed);
	double myrand();
	double myrandn();
	double mycrand();

	//static int StaticTest();


private:
/* variables are declared Static so that they cannot conflict with names of */
/* other global variables in other files.  See K&R, p 80, for scope of Static */
	double oldrand[55];      /* Array of 55 random numbers */
	int	  jrand;               /* current random number */
	double rndx2;            /* used with random normal deviate */
	int	  rndcalcflag;         /* used with random normal deviate */

	// Increments with each instance of this class so a count is maintained.
	static int instanceCnt;
	//static int StaticTestCallCout;

};
#endif
