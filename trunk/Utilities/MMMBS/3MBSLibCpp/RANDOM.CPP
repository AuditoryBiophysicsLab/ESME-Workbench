/* random.c - contains random number generator and related utilities,
   including advance_random, warmup_random, random, randomize,
   flip, and rnd */

#include <memory.h>
#include "random.h"
#include <crtdbg.h>

int C3MBRandom::instanceCnt = 0;
//int C3MBRandom::StaticTestCallCout = 0;

/*
int C3MBRandom::StaticTest()
{
	static int callCount = 0;

	callCount++;
	StaticTestCallCout++;

	return callCount;
}
*/

C3MBRandom::C3MBRandom()
{
	mysrand(0);
	instanceCnt++;
}
C3MBRandom::~C3MBRandom(){}

int C3MBRandom::advance_random()
/* Create next batch of 55 random numbers */
{
    int j1;
    double new_random;

    for (j1 = 0; j1 < 24; j1++)
    {
        new_random = oldrand[j1] - oldrand[j1 + 31];
        if (new_random < 0.0)
            new_random = new_random + 1.0;
        oldrand[j1] = new_random;
    }
    for (j1 = 24; j1 < 55; j1++)
    {
        new_random = oldrand[j1] - oldrand[j1 - 24];
        if (new_random < 0.0)
            new_random = new_random + 1.0;
        oldrand[j1] = new_random;
    }
    return 0;
}

int C3MBRandom::flip(double prob)
/* Flip a biased coin - true if heads */
{
//	copyIt(prob);
    double d = randomperc ();

    if (randomperc () <= prob)
        return (1);
    else
        return (0);
}

int C3MBRandom::initrandomnormaldeviate()
/* initialization routine for randomnormaldeviate */
{
    rndcalcflag = 1;
    return 0;
}

double C3MBRandom::noise(double mu, double sigma)
/* normal noise with specified mean & std dev: mu & sigma */
{
    double d = randomnormaldeviate ();
    return ((randomnormaldeviate () * sigma) + mu);
}

int C3MBRandom::randomize()
/* Get seed number for random and start it up */
{
    double randomseed;
    /*  double drand48 (); */
    int j1;

    for (j1 = 0; j1 <= 54; j1++)
	{
        oldrand[j1] = 0.0;
//		copyIt(oldrand[j1]);
	}
	jrand = 0;
    randomseed = ((double) rand()/RAND_MAX);
	//copyItDog(cow);

    warmup_random (randomseed);
    return 0;
}

double C3MBRandom::randomnormaldeviate()
/* random normal deviate after ACM algorithm 267 / Box-Muller Method */
{
    double d = randomperc ();
    double t, rndx1;

    if (rndcalcflag)
    {
        rndx1 = sqrt (-2.0 * log ((double) randomperc ()));
//		copyIt(rndx1);
        t = 6.2831853072 * (double) randomperc ();
//		copyIt(t);
        rndx2 = rndx1 * sin (t);
//		copyIt(rndx2);
        rndcalcflag = 0;
        return (rndx1 * cos (t));
    }
    else
    {
        rndcalcflag = 1;
        return (rndx2);
    }
}

double C3MBRandom::randomperc()
/* Fetch a single random number between 0.0 and 1.0 - Subtractive Method */
/* See Knuth, D. (1969), v. 2 for details */
/* name changed from random() to avoid library conflicts on some machines */
{
	double ret;
    jrand++;
    if(jrand >= 55)
    {
        jrand = 1;
        advance_random ();
    }
    ret = ((double) oldrand[jrand]);
	//copyIt(ret);
	return ret;
}

int C3MBRandom::rnd(int low, int high)
/* Pick a random integer between low and high */
{
    int i;
    double d = randomperc ();

    if (low >= high)
        i = low;
    else
    {
        i = (int)(randomperc () * (high - low + 1)) + low;
        if (i > high)
            i = high;
    }
    return (i);
}

double C3MBRandom::rndreal(double lo, double hi) 
/* real random number between specified limits */
{
	double ret = (double)((randomperc () * (hi - lo)) + lo);
	return ret;
}


int C3MBRandom::warmup_random(double random_seed)
/* Get random off and running */
{
    int j1, ii;
    double new_random, prev_random;

    oldrand[54] = random_seed;
    new_random = 0.000000001;
    prev_random = random_seed;
    for (j1 = 1; j1 <= 54; j1++)
    {
        ii = (21 * j1) % 54;
        oldrand[ii] = new_random;
        new_random = prev_random - new_random;
        if (new_random < 0.0)
            new_random = new_random + 1.0;
        prev_random = oldrand[ii];
    }

  advance_random ();
  advance_random ();
  advance_random ();

  jrand = 0;
  return 0;
}

// Called externally
void C3MBRandom::mysrand(double RandSeed)
{
    /* initialize random number generator */
    /*  srand48 (time (0)); */
    srand((long int)RandSeed);
    randomize ();
    initrandomnormaldeviate ();
}

// Called externally
double C3MBRandom::myrand()
{
    return randomperc();
}

double C3MBRandom::myrandn()
{
    return randomnormaldeviate();
}

const double PI = 3.14159265358979;

double C3MBRandom::mycrand()
{
	double ret = tan(PI*(myrand() - 1/2));
	return ret;
}


