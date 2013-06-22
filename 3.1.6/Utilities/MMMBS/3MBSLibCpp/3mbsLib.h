#ifndef MMMBSLIB_H
#define MMMBSLIB_H

#include "staticLib.h"
#include "Animat.h"
#include "Pod.h"

#include "datatypes.h"
#include "SpeciesModel.h"
#include "Scenario.h"
#include "Species.h"
#include "EnvironmentData.h"


// Macros
// 0x000000ff hex = 255 decimal
//#define GetSuperVersion(Version) (((Version) >> 16)&0x0000ffff)
//#define GetSubVersion(Version) (0x0000ffff & (Version))

//#define Combine2x08bitValuesInto16Bits(V1, V2)) ((((V1)& 0x000000ff) << 8) | ((V2) & 0x000000ff))
//#define Combine2x16bitValuesInto32Bits(V1, V2) (((V1) << 16) | ((V2) & 0x0000ffff))



//#define SetByteValues32bit(B3, B2, B1, B0) ((B3&255) << 24 | (B2&255) << 16 | (B1&255) << 8 | (B0&255) << 0)
//#define GetByteValue32bit(_32BitWord, BIndex) ((_32BitWord) & (255 << ((BIndex) * 8))


//#define CombineSuperAndSubVer(Super, Sub) (((Super) << 16) | ((Sub) & 0x0000ffff))
//#define GetSuperVersion(Version) (((Version) >> 16)&0x0000ffff)
//#define GetSubVersion(Version) (0x0000ffff & (Version))



#endif // MMMBSLIB_H
