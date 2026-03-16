#ifndef SEGY_H
#define SEGY_H

#include "common.h"

DLLEXPORT int readSegyTextHeader(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

DLLEXPORT int readSegyBinaryHeader(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

#endif