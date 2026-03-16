#ifndef IO_H
#define IO_H

#include "common.h"

/**
 * Opens file for edit
 * @param path - file absolute path
 */
DLLEXPORT int openFile(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

/**
 * Close file
 * @param FILE - file for close
 */
DLLEXPORT int closeFile(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

#endif