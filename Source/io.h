#ifndef IO_H
#define IO_H

#include "common.h"

/**
 * Opens file for edit
 * @param path - file absolute path
 */
DLLEXPORT int openFile(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);


/**
 * Reads byte array from the file
 * @param file - file to read
 * @param positions - list of start positions
 * @param counts - how many bytes will be read
 * @param length - length of positions and counts must be the same
 */
DLLEXPORT int readByteArray(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);


/**
 * Close file
 * @param FILE - file for close
 */
DLLEXPORT int closeFile(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

#endif