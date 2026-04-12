#ifndef IO_H
#define IO_H

#include "common.h"

//openFile["path"] -> file
DLLEXPORT int openFile(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);


//readByteArray[file, {positions}, partSize] -> <byteArray>
DLLEXPORT int readByteArray(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);


//writeByteArray[file, <byteArray>, {positions}, partSize]
DLLEXPORT int writeByteArray(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);


//closeFile[file]
DLLEXPORT int closeFile(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);


#endif
