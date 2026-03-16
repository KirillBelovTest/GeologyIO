#include "io.h"

DLLEXPORT int openFile(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    if (Argc != 1) {
        return LIBRARY_FUNCTION_ERROR;
    }

    char *path = MArgument_getUTF8String(Args[0]);
    if (path == NULL) {
        return LIBRARY_TYPE_ERROR;
    }

    FILE *file = fopen(path, "r+b");
    if (file == NULL) {
        file = fopen(path, "w+b");
    }

    libData->UTF8String_disown(path);

    if (file == NULL) {
        return LIBRARY_FUNCTION_ERROR;
    }

    MArgument_setInteger(Res, (mint)(uintptr_t)file);
    return LIBRARY_NO_ERROR;
}

DLLEXPORT int closeFile(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    if (Argc != 1) {
        return LIBRARY_FUNCTION_ERROR;
    }

    FILE *file = (FILE*)(uintptr_t)MArgument_getInteger(Args[0]);

    if (file != NULL) {
        fclose(file);
    }

    MArgument_setInteger(Res, 0);
    return LIBRARY_NO_ERROR;
}