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

DLLEXPORT int readByteArray(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    FILE *file = (FILE*)(uintptr_t)MArgument_getInteger(Args[0]);
    MTensor positions = MArgument_getMTensor(Args[1]);
    mint *positionArray = libData->MTensor_getIntegerData(positions);
    MTensor counts = MArgument_getMTensor(Args[2]);
    mint *countArray = libData->MTensor_getIntegerData(counts);
    mint length = MArgument_getInteger(Args[3]);

    mint dims[1] = {0};
    for (mint i = 0; i < length; i++) {
        dims[0] += countArray[i];
    }

    MNumericArray byteArray;
    libData->numericarrayLibraryFunctions->MNumericArray_new(MNumericArray_Type_UBit8, 1, dims, &byteArray);
    uint8_t *data = libData->numericarrayLibraryFunctions->MNumericArray_getData(byteArray);

    mint pos = 0;
    for (mint i = 0; i < length; i++) {
        fseek(file, positionArray[i], SEEK_SET);
        fread(&data[pos], countArray[i], 1, file);
        pos += countArray[i];
    }

    MArgument_setMNumericArray(Res, byteArray);
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