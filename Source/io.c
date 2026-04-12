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
    if (Argc != 3) {
        return LIBRARY_FUNCTION_ERROR;
    }

    FILE *file = (FILE*)(uintptr_t)MArgument_getInteger(Args[0]);

    MTensor positions = MArgument_getMTensor(Args[1]);
    mint *positions_array = libData->MTensor_getIntegerData(positions);
    mint positions_length = libData->MTensor_getFlattenedLength(positions);

    mint part_size = MArgument_getInteger(Args[2]);
    if (part_size <= 0) {
        return LIBRARY_DIMENSION_ERROR;
    }

    mint dims[1] = {part_size * positions_length};

    MNumericArray byte_array;
    libData->numericarrayLibraryFunctions->MNumericArray_new(MNumericArray_Type_UBit8, 1, dims, &byte_array);
    uint8_t *data = libData->numericarrayLibraryFunctions->MNumericArray_getData(byte_array);

    mint position = 0;
    for (mint i = 0; i < positions_length; i++) {
        fseek(file, positions_array[i], SEEK_SET);
        fread(&data[position], part_size, 1, file);
        position += part_size;
    }

    MArgument_setMNumericArray(Res, byte_array);
    return LIBRARY_NO_ERROR;
}


DLLEXPORT int writeByteArray(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    if (Argc != 4) {
        return LIBRARY_FUNCTION_ERROR;
    }

    FILE *file = (FILE*)(uintptr_t)MArgument_getInteger(Args[0]);
    if (!file) {
        return LIBRARY_FUNCTION_ERROR;
    }

    MNumericArray byte_array = MArgument_getMNumericArray(Args[1]);
    uint8_t *data = libData->numericarrayLibraryFunctions->MNumericArray_getData(byte_array);
    mint byte_array_size = libData->numericarrayLibraryFunctions->MNumericArray_getFlattenedLength(byte_array);

    MTensor positions = MArgument_getMTensor(Args[2]);
    mint *positions_array = libData->MTensor_getIntegerData(positions);
    mint positions_length = libData->MTensor_getFlattenedLength(positions);

    mint part_size = MArgument_getInteger(Args[3]);
    if (part_size <= 0 ||
        positions_array[positions_length - 1] + part_size > byte_array_size) {
        libData->numericarrayLibraryFunctions->MNumericArray_disown(byte_array);
        return LIBRARY_DIMENSION_ERROR;
    }

    mint position = 0;
    for (mint i = 0; i < positions_length; i++) {
        fseek(file, positions_array[i], SEEK_SET);
        fwrite(&data[position], part_size, 1, file);
        position += part_size;
    }

    libData->numericarrayLibraryFunctions->MNumericArray_disown(byte_array);
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
    else {
        return LIBRARY_FUNCTION_ERROR;
    }

    return LIBRARY_NO_ERROR;
}
