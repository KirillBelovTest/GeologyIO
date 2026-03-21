#include "io.h"
#include "segy.h"
#include "numbers.h"

#define SEGY_TEXT_HEADER_SIZE 3200
#define SEGY_BINARY_HEADER_SIZE 400
#define SEGY_BINARY_HEADER_LENGTH 30
#define SEGY_TRACE_HEADER_SIZE 240
#define SEGY_TRACE_HEADER_LENGTH 80

DLLEXPORT int readSegyTextHeader(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    if (Argc != 1) {
        return LIBRARY_FUNCTION_ERROR;
    }

    FILE *file = (FILE*)(uintptr_t)MArgument_getInteger(Args[0]);
    if (file == NULL) {
        return LIBRARY_FUNCTION_ERROR;
    }

    if (ferror(file) || feof(file)) {
        clearerr(file);
        return LIBRARY_FUNCTION_ERROR;
    }

    if (fseek(file, 0, SEEK_SET) != 0) {
        return LIBRARY_FUNCTION_ERROR;
    }

    mint length = SEGY_TEXT_HEADER_SIZE;
    MNumericArray textHeaderByteArray;
    libData->numericarrayLibraryFunctions->MNumericArray_new(MNumericArray_Type_UBit8, 1, &length, &textHeaderByteArray);
    uint8_t *buffer = (uint8_t *)libData->numericarrayLibraryFunctions->MNumericArray_getData(textHeaderByteArray);

    size_t readLength = fread(buffer, SEGY_TEXT_HEADER_SIZE, 1, file);
    if (readLength != SEGY_TEXT_HEADER_SIZE) {
        return LIBRARY_FUNCTION_ERROR;
    }

    MArgument_setMNumericArray(Res, textHeaderByteArray);
    return LIBRARY_NO_ERROR;
}

DLLEXPORT int readSegyBinaryHeader(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    if (Argc != 1) {
        return LIBRARY_FUNCTION_ERROR;
    }

    FILE *file = (FILE*)(uintptr_t)MArgument_getInteger(Args[0]);
    if (file == NULL) {
        return LIBRARY_FUNCTION_ERROR;
    }

    if (ferror(file) || feof(file)) {
        clearerr(file);
        return LIBRARY_FUNCTION_ERROR;
    }

    if (fseek(file, SEGY_TEXT_HEADER_SIZE, SEEK_SET) != 0) {
        return LIBRARY_FUNCTION_ERROR;
    }

    mint length = SEGY_BINARY_HEADER_SIZE;
    MNumericArray binaryHeaderByteArray;
    libData->numericarrayLibraryFunctions->MNumericArray_new(MNumericArray_Type_UBit8, 1, &length, &binaryHeaderByteArray);
    uint8_t *buffer = (uint8_t *)libData->numericarrayLibraryFunctions->MNumericArray_getData(binaryHeaderByteArray);

    size_t readLength = fread(buffer, SEGY_BINARY_HEADER_SIZE, 1, file);
    if (readLength != SEGY_BINARY_HEADER_SIZE) {
        return LIBRARY_FUNCTION_ERROR;
    }

    MArgument_setMNumericArray(Res, binaryHeaderByteArray);
    return LIBRARY_NO_ERROR;
}

DLLEXPORT int readSegyTraceHeader(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    FILE *file = (FILE*)(uintptr_t)MArgument_getInteger(Args[0]);
    mint index = MArgument_getInteger(Args[1]);
    mint traceSize = MArgument_getInteger(Args[2]);

    fseek(file, SEGY_TEXT_HEADER_SIZE + SEGY_BINARY_HEADER_SIZE + traceSize * index, SEEK_SET);

    mint length = SEGY_TRACE_HEADER_SIZE;
    MNumericArray traceHeaderByteArray;
    libData->numericarrayLibraryFunctions->MNumericArray_new(MNumericArray_Type_UBit8, 1, &length, &traceHeaderByteArray);
    uint8_t *buffer = (uint8_t *)libData->numericarrayLibraryFunctions->MNumericArray_getData(traceHeaderByteArray);

    fread(buffer, 1, SEGY_TRACE_HEADER_SIZE, file);

    MArgument_setMNumericArray(Res, traceHeaderByteArray);
    return LIBRARY_NO_ERROR;
}

DLLEXPORT int getSegyTraceHeaders(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    FILE *file = (FILE*)(uintptr_t)MArgument_getInteger(Args[0]);
    MTensor indexesTensor = MArgument_getMTensor(Args[1]);
    mint *indexes = libData->MTensor_getIntegerData(indexesTensor);
    mint count = MArgument_getInteger(Args[2]);
    mint traceSize = MArgument_getInteger(Args[3]);

    mint dims[2] = {count, SEGY_TRACE_HEADER_LENGTH};
    MTensor traceHeaders;
    libData->MTensor_new(MType_Integer, 2, dims, &traceHeaders);
    mint *data = libData->MTensor_getIntegerData(traceHeaders);
    uint8_t buffer[SEGY_TRACE_HEADER_SIZE];

    long firstTracePosition = SEGY_TEXT_HEADER_SIZE + SEGY_BINARY_HEADER_SIZE;
    for (mint i = 0; i < count; i++){
        long index = indexes[i];
        fseek(file, firstTracePosition + traceSize * index, SEEK_SET);
        fread(buffer, SEGY_TRACE_HEADER_SIZE, 1, file);
        mint position = i * SEGY_TRACE_HEADER_LENGTH;
        segy_trace_header_byte_array_to_mint(buffer, &data[position]);
    }

    MArgument_setMTensor(Res, traceHeaders);
    return LIBRARY_NO_ERROR;
}

DLLEXPORT int getSegyTracesData(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    FILE *file = (FILE*)(uintptr_t)MArgument_getInteger(Args[0]);
    MTensor indexesTensor = MArgument_getMTensor(Args[1]);
    mint *indexes = libData->MTensor_getIntegerData(indexesTensor);
    mint count = MArgument_getInteger(Args[2]);
    mint traceSize = MArgument_getInteger(Args[3]);
    mint fromSample = MArgument_getInteger(Args[4]);
    mint samplesCount = MArgument_getInteger(Args[5]);

    mint dims[2] = {count, samplesCount};
    MNumericArray traceData;
    libData->numericarrayLibraryFunctions->MNumericArray_new(MNumericArray_Type_Real64, 2, dims, &traceData);
    mreal *data = (mreal *)libData->numericarrayLibraryFunctions->MNumericArray_getData(traceData);
    uint8_t *buffer = malloc(samplesCount * 4);

    long firstTracePosition = SEGY_TEXT_HEADER_SIZE + SEGY_BINARY_HEADER_SIZE + SEGY_TRACE_HEADER_SIZE;
    for (mint i = 0; i < count; i++){
        long index = indexes[i];
        fseek(file, firstTracePosition + traceSize * index + fromSample * 4, SEEK_SET);
        fread(buffer, samplesCount * 4, 1, file);
        mint position = i * samplesCount;
        ibm_32_byte_array_to_double(buffer, &data[position], samplesCount);
    }

    free(buffer);
    MArgument_setMNumericArray(Res, traceData);
    return LIBRARY_NO_ERROR;
}

DLLEXPORT int readSegyTraceData(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    FILE *file = (FILE*)(uintptr_t)MArgument_getInteger(Args[0]);
    mint index = MArgument_getInteger(Args[1]);
    mint traceSize = MArgument_getInteger(Args[2]);

    fseek(file, SEGY_TEXT_HEADER_SIZE + SEGY_BINARY_HEADER_SIZE + traceSize * index + SEGY_TRACE_HEADER_SIZE, SEEK_SET);

    mint length = traceSize - SEGY_TRACE_HEADER_SIZE;
    MNumericArray traceByteArray;
    libData->numericarrayLibraryFunctions->MNumericArray_new(MNumericArray_Type_UBit8, 1, &length, &traceByteArray);
    uint8_t *buffer = (uint8_t *)libData->numericarrayLibraryFunctions->MNumericArray_getData(traceByteArray);

    fread(buffer, length, 1, file);

    MArgument_setMNumericArray(Res, traceByteArray);
    return LIBRARY_NO_ERROR;
}

void segy_binary_header_byte_array_to_mint(uint8_t *byteArray, mint *data) {
    SEGYBinaryHeader *header = (SEGYBinaryHeader *)byteArray;

    data[0]  = (mint)bswap_32(header->jobId);
    data[1]  = (mint)bswap_32(header->lineNumber);
    data[2]  = (mint)bswap_32(header->reelNumber);
    data[3]  = (mint)bswap_16(header->tracesPerEnsemble);
    data[4]  = (mint)bswap_16(header->auxTracesPerEnsemble);
    data[5]  = (mint)bswap_16(header->sampleInterval);
    data[6]  = (mint)bswap_16(header->sampleIntervalOrig);
    data[7]  = (mint)bswap_16(header->samplesPerTrace);
    data[8]  = (mint)bswap_16(header->samplesPerTraceOrig);
    data[9]  = (mint)bswap_16(header->formatCode);
    data[10] = (mint)bswap_16(header->ensembleFold);
    data[11] = (mint)bswap_16(header->traceSorting);
    data[12] = (mint)bswap_16(header->verticalSumCode);
    data[13] = (mint)bswap_16(header->sweepFreqStart);
    data[14] = (mint)bswap_16(header->sweepFreqEnd);
    data[15] = (mint)bswap_16(header->sweepLength);
    data[16] = (mint)bswap_16(header->sweepTypeCode);
    data[17] = (mint)bswap_16(header->sweepChannel);
    data[18] = (mint)bswap_16(header->sweepTaperStart);
    data[19] = (mint)bswap_16(header->sweepTaperEnd);
    data[20] = (mint)bswap_16(header->taperType);
    data[21] = (mint)bswap_16(header->correlatedFlag);
    data[22] = (mint)bswap_16(header->binaryGainRecovery);
    data[23] = (mint)bswap_16(header->amplitudeRecovery);
    data[24] = (mint)bswap_16(header->measurementSystem);
    data[25] = (mint)bswap_16(header->impulsePolarity);
    data[26] = (mint)bswap_16(header->vibratoryPolarity);
    data[27] = (mint)bswap_16(header->segyVersion);
    data[28] = (mint)bswap_16(header->fixedLengthFlag);
    data[29] = (mint)bswap_16(header->extTextHeadersNum);
}

DLLEXPORT int byteArrayToSegyBinaryHeader(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    if (Argc < 1) return LIBRARY_FUNCTION_ERROR;

    MNumericArray byteArray = MArgument_getMNumericArray(Args[0]);

    mint len = libData->numericarrayLibraryFunctions->MNumericArray_getFlattenedLength(byteArray);
    if (len != SEGY_BINARY_HEADER_SIZE) return LIBRARY_DIMENSION_ERROR;

    uint8_t *bytes = (uint8_t*)libData->numericarrayLibraryFunctions->MNumericArray_getData(byteArray);

    MTensor binaryHeaderList;
    const mint dims[1] = {SEGY_BINARY_HEADER_LENGTH};
    int err = libData->MTensor_new(MType_Integer, 1, dims, &binaryHeaderList);
    if (err) return err;

    mint *data = libData->MTensor_getIntegerData(binaryHeaderList);

    segy_binary_header_byte_array_to_mint(bytes, data);

    libData->numericarrayLibraryFunctions->MNumericArray_disown(byteArray);
    MArgument_setMTensor(Res, binaryHeaderList);
    return LIBRARY_NO_ERROR;
}

void segy_trace_header_byte_array_to_mint(uint8_t *input, mint *output) {
    SEGYTraceHeader *header = (SEGYTraceHeader*)input;

    output[0] = (mint)bswap_32(header->tracl);
    output[1] = (mint)bswap_32(header->tracr);
    output[2] = (mint)bswap_32(header->fldr);
    output[3] = (mint)bswap_32(header->tracf);
    output[4] = (mint)bswap_32(header->ep);
    output[5] = (mint)bswap_32(header->cdp);
    output[6] = (mint)bswap_32(header->cdpt);
    output[7] = (mint)bswap_16(header->trid);
    output[8] = (mint)bswap_16(header->nvs);
    output[9] = (mint)bswap_16(header->nhs);
    output[10] = (mint)bswap_16(header->duse);
    output[11] = (mint)bswap_32(header->offset);
    output[12] = (mint)bswap_32(header->gelev);
    output[13] = (mint)bswap_32(header->selev);
    output[14] = (mint)bswap_32(header->sdepth);
    output[15] = (mint)bswap_32(header->gdel);
    output[16] = (mint)bswap_32(header->sdel);
    output[17] = (mint)bswap_32(header->swdep);
    output[18] = (mint)bswap_32(header->gwdep);
    output[19] = (mint)bswap_16(header->scalel);
    output[20] = (mint)bswap_16(header->scalco);
    output[21] = (mint)bswap_32(header->sx);
    output[22] = (mint)bswap_32(header->sy);
    output[23] = (mint)bswap_32(header->gx);
    output[24] = (mint)bswap_32(header->gy);
    output[25] = (mint)bswap_16(header->counit);
    output[26] = (mint)bswap_16(header->wevel);
    output[27] = (mint)bswap_16(header->swevel);
    output[28] = (mint)bswap_16(header->sut);
    output[29] = (mint)bswap_16(header->gut);
    output[30] = (mint)bswap_16(header->sstat);
    output[31] = (mint)bswap_16(header->gstat);
    output[32] = (mint)bswap_16(header->tstat);
    output[33] = (mint)bswap_16(header->laga);
    output[34] = (mint)bswap_16(header->lagb);
    output[35] = (mint)bswap_16(header->delrt);
    output[36] = (mint)bswap_16(header->muts);
    output[37] = (mint)bswap_16(header->mute);
    output[38] = (mint)bswap_16(header->ns);
    output[39] = (mint)bswap_16(header->dt);
    output[40] = (mint)bswap_16(header->gain);
    output[41] = (mint)bswap_16(header->igain);
    output[42] = (mint)bswap_16(header->gaing);
    output[43] = (mint)bswap_16(header->corr);
    output[44] = (mint)bswap_16(header->sfs);
    output[45] = (mint)bswap_16(header->sfe);
    output[46] = (mint)bswap_16(header->slen);
    output[47] = (mint)bswap_16(header->styp);
    output[48] = (mint)bswap_16(header->stas);
    output[49] = (mint)bswap_16(header->stae);
    output[50] = (mint)bswap_16(header->tatyp);
    output[51] = (mint)bswap_16(header->afilf);
    output[52] = (mint)bswap_16(header->afils);
    output[53] = (mint)bswap_16(header->nofilf);
    output[54] = (mint)bswap_16(header->nofils);
    output[55] = (mint)bswap_16(header->lcf);
    output[56] = (mint)bswap_16(header->hcf);
    output[57] = (mint)bswap_16(header->lcs);
    output[58] = (mint)bswap_16(header->hcs);
    output[59] = (mint)bswap_16(header->year);
    output[60] = (mint)bswap_16(header->day);
    output[61] = (mint)bswap_16(header->hour);
    output[62] = (mint)bswap_16(header->minute);
    output[63] = (mint)bswap_16(header->sec);
    output[64] = (mint)bswap_16(header->tny);
    output[65] = (mint)bswap_16(header->twt);
    output[66] = (mint)bswap_16(header->geono);
    output[67] = (mint)bswap_16(header->grnors);
    output[68] = (mint)bswap_16(header->grnofr);
    output[69] = (mint)bswap_16(header->grnols);
    output[70] = (mint)bswap_16(header->gaps);
    output[71] = (mint)bswap_16(header->otrav);
    output[72] = (mint)bswap_32(header->cdpx);
    output[73] = (mint)bswap_32(header->cdpy);
    output[74] = (mint)bswap_32(header->iline);
    output[75] = (mint)bswap_32(header->xline);
    output[76] = (mint)bswap_32(header->shpoint);
    output[77] = (mint)bswap_16(header->shpscal);
    output[78] = (mint)bswap_16(header->tvalunit);
    output[79] = (mint)bswap_32(header->transc);
}

DLLEXPORT int byteArrayToSegyTraceHeader(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    MNumericArray byteArray = MArgument_getMNumericArray(Args[0]);
    uint8_t *bytes = libData->numericarrayLibraryFunctions->MNumericArray_getData(byteArray);
    MTensor headerList;
    const mint dims[1] = {SEGY_TRACE_HEADER_LENGTH};
    libData->MTensor_new(MType_Integer, 1, dims, &headerList);
    mint *data = libData->MTensor_getIntegerData(headerList);
    segy_trace_header_byte_array_to_mint(bytes, data);
    libData->numericarrayLibraryFunctions->MNumericArray_disown(byteArray);
    MArgument_setMTensor(Res, headerList);
    return LIBRARY_NO_ERROR;
}