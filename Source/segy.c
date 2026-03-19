#include "io.h"
#include "segy.h"

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

    size_t readLength = fread(buffer, 1, SEGY_TEXT_HEADER_SIZE, file);
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

    size_t readLength = fread(buffer, 1, SEGY_BINARY_HEADER_SIZE, file);
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

DLLEXPORT int readSegyTraceData(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    FILE *file = (FILE*)(uintptr_t)MArgument_getInteger(Args[0]);
    mint index = MArgument_getInteger(Args[1]);
    mint traceSize = MArgument_getInteger(Args[2]);

    fseek(file, SEGY_TEXT_HEADER_SIZE + SEGY_BINARY_HEADER_SIZE + traceSize * index + SEGY_TRACE_HEADER_SIZE, SEEK_SET);

    mint length = traceSize - SEGY_TRACE_HEADER_SIZE;
    MNumericArray traceByteArray;
    libData->numericarrayLibraryFunctions->MNumericArray_new(MNumericArray_Type_UBit8, 1, &length, &traceByteArray);
    uint8_t *buffer = (uint8_t *)libData->numericarrayLibraryFunctions->MNumericArray_getData(traceByteArray);

    fread(buffer, 1, length, file);

    MArgument_setMNumericArray(Res, traceByteArray);
    return LIBRARY_NO_ERROR;
}

DLLEXPORT int byteArrayToSegyBinaryHeader(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    if (Argc < 1) return LIBRARY_FUNCTION_ERROR;

    MNumericArray byteArray = MArgument_getMNumericArray(Args[0]);

    mint len = libData->numericarrayLibraryFunctions->MNumericArray_getFlattenedLength(byteArray);
    if (len != SEGY_BINARY_HEADER_SIZE) return LIBRARY_DIMENSION_ERROR;

    uint8_t *bytes = (uint8_t*)libData->numericarrayLibraryFunctions->MNumericArray_getData(byteArray);
    SEGYBinaryHeader *header = (SEGYBinaryHeader*)bytes;

    MTensor binaryHeaderList;
    const mint dims[1] = {SEGY_BINARY_HEADER_LENGTH};
    int err = libData->MTensor_new(MType_Integer, 1, dims, &binaryHeaderList);
    if (err) return err;

    mint *data = libData->MTensor_getIntegerData(binaryHeaderList);

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

    libData->numericarrayLibraryFunctions->MNumericArray_disown(byteArray);
    MArgument_setMTensor(Res, binaryHeaderList);
    return LIBRARY_NO_ERROR;
}

DLLEXPORT int byteArrayToSegyTraceHeader(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    MNumericArray byteArray = MArgument_getMNumericArray(Args[0]);
    uint8_t *bytes = libData->numericarrayLibraryFunctions->MNumericArray_getData(byteArray);
    SEGYTraceHeader *header = (SEGYTraceHeader*)bytes;

    MTensor headerList;
    const mint dims[1] = {SEGY_TRACE_HEADER_LENGTH};
    libData->MTensor_new(MType_Integer, 1, dims, &headerList);
    mint *data = libData->MTensor_getIntegerData(headerList);

    data[0] = (mint)bswap_32(header->tracl);
    data[1] = (mint)bswap_32(header->tracr);
    data[2] = (mint)bswap_32(header->fldr);
    data[3] = (mint)bswap_32(header->tracf);
    data[4] = (mint)bswap_32(header->ep);
    data[5] = (mint)bswap_32(header->cdp);
    data[6] = (mint)bswap_32(header->cdpt);
    data[7] = (mint)bswap_16(header->trid);
    data[8] = (mint)bswap_16(header->nvs);
    data[9] = (mint)bswap_16(header->nhs);
    data[10] = (mint)bswap_16(header->duse);
    data[11] = (mint)bswap_32(header->offset);
    data[12] = (mint)bswap_32(header->gelev);
    data[13] = (mint)bswap_32(header->selev);
    data[14] = (mint)bswap_32(header->sdepth);
    data[15] = (mint)bswap_32(header->gdel);
    data[16] = (mint)bswap_32(header->sdel);
    data[17] = (mint)bswap_32(header->swdep);
    data[18] = (mint)bswap_32(header->gwdep);
    data[19] = (mint)bswap_16(header->scalel);
    data[20] = (mint)bswap_16(header->scalco);
    data[21] = (mint)bswap_32(header->sx);
    data[22] = (mint)bswap_32(header->sy);
    data[23] = (mint)bswap_32(header->gx);
    data[24] = (mint)bswap_32(header->gy);
    data[25] = (mint)bswap_16(header->counit);
    data[26] = (mint)bswap_16(header->wevel);
    data[27] = (mint)bswap_16(header->swevel);
    data[28] = (mint)bswap_16(header->sut);
    data[29] = (mint)bswap_16(header->gut);
    data[30] = (mint)bswap_16(header->sstat);
    data[31] = (mint)bswap_16(header->gstat);
    data[32] = (mint)bswap_16(header->tstat);
    data[33] = (mint)bswap_16(header->laga);
    data[34] = (mint)bswap_16(header->lagb);
    data[35] = (mint)bswap_16(header->delrt);
    data[36] = (mint)bswap_16(header->muts);
    data[37] = (mint)bswap_16(header->mute);
    data[38] = (mint)bswap_16(header->ns);
    data[39] = (mint)bswap_16(header->dt);
    data[40] = (mint)bswap_16(header->gain);
    data[41] = (mint)bswap_16(header->igain);
    data[42] = (mint)bswap_16(header->gaing);
    data[43] = (mint)bswap_16(header->corr);
    data[44] = (mint)bswap_16(header->sfs);
    data[45] = (mint)bswap_16(header->sfe);
    data[46] = (mint)bswap_16(header->slen);
    data[47] = (mint)bswap_16(header->styp);
    data[48] = (mint)bswap_16(header->stas);
    data[49] = (mint)bswap_16(header->stae);
    data[50] = (mint)bswap_16(header->tatyp);
    data[51] = (mint)bswap_16(header->afilf);
    data[52] = (mint)bswap_16(header->afils);
    data[53] = (mint)bswap_16(header->nofilf);
    data[54] = (mint)bswap_16(header->nofils);
    data[55] = (mint)bswap_16(header->lcf);
    data[56] = (mint)bswap_16(header->hcf);
    data[57] = (mint)bswap_16(header->lcs);
    data[58] = (mint)bswap_16(header->hcs);
    data[59] = (mint)bswap_16(header->year);
    data[60] = (mint)bswap_16(header->day);
    data[61] = (mint)bswap_16(header->hour);
    data[62] = (mint)bswap_16(header->minute);
    data[63] = (mint)bswap_16(header->sec);
    data[64] = (mint)bswap_16(header->tny);
    data[65] = (mint)bswap_16(header->twt);
    data[66] = (mint)bswap_16(header->geono);
    data[67] = (mint)bswap_16(header->grnors);
    data[68] = (mint)bswap_16(header->grnofr);
    data[69] = (mint)bswap_16(header->grnols);
    data[70] = (mint)bswap_16(header->gaps);
    data[71] = (mint)bswap_16(header->otrav);
    data[72] = (mint)bswap_32(header->cdpx);
    data[73] = (mint)bswap_32(header->cdpy);
    data[74] = (mint)bswap_32(header->iline);
    data[75] = (mint)bswap_32(header->xline);
    data[76] = (mint)bswap_32(header->shpoint);
    data[77] = (mint)bswap_16(header->shpscal);
    data[78] = (mint)bswap_16(header->tvalunit);
    data[79] = (mint)bswap_32(header->transc);

    libData->numericarrayLibraryFunctions->MNumericArray_disown(byteArray);
    MArgument_setMTensor(Res, headerList);
    return LIBRARY_NO_ERROR;
}