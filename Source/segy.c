#include "io.h"
#include "segy.h"

#define SEGY_TEXT_HEADER_SIZE 3200
#define SEGY_BINARY_HEADER_SIZE 400
#define SEGY_TRACE_HEADER_SIZE 240

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

DLLEXPORT int byteArrayToSegyTraceHeader(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    MNumericArray traceHeaderByteArray = MArgument_getMNumericArray(Args[0]);
    uint8_t *traceHeaderBytes = libData->numericarrayLibraryFunctions->MNumericArray_getData(traceHeaderByteArray);
    TraceHeader *traceHeader = (TraceHeader*)traceHeaderBytes;

    mint dims[1] = {91};
    MTensor traceHeaderList;
    libData->MTensor_new(MType_Integer, 1, &dims, &traceHeaderList);

    mint *data = libData->MTensor_getIntegerData(traceHeaderList);

    // 1 (int32)
    data[0]  = bswap_32(traceHeader->TraceSequenceLine);
    data[1]  = bswap_32(traceHeader->TraceSequenceFile);
    data[2]  = bswap_32(traceHeader->FieldRecord);
    data[3]  = bswap_32(traceHeader->TraceNumber);
    data[4]  = bswap_32(traceHeader->EnergySourcePoint);
    data[5]  = bswap_32(traceHeader->cdp);
    data[6]  = bswap_32(traceHeader->cdpTrace);

    // 2 (int16 -> expand to mint)
    data[7]  = bswap_16(traceHeader->TraceIdenitifactionCode);
    data[8]  = bswap_16(traceHeader->NSummedTraces);
    data[9]  = bswap_16(traceHeader->NStackedTraces);
    data[10] = bswap_16(traceHeader->DataUse);

    // 3 (int32)
    data[11] = bswap_32(traceHeader->offset);
    data[12] = bswap_32(traceHeader->ReceiverGroupElevation);
    data[13] = bswap_32(traceHeader->SourceSurfaceElevation);
    data[14] = bswap_32(traceHeader->SourceDepth);
    data[15] = bswap_32(traceHeader->ReceiverDatumElevation);
    data[16] = bswap_32(traceHeader->SourceDatumElevation);
    data[17] = bswap_32(traceHeader->SourceWaterDepth);
    data[18] = bswap_32(traceHeader->GroupWaterDepth);

    // 4 (int16 + int32)
    data[19] = bswap_16(traceHeader->ElevationScalar);
    data[20] = bswap_16(traceHeader->SourceGroupScalar);
    data[21] = bswap_32(traceHeader->SourceX);
    data[22] = bswap_32(traceHeader->SourceY);
    data[23] = bswap_32(traceHeader->GroupX);
    data[24] = bswap_32(traceHeader->GroupY);

    // 5 (int16)
    data[25] = bswap_16(traceHeader->CoordinateUnits);
    data[26] = bswap_16(traceHeader->WeatheringVelocity);
    data[27] = bswap_16(traceHeader->SubWeatheringVelocity);
    data[28] = bswap_16(traceHeader->SourceUpholeTime);
    data[29] = bswap_16(traceHeader->GroupUpholeTime);
    data[30] = bswap_16(traceHeader->SourceStaticCorrection);
    data[31] = bswap_16(traceHeader->GroupStaticCorrection);
    data[32] = bswap_16(traceHeader->TotalStaticApplied);

    // 6 (int16 + uint16)
    data[33] = bswap_16(traceHeader->LagTimeA);
    data[34] = bswap_16(traceHeader->LagTimeB);
    data[35] = bswap_16(traceHeader->DelayRecordingTime);
    data[36] = bswap_16(traceHeader->MuteTimeStart);
    data[37] = bswap_16(traceHeader->MuteTimeEND);
    data[38] = bswap_16(traceHeader->ns);      // uint16
    data[39] = bswap_16(traceHeader->dt);      // uint16

    // 7 (int16)
    data[40] = bswap_16(traceHeader->GainType);
    data[41] = bswap_16(traceHeader->InstrumentGainConstant);
    data[42] = bswap_16(traceHeader->InstrumentInitialGain);
    data[43] = bswap_16(traceHeader->Correlated);
    data[44] = bswap_16(traceHeader->SweepFrequenceStart);
    data[45] = bswap_16(traceHeader->SweepFrequenceEnd);
    data[46] = bswap_16(traceHeader->SweepLength);
    data[47] = bswap_16(traceHeader->SweepType);
    data[48] = bswap_16(traceHeader->SweepTraceTaperLengthStart);
    data[49] = bswap_16(traceHeader->SweepTraceTaperLengthEnd);
    data[50] = bswap_16(traceHeader->TaperType);

    // 8 (int16)
    data[51] = bswap_16(traceHeader->AliasFilterFrequency);
    data[52] = bswap_16(traceHeader->AliasFilterSlope);
    data[53] = bswap_16(traceHeader->NotchFilterFrequency);
    data[54] = bswap_16(traceHeader->NotchFilterSlope);
    data[55] = bswap_16(traceHeader->LowCutFrequency);
    data[56] = bswap_16(traceHeader->HighCutFrequency);
    data[57] = bswap_16(traceHeader->LowCutSlope);
    data[58] = bswap_16(traceHeader->HighCutSlope);

    // 9 (int16)
    data[59] = bswap_16(traceHeader->YearDataRecorded);
    data[60] = bswap_16(traceHeader->DayOfYear);
    data[61] = bswap_16(traceHeader->HourOfDay);
    data[62] = bswap_16(traceHeader->MinuteOfHour);
    data[63] = bswap_16(traceHeader->SecondOfMinute);
    data[64] = bswap_16(traceHeader->TimeBaseCode);
    data[65] = bswap_16(traceHeader->TraceWeightningFactor);
    data[66] = bswap_16(traceHeader->GeophoneGroupNumberRoll1);
    data[67] = bswap_16(traceHeader->GeophoneGroupNumberFirstTraceOrigField);
    data[68] = bswap_16(traceHeader->GeophoneGroupNumberLastTraceOrigField);
    data[69] = bswap_16(traceHeader->GapSize);
    data[70] = bswap_16(traceHeader->OverTravel);

    // 10 (int32 + int16)
    data[71] = bswap_32(traceHeader->cdpX);
    data[72] = bswap_32(traceHeader->cdpY);
    data[73] = bswap_32(traceHeader->Inline3D);
    data[74] = bswap_32(traceHeader->Crossline3D);
    data[75] = bswap_32(traceHeader->ShotPoint);
    data[76] = bswap_16(traceHeader->ShotPointScalar);
    data[77] = bswap_16(traceHeader->TraceValueMeasurementUnit);

    // 11 (int32 + int16)
    data[78] = bswap_32(traceHeader->TransductionConstantMantissa);
    data[79] = bswap_16(traceHeader->TransductionConstantPower);
    data[80] = bswap_16(traceHeader->TransductionUnit);
    data[81] = bswap_16(traceHeader->TraceIdentifier);
    data[82] = bswap_16(traceHeader->ScalarTraceHeader);
    data[83] = bswap_16(traceHeader->SourceType);

    // 12 (int32 + int16)
    data[84] = bswap_32(traceHeader->SourceEnergyDirectionMantissa);
    data[85] = bswap_16(traceHeader->SourceEnergyDirectionExponent);
    data[86] = bswap_32(traceHeader->SourceMeasurementMantissa);
    data[87] = bswap_16(traceHeader->SourceMeasurementExponent);
    data[88] = bswap_16(traceHeader->SourceMeasurementUnit);

    // 13 (int32)
    data[89] = bswap_32(traceHeader->UnassignedInt1);
    data[90] = bswap_32(traceHeader->UnassignedInt2);

    libData->numericarrayLibraryFunctions->MNumericArray_disown(traceHeaderByteArray);

    MArgument_setMTensor(Res, traceHeaderList);
    return LIBRARY_NO_ERROR;
}