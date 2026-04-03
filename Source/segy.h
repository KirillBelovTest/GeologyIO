#ifndef SEGY_H
#define SEGY_H

#define IBM_32_FLOAT_FORMAT_CODE 1
#define IBM_32_FLOAT_SAMPLE_SIZE 4

#ifdef _WIN32
    #define bswap_16(x) _byteswap_ushort(x)
    #define bswap_32(x) _byteswap_ulong(x)
    #define bswap_64(x) _byteswap_uint64(x)
#elif defined(__APPLE__)
    #include <libkern/OSByteOrder.h>
    #define bswap_16(x) OSSwapInt16(x)
    #define bswap_32(x) OSSwapInt32(x)
    #define bswap_64(x) OSSwapInt64(x)
#else
    #include <byteswap.h>
#endif

#include "common.h"

DLLEXPORT int readSegyTextHeaderByteArray(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

DLLEXPORT int readSegyBinaryHeaderByteArray(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

DLLEXPORT int readSegyTraceHeader(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

DLLEXPORT int readSegyTraceData(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

DLLEXPORT int byteArrayToSegyBinaryHeader(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

DLLEXPORT int byteArrayToSegyTraceHeader(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

DLLEXPORT int getSegyTraceHeaders(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

DLLEXPORT int getSegyTracesData(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

void segy_trace_header_byte_array_to_mint(uint8_t *input, mint *output);

void segy_binary_header_byte_array_to_mint(uint8_t *byteArray, mint *data);

#pragma pack(push, 1)
typedef struct {
    int32_t  jobId;                // 3201
    int32_t  lineNumber;           // 3205
    int32_t  reelNumber;           // 3209
    uint16_t tracesPerEnsemble;    // 3213
    uint16_t auxTracesPerEnsemble; // 3215
    uint16_t sampleInterval;       // 3217
    uint16_t sampleIntervalOrig;   // 3219
    uint16_t samplesPerTrace;      // 3221
    uint16_t samplesPerTraceOrig;  // 3223
    uint16_t formatCode;           // 3225
    uint16_t ensembleFold;         // 3227
    uint16_t traceSorting;         // 3229
    uint16_t verticalSumCode;      // 3231
    uint16_t sweepFreqStart;       // 3233
    uint16_t sweepFreqEnd;         // 3235
    uint16_t sweepLength;          // 3237
    uint16_t sweepTypeCode;        // 3239
    uint16_t sweepChannel;         // 3241
    uint16_t sweepTaperStart;      // 3243
    uint16_t sweepTaperEnd;        // 3245
    uint16_t taperType;            // 3247
    uint16_t correlatedFlag;       // 3249
    uint16_t binaryGainRecovery;   // 3251
    uint16_t amplitudeRecovery;    // 3253
    uint16_t measurementSystem;    // 3255
    uint16_t impulsePolarity;      // 3257
    uint16_t vibratoryPolarity;    // 3259
    uint8_t  unassigned1[240];     // 3261-3500
    uint16_t segyVersion;          // 3501
    uint16_t fixedLengthFlag;      // 3503
    uint16_t extTextHeadersNum;    // 3505
    uint8_t  unassigned2[94];      // 3507-3600
} SEGYBinaryHeader;
#pragma pack(pop)

#pragma pack(push, 1)
typedef struct {
    int32_t tracl;    // 1-4
    int32_t tracr;    // 5-8
    int32_t fldr;     // 9-12
    int32_t tracf;    // 13-16
    int32_t ep;       // 17-20
    int32_t cdp;      // 21-24
    int32_t cdpt;     // 25-28
    int16_t trid;     // 29-30
    int16_t nvs;      // 31-32
    int16_t nhs;      // 33-34
    int16_t duse;     // 35-36
    int32_t offset;   // 37-40
    int32_t gelev;    // 41-44
    int32_t selev;    // 45-48
    int32_t sdepth;   // 49-52
    int32_t gdel;     // 53-56
    int32_t sdel;     // 57-60
    int32_t swdep;    // 61-64
    int32_t gwdep;    // 65-68
    int16_t scalel;   // 69-70
    int16_t scalco;   // 71-72
    int32_t sx;       // 73-76
    int32_t sy;       // 77-80
    int32_t gx;       // 81-84
    int32_t gy;       // 85-88
    int16_t counit;   // 89-90
    int16_t wevel;    // 91-92
    int16_t swevel;   // 93-94
    int16_t sut;      // 95-96
    int16_t gut;      // 97-98
    int16_t sstat;    // 99-100
    int16_t gstat;    // 101-102
    int16_t tstat;    // 103-104
    int16_t laga;     // 105-106
    int16_t lagb;     // 107-108
    int16_t delrt;    // 109-110
    int16_t muts;     // 111-112
    int16_t mute;     // 113-114
    uint16_t ns;      // 115-116
    uint16_t dt;      // 117-118
    int16_t gain;     // 119-120
    int16_t igain;    // 121-122
    int16_t gaing;    // 123-124
    int16_t corr;     // 125-126
    int16_t sfs;      // 127-128
    int16_t sfe;      // 129-130
    int16_t slen;     // 131-132
    int16_t styp;     // 133-134
    int16_t stas;     // 135-136
    int16_t stae;     // 137-138
    int16_t tatyp;    // 139-140
    int16_t afilf;    // 141-142
    int16_t afils;    // 143-144
    int16_t nofilf;   // 145-146
    int16_t nofils;   // 147-148
    int16_t lcf;      // 149-150
    int16_t hcf;      // 151-152
    int16_t lcs;      // 153-154
    int16_t hcs;      // 155-156
    int16_t year;     // 157-158
    int16_t day;      // 159-160
    int16_t hour;     // 161-162
    int16_t minute;   // 163-164
    int16_t sec;      // 165-166
    int16_t tny;      // 167-168
    int16_t twt;      // 169-170
    int16_t geono;    // 171-172
    int16_t grnors;   // 173-174
    int16_t grnofr;   // 175-176
    int16_t grnols;   // 177-178
    int16_t gaps;     // 179-180
    int16_t otrav;    // 181-182
    int32_t cdpx;     // 181-184
    int32_t cdpy;     // 185-188
    int32_t iline;    // 189-192
    int32_t xline;    // 193-196
    int32_t shpoint;  // 197-200
    int16_t shpscal;  // 201-202
    int16_t tvalunit; // 203-204
    int32_t transc;   // 205-208
    uint8_t rest[32]; // 209-240
} SEGYTraceHeader;
#pragma pack(pop)

#endif