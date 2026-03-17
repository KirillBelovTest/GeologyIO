#ifndef SEGY_H
#define SEGY_H

#ifdef _WIN32
    #define bswap_16(x) _byteswap_ushort(x)
    #define bswap_32(x) _byteswap_ulong(x)
    #define bswap_64(x) _byteswap_uint64(x)
#else
    #include <byteswap.h>
#endif

#include "common.h"

DLLEXPORT int readSegyTextHeader(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

DLLEXPORT int readSegyBinaryHeader(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

DLLEXPORT int readSegyTraceHeader(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

DLLEXPORT int readSegyTraceData(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

DLLEXPORT int byteArrayToSegyTraceHeader(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

#pragma pack(push, 1)
typedef struct {
    //   0
    int32_t  TraceSequenceLine;           // 0-3
    int32_t  TraceSequenceFile;           // 4-7
    int32_t  FieldRecord;                 // 8-11
    int32_t  TraceNumber;                 // 12-15
    int32_t  EnergySourcePoint;           // 16-19
    int32_t  cdp;                         // 20-23
    int32_t  cdpTrace;                    // 24-27

    //   28
    int16_t  TraceIdenitifactionCode;     // 28-29
    int16_t  NSummedTraces;               // 30-31
    int16_t  NStackedTraces;              // 32-33
    int16_t  DataUse;                     // 34-35

    //   36
    int32_t  offset;                      // 36-39
    int32_t  ReceiverGroupElevation;      // 40-43
    int32_t  SourceSurfaceElevation;      // 44-47
    int32_t  SourceDepth;                 // 48-51
    int32_t  ReceiverDatumElevation;      // 52-55
    int32_t  SourceDatumElevation;        // 56-59
    int32_t  SourceWaterDepth;            // 60-63
    int32_t  GroupWaterDepth;             // 64-67

    //   68
    int16_t  ElevationScalar;             // 68-69
    int16_t  SourceGroupScalar;           // 70-71
    int32_t  SourceX;                     // 72-75
    int32_t  SourceY;                     // 76-79
    int32_t  GroupX;                      // 80-83
    int32_t  GroupY;                      // 84-87

    //   88
    int16_t  CoordinateUnits;             // 88-89
    int16_t  WeatheringVelocity;          // 90-91
    int16_t  SubWeatheringVelocity;       // 92-93
    int16_t  SourceUpholeTime;            // 94-95
    int16_t  GroupUpholeTime;             // 96-97
    int16_t  SourceStaticCorrection;      // 98-99
    int16_t  GroupStaticCorrection;       // 100-101
    int16_t  TotalStaticApplied;          // 102-103

    //   104
    int16_t  LagTimeA;                    // 104-105
    int16_t  LagTimeB;                    // 106-107
    int16_t  DelayRecordingTime;          // 108-109
    int16_t  MuteTimeStart;               // 110-111
    int16_t  MuteTimeEND;                 // 112-113
    uint16_t ns;                          // 114-115
    uint16_t dt;                          // 116-117

    //   118
    int16_t  GainType;                    // 118-119
    int16_t  InstrumentGainConstant;      // 120-121
    int16_t  InstrumentInitialGain;       // 122-123
    int16_t  Correlated;                  // 124-125
    int16_t  SweepFrequenceStart;         // 126-127
    int16_t  SweepFrequenceEnd;           // 128-129
    int16_t  SweepLength;                 // 130-131
    int16_t  SweepType;                   // 132-133
    int16_t  SweepTraceTaperLengthStart;  // 134-135
    int16_t  SweepTraceTaperLengthEnd;    // 136-137
    int16_t  TaperType;                   // 138-139

    //   140
    int16_t  AliasFilterFrequency;        // 140-141
    int16_t  AliasFilterSlope;            // 142-143
    int16_t  NotchFilterFrequency;        // 144-145
    int16_t  NotchFilterSlope;            // 146-147
    int16_t  LowCutFrequency;             // 148-149
    int16_t  HighCutFrequency;            // 150-151
    int16_t  LowCutSlope;                 // 152-153
    int16_t  HighCutSlope;                // 154-155

    //   156
    int16_t  YearDataRecorded;            // 156-157
    int16_t  DayOfYear;                   // 158-159
    int16_t  HourOfDay;                   // 160-161
    int16_t  MinuteOfHour;                // 162-163
    int16_t  SecondOfMinute;              // 164-165
    int16_t  TimeBaseCode;                // 166-167
    int16_t  TraceWeightningFactor;       // 168-169
    int16_t  GeophoneGroupNumberRoll1;    // 170-171
    int16_t  GeophoneGroupNumberFirstTraceOrigField; // 172-173
    int16_t  GeophoneGroupNumberLastTraceOrigField;  // 174-175
    int16_t  GapSize;                     // 176-177
    int16_t  OverTravel;                  // 178-179

    //   180
    int32_t  cdpX;                        // 180-183
    int32_t  cdpY;                        // 184-187
    int32_t  Inline3D;                    // 188-191
    int32_t  Crossline3D;                 // 192-195
    int32_t  ShotPoint;                   // 196-199
    int16_t  ShotPointScalar;             // 200-201
    int16_t  TraceValueMeasurementUnit;   // 202-203

    //   204
    int32_t  TransductionConstantMantissa; // 204-207
    int16_t  TransductionConstantPower;    // 208-209
    int16_t  TransductionUnit;             // 210-211
    int16_t  TraceIdentifier;              // 212-213
    int16_t  ScalarTraceHeader;            // 214-215
    int16_t  SourceType;                   // 216-217

    //   218
    int32_t  SourceEnergyDirectionMantissa; // 218-221
    int16_t  SourceEnergyDirectionExponent; // 222-223
    int32_t  SourceMeasurementMantissa;     // 224-227
    int16_t  SourceMeasurementExponent;     // 228-229
    int16_t  SourceMeasurementUnit;         // 230-231

    //   232
    int32_t  UnassignedInt1;                // 232-235
    int32_t  UnassignedInt2;                // 236-239
} TraceHeader;
#pragma pack(pop)

#endif