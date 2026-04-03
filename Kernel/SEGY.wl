(* :Package: *)

BeginPackage["WLJS`GeologyIO`SEGY`", {
    "WLJS`GeologyIO`Library`",
    "WLJS`GeologyIO`Numbers`",
    "WLJS`GeologyIO`EBCDIC`",
    "WLJS`GeologyIO`IO`",
    "LibraryLink`"
}];


SEGYOpen::usage =
"SEGYOpen[data] open seg-y file for the editing.";


SEGYFile::usage =
"SEGYFile[data] editable seg-y file representation.";


SEGYImport::usage =
"SEGYImport[file] import seg-y file as SEGYData.";


SEGYData::usage =
"SEGYData[data] seg-y in-memory representation.";


SEGYExport::usage =
"SEGYExport[file, segy] export segy to target file.";


SEGYClose::usage =
"SEGYClose[data] close editable seg-y file.";


Begin["`Private`"];


SEGYOpen[file_String?FileExistsQ] :=
Module[{
    path = AbsoluteFileName[file],
    stream,
    fileSize,
    numberOfSamplesForReel,
    samplesFormatCode,
    traceSize,
    numberDataTraces
},
    If[!KeyExistsQ[$streams, path],
        $streams[path] = WLJS`GeologyIO`IO`Private`openFile[path]
    ];

    stream = $streams[path];

    fileSize = FileByteCount[path];

    numberOfSamplesForReelByteArray = WLJS`GeologyIO`IO`Private`readByteArray[stream, {3220}, {2}, 1];
    numberOfSamplesForReel = ImportByteArray[numberOfSamplesForReelByteArray, "UnsignedInteger16", ByteOrdering -> 1][[1]];

    samplesFormatCodeByteArray = WLJS`GeologyIO`IO`Private`readByteArray[stream, {3224}, {2}, 1];
    samplesFormatCode = ImportByteArray[samplesFormatCodeByteArray, "UnsignedInteger16", ByteOrdering -> 1][[1]];

    traceSize = numberOfSamplesForReel * $sampleSize[samplesFormatCode] + $traceHeaderSize;
    numberDataTraces = (FileByteCount[file] - $fileHeaderSize) / traceSize;

    SEGYFile[<|
        "Path" -> path,
        "Stream" -> stream,
        "FileSize" -> fileSize,
        "TraceSize" -> traceSize,
        "NumberDataTraces" -> numberDataTraces,
        "SamplesFormatCode" -> samplesFormatCode,
        "NumberOfSamplesForReel" -> numberOfSamplesForReel
    |>]
];


SEGYFile[assoc_Association][key_String] :=
assoc[[key]];


SEGYFile[assoc_Association]["TraceHeaders", traceNumbers: {__Integer}] :=
With[{
    stream = assoc["Stream"],
    tracesCount = Length[traceNumbers],
    traceSize = assoc["TraceSize"],
    formatCode = assoc["SamplesFormatCode"]
},
    getSegyTraceHeaders[stream,
        traceNumbers,
        traceStartPositions,
        tracesCount,
        samplesCount,
        traceSize,
        formatCode
    ]
];


SEGYFile[assoc_Association]["TracesData",
    traceNumbers: {__Integer},
    traceStartPositions: {__Integer},
    samplesCount_Integer] :=
With[{
    stream = assoc["Stream"],
    tracesCount = Length[traceNumbers],
    traceSize = assoc["TraceSize"],
    formatCode = assoc["SamplesFormatCode"]
},
    getSegyTracesData[stream,
        traceNumbers,
        traceStartPositions,
        tracesCount,
        samplesCount,
        traceSize,
        formatCode
    ]
];






SEGYImport[segyFile_SEGYFile] :=
Module[{
    stream,
    numberOfSamplesForReel,
    numberDataTraces,
    traceHeaders,
    traceData
},
    stream = segyFile["Stream"];
    numberOfSamplesForReel = segyFile["NumberOfSamplesForReel"];
    traceSize = segyFile["TraceSize"];
    numberDataTraces = segyFile["NumberDataTraces"];

    traceHeaders = getSegyTraceHeaders[stream, Range[numberDataTraces], numberDataTraces, traceSize];
    traceData = getSegyTracesData[stream, Range[numberDataTraces], numberDataTraces, traceSize, 0, numberOfSamplesForReel];

    SEGYData[<|
        "TextHeader" -> segyFile["TextHeader"],
        "BinaryHeader" -> segyFile["BinaryHeader"],
        "TraceHeaders" -> traceHeaders,
        "TraceData" -> traceData
    |>]
];


If[!ValueQ[$streams], $streams = <||>];


$sampleSize = <|
    1 -> 4
|>;


$traceHeaderSize = 240;


$fileHeaderSize = 3600;


$binaryHeaderKeys = <|
    "jobId" -> 1, "lineNumber" -> 2, "reelNumber" -> 3, "tracesPerEnsemble" -> 4,
    "auxTracesPerEnsemble" -> 5, "sampleInterval" -> 6, "sampleIntervalOrig" -> 7,
    "samplesPerTrace" -> 8, "samplesPerTraceOrig" -> 9, "formatCode" -> 10,
    "ensembleFold" -> 11, "traceSorting" -> 12, "verticalSumCode" -> 13,
    "sweepFreqStart" -> 14, "sweepFreqEnd" -> 15, "sweepLength" -> 16,
    "sweepTypeCode" -> 17, "sweepChannel" -> 18, "sweepTaperStart" -> 19,
    "sweepTaperEnd" -> 20, "taperType" -> 21, "correlatedFlag" -> 22,
    "binaryGainRecovery" -> 23, "amplitudeRecovery" -> 24, "measurementSystem" -> 25,
    "impulsePolarity" -> 26, "vibratoryPolarity" -> 27, "segyVersion" -> 28,
    "fixedLengthFlag" -> 29, "extTextHeadersNum" -> 30
|>;


$traceHeaderKeys = <|
    "tracl" -> 1, "tracr" -> 2, "fldr" -> 3, "tracf" -> 4, "ep" -> 5,
    "cdp" -> 6, "cdpt" -> 7, "trid" -> 8, "nvs" -> 9, "nhs" -> 10,
    "duse" -> 11, "offset" -> 12, "gelev" -> 13, "selev" -> 14, "sdepth" -> 15,
    "gdel" -> 16, "sdel" -> 17, "swdep" -> 18, "gwdep" -> 19, "scalel" -> 20,
    "scalco" -> 21, "sx" -> 22, "sy" -> 23, "gx" -> 24, "gy" -> 25,
    "counit" -> 26, "wevel" -> 27, "swevel" -> 28, "sut" -> 29, "gut" -> 30,
    "sstat" -> 31, "gstat" -> 32, "tstat" -> 33, "laga" -> 34, "lagb" -> 35,
    "delrt" -> 36, "muts" -> 37, "mute" -> 38, "ns" -> 39, "dt" -> 40,
    "gain" -> 41, "igain" -> 42, "gaing" -> 43, "corr" -> 44, "sfs" -> 45,
    "sfe" -> 46, "slen" -> 47, "styp" -> 48, "stas" -> 49, "stae" -> 50,
    "tatyp" -> 51, "afilf" -> 52, "afils" -> 53, "nofilf" -> 54, "nofils" -> 55,
    "lcf" -> 56, "hcf" -> 57, "lcs" -> 58, "hcs" -> 59, "year" -> 60,
    "day" -> 61, "hour" -> 62, "minute" -> 63, "sec" -> 64, "tny" -> 65,
    "twt" -> 66, "geono" -> 67, "grnors" -> 68, "grnofr" -> 69, "grnols" -> 70,
    "gaps" -> 71, "otrav" -> 72, "cdpx" -> 73, "cdpy" -> 74, "iline" -> 75,
    "xline" -> 76, "shpoint" -> 77, "shpscal" -> 78, "tvalunit" -> 79, "transc" -> 80
|>;


getTraceData[byteArray_ByteArray, 1] :=
IBM32ByteArrayToReal[byteArray];


readSegyTextHeaderByteArray::usage =
"readSegyTextHeaderByteArray[file]";


readSegyTextHeaderByteArray =
LibraryFunctionLoad[$GeologyIOLibrary, "readSegyTextHeaderByteArray", {Integer}, LibraryDataType[ByteArray]];


readSegyBinaryHeaderByteArray::usage =
"readSegyBinaryHeaderByteArray[file]";


readSegyBinaryHeaderByteArray =
LibraryFunctionLoad[$GeologyIOLibrary, "readSegyBinaryHeaderByteArray", {Integer}, LibraryDataType[ByteArray]];


readSegyTraceHeaderByteArray::usage =
"readSegyTraceHeaderByteArray[file, traceNumber, traceByteCount]";


readSegyTraceHeaderByteArray =
LibraryFunctionLoad[$GeologyIOLibrary, "readSegyTraceHeaderByteArray", {Integer, Integer, Integer}, LibraryDataType[ByteArray]];


getSegyTraceHeaders::usage =
"getSegyTraceHeaders[file, indexes, count, traceSize]";


getSegyTraceHeaders =
LibraryFunctionLoad[$GeologyIOLibrary, "getSegyTraceHeaders", {Integer, {_Integer, 1}, Integer, Integer}, {Integer, 2}];


readSegyTraceData::usage =
"readSegyTraceData[file, traceNumber, traceSize]";


readSegyTraceData =
LibraryFunctionLoad[$GeologyIOLibrary, "readSegyTraceData", {Integer, Integer, Integer}, LibraryDataType[ByteArray]];


getSegyTracesData::usage =
"getSegyTracesData[file, traceNumbers, traceStartPositions, tracesCount, samplesCount, fullTraceSize, samplesFormatCode]";


getSegyTracesData =
LibraryFunctionLoad[$GeologyIOLibrary, "getSegyTracesData", {Integer, {_Integer, 1}, {_Integer, 1}, Integer, Integer, Integer, Integer}, LibraryDataType[NumericArray, "Real64", 2]];


byteArrayToSegyBinaryHeader::usage =
"byteArrayToSegyBinaryHeader[byteArray]";


byteArrayToSegyBinaryHeader =
LibraryFunctionLoad[$GeologyIOLibrary, "byteArrayToSegyBinaryHeader", {{LibraryDataType[ByteArray], "Shared"}}, {Integer, 1}];


byteArrayToSegyTraceHeader::usage =
"byteArrayToSegyTraceHeader[byteArray]";


byteArrayToSegyTraceHeader =
LibraryFunctionLoad[$GeologyIOLibrary, "byteArrayToSegyTraceHeader", {{LibraryDataType[ByteArray], "Shared"}}, {Integer, 1}];


End[(*`Private`*)];


EndPackage[(*KirillBelov`GeologyIO`SEGY`*)];