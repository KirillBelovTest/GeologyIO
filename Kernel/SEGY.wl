(* :Package: *)

BeginPackage["WLJS`GeologyIO`SEGY`", {
    "WLJS`GeologyIO`Numbers`",
    "WLJS`GeologyIO`EBCDIC`",
    "WLJS`GeologyIO`IO`",
    "LibraryLink`"
}];


SEGYImport::usage =
"SEGYImport[file] import seg-y file as SEGYData.";


SEGYExport::usage =
"SEGYExport[file, segy] export segy to target file.";


SEGYData::usage =
"SEGYData[data] seg-y in-memory representation.";


SEGYOpen::usage =
"SEGYOpen[data] open seg-y file for the editing.";


SEGYClose::usage =
"SEGYClose[data] close editable seg-y file.";


SEGYFile::usage =
"SEGYFile[data] editable seg-y file representation.";


Begin["`Private`"];


(*Public*)


Options[SEGYImport] = {
    "NumberDataTraces" -> Automatic
};


SEGYImport[file: _String | _File, OptionsPattern[]] /;
FileExistsQ[file] :=
Module[{
    fileName = AbsoluteFileName[file],
    stream,
    textHeaderByteArray,
    textHeader,
    binaryHeaderByteArray,
    binaryHeader,
    numberOfSamplesForReel,
    samplesFormatCode,
    traceByteCount,
    numberDataTraces,
    metadata,
    traceHeaders,
    traceData
},
    If[KeyExistsQ[$streams, fileName],
        stream = $streams[fileName],
        stream = GeologyIOOpenFile[fileName]
    ];

    textHeaderByteArray = readSegyTextHeader[stream];
    textHeader = EBCDICToString[textHeaderByteArray];

    binaryHeaderByteArray = readSegyBinaryHeader[stream];
    binaryHeader = byteArrayToSegyBinaryHeader[binaryHeaderByteArray];

    numberOfSamplesForReel = binaryHeader[[8]];
    samplesFormatCode = binaryHeader[[10]];
    traceByteCount = numberOfSamplesForReel * $sampleSize[samplesFormatCode] + 240;

    numberDataTraces = (FileByteCount[file] - 3600) / traceByteCount;

    metadata = <|
        "File" -> AbsoluteFileName[file],
        "Stream" -> stream,
        "NTraces" -> numberDataTraces,
        "NSamples" -> numberOfSamplesForReel,
        "TraceByteCount" -> traceByteCount
    |>;

    traceHeaders = getSegyTraceHeaders[stream, Range[numberDataTraces], numberDataTraces, traceByteCount];
    traceData = getSegyTracesData[stream, Range[numberDataTraces], numberDataTraces, traceByteCount, 0, numberOfSamplesForReel];

    (*Return*)
    SEGYData[<|
        "Metadata" -> metadata,
        "TextHeader" -> textHeader,
        "BinaryHeader" -> binaryHeader,
        "TraceHeaders" -> traceHeaders,
        "TraceData" -> traceData
    |>]
];


SEGYData[assoc_Association][part__] :=
assoc[[part]];


(*Internal*)


If[!ValueQ[$streams], $streams = <||>];


$sampleSize = <|
    1 -> 4
|>;


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


$directory =
DirectoryName[$InputFileName, 2];


$libraryLinkVersion =
Which[
    $VersionNumber >= 14.1,
        LibraryVersionInformation[FindLibrary["demo"]]["WolframLibraryVersion"],
    $VersionNumber >= 13.1,
        7,
    $VersionNumber >= 12.1,
        6,
    $VersionNumber >= 12.0,
        5,
    $VersionNumber >= 11.2,
        4,
    $VersionNumber >= 10.0,
        3,
    $VersionNumber >= 9.0,
        2,
    True,
        1
];


$libraryDirectory =
FileNameJoin[{
    $directory,
    "LibraryResources",
    $SystemID <> "-v" <> ToString[$libraryLinkVersion]
}];


$library =
Block[{$LibraryPath = $libraryDirectory}, FindLibrary["geologyio"]];


readSegyTextHeader::usage =
"readSegyTextHeader[file]";


readSegyTextHeader =
LibraryFunctionLoad[$library, "readSegyTextHeader", {Integer}, LibraryDataType[ByteArray]];


readSegyBinaryHeader::usage =
"readSegyBinaryHeader[file]";


readSegyBinaryHeader =
LibraryFunctionLoad[$library, "readSegyBinaryHeader", {Integer}, LibraryDataType[ByteArray]];


readSegyTraceHeader::usage =
"readSegyTraceHeader[file, traceNumber, traceSize]";


readSegyTraceHeader =
LibraryFunctionLoad[$library, "readSegyTraceHeader", {Integer, Integer, Integer}, LibraryDataType[ByteArray]];


getSegyTraceHeaders::usage =
"getSegyTraceHeaders[file, indexes, count, traceSize]";


getSegyTraceHeaders =
LibraryFunctionLoad[$library, "getSegyTraceHeaders", {Integer, {_Integer, 1}, Integer, Integer}, {Integer, 2}];


readSegyTraceData::usage =
"readSegyTraceData[file, traceNumber, traceSize]";


readSegyTraceData =
LibraryFunctionLoad[$library, "readSegyTraceData", {Integer, Integer, Integer}, LibraryDataType[ByteArray]];


getSegyTracesData::usage =
"getSegyTracesData[file, indexes, count, traceSize, fromSample, samplesCount]";


getSegyTracesData =
LibraryFunctionLoad[$library, "getSegyTracesData", {Integer, {_Integer, 1}, Integer, Integer, Integer, Integer}, LibraryDataType[NumericArray, "Real64", 2]];


byteArrayToSegyBinaryHeader::usage =
"byteArrayToSegyBinaryHeader[byteArray]";


byteArrayToSegyBinaryHeader =
LibraryFunctionLoad[$library, "byteArrayToSegyBinaryHeader", {{LibraryDataType[ByteArray], "Shared"}}, {Integer, 1}];


byteArrayToSegyTraceHeader::usage =
"byteArrayToSegyTraceHeader[byteArray]";


byteArrayToSegyTraceHeader =
LibraryFunctionLoad[$library, "byteArrayToSegyTraceHeader", {{LibraryDataType[ByteArray], "Shared"}}, {Integer, 1}];


End[(*`Private`*)];


EndPackage[(*KirillBelov`GeologyIO`SEGY`*)];