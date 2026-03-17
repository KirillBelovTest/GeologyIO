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
    stream = GeologyIOOpenFile[AbsoluteFileName[file]],
    textHeaderByteArray,
    textHeader,
    binaryHeaderByteArray,
    binaryHeader,
    numberOfSamplesForReel,
    samplesFormatCode,
    traceByteCount,
    numberDataTraces,
    traces,
    metadata
},
    textHeaderByteArray = readSegyTextHeader[stream];
    textHeader = EBCDICToString[textHeaderByteArray];

    binaryHeaderByteArray = readSegyBinaryHeader[stream];
    binaryHeader = getBinaryHeader[binaryHeaderByteArray];

    numberOfSamplesForReel = binaryHeader["NumberOfSamplesForReel"];
    samplesFormatCode = binaryHeader["SamplesFormatCode"];
    traceByteCount = numberOfSamplesForReel * $sampleSize[samplesFormatCode] + 240;

    numberDataTraces =
    If[IntegerQ[OptionValue["NumberDataTraces"]] && Positive[OptionValue["NumberDataTraces"]],
        OptionValue["NumberDataTraces"],
    (*Else*)
        (FileByteCount[file] - 3600) / traceByteCount
    ];

    metadata = <|
        "File" -> AbsoluteFileName[file],
        "Stream" -> stream,
        "NTraces" -> numberDataTraces
    |>;

    traces = Table[
        traceHeaderByteArray = readSegyTraceHeader[stream, i, traceByteCount];
        traceDataByteArray = readSegyTraceData[stream, i, traceByteCount];

        <|
            "Header" -> byteArrayToSegyTraceHeader[traceHeaderByteArray],
            "Data" -> getTraceData[traceDataByteArray, samplesFormatCode]
        |>,
        {i, 1, numberDataTraces}
    ];

    (*Return*)
    SEGYData[<|
        "Metadata" -> metadata,
        "TextHeader" -> textHeader,
        "BinaryHeader" -> binaryHeader,
        "Traces" -> traces
    |>]
];


(*Internal*)


getBinaryHeader[binaryHeaderByteArray_ByteArray] :=
    Association @
    Map[#Name -> ImportByteArray[binaryHeaderByteArray[[#Position]], #Type, ByteOrdering -> 1][[1]]&] @
    $binaryHeaderSpec;


$sampleSize = <|
    1 -> 4
|>;


$binaryHeaderSpec = {
    <|"Name" -> "JobID",                        "Position" -> 01 ;; 04, "Type" -> "UnsignedInteger32"|>,
    <|"Name" -> "LineNumber",                   "Position" -> 05 ;; 08, "Type" -> "UnsignedInteger32"|>,
    <|"Name" -> "ReelNumber",                   "Position" -> 09 ;; 12, "Type" -> "UnsignedInteger32"|>,
    <|"Name" -> "NumberDataTraces",             "Position" -> 13 ;; 14, "Type" -> "UnsignedInteger16"|>,
    <|"Name" -> "NumberAuxTraces",              "Position" -> 15 ;; 16, "Type" -> "UnsignedInteger16"|>,
    <|"Name" -> "IntervalReelRecord",           "Position" -> 17 ;; 18, "Type" -> "UnsignedInteger16"|>,
    <|"Name" -> "IntervalFieldRecord",          "Position" -> 19 ;; 20, "Type" -> "UnsignedInteger16"|>,
    <|"Name" -> "NumberOfSamplesForReel",       "Position" -> 21 ;; 22, "Type" -> "UnsignedInteger16"|>,
    <|"Name" -> "NumberOfSamplesForField",      "Position" -> 23 ;; 24, "Type" -> "UnsignedInteger16"|>,
    <|"Name" -> "SamplesFormatCode",            "Position" -> 25 ;; 26, "Type" -> "UnsignedInteger16"|>,
    <|"Name" -> "CDPFold",                      "Position" -> 27 ;; 28, "Type" -> "UnsignedInteger16"|>,
    <|"Name" -> "TraceSortingCode",             "Position" -> 29 ;; 30, "Type" -> "Integer16"        |>,
    <|"Name" -> "VerticalSumCode",              "Position" -> 31 ;; 32, "Type" -> "UnsignedInteger16"|>,
    <|"Name" -> "SweepFrequencyAtStart",        "Position" -> 33 ;; 34, "Type" -> "UnsignedInteger16"|>,
    <|"Name" -> "SweepFrequencyAtEnd",          "Position" -> 35 ;; 36, "Type" -> "UnsignedInteger16"|>,
    <|"Name" -> "SweepLength",                  "Position" -> 37 ;; 38, "Type" -> "UnsignedInteger16"|>,
    <|"Name" -> "SweepTypeCode",                "Position" -> 39 ;; 40, "Type" -> "UnsignedInteger16"|>,
    <|"Name" -> "TraceNumberOfSweepChannel",    "Position" -> 41 ;; 42, "Type" -> "UnsignedInteger16"|>,
    <|"Name" -> "SweepTraceTaperLengthAtStart", "Position" -> 43 ;; 44, "Type" -> "UnsignedInteger16"|>,
    <|"Name" -> "SweepTraceTaperLength",        "Position" -> 45 ;; 46, "Type" -> "UnsignedInteger16"|>,
    <|"Name" -> "TaperType",                    "Position" -> 47 ;; 48, "Type" -> "UnsignedInteger16"|>,
    <|"Name" -> "CorrelatedDataTraces",         "Position" -> 49 ;; 50, "Type" -> "UnsignedInteger16"|>,
    <|"Name" -> "BinaryGainRecovered",          "Position" -> 51 ;; 52, "Type" -> "UnsignedInteger16"|>,
    <|"Name" -> "AmplitudeRecoveryMethod",      "Position" -> 53 ;; 54, "Type" -> "UnsignedInteger16"|>,
    <|"Name" -> "MeasurementSystem",            "Position" -> 55 ;; 56, "Type" -> "UnsignedInteger16"|>,
    <|"Name" -> "ImpulseSignal",                "Position" -> 57 ;; 58, "Type" -> "UnsignedInteger16"|>,
    <|"Name" -> "VibratoryPolarityCode",        "Position" -> 59 ;; 60, "Type" -> "UnsignedInteger16"|>
};


getTraceData[byteArray_ByteArray, samplesFormatCode_Integer] :=
Switch[samplesFormatCode,
    1, IBM32ByteArrayToReal[byteArray]
];


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


readSegyTraceData::usage =
"readSegyTraceData[file, traceNumber, traceSize]";


readSegyTraceData =
LibraryFunctionLoad[$library, "readSegyTraceData", {Integer, Integer, Integer}, LibraryDataType[ByteArray]];


byteArrayToSegyTraceHeader::usage =
"byteArrayToSegyTraceHeader[byteArray]";


byteArrayToSegyTraceHeader =
LibraryFunctionLoad[$library, "byteArrayToSegyTraceHeader", {{LibraryDataType[ByteArray], "Shared"}}, {Integer, 1}];


End[(*`Private`*)];


EndPackage[(*KirillBelov`GeologyIO`SEGY`*)];