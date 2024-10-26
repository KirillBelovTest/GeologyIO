(* :Package: *)

BeginPackage["KirillBelov`GeologyIO`SEGY`", {
	"KirillBelov`GeologyIO`Numbers`"
}]; 


(*Names*)


SEGYImport::usage = 
"SEGYImport[file] import seg-y file as SEGYData."; 


SEGYExport::usage = 
"SEGYExport[file, segy] export segy to target file."; 


SEGYData::usage = 
"SEGYData[data] seg-y files representation."; 


Begin["`Private`"]; 


(*Public*)


SEGYImport[file_String] := 
Module[{
    stream = OpenRead[file, BinaryFormat -> True], 
    textHeader, 
    binaryHeader, 
    traceHeaders, 
    traces, 

    numberDataTraces, 
    numberOfSamplesForReel, 
    samplesFormatCode
}, 
    textHeader = getTextHeader[stream]; 
    binaryHeader = getBinaryHeader[stream]; 

    numberDataTraces = binaryHeader["NumberDataTraces"]; 
    numberOfSamplesForReel = binaryHeader["NumberOfSamplesForReel"]; 
    samplesFormatCode = binaryHeader["SamplesFormatCode"]; 

    traceHeaders = ConstantArray[<||>, numberDataTraces]; 
    traces = ConstantArray[{}, numberDataTraces]; 

    With[{
        getTraceHeader = getTraceHeaderFunc[samplesFormatCode, numberOfSamplesForReel], 
        getTrace = getTraceFunc[samplesFormatCode, numberOfSamplesForReel]
    }, 
        Table[
            traceHeaders[[i]] = getTraceHeader[stream, i];
            traces[[i]] = getTrace[stream, i]; , 
            {i, 1, numberDataTraces}
        ]; 
    ]; 

    (*Return*)
    SEGYData[
        "TextHeader" -> textHeader, 
        "BinaryHeader" -> binaryHeader, 
        "TraceHeaders" -> traceheaders, 
        "Traces" -> traces
    ]
]; 


(*Internal*)


getTextHeader[stream_] := 
With[{byteArray = ReadByteArray[stream, 3200]}, 
    EBCDICToString[byteArray]
]; 


End[(*`Private`*)]; 


EndPackage[(*KirillBelov`GeologyIO`SEGY`*)]; 