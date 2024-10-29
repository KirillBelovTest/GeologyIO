(* :Package: *)

BeginPackage["KirillBelov`GeologyIO`SEGY`", {
    "KirillBelov`GeologyIO`EBCDIC`", 
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


Options[SEGYImport] = {
    "NumberDataTraces" -> Automatic
};


SEGYImport[file_String, OptionsPattern[]] := 
Module[{
    stream = OpenRead[file, BinaryFormat -> True], 
    textHeaderByteArray, 
    textHeader, 
    binaryHeaderByteArray, 
    binaryHeader, 
    traceHeaderByteArray, 
    traceHeaders, 
    traceByteArray, 
    traces, 

    numberDataTraces, 
    numberOfSamplesForReel, 
    samplesFormatCode, 
    tarceByteCount
}, 
    textHeaderByteArray = ReadByteArray[stream, 3200]; 
    textHeader = EBCDICToString[textHeaderByteArray]; 

    binaryHeaderByteArray = ReadByteArray[stream, 400]; 
    binaryHeader = 
        Association @ 
        Map[#Name -> ImportByteArray[binaryHeaderByteArray[[#Position]], #Type, ByteOrdering -> 1][[1]]&] @ 
        $binaryHeaderSpec;
    
    numberOfSamplesForReel = binaryHeader["NumberOfSamplesForReel"]; 
    samplesFormatCode = binaryHeader["SamplesFormatCode"]; 

    tarceByteCount = numberOfSamplesForReel * $sampleSize[samplesFormatCode]; 

    numberDataTraces = 
    If[IntegerQ[OptionValue["NumberDataTraces"]] && Positive[OptionValue["NumberDataTraces"]], 
        OptionValue["NumberDataTraces"], 
    (*Else*)
        (FileByteCount[file] - 3600) / (tarceByteCount + 240)
    ]; 

    traceHeaders = ConstantArray[<||>, numberDataTraces]; 
    traces = ConstantArray[{}, numberDataTraces]; 

    Table[
        traceHeaderByteArray = ReadByteArray[stream, 240]; 
        traceHeaders[[i]] = Association @ MapThread[#1 -> #2&, {Keys[$traceHeaderSpec], getTraceHeader[Normal[traceHeaderByteArray]]}]; 

        traceByteArray = ReadByteArray[stream, tarceByteCount]; 
        traces[[i]] = getTrace[traceByteArray, samplesFormatCode];, 

        {i, 1, numberDataTraces} 
    ]; 

    (*Return*)
    SEGYData[
        "TextHeader" -> textHeader, 
        "BinaryHeader" -> binaryHeader, 
        "TraceHeaders" -> traceHeaders, 
        "Traces" -> traces
    ]
]; 


(*Internal*)


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


$traceHeaderSpec = <|
	"tracl" -> {1,4,1}, "tracr"->{5,8,1}, "fldr"->{9,12,1}, "tracf"->{13,16,1}, "ep"->{17,20,1}, 
	"cdp" -> {21,24,1}, "cdpt"->{25,28,1}, "trid"->{29,30,-1}, "nvs"->{31,32,1}, "nhs"->{33,34,1}, 
	"duse" -> {35,36,1}, "offset"->{37,40,-1}, "gelev"->{41,44,-1}, "selev"->{45,48,-1}, "sdepth"->{49,52,-1}, 
	"gdel" -> {53,56,-1},"sdel"->{57,60,-1},"swdep"->{61,64,-1},"gwdep"->{65,68,-1},"scalel"->{69,70,-1}, 
	"scalco" -> {71,72,2},"sx"->{73,76,-1},"sy"->{77,80,-1},"gx"->{81,84,-1},"gy"->{85,88,-1}, 
	"counit" -> {89,90,1},"wevel"->{91,92,1},"swevel"->{93,94,1},"sut"->{95,96,-1},"gut"->{97,98,-1}, 
	"sstat" -> {99,100,-1}, "gstat"->{101,102,-1},"tstat"->{103,104,-1},"laga"->{105,106,-1},"lagb"->{107,108,-1}, 
	"delrt" -> {109,110,-1}, "muts"->{111,112,-1},"mute"->{113,114,-1},"ns"->{115,116,1},"dt"->{117,118,1}, 
	"gain" -> {119,120,1}, "igc"->{121,122,-1},"igi"->{123,124,-1},"corr"->{125,126,1},"sfs"->{127,128,1}, 
	"sfe" -> {129,130,1}, "slen"->{131,134,1},"styp"->{133,134,1},"stas"->{135,136,1},"stae"->{137,138,1}, 
	"tatyp" -> {139,140,1}, "afilf"->{141,142,1},"afils"->{143,144,1},"nofilf"->{145,146,1},"nofils"->{147,148,1}, 
	"lcf" -> {149,150,1}, "hcf"->{151,152,1},"lcs"->{153,154,1},"hcs"->{155,156,1},"year"->{157,158,1}, 
	"day" -> {159,160,1}, "hour"->{161,162,1},"minute"->{163,164,1},"sec"->{165,166,1},"timbas"->{167,168,1}, 
	"trwf" -> {169,170,1}, "grnors"->{171,172,1},"grnofr"->{173,174,1},"grnlof"->{175,176,1},"gaps"->{177,178,1}, 
	"ofrav" -> {179,180,1}, "cdpx"->{181,184,-1},"cdpy"->{185,188,-1},"iline"->{189,192,-1},"xline"->{193,196,-1}, 
	"spnum" -> {197,200,1}, "spscal"->{201,202,-1},"trunit"->{203,204,-1}
|>; 


getTraceHeader = 
With[{spec = Values[$traceHeaderSpec]}, 
	Compile[{
		{bytes, _Integer, 1}
	}, 
		Module[{blist = {0}, len = 0, beg = 0, end = 0, type = 0}, 
			Table[
				len = param[[2]] - param[[1]]; 
				beg = param[[1]]; 
				end = param[[2]]; 
				type = param[[3]]; 
				blist = bytes[[beg ;; end]]; 
				Round[Which[
					type == -1 && len == 1, 
						If[blist[[1]] > 127, 
							(BitAnd[127, blist[[1]]] * 256 + blist[[2]]) - 256^2/2, 
							(BitAnd[127, blist[[1]]] * 256 + blist[[2]])
						], 
						
					type == -1 && len == 3, 
						If[blist[[1]] > 127, 
							BitAnd[127, blist[[1]]] * 256^3 + 
								blist[[2]] * 256^2 + 
								blist[[3]] * 256^1 + 
								blist[[4]] - 256^4/2, 
							BitAnd[127, blist[[1]]] * 256^3 + 
								blist[[2]] * 256^2 + 
								blist[[3]] * 256^1 + 
								blist[[4]]
						], 
						
					type == 1 && len == 1, 
						blist[[1]] * 256 + 
						blist[[2]], 
						
					type == 1 && len == 3, 
						blist[[1]] * 256^3 + 
						blist[[2]] * 256^2 + 
						blist[[3]] * 256^1 + 
						blist[[4]], 
						
					type == 2 && len == 1, 
						BitAnd[127, blist[[1]]] - If[blist[[1]] > 127, 128, 0], 
							
					True, 
						Total[blist]
				]], 
				{param, spec}
			]
		]
	]
]; 


getTrace[byteArray_, samplesFormatCode_] := 
Switch[samplesFormatCode, 
    1, FromIBMFloat32[byteArray]
]; 


End[(*`Private`*)]; 


EndPackage[(*KirillBelov`GeologyIO`SEGY`*)]; 