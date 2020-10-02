(* ::Package:: *)

(* ::Title:: *)
(*GeologyIO*)


(* ::Chapter:: *)
(*Prolog*)


(* ::Section::Closed:: *)
(*Info*)


(*Autor: Kirill Belov*)


(* ::Section::Closed:: *)
(*Begin package*)


BeginPackage["GeologyIO`"]


(* ::Section::Closed:: *)
(*Clear*)


Unprotect["`*"]
ClearAll["`*"]


(* ::Section::Closed:: *)
(*Names*)


GeologySeismicData


GeologyWellData


GeologyInterpretationData


SEGYImport::usage = 
"SEGYImport[file.segy]"


SEGYExport::usage = 
"SEGYExport[file.segy, data]"


SEGYRead::usage = 
"SEGYRead[unloaded, part]"


SEGYWrite::usage = 
"SEGYWrite[file.segy, data]"


SEGDImport


SEGDExport


SEGBImport


SEGBExport


LASImport


LASExport


IRAPImport


IRAPExport


ZMAPImport


ZMAPExport


CharizmaImport


CharizmaExport


(* ::Section::Closed:: *)
(*Begin private*)


Begin["`Private`"]


(* ::Chapter:: *)
(*SEG*)


(* ::Section::Closed:: *)
(*EBCDIC*)


$EBCDIC = {
	{0, "" (*NUL*)}, {1, "SOH"}, {2, "STX"}, {3, "ETX"}, {4, "SEL"}, {5, "\t"(*HT*)}, {6, "RNL"}, {7, "DEL"}, 
	{8, "GE"}, {9, "SPS"}, {10, "RPT"}, {11, "VT"}, {12, "FF"}, {13, "\r"(*CR*)}, {14, "SO"}, {15, "SI"}, 
	{16, "DLE"}, {17, "DC1"}, {18, "DC2"}, {19, "DC3"}, {20, "RES/ENP"}, {21, "\n"(*NL*)}, {22, "BS"}, {23, "POC"}, 
	{24, "CAN"}, {25, "EM"}, {26, "UBS"}, {27, "CU1"}, {28, "IFS"}, {29, "IGS"}, {30, "IRS"}, {31, "IUS/ITB"}, 
	{32, "DS"}, {33, "SOS"}, {34, "FS"}, {35, "WUS"}, {36, "BYP/INP"}, {37, "\n"(*LF*)}, {38, "ETB"}, {39, "ESC"}, 
	{40, "SA"}, {41, ""}, {42, "SM/SW"}, {43, "CSP"}, {44, "MFA"}, {45, "ENQ"}, {46, "ACK"}, {47, "BEL"}, 
	{48, ""}, {49, ""}, {50, "SYN"}, {51, "IR"}, {52, "PP"}, {53, "TRN"}, {54, "NBS"}, {55, "EOT"}, 
	{56, "SBS"}, {57, "\t" (*IT*)}, {58, "RFF"}, {59, "CU3"}, {60, "DC4"}, {61, "NAK"}, {62, ""}, {63, "SUB"}, 
	{64, " " (*SP*)}, {65, ""}, {66, ""}, {67, ""}, {68, ""}, {69, ""}, {70, ""}, {71, ""}, 
	{72, ""}, {73, ""}, {74, "\[Cent]"}, {75, "."}, {76, "<"}, {77, "("}, {78, "+"}, {79, "|"}, 
	{80, "&"}, {81, ""}, {82, ""}, {83, ""}, {84, ""}, {85, ""}, {86, ""}, {87, ""}, 
	{88, ""}, {89, ""}, {90, "!"}, {91, "$"}, {92, "*"}, {93, ")"}, {94, ";"}, {95, "\[Not]"}, 
	{96, "-"}, {97, "/"}, {98, ""}, {99, ""}, {100, ""}, {101, ""}, {102, ""}, {103, ""}, 
	{104, ""}, {105, ""}, {106, "|"}, {107, ","}, {108, "%"}, {109, "_"}, {110, ">"}, {111, "?"}, 
	{112, ""}, {113, ""}, {114, ""}, {115, ""}, {116, ""}, {117, ""}, {118, ""}, {119, ""}, 
	{120, ""}, {121, "\:02cb"}, {122, ":"}, {123, "#"}, {124, "@"}, {125, "'"}, {126, "="}, {127, "\""}, 
	{128, ""}, {129, "a"}, {130, "b"}, {131, "c"}, {132, "d"}, {133, "e"}, {134, "f"}, {135, "g"}, 
	{136, "h"}, {137, "i"}, {138, ""}, {139, ""}, {140, ""}, {141, ""}, {142, ""}, {143, ""}, 
	{144, ""}, {145, "j"}, {146, "k"}, {147, "l"}, {148, "m"}, {149, "n"}, {150, "o"}, {151, "p"}, 
	{152, "q"}, {153, "r"}, {154, ""}, {155, ""}, {156, ""}, {157, ""}, {158, ""}, {159, ""}, 
	{160, ""}, {161, "~"}, {162, "s"}, {163, "t"}, {164, "u"}, {165, "v"}, {166, "w"}, {167, "x"}, 
	{168, "y"}, {169, "z"}, {170, ""}, {171, ""}, {172, ""}, {173, ""}, {174, ""}, {175, ""}, 
	{176, ""}, {177, ""}, {178, ""}, {179, ""}, {180, ""}, {181, ""}, {182, ""}, {183, ""}, 
	{184, ""}, {185, ""}, {186, ""}, {187, ""}, {188, ""}, {189, ""}, {190, ""}, {191, ""}, 
	{192, "{"}, {193, "A"}, {194, "B"}, {195, "C"}, {196, "D"}, {197, "E"}, {198, "F"}, {199, "G"}, 
	{200, "H"}, {201, "I"}, {202, ""}, {203, ""}, {204, ""}, {205, ""}, {206, ""}, {207, ""}, 
	{208, "}"}, {209, "J"}, {210, "K"}, {211, "L"}, {212, "M"}, {213, "N"}, {214, "O"}, {215, "P"}, 
	{216, "Q"}, {217, "R"}, {218, ""}, {219, ""}, {220, ""}, {221, ""}, {222, ""}, {223, ""}, 
	{224, "\\"}, {225, ""}, {226, "S"}, {227, "T"}, {228, "U"}, {229, "V"}, {230, "W"}, {231, "X"}, 
	{232, "Y"}, {233, "Z"}, {234, ""}, {235, ""}, {236, ""}, {237, ""}, {238, ""}, {239, ""}, 
	{240, "0"}, {241, "1"}, {242, "2"}, {243, "3"}, {244, "4"}, {245, "5"}, {246, "6"}, {247, "7"}, 
	{248, "8"}, {249, "9"}, {250, "|"}, {251, ""}, {252, ""}, {253, ""}, {254, ""}, {255, "EO"}
}


$EBCDICByteToString := $EBCDICByteString = 
	Dispatch[Map[Apply[Rule]] @ $EBCDIC]


EBCDICByteToString[bytes: {__Integer}] := 
	StringJoin[bytes /. $EBCDICByteToString]


EBCDICByteToString[bytes_ByteArray] := 
	EBCDICByteToString[Normal[bytes]]


$EBCDICStringToByte := $EBCDICStringToByte = 
	Dispatch[Map[Apply[Rule]] @ Map[Reverse] @ $EBCDIC]


EBCDICStringToByte[string__Integer] := 
	ByteArray[Characters[string] /. $EBCDICStringToByte]


(* ::Section:: *)
(*IBM Float 32*)


IBMFloat32ToNumber[target: "MVM" | "C"] := IBMFloat32ToNumber[target] = 
	Compile[{{bytes, _Integer, 1}}, 
		
		Block[{sign, exp, fract}, 
			
			Table[
				(* sign of the number *) 
				sign = (-1.0)^UnitStep[bytes[[i4th]] - 127.5]; 
				
				(* 16th exp *)
				exp = 16.0^(BitAnd[127, bytes[[i4th]]] - 64); 
				
				(* fraction part *) 
				fract = (
					bytes[[i4th + 1]] * 256.0^2 + 
					bytes[[i4th + 2]] * 256.0 + 
					bytes[[i4th + 3]]
				) / (256.0^3); 
				
				If[fract == 0.0, fract = 1.0]; 
				If[bytes[[i4th ;; i4th + 3]] == {0, 0, 0, 0}, exp = 0.0];
				
				sign * exp * fract, 
				
				{i4th, 1, Length[bytes], 4}
			]
		], 
		
		CompilationTarget -> target, 
		RuntimeAttributes -> {Listable}, 
		Parallelization -> True
	]


(* ::Section:: *)
(*SEGYImport*)


(* ::Text:: *)
(*SEGY TextHeader*)


SEGYTextHeaderToString::argx = 
"Expected ByteArray of size 3200"


SEGYTextHeaderToString[bytes_ByteArray] /; Length[bytes] === 3200 := 
	EBCDICByteToString[bytes]


SEGYTextHeaderToString[___] := 
	Message[SEGYTextHeaderToString::argx]


(* ::Text:: *)
(*SEGY BinaryHeader*)


$SEGYBinaryHeaderSpecification = {
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
}


SEGYBinaryHeaderToAssociation[bytes_ByteArray] /; Length[bytes] === 400 := 
	Association @ 
	Map[#Name -> First @ ImportString[ExportString[Reverse[bytes[[#Position]]], "Binary"], #Type]&] @ 
	$SEGYBinaryHeaderSpecification


(* ::Text:: *)
(*SEGY Traces*)


$SEGYTraceHeaderTypes = <|
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
|>


SEGYTraceHeaderToAssociation[bytes_ByteArray] := 
	Normal[bytes]


(*
	1 = 4-byte IBM floating-point
	2 = 4-byte, two's complement integer
	3 = 2-byte, two's complement integer
	4 = 4-byte fixed-point with gain (obsolete)
	5 = 4-byte IEEE floating-point
	6 = 8-byte IEEE floating-point
	7 = 3-byte two's complement integer
	8 = 1-byte, two's complement integer
	9 = 8-byte, two's complement integer
	10 = 4-byte, unsigned integer
	11 = 2-byte, unsigned integer
	12 = 8-byte, unsigned integer
	15 = 3-byte, unsigned integer
	16 = 1-byte, unsigned integer
*)


$NumberTypes = 
	"Integer8" | "Integer16" | "Integer32" | "Integer64" | 
	"UnsignedInteger8" | "UnsignedInteger16" | "UnsignedInteger32" | "UnsignedInteger64" | 
	"Real32" | "Real64"


FromByteArray[type: $NumberTypes] := 
	ImportByteArray[#, type]&


$SEGYSamplesFormatCodes = <|
	1  -> <|"SampleByteCount" -> 4, "ToNumber" -> IBMFloat32ToNumber["MVM"], 	"ToBytes" -> IBMFloat32ToBytes["MVM"]|>, 
	2  -> <|"SampleByteCount" -> 4, "ToNumber" -> FromByteArray["Integer32"], 	"ToBytes" -> IBMFloat32ToBytes|>, 
	3  -> <|"SampleByteCount" -> 2, "ToNumber" -> FromByteArray["Integer16"], 	"ToBytes" -> IBMFloat32ToBytes|>, 
	4  -> <|"SampleByteCount" -> 4, "ToNumber" -> IBMFloat32ToNumber["MVM"], 	"ToBytes" -> IBMFloat32ToBytes["MVM"]|>, 
	5  -> <|"SampleByteCount" -> 4, "ToNumber" -> FromByteArray["Real32"], 		"ToBytes" -> IBMFloat32ToBytes|>, 
	6  -> <|"SampleByteCount" -> 8, "ToNumber" -> FromByteArray["Real64"], 		"ToBytes" -> IBMFloat32ToBytes|>, 
	7  -> <|"SampleByteCount" -> 3, "ToNumber" -> IBMFloat32ToNumber, "ToBytes" -> IBMFloat32ToBytes|>, 
	8  -> <|"SampleByteCount" -> 1, "ToNumber" -> IBMFloat32ToNumber, "ToBytes" -> IBMFloat32ToBytes|>, 
	9  -> <|"SampleByteCount" -> 8, "ToNumber" -> IBMFloat32ToNumber, "ToBytes" -> IBMFloat32ToBytes|>, 
	10 -> <|"SampleByteCount" -> 4, "ToNumber" -> IBMFloat32ToNumber, "ToBytes" -> IBMFloat32ToBytes|>, 
	11 -> <|"SampleByteCount" -> 2, "ToNumber" -> IBMFloat32ToNumber, "ToBytes" -> IBMFloat32ToBytes|>, 
	12 -> <|"SampleByteCount" -> 8, "ToNumber" -> IBMFloat32ToNumber, "ToBytes" -> IBMFloat32ToBytes|>, 
	13 -> <|"SampleByteCount" -> 0, "ToNumber" -> IBMFloat32ToNumber, "ToBytes" -> IBMFloat32ToBytes|>, 
	14 -> <|"SampleByteCount" -> 0, "ToNumber" -> IBMFloat32ToNumber, "ToBytes" -> IBMFloat32ToBytes|>, 
	15 -> <|"SampleByteCount" -> 3, "ToNumber" -> IBMFloat32ToNumber, "ToBytes" -> IBMFloat32ToBytes|>, 
	16 -> <|"SampleByteCount" -> 1, "ToNumber" -> IBMFloat32ToNumber, "ToBytes" -> IBMFloat32ToBytes|>
|>


SEGYTraceDataToList[bytes_ByteArray, samplesFormatCode_Integer] := 
	$SEGYSamplesFormatCodes[[Key[samplesFormatCode], "ToNumber"]][Normal[bytes]]


(* ::Text:: *)
(*SEGY Import*)


SEGYImport[file: (_String | _File) ? FileExistsQ] /; 
StringMatchQ[FileExtension[file], {"sgy", "segy"}, IgnoreCase -> True] && 
FileByteCount[file] > 3200 + 400 + 240 := 
	Block[
		{
			fileByteCount, stream, textHeader, binaryHeader, traces, header, data, info, 
			numberOfSamplesForReel, samplesFormatCode, traceByteCount, sampleByteCount
		}, 
		
		fileByteCount = FileByteCount[file]; 
		
		stream = OpenRead[file, BinaryFormat -> True]; 
		
		textHeader = SEGYTextHeaderToString[ReadByteArray[stream, 3200]]; 
		
		SetStreamPosition[stream, 3200]; 
		
		binaryHeader = SEGYBinaryHeaderToAssociation[ReadByteArray[stream, 400]]; 
		
		samplesFormatCode = binaryHeader["SamplesFormatCode"]; 
		
		sampleByteCount = $SEGYSamplesFormatCodes[[Key[samplesFormatCode], "SampleByteCount"]]; 
		
		numberOfSamplesForReel = binaryHeader[["NumberOfSamplesForReel"]]; 
		
		traceByteCount = sampleByteCount * numberOfSamplesForReel; 
		
		traces = Table[
			SetStreamPosition[stream, position]; 
			header = SEGYTraceHeaderToAssociation[ReadByteArray[stream, 240]]; 
			
			SetStreamPosition[stream, position + 240]; 
			data = SEGYTraceDataToList[ReadByteArray[stream, traceByteCount], samplesFormatCode]; 
			
			<|
				"Header" -> header, 
				"Data" -> data
			|>, 
			
			{position, 3600, fileByteCount - 240 - traceByteCount, 240 + traceByteCount}
		];
		
		Close[stream]; 
		
		GeologySeismicData[<|
			"TextHeader" -> textHeader, 
			"BinaryHeader" -> binaryHeader, 
			"Traces" -> traces, 
			"Info" -> info
		|>]
	]


(* ::Section:: *)
(*SEGYExport*)


SEGYExport[file: _String | _File, segy_GeologySeismiData] /; 
StringMatchQ[FileExtension[file], {"sgy", "segy"}, IgnoreCase -> True] := 
	Block[{stream}, 
		
		stream = OpenWrite[file, BinaryFormat -> True]; 
		
		BinaryWrite[stream, TextHeaderToBinary[segy["TextHeader"]]]; 
		
		BinaryWrite[stream, BinaryHeaderToBinary[segy["TextHeader"]]]; 
		
		Return[file]
	]


(* ::Chapter:: *)
(*LAS*)


(* ::Chapter:: *)
(*SurferGrid*)


(* ::Chapter:: *)
(*CPSGrid*)


(* ::Chapter:: *)
(*IRAPGrid*)


(* ::Chapter:: *)
(*ZMAPGrid*)


(* ::Chapter:: *)
(*Charisma*)


(* ::Chapter:: *)
(*Epilog*)


(* ::Section::Closed:: *)
(*End private*)


End[] (*`Private`*)


(* ::Section::Closed:: *)
(*Protection*)


SetAttributes[Evaluate[Names["`*"]], {ReadProtected, Protected}]


(* ::Section::Closed:: *)
(*End package*)


EndPackage[] (*GeologyIO`*)
