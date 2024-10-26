(* ::Package:: *)

(* ::Title:: *)
(*GeologyIO*)


(* ::Section:: *)
(*Info*)


(* :Title: GeologyIO *)
(* :Context: GeologyIO` *)
(* :Version: 1.0.0 *)
(* :Keywords: Geology; SEGY; SEG-Y; LAS; IRAP; IrapGrid; ZMAP; ZMapGrid *)
(* :Authors: Anton Ekimenko; Kirill Belov *)
(* :Developer: Kirill Belov *)
(* :Email: KirillBelovTest@gmail.com *)
(* :Description: 
	
*)


(* ::Section:: *)
(*Begin package*)


BeginPackage["KirillBelov`GeologyIO`"]; 


(* ::Section:: *)
(*Names*)


(* ::Text:: *)
(*GeologyIO*)


$GeologyIODirectory::usage = 
"GeologyIO installation directory"; 


(* ::Text:: *)
(*SEGY*)


SEGYImport::usage = 
"SEGYImport[\"file.segy\"]
SEGYImport[\"http://url.segy\"]
SEGYImport[\"file.segy\", options]"; 


SEGYExport::usage = 
"SEGYExport[\"file.segy\", segydata]
SEGYExport[\"file.segy\", segydata, options]"


SEGYLoad::usage = 
"SEGYLoad[segydataunloaded], 
SEGYLoad[segydataunloaded, options]"


SEGYWrite::usage = 
"SEGYWrite[\"file.segy\", segydata]
SEGYWrite[\"file.segy\", trace]
SEGYWrite[\"file.segy\", traces]
SEGYWrite[\"file.segy\", {traceHeadersRules, traces}]
SEGYWrite[\"file.segy\", {\"TraceHeaders\" -> traceHeaders, \"Traces\" -> traces}]"


SEGYData::usage = 
"SEGYData[textHeader, binaryHeader, traceHeaders, traces]
SEGYData[array2D]
SEGYData[array2D, options]"


(* ::Text:: *)
(*LAS*)


LASImport::usage = 
"LASImport[\"file.las\", options]"


LASExport::usage = 
"LASExport[\"file.las\", lasdata, options]"


LASData::usage = 
"LASData[Version, Well, CurveInfo, CurveData]"


(* ::Text:: *)
(*ZMAP*)


ZMAPGridImport::usage = 
"ZMAPGridImport[\"path\"]"


ZMapGridExport::usage = 
"ZMapGridExport[\"path\", zmapgriddata]"


ZMapGridData::usage = 
"ZMapGridData[]"


(* ::Text:: *)
(*IRAP*)


IrapGridImport::usage = 
"IrapGridImport[\"path\"]"


IrapGridExport::usage = 
"IrapGridExport[\"path\", irapgriddata]"


IrapGridData::usage = 
"IrapGridData[]"


(* ::Section:: *)
(*Private*)


Begin["`Private`"]


(* ::Section:: *)
(*$GeologyIODirectory*)


$GeologyIODirectory = DirectoryName[$InputFileName]


(* ::Section:: *)
(*EBCDICEncoding*)


$EBCDICCharacters = {
	0 -> "" (*"NUL"*), 1 -> "SOH", 2 -> "STX", 3 -> "ETX", 4 -> "PF", 
	5 -> "	"(*"HT"*), 6 -> "LC", 7 -> "DEL", 10 -> "SMM", 11 -> "VT", 
	12 -> "FF", 13 -> "\r"(*"CR"*), 14 -> "SO", 15 -> "SI", 16 -> "DLE", 
	17 -> "DC1", 18 -> "DC2", 19 -> "TM", 20 -> "RES", 21 -> "NL", 
	22 -> "BS", 23 -> "IL", 24 -> "CAN", 25 -> "EM", 26 -> "CC", 
	27 -> "CU1", 28 -> "IFS", 29 -> "IGS", 30 -> "IRS", 31 -> "IUS", 
	32 -> "DS", 33 -> "SOS", 34 -> "FS", 36 -> "BYP", 37 -> "\n" (*"LF"*), 
	38 -> "ETB", 39 -> "ESC", 42 -> "SM", 43 -> "CU2", 45 -> "ENQ", 
	46 -> "ACK", 47 -> "BEL", 50 -> "SYN", 52 -> "PN", 53 -> "RS", 
	54 -> "UC", 55 -> "EOT", 59 -> "CU3", 60 -> "DC4", 61 -> "NAK", 
	63 -> "SUB", 64 -> " " (* SP *), 74 -> "?", 75 -> ".", 76 -> "<", 
	77 -> "(", 78 -> "+", 79 -> "|", 80 -> "&", 90 -> "!", 91 -> "$", 
	92 -> "*", 93 -> ")", 94 -> ";", 95 -> "\[Not]", 96 -> "\.97", 97 -> "/", 
	107 -> ",", 108 -> "%", 109 -> "_", 110 -> ">", 111 -> "?", 
	122 -> ":", 123 -> "#", 124 -> "@", 125 -> "'", 126 -> "=", 
	127 -> "\"", 129 -> "a", 130 -> "b", 131 -> "c", 132 -> "d", 
	133 -> "e", 134 -> "f", 135 -> "g", 136 -> "h", 137 -> "i", 
	145 -> "j", 146 -> "k", 147 -> "l", 148 -> "m", 149 -> "n", 
	150 -> "o", 151 -> "p", 152 -> "q", 153 -> "r", 162 -> "s", 
	163 -> "t", 164 -> "u", 165 -> "v", 166 -> "w", 167 -> "x", 
	168 -> "y", 169 -> "z", 192 -> "{", 193 -> "A", 194 -> "B", 
	195 -> "C", 196 -> "D", 197 -> "E", 198 -> "F", 199 -> "G", 
	200 -> "H", 201 -> "I", 208 -> "}", 209 -> "J", 210 -> "K", 
	211 -> "L", 212 -> "M", 213 -> "N", 214 -> "O", 215 -> "P", 
	216 -> "Q", 217 -> "R", 224 -> "\\", 226 -> "S", 227 -> "T", 
	228 -> "U", 229 -> "V", 230 -> "W", 231 -> "X", 232 -> "Y", 
	233 -> "Z", 240 -> "0", 241 -> "1", 242 -> "2", 243 -> "3", 
	244 -> "4", 245 -> "5", 246 -> "6", 247 -> "7", 248 -> "8", 
	249 -> "9", 255 -> "E0" 
}; 


$fromEBCDICCharactersDispatch = 
Dispatch[$EBCDICCharacters]; 


$toEBCDICCharactersDispatch = 
Dispatch[($EBCDICCharacters)[[All, {2, 1}]]]; 


fromEBCDICCharacterCode[bytes: {__Integer}] := 
StringJoin[(bytes /. $fromEBCDICCharactersDispatch) //. {_Integer :> "?"}]; 


toEBCDICCharacterCode[text_String] := 
(StringSplit[text, ""] /. $toEBCDICCharactersDispatch) /. {_String :> 74}; 


(* ::Section:: *)
(*TextHeader converter*)


fromSEGYTextHeader::argerr = 
"expected array of 3200 bytes; actual `1`"; 


fromSEGYTextHeader[bytes: {__Integer}] /; 
If[Length[bytes] === 3200, 
	True, 
	Message[fromSEGYTextHeader::argerr, args]; False
] := 
	fromEBCDICCharacterCode[bytes]


toSEGYTextHeader[text_String] := 
Join[toEBCDICCharacterCode[text], ConstantArray[0, 3200]][[1 ;; 3200]]


(* ::Section:: *)
(*BinaryHeader converter*)


$SEGYBinaryHeaderSpetification = {
	{"Name" -> "JobID", "Position" -> 01 ;; 04, "Type" -> "UnsignedInteger32"}, 
	{"Name" -> "LineNumber", "Position" -> 05 ;; 08, "Type" -> "UnsignedInteger32"}, 
	{"Name" -> "ReelNumber", "Position" -> 09 ;; 12, "Type" -> "UnsignedInteger32"}, 
	{"Name" -> "NumberDataTraces", "Position" -> 13 ;; 14, "Type" -> "UnsignedInteger16"}, 
	{"Name" -> "NumberAuxTraces", "Position" -> 15 ;; 16, "Type" -> "UnsignedInteger16"}, 
	{"Name" -> "IntervalReelRecord", "Position" -> 17 ;; 18, "Type" -> "UnsignedInteger16"}, 
	{"Name" -> "IntervalFieldRecord", "Position" -> 19 ;; 20, "Type" -> "UnsignedInteger16"}, 
	{"Name" -> "NumberOfSamplesForReel", "Position" -> 21 ;; 22, "Type" -> "UnsignedInteger16"}, 
	{"Name" -> "NumberOfSamplesForField", "Position" -> 23 ;; 24, "Type" -> "UnsignedInteger16"}, 
	{"Name" -> "SamplesFormatCode", "Position" -> 25 ;; 26, "Type" -> "UnsignedInteger16"}, 
	{"Name" -> "CDPFold", "Position" -> 27 ;; 28, "Type" -> "UnsignedInteger16"}, 
	{"Name" -> "TraceSortingCode", "Position" -> 29 ;; 30, "Type" -> "Integer16"}, 
	{"Name" -> "VerticalSumCode", "Position" -> 31 ;; 32, "Type" -> "UnsignedInteger16"}, 
	{"Name" -> "SweepFrequencyAtStart", "Position" -> 33 ;; 34, "Type" -> "UnsignedInteger16"}, 
	{"Name" -> "SweepFrequencyAtEnd", "Position" -> 35 ;; 36, "Type" -> "UnsignedInteger16"}, 
	{"Name" -> "SweepLength", "Position" -> 37 ;; 38, "Type" -> "UnsignedInteger16"}, 
	{"Name" -> "SweepTypeCode", "Position" -> 39 ;; 40, "Type" -> "UnsignedInteger16"}, 
	{"Name" -> "TraceNumberOfSweepChannel", "Position" -> 41 ;; 42, "Type" -> "UnsignedInteger16"}, 
	{"Name" -> "SweepTraceTaperLengthAtStart", "Position" -> 43 ;; 44, "Type" -> "UnsignedInteger16"}, 
	{"Name" -> "SweepTraceTaperLength", "Position" -> 45 ;; 46, "Type" -> "UnsignedInteger16"}, 
	{"Name" -> "TaperType", "Position" -> 47 ;; 48, "Type" -> "UnsignedInteger16"}, 
	{"Name" -> "CorrelatedDataTraces", "Position" -> 49 ;; 50, "Type" -> "UnsignedInteger16"}, 
	{"Name" -> "BinaryGainRecovered", "Position" -> 51 ;; 52, "Type" -> "UnsignedInteger16"}, 
	{"Name" -> "AmplitudeRecoveryMethod", "Position" -> 53 ;; 54, "Type" -> "UnsignedInteger16"}, 
	{"Name" -> "MeasurementSystem", "Position" -> 55 ;; 56, "Type" -> "UnsignedInteger16"}, 
	{"Name" -> "ImpulseSignal", "Position" -> 57 ;; 58, "Type" -> "UnsignedInteger16"}, 
	{"Name" -> "VibratoryPolarityCode", "Position" -> 59 ;; 60, "Type" -> "UnsignedInteger16"}
}


fromSEGYBinaryHeader[bytes: {__Integer}] /; 
Length[bytes] === 400 := 
Module[{
	name, blist, type
}, 
	Table[
		blist = bytes[["Position" /. header]];
		type = "Type" /. header;
		name = "Name" /. header;
			
		name -> ImportString[ExportString[Reverse[blist], "Binary"], type][[1]], 
		{header, $SEGYBinaryHeaderSpetification}
	]
]; 


toSEGYBinaryHeader[rules: {(_String -> _Integer)..}] /; 
	$SEGYBinaryHeaderSpetification[[All, 1, 2]] === rules[[All, 1]] := 
	Module[{
		name, type, position, bytes
	}, 
	bytes = ConstantArray[0, 400];
	Table[
		name = "Name" /. header; 
		type = "Type" /. header; 
		position = "Position" /. header; 
		bytes[[position]] = Reverse[ImportString[ExportString[name /. rules, type], "Byte"]], (*To correct!*)
		{header, $SEGYBinaryHeaderSpetification}
	]; 
	
	Return[bytes]
]; 


(* ::Section:: *)
(*BinaryHeader constrctor*)


Options[createSEGYBinaryHeader] = 
{
	"SamplesFormatCode" -> Automatic, 
	"NumberDataTraces" -> Automatic, 
	"NumberOfSamplesForReel" -> Automatic
}; 


createSEGYBinaryHeader[data: {{__Integer}..} | {{__Real}..}, OptionsPattern[]] := 
Module[{
	bytes = ConstantArray[0, 400], 
	samplesFormatCodeBytes = 
	IntegerDigits[If[# === Automatic, If[IntegerQ[data[[1, 1]]], 2, 1], #], 256, 2]&[OptionValue["SamplesFormatCode"]], 
	numberDataTracesBytes = 
	IntegerDigits[If[# === Automatic, Length[data], #], 256, 2]&[OptionValue["NumberDataTraces"]], 
	numberOfSamplesForReelBytes = 
	IntegerDigits[If[# === Automatic, Length[data[[1]]], #], 256, 2]&[OptionValue["NumberOfSamplesForReel"]], 
	intervalReelRecord = 
	IntegerDigits[1000, 256, 2], 
	measurementSystem = 
	IntegerDigits[2, 256, 2]		
}, 
	bytes[[25 ;; 26]] = samplesFormatCodeBytes;
	bytes[[13 ;; 14]] = numberDataTracesBytes;
	bytes[[17 ;; 18]] = intervalReelRecord;
	bytes[[21 ;; 22]] = numberOfSamplesForReelBytes;
	bytes[[55 ;; 56]] = measurementSystem;
		
	fromSEGYBinaryHeader[bytes]
]; 


createSEGYBinaryHeader[{}, OptionsPattern[]] := 
Module[{
	bytes = ConstantArray[0, 400], 
	samplesFormatCodeBytes = IntegerDigits[If[# === Automatic, 1, #], 256, 2]&[OptionValue["SamplesFormatCode"]], 
	numberDataTracesBytes = IntegerDigits[If[# === Automatic, 0, #], 256, 2]&[OptionValue["NumberDataTraces"]], 
	numberOfSamplesForReelBytes = IntegerDigits[If[# === Automatic, 0, #], 256, 2]&[OptionValue["NumberOfSamplesForReel"]], 
	intervalReelRecord = IntegerDigits[1000, 256, 2], 
	measurementSystem = IntegerDigits[2, 256, 2]		
}, 
	bytes[[25 ;; 26]] = samplesFormatCodeBytes;
	bytes[[13 ;; 14]] = numberDataTracesBytes;
	bytes[[17 ;; 18]] = intervalReelRecord;
	bytes[[21 ;; 22]] = numberOfSamplesForReelBytes;
	bytes[[55 ;; 56]] = measurementSystem;
		
	fromSEGYBinaryHeader[bytes]
]


(* ::Section:: *)
(*TraceHeader specification*)


$SEGYTraceHeaderSpecification = <|
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


traceHeaderKeyQ[___] := 
False; 


traceHeaderKeyQ[k_String] := 
MemberQ[Keys[$SEGYTraceHeaderSpecification], ToLowerCase[k]]; 


(* ::Section:: *)
(*TraceHeader converter*)


fromSEGYTraceHeader[target: "MVM" | "C"] := fromSEGYTraceHeader[target] = 
With[{$SEGYTraceHeaderSpetification = Values[$SEGYTraceHeaderSpecification]}, 
	Compile[{
		{bytes, _Integer, 1}
	}, 
		Module[{blist = {0}, len = 0, beg = 0, end = 0, type = 0, 
		spec = $SEGYTraceHeaderSpetification}, 
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


toSEGYTraceHeader[target: "MVM" | "C"] := toSEGYTraceHeader[target] = 
With[{$SEGYTraceHeaderSpetification = Values[$SEGYTraceHeaderSpecification]}, 
	Compile[{
		{values, _Integer, 1}
	}, 
		Module[{
			bytes = Table[0, {240}], s, val, 
			spec = $SEGYTraceHeaderSpecification
		}, 
			Table[
				s = spec[[i]];
				val = values[[i]];
				If[s[[2]] - s[[1]] === 1, 
					bytes[[s[[1]] ;; s[[2]]]] = IntegerDigits[val, 256, 2], 
					bytes[[s[[1]] ;; s[[2]]]] = IntegerDigits[val, 256, 4]
				]; 
				If[val < 0, bytes[[s[[1]]]] = bytes[[s[[1]]]] + 128];, 
				{i, 1, Length[spec]}
			]; 
				
			Return[bytes]
		]
	]
]; 


(* ::Section:: *)
(*Number formats*)


fromIBMFloat32[target: "MVM" | "C"] := fromIBMFloat32[target] = 
Compile[{{bytes, _Integer, 1}}, 

	Module[{sign, exp, fract}, 

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
			If[bytes[[i4th]] == 0 && bytes[[i4th + 1]] == 0 && bytes[[i4th + 2]] == 0 && bytes[[i4th + 3]] == 0, exp = 0.0];
				
			sign * exp * fract, 

			{i4th, 1, Length[bytes], 4}
		]
	], 
		
	CompilationTarget -> target
]; 


toIBMFloat32[target: "MVM" | "C"] := toIBMFloat32[target] =  
Compile[{{numbers, _Real, 1}}, 
	Module[{number = 0.0, rsign = 0, exp = 0, firstbyte = 0, fractbytes = {0, 0, 0}, bytes = Table[0, {4 * Length[numbers]}]}, 

		Table[
			number = numbers[[i]]; 

			If[
				And[
					Abs[number] < 10.0^75, 
					Abs[number] > 10.0^-75
				], 

				(* bit for the represintation of the sign of the number *) 
   	    		rsign = UnitStep[-number]; 

       			(* 16-th exponent *) 
				exp = Ceiling[Log[16, Abs[number]]]; 

				(* first byte *) 
				firstbyte = exp + 64 + rsign * 128; 

       			(* bytes og the fraction part *) 
				fractbytes = IntegerDigits[Floor[256.0^3  * number / (16.0^exp)], 256, 3]; 

       			(* return *) 
   				bytes[[4i - 3]] = firstbyte;
   				bytes[[4i - 2 ;; 4i]] = fractbytes;
			], 
				
   			{i, 1, Length[numbers]}
		]; 
			
		Return[bytes]
    ], 

	CompilationTarget -> target
]; 


fromInt32[target: "MVM" | "C"] := fromInt32[target] = 
Compile[{
	{bytes, _Integer, 1}
}, 
	Table[
		If[bytes[[i]] > 127, 
			((bytes[[i]] - 128) * 256^3 + bytes[[i + 1]] * 256^2 + bytes[[i + 2]] * 256 + bytes[[i + 3]]) - 2147483648, 
			bytes[[i]] * 256^3 + bytes[[i + 1]] * 256^2 + bytes[[i + 2]] * 256 + bytes[[i + 3]]
		], 
		
		{i, 1, Length[bytes], 4}
	], 
		
	CompilationTarget -> target
]; 


toInt32[target: "MVM" | "C"] := toInt32[target] = 
Compile[{
	{numbers, _Integer, 1}
}, 
	Module[{bytes = Table[0, {4 * Length[numbers]}]}, 
		Table[
			If[numbers[[i]] >= 0, 
				bytes[[4i - 3 ;; 4i]] = IntegerDigits[numbers[[i]], 256, 4], 
				bytes[[4i - 3 ;; 4i]] = {255, 255, 255, 255} - IntegerDigits[numbers[[i]] + 1, 256, 4]
			], 
			{i, 1, Length[numbers]}
		]; 
			
		bytes
	], 
		
	CompilationTarget -> target
]; 


fromInt16[target: "MVM" | "C"] := fromInt16[target] = 
	Compile[{
		{bytes, _Integer, 1}
	}, 
		Table[
			If[bytes[[i]] > 127, 
				((bytes[[i]] - 128) * 256 + bytes[[i + 1]]) - 32768, 
				bytes[[i]] * 256 + bytes[[i + 1]]
			], 
			
			{i, 1, Length[bytes], 2}
		], 
		
		CompilationTarget -> target
	]


toInt16[target: "MVM" | "C"] := toInt16[target] = 
Compile[{
	{nums, _Integer, 1}
}, 
	Module[{
		bytes = Table[0, {2 Length[nums]}]
	}, 
		Table[
			If[nums[[i]] < 0, 
				bytes[[2i - 1 ;; 2i]] = {255, 255} - IntegerDigits[nums[[i]] + 1, 256, 2], 
				bytes[[2i - 1 ;; 2i]] = IntegerDigits[nums[[i]], 256, 2]
			], 
			{i, 1, Length[nums]}
		]; 
			
		Return[bytes]
	], 
		
	CompilationTarget -> target
]; 


fromInt8[target: "MVM" | "C"] := fromInt8[target] = 
Compile[{
	{bytes, _Integer, 1}
}, 
	Table[If[b > 127, 127 - b, b], {b, bytes}], 
		
		CompilationTarget -> target
]; 


toInt8[target: "MVM" | "C"] := toInt8[target] = 
	Compile[{
		{numbers, _Integer, 1}
	}, 
		Table[If[n < 0, - n + 127, n], {n, numbers}], 
		
		CompilationTarget -> target
	]


(* ::Section:: *)
(*Trace converter*)


(*
	1 = 4 byte IBM floating-point
	2 = 4-byte two's complement integer
	3 = 2-byte two's complement integer
	4 = 4-byte fixed-point with gain
	5 = 4-byte IEEE floating point
	6 = Not currently used
	7 = Not currently used
	8 = 1-bytes, two's complement integer
*)


SEGYNumberSize[1] = 4


SEGYNumberSize[2] = 4


SEGYNumberSize[3] = 2


SEGYNumberSize[4] = 4


SEGYNumberSize[5] = 4


SEGYNumberSize[8] = 1


fromSEGYTrace[samplesFormatCode: 1 | 2 | 3 | 4 | 5 | 8, compilationTarger: "MVM" | "C"] := fromSEGYTrace[samplesFormatCode, compilationTarger] = 
	Switch[samplesFormatCode, 
		1, fromIBMFloat32[compilationTarger], 
		2, fromInt32[compilationTarger], 
		3, fromInt16[compilationTarger], 
		8, fromInt8[compilationTarger]
	]


toSEGYTrace[samplesFormatCode: 1 | 2 | 3 | 4 | 5 | 8, compilationTarger: "MVM" | "C"] := toSEGYTrace[samplesFormatCode, compilationTarger] = 
	Switch[samplesFormatCode, 
		1, toIBMFloat32[compilationTarger], 
		2, toInt32[compilationTarger], 
		3, toInt16[compilationTarger], 
		8, toInt8[compilationTarger]
	]


(* ::Section:: *)
(*SEGYImport*)


Options[SEGYImport] = 
	{
		"NumberDataTraces" -> Automatic, 
		"NumberOfSamplesForReel" -> Automatic, 
		"SamplesFormatCode" -> Automatic, 
		"CompilationTarget" -> "MVM", 
		"Loading" -> "Memory"
	}


SyntaxInformation[SEGYImport] = 
	{"ArgumentsPattern" -> {_, OptionsPattern[]}, "OptionNames" -> Keys[Association[Options[SEGYImport]]]}


If[$VersionNumber > 10, FE`Evaluate[FEPrivate`AddSpecialArgCompletion["SEGYImport" -> {2}]]]


SEGYImport[file: (_String | _File)?FileExistsQ, OptionsPattern[]] /; 
	
	StringMatchQ[FileExtension[file], "sgy" | "segy", IgnoreCase -> True] && 
	FileByteCount[file] >= 3600 := 
	Module[{
		stream, 
		numberDataTraces, numberOfSamplesForReel, samplesFormatCode, compilationTarget, loading, traceByteSize,  
		textHeader, binaryHeader, traceHeaders, traces, 
		traceHeaderPositions, tracePositions
	}, 
		(*open input stream for reading bytes*)
		stream = OpenRead[file, BinaryFormat -> True];
	
		(*reading textheader*)
		SetStreamPosition[stream, 0];
		textHeader = fromSEGYTextHeader[BinaryReadList[stream, "Byte", 3200]]; 
		
		(*reading binaryheader*)
		SetStreamPosition[stream, 3200];
		binaryHeader = fromSEGYBinaryHeader[BinaryReadList[stream, "Byte", 400]]; 
	
		(*options*)
		
		(*check trace-params*)
		
		If[MatchQ[OptionValue["SamplesFormatCode"], 1 | 2 | 3 | 4 | 5 | 8], 
			samplesFormatCode = OptionValue["SamplesFormatCode"], 
			samplesFormatCode = "SamplesFormatCode" /. binaryHeader
		];
		
		If[IntegerQ[OptionValue["NumberOfSamplesForReel"]] && Positive[OptionValue["NumberOfSamplesForReel"]], 
			numberOfSamplesForReel = OptionValue["NumberOfSamplesForReel"], 
			numberOfSamplesForReel = "NumberOfSamplesForReel" /. binaryHeader
		];
		
		If[IntegerQ[OptionValue["NumberDataTraces"]] && Positive[OptionValue["NumberDataTraces"]], 
			numberDataTraces = OptionValue["NumberDataTraces"], 
			numberDataTraces = (FileByteCount[file] - 3600) / (numberOfSamplesForReel * SEGYNumberSize[samplesFormatCode] + 240)
		];
		
		compilationTarget = If[# === "MVM" || # === "C", #, "MVM"]&@OptionValue["CompilationTarget"];
		
		loading = If[MatchQ[#, "Memory" | "Delayed" | "Header"], #, "Memory"]&@OptionValue["Loading"];
		
		traceByteSize = numberOfSamplesForReel * SEGYNumberSize[samplesFormatCode]; 
		
		Which[
			loading === "Memory", 
		
			(*creating trace headers array*)
			traceHeaders = ConstantArray[0, {numberDataTraces, Length[$SEGYTraceHeaderSpecification]}];
		
			(*reading trace headers*)
			Table[
				SetStreamPosition[stream, 3600 + i * (240 + traceByteSize)]; 
				traceHeaders[[i + 1]] = fromSEGYTraceHeader[compilationTarget][BinaryReadList[stream, "Byte", 240]], 
				{i, 0, numberDataTraces - 1}
			]; 
		
			(*creating traces array*)
			Switch[samplesFormatCode, 
				1 | 4 | 5, traces = ConstantArray[0.0, {numberDataTraces, numberOfSamplesForReel}], 
				2 | 3 | 8, traces = ConstantArray[0, {numberDataTraces, numberOfSamplesForReel}], 
				_, traces = ConstantArray[0, {numberDataTraces, numberOfSamplesForReel}]
			];
		
			(*reading traces*)
			Table[
				SetStreamPosition[stream, 3600 + i * (240 + traceByteSize) + 240]; 
				traces[[i + 1]] = 
				fromSEGYTrace[samplesFormatCode, compilationTarget][BinaryReadList[stream, "Byte", traceByteSize]], 
				{i, 0, numberDataTraces - 1}
			];
		
			(*close file*)
			Close[stream];
		
			(*RETURN*) 
			Return[
				SEGYData[{
					"TextHeader" -> textHeader, 
					"BinaryHeader" -> binaryHeader, 
					"TraceHeaders" -> {Keys[$SEGYTraceHeaderSpecification], traceHeaders}, 
					"Traces" -> traces
				}]
			], 
			
			loading === "Delayed", 
			
			(*close file*)
			Close[stream];
			
			traceHeaderPositions = 
			Range[3600, 3600 + (240 + traceByteSize) * (numberDataTraces - 1), 240 + traceByteSize];
			
			tracePositions = 
			Range[3600 + 240, 3600 + (240 + traceByteSize) * (numberDataTraces - 1) + 240, 240 + traceByteSize];
			
			(*RETURN*)
			Return[
				SEGYData[{
					"TextHeader" -> textHeader, 
					"BinaryHeader" -> binaryHeader, 
					"TraceHeadersUnloaded" -> {
						"File" -> ExpandFileName[file], 
						"Positions" -> traceHeaderPositions
					}, 
					"TracesUnloaded" -> {
						"File" -> ExpandFileName[file], 
						"SamplesFormatCode" -> samplesFormatCode, 
						"NumberOfSamplesForReel" -> numberOfSamplesForReel, 
						"Positions" -> tracePositions
					}
				}]
			], 
			
			loading === "Header", 
			
			(*close file*)
			Close[stream];
		
			(*RETURN*) 
			Return[
				SEGYData[{
					"TextHeader" -> textHeader, 
					"BinaryHeader" -> binaryHeader, 
					"TraceHeaders" -> {Keys[$SEGYTraceHeaderSpecification], {}}, 
					"Traces" -> {}
				}]
			]
		]
	]


(* ::Section:: *)
(*SEGYLoad*)


Options[SEGYLoad] = 
	{"CompilationTarget" -> "MVM", "TraceHeadersForm" -> Rule}


SyntaxInformation[SEGYLoad] = 
	{"ArgumentsPattern" -> {_, OptionsPattern[]}, "OptionNames" -> Keys[Association[Options[SEGYLoad]]]}


SEGYLoad[{"File" -> file_String?FileExistsQ, "Positions" -> positions: {__Integer}}, OptionsPattern[]] /; 
	StringMatchQ[FileExtension[file], "segy" | "sgy", IgnoreCase -> True] := 
	Module[{
		stream = OpenRead[file, BinaryFormat -> True], 
		compilationTarget = If[# === "MVM" || # === "C", #, "MVM"]&@OptionValue["CompilationTarget"], 
		traceHeaders = ConstantArray[0, {Length[positions], Length[$SEGYTraceHeaderSpecification]}]
	}, 
		Table[
			SetStreamPosition[stream, positions[[i]]];
			traceHeaders[[i]] = fromSEGYTraceHeader[compilationTarget][BinaryReadList[stream, "Byte", 240]], 
				
			{i, 1, Length[positions]}
		]; 
		Close[stream]; 
		
		Switch[OptionValue["TraceHeadersForm"], 
			List, 
			Return[{Keys[$SEGYTraceHeaderSpecification], traceHeaders}], 
			
			Rule, 
			Return[Map[MapThread[Rule, {Keys[$SEGYTraceHeaderSpecification], #}]&, traceHeaders]], 
			
			Association, 
			Return[Association[MapIndexed[Rule[#2[[1]], #1]&, (Association /@ Map[MapThread[Rule, {Keys[$SEGYTraceHeaderSpecification], #}]&, traceHeaders])]]], 
			
			Array, 
			Return[traceHeaders], 
			
			_, 
			Return[traceHeaders]
		]
	]


SEGYLoad[{"File" -> file_String?FileExistsQ, "Positions" -> position_Integer}, OptionsPattern[]] /; 
	StringMatchQ[FileExtension[file], "segy" | "sgy", IgnoreCase -> True] := 
	Module[{
		stream = OpenRead[file, BinaryFormat -> True], 
		compilationTarget = If[# === "MVM" || # === "C", #, "MVM"]&@OptionValue["CompilationTarget"], 
		traceHeader
	}, 
		SetStreamPosition[stream, position];
		traceHeader = fromSEGYTraceHeader[compilationTarget][BinaryReadList[stream, "Byte", 240]]; 
		Close[stream]; 
		
		Return[MapThread[Rule, {Keys[$SEGYTraceHeaderSpecification], traceHeader}]]
	]


SEGYLoad[{
	"File" -> file_String?FileExistsQ, 
	"SamplesFormatCode" -> samplesFormatCode: 1 | 2 | 3 | 4 | 5 | 8, 
	"NumberOfSamplesForReel" -> numberOfSamplesForReel_Integer?Positive, 
	"Positions" -> positions: {__Integer?Positive}
}, OptionsPattern[]] /; 
	StringMatchQ[FileExtension[file], "segy" | "sgy", IgnoreCase -> True] := 
	Module[{
		stream = OpenRead[file, BinaryFormat -> True], 
		compilationTarget = If[# === "MVM" || # === "C", #, "MVM"]&@OptionValue["CompilationTarget"], 
		traces = ConstantArray[If[MatchQ[samplesFormatCode, 1 | 4 | 5], 0.0, 0], {Length[positions], numberOfSamplesForReel}]
	}, 
		Table[
			SetStreamPosition[stream, positions[[i]]];
			traces[[i]] = fromSEGYTrace[samplesFormatCode, compilationTarget][BinaryReadList[stream, "Byte", numberOfSamplesForReel * SEGYNumberSize[samplesFormatCode]]], 
				
			{i, 1, Length[positions]}
		];
		Close[stream];  
		
		Return[traces]
	]


SEGYLoad[{
	"File" -> file_String?FileExistsQ, 
	"SamplesFormatCode" -> samplesFormatCode: 1 | 2 | 3 | 4 | 5 | 8, 
	"NumberOfSamplesForReel" -> numberOfSamplesForReel_Integer?Positive, 
	"Positions" -> position_Integer
}, OptionsPattern[]] /; 
	StringMatchQ[FileExtension[file], "segy" | "sgy", IgnoreCase -> True] := 
	Module[{
		stream = OpenRead[file, BinaryFormat -> True], 
		compilationTarget = If[# === "MVM" || # === "C", #, "MVM"]&@OptionValue["CompilationTarget"], 
		trace
	}, 
		SetStreamPosition[stream, position];
		trace = fromSEGYTrace[samplesFormatCode, compilationTarget][BinaryReadList[stream, "Byte", numberOfSamplesForReel * SEGYNumberSize[samplesFormatCode]]]; 
		Close[stream]; 
		
		Return[trace]
	]


(* ::Section:: *)
(*SEGYExport*)


Options[SEGYExport] = 
	{"CompilationTarget" -> "MVM", "SamplesFormatCode" -> Automatic, "NumberDataTraces" -> Automatic}


SyntaxInformation[SEGYExport] = 
	{"ArgumentsPattern" -> {_, _, OptionsPattern[]}, Keys[Association[Options[SEGYExport]]]}


If[$VersionNumber > 10, FE`Evaluate[FEPrivate`AddSpecialArgCompletion["SEGYExport" -> {2}]]]


SEGYExport[file_String, segy_SEGYData?memorySEGYQ, OptionsPattern[]] /; 
	StringMatchQ[FileExtension[file], "sgy" | "segy", IgnoreCase -> True] := 
	Module[{
		stream = OpenWrite[file, BinaryFormat -> True], 
		
		compilationTarget = 
		If[# === "MVM" || # === "C", #, "MVM"]&@OptionValue["CompilationTarget"], 
		
		samplesFormatCode = 
		If[# === Automatic, segy["BinaryHeader", "SamplesFormatCode"], #]&@OptionValue["SamplesFormatCode"], 
		
			
		numberDataTraces = 
		If[# === Automatic, Length[segy["traces"]], #]&@OptionValue["NumberDataTraces"]
	}, 
		BinaryWrite[stream, toSEGYTextHeader[segy[[1, 1, 2]]], "Byte"]; 
		BinaryWrite[stream, toSEGYBinaryHeader[segy[[1, 2, 2]]], "Byte"];
		Table[
			BinaryWrite[stream, toSEGYTraceHeader[compilationTarget][segy[[1, 3, 2, 2, i]]], "Byte"];
			BinaryWrite[stream, toSEGYTrace[samplesFormatCode, compilationTarget][segy[[1, 4, 2, i]]], "Byte"];, 
			{i, 1, numberDataTraces}
		];
		
		Close[stream];
		
		Return[file] 
		
	]


(* ::Section:: *)
(*SEGYWrite*)


SEGYWrite::wrterr = 
"Data can't be write: `1`"


Options[SEGYWrite] = 
	{
		"CompilationTarget" -> "MVM", 
		"SamplesFormatCode" -> Automatic, 
		"NumberDataTraces" -> Automatic, 
		"NumberOfSamplesForReel" -> Automatic
	}


SyntaxInformation[SEGYWrite] = 
	{"ArgumentsPattern" -> {_, _, OptionsPattern[]}, Keys[Association[Options[SEGYWrite]]]}


(*
	Start writing:
	SEGYWrite[file, segy] - write only text and binary headers
*)
SEGYWrite[file_String, segy_SEGYData, OptionsPattern[]] /; 
	StringMatchQ[FileExtension[file], "sgy" | "segy", IgnoreCase -> True] := 
	Module[{
		stream = OpenWrite[file, BinaryFormat -> True], 
		
		segyTemp, textHeader, binaryHeader, 
		
		samplesFormatCode = If[# === Automatic, 
			segy["BinaryHeader", "SamplesFormatCode"], 
			If[MatchQ[#, 1 | 2 | 3 | 4 | 5 | 8], #, 1]
		]&@OptionValue["SamplesFormatCode"], 
		
		numberDataTraces = If[# === Automatic, 
			segy["BinaryHeader", "NumberDataTraces"], 
			If[MatchQ[#, 1 | 2 | 3 | 4 | 5 | 8], #, 1]
		]&@OptionValue["NumberDataTraces"], 
		
		numberOfSamplesForReel = If[# === Automatic, 
			segy["BinaryHeader", "NumberOfSamplesForReel"], 
			If[MatchQ[#, 1 | 2 | 3 | 4 | 5 | 8], #, 1]
		]&@OptionValue["NumberOfSamplesForReel"]
	}, 
		segyTemp = segy["Header"];
		
		segyTemp["BinaryHeader", "SamplesFormatCode"] = samplesFormatCode;
		segyTemp["BinaryHeader", "NumberDataTraces"] = numberDataTraces;
		segyTemp["BinaryHeader", "NumberOfSamplesForReel"] = numberOfSamplesForReel;
		
		textHeader = segyTemp["TextHeader"];
		binaryHeader = segyTemp["BinaryHeader"];

		BinaryWrite[stream, toSEGYTextHeader[textHeader], "Byte"];
		BinaryWrite[stream, toSEGYBinaryHeader[binaryHeader], "Byte"];
		
		Return[Close[stream]]
	]

(*
	Append data to the file:
	SEGYWrite[file, {"traceheaders" -> traceHeaders, "traces" -> traces}]
*)
SEGYWrite[file_String?FileExistsQ, 
	{
		_?traceHeadersKeyQ -> (traceHeaders: {{__Integer}.. } ?(MatchQ[Dimensions[#], {_?Positive, Length[$SEGYTraceHeaderSpecification]}]&)), 
		_?tracesKeyQ -> traces: {{__Real}.. } | {{__Integer}.. }
	}, OptionsPattern[]] /; 
	StringMatchQ[FileExtension[file], "sgy" | "segy", IgnoreCase -> True] && 
	FileByteCount[file] >= 3600 && 
	Length[traceHeaders] === Length[traces] && 
	MatrixQ[traces] && MatrixQ[traceHeaders] && 
	Developer`PackedArrayQ[traces] && Developer`PackedArrayQ[traceHeaders] := 
	Module[{
		fileHeader = SEGYImport[file, "Loading" -> "Header"], 
		fileSize = FileByteCount[file], 
		stream, 
		
		compilationTarget, 
		samplesFormatCode, 
		numberDataTraces, 
		numberOfSamplesForReel
	}, 
		
		compilationTarget = 
		If[# === "MVM" || # === "C", #, "MVM"]&@OptionValue["CompilationTarget"]; 
	
		samplesFormatCode = If[# === Automatic, 
			fileHeader["BinaryHeader", "SamplesFormatCode"], 
			If[MatchQ[#, 1 | 2 | 3 | 4 | 5 | 8], #, fileHeader["BinaryHeader", "SamplesFormatCode"]]
		]&@OptionValue["SamplesFormatCode"]; 
		
		numberDataTraces = If[# === Automatic, 
			fileHeader["BinaryHeader", "NumberDataTraces"], 
			If[IntegerQ[#], #, fileHeader["BinaryHeader", "NumberDataTraces"]]
		]&@OptionValue["NumberDataTraces"]; 
		
		numberOfSamplesForReel = If[# === Automatic, 
			fileHeader["BinaryHeader", "NumberOfSamplesForReel"], 
			If[IntegerQ[#], #, fileHeader["BinaryHeader", "NumberOfSamplesForReel"]]
		]&@OptionValue["NumberOfSamplesForReel"]; 
	
		If[
			numberOfSamplesForReel =!= Length[traces[[1]]], 
			Message[SEGYWrite::wrterr, "not valid length of the trace"]; Return[Null]
		];
		
		If[
			If[MatchQ[samplesFormatCode, 1], Real, Integer] =!= Head[traces[[1, 1]]], 
			Message[SEGYWrite::wrterr, "not valid number format of the trace"]; Return[Null]
		];
		
		If[
			fileSize - 3600 > (240 + numberOfSamplesForReel * SEGYNumberSize[samplesFormatCode]) * (numberDataTraces - Length[traces]), 
			Message[SEGYWrite::wrterr, "out of trace limit for the file"]; Return[Null]
		];
	
		stream = OpenAppend[file, BinaryFormat -> True]; 
		
		Table[
			BinaryWrite[stream, toSEGYTraceHeader[compilationTarget][traceHeaders[[i]]], "Byte"];
			BinaryWrite[stream, toSEGYTrace[samplesFormatCode, compilationTarget][traces[[i]]], "Byte"];, 
			
			{i, 1, Length[traces]}
		];
		
		Close[stream]
	]


SEGYWrite[file_String?FileExistsQ, 
	{
		_?traceHeadersKeyQ -> (traceHeaders: ({{__Integer}.. })?(MatchQ[Dimensions[#], {_?Positive, Length[$SEGYTraceHeaderSpecification]}]&)), 
		_?tracesKeyQ -> traces: {{__Real}.. } | {{__Integer}.. }
	}, opts: OptionsPattern[]] /; 
	Length[traceHeaders] === Length[traces] && 
	MatrixQ[traces] && MatrixQ[traceHeaders] && 
	(Not[Developer`PackedArrayQ[traces]] || Not[Developer`PackedArrayQ[traceHeaders]]) := 
	SEGYWrite[file, {
		"TraceHeaders" -> Developer`ToPackedArray[traceHeaders], 
		"Traces" -> Developer`ToPackedArray[traces]
	}, opts]


SEGYWrite[file_String?FileExistsQ, traces: ({{__Real}.. } | {{__Integer}.. })?MatrixQ, opts: OptionsPattern[]] := 
	SEGYWrite[file, {
		"TraceHeaders" -> Developer`ToPackedArray[ConstantArray[0, {Length[traces], Length[$SEGYTraceHeaderSpecification]}]], 
		"Traces" -> Developer`ToPackedArray[traces]
	}, opts]


SEGYWrite[file_String?FileExistsQ, {
	traceHeaders: {{(_String -> _Integer).. }.. } ?(MatchQ[ToLowerCase[#[[All, All, 1]]], {ToLowerCase[Keys[$SEGYTraceHeaderSpecification]].. }]&), 
	traces: {{__Real}.. } | {{__Integer}.. } ?MatrixQ
}, opts: OptionsPattern[]] := 
	SEGYWrite[file, {
		"TraceHeaders" -> Developer`ToPackedArray[traceHeaders[[All, All, 2]]], 
		"Traces" -> Developer`ToPackedArray[traces]
	}, opts]


SEGYWrite[file_String?FileExistsQ, trace: {__Real} | {__Integer}, opts: OptionsPattern[]] := 
	SEGYWrite[file, {
		"TraceHeaders" -> Developer`ToPackedArray[{ConstantArray[0, Length[$SEGYTraceHeaderSpecification]]}], 
		"Traces" -> Developer`ToPackedArray[{trace}]
	}, opts]


(* ::Section:: *)
(*Pattern tests for getters*)


headerKeyQ[key_String] := 
	StringMatchQ[key, "Header", IgnoreCase -> True]


textHeaderKeyQ[key_String] := 
	StringMatchQ[key, "TextHeader", IgnoreCase -> True]


binaryHeaderKeyQ[key_String] := 
	StringMatchQ[key, "BinaryHeader", IgnoreCase -> True]


traceHeadersKeyQ[key_String] := 
	StringMatchQ[key, "TraceHeaders", IgnoreCase -> True]


traceHeadersUnloadedKeyQ[key_String] := 
	StringMatchQ[key, "TraceHeadersUnloaded", IgnoreCase -> True]


tracesKeyQ[key_String] := 
	StringMatchQ[key, "Traces", IgnoreCase -> True]


tracesUnloadedKeyQ[key_String] := 
	StringMatchQ[key, "TracesUnloaded", IgnoreCase -> True]


stringEQ[s_String] := 
	StringMatchQ[#, s, IgnoreCase -> True]&


(* ::Section:: *)
(*Pattern tests for SEGYData types*)


headerSEGYQ[segy_SEGYData] := 
	segy[[1, All, 1]] === {"TextHeader", "BinaryHeader", "TraceHeaders", "Traces"} && 
	segy[[1, -1, -1]] === {} && segy[[1, -2, -1, -1]] === {}
	(*MatchQ[
		segy, 
		SEGYData[{
			"TextHeader" -> _String, 
			"BinaryHeader" -> {__Rule}, 
			"TraceHeaders" -> {$SEGYTraceHeadersKeys, {} | {{__Integer}.. }}, 
			"Traces" -> {}
		}] | 
		SEGYData[{
			"TextHeader" -> _String, 
			"BinaryHeader" -> {__Rule}, 
			"TraceHeaders" -> {$SEGYTraceHeadersKeys, {}}, 
			"Traces" -> {} | {{__Integer}.. } | {{__Real}.. }
		}]
	]*)


memorySEGYQ[segy_SEGYData] := 
	segy[[1, All, 1]] === {"TextHeader", "BinaryHeader", "TraceHeaders", "Traces"} && 
	segy[[1, -1, -1]] =!= {} && segy[[1, -2, -1, -1]] =!= {}
	(*MatchQ[
		segy, 
		SEGYData[{
			"TextHeader" -> _String, 
			"BinaryHeader" -> {__Rule}, 
			"TraceHeaders" -> {$SEGYTraceHeadersKeys, {{__Integer}.. }}, 
			"Traces" -> {{__Integer}.. } | {{__Real}.. }
		}]
	]*)


delayedSEGYQ[segy_SEGYData] := 
	segy[[1, All, 1]] === {"TextHeader", "BinaryHeader", "TraceHeadersUnloaded", "TracesUnloaded"}
	(*MatchQ[
		segy, 
		SEGYData[{
			"TextHeader" -> _String, 
			"BinaryHeader" -> {__Rule}, 
			"TraceHeadersUnloaded" -> {
				"File" -> _String?((FileExistsQ[#] && StringMatchQ[FileExtension[#], "sgy" | "segy", IgnoreCase -> True])&), 
				"Positions" -> {__Integer?Positive}
			}, 
			"TracesUnloaded" -> {
				"File" -> _String?((FileExistsQ[#] && StringMatchQ[FileExtension[#], "sgy" | "segy", IgnoreCase -> True])&), 
				"SamplesFormatCode" -> 1 | 2 | 3 | 4 | 5 | 8, 
				"NumberOfSamplesForReel" -> _Integer?Positive, 
				"Positions" -> {__Integer?Positive}
			}
		}]
	]*)


(* ::Section:: *)
(*SEGYData constructor*)


(* Messages *)


SEGYData::seterr = 
"This setter not available"


(* Constructor *)


(* options for the constructor *)
Options[SEGYData] = 
	{
		"TextHeader" -> "TextHeader generated by the Wolfram Language with the package GeologyIO", 
		"SamplesFormatCode" -> Automatic, 
		"NumberDataTraces" -> Automatic, 
		"NumberOfSamplesForReel" -> Automatic
	}


(* segy-object constructor from the 2D array *)
SEGYData[array2D: {{__Integer}..} | {{__Real}..}, opts: OptionsPattern[]] := 
	SEGYData[{
		"TextHeader" -> OptionValue["TextHeader"], 
		"BinaryHeader" -> createSEGYBinaryHeader[array2D, FilterRules[{opts}, Options[createSEGYBinaryHeader]]], 
		"TraceHeaders" -> {
			Keys[$SEGYTraceHeaderSpecification], 
			ConstantArray[0, {Length[array2D], Length[$SEGYTraceHeadersKeys]}]
		}, 
		"Traces" -> Developer`ToPackedArray[array2D]
	}]


(* constructor for the segy-object that contains only header *)
SEGYData[{}, opts: OptionsPattern[]] := 
	SEGYData[{
		"TextHeader" -> OptionValue["TextHeader"], 
		"BinaryHeader" -> createSEGYBinaryHeader[{}, FilterRules[{opts}, Options[createSEGYBinaryHeader]]], 
		"TraceHeaders" -> {Keys[$SEGYTraceHeaderSpecification], {}}, 
		"Traces" -> {}
	}]


(* Header *)


(* getter: segy["Header"] => SEGYData[<<Loading: Header, ..>>]*)
(segy_SEGYData)[key_String?headerKeyQ] := 
	Module[{
		textHeader = segy["TextHeader"], 
		binaryHeader = segy["BinaryHeader"]
	}, 
		SEGYData[{
			"TextHeader" -> textHeader, 
			"BinaryHeader" -> binaryHeader, 
			"TraceHeaders" -> {Keys[$SEGYTraceHeadersSpecification], {}}, 
			"Traces" -> {}
		}]
	]


(* ::Section:: *)
(*SEGYData getters*)


(* TextHeader *)


(segy_SEGYData)[key_String?textHeaderKeyQ] := segy[[1, 1, -1]]


(* BinaryHeader *)


(segy_SEGYData)[key_String?binaryHeaderKeyQ] := 
	segy[[1, 2, -1]]


(segy_SEGYData)[key_String?binaryHeaderKeyQ, k_String] /; 
	MemberQ[ToLowerCase["Name" /. $SEGYBinaryHeaderSpetification], ToLowerCase[k]] := 
	Last[SelectFirst[segy[[1, 2, -1]], StringMatchQ[First[#], k, IgnoreCase -> True]&]]


(segy_SEGYData)[key_String?binaryHeaderKeyQ, k: {__String}] /; 
	MemberQ[ToLowerCase["Name" /. $SEGYBinaryHeaderSpetification], ToLowerCase[k]] := 
	Table[SelectFirst[segy[[1, 2, -1]], StringMatchQ[First[#], ki, IgnoreCase -> True]&], {ki, k}]


(* TraceHeaders *)


(segy_SEGYData?memorySEGYQ)[key_String?traceHeadersKeyQ] := 
	segy[[1, 3, -1]]


(segy_SEGYData?memorySEGYQ)[key_String?traceHeadersKeyQ, i: {__Integer} | Span[__] | All] := 
	MapThread[Rule, {segy[[1, 3, -1, 1]], #}]& /@ segy[[1, 3, -1, -1, i]]


(segy_SEGYData?memorySEGYQ)[key_String?traceHeadersKeyQ, i_Integer] := 
	MapThread[Rule, {segy[[1, 3, -1, 1]], segy[[1, 3, -1, -1, i]]}]


(segy_SEGYData?memorySEGYQ)[key_String?traceHeadersKeyQ, i_Integer, k_String?traceHeaderKeyQ] := 
	ToLowerCase[k] /. MapThread[Rule, {segy[[1, 3, -1, 1]], segy[[1, 3, -1, -1, i]]}]


(segy_SEGYData?memorySEGYQ)[key_String?traceHeadersKeyQ, i_Integer, k: {__String?traceHeaderKeyQ}] := 
	Normal[KeySelect[MapThread[Rule, {segy[[1, 3, -1, 1]], segy[[1, 3, -1, -1, i]]}], MemberQ[ToLowerCase[k], #]&]]


(* Traces *)


(segy_SEGYData?memorySEGYQ)[key_String?tracesKeyQ] := 
	segy[[1, 4, -1]]


(segy_SEGYData?memorySEGYQ)[key_String?tracesKeyQ, i: _Integer | {__Integer} | _Span | All] := 
	segy[[1, 4, -1, i]]


(segy_SEGYData?memorySEGYQ)[key_String?tracesKeyQ, i: _Integer | {__Integer} | _Span | All, j: _Integer | {__Integer} | _Span | All] := 
	segy[[1, 4, -1, i, j]]


(* TraceHeadersUnloaded *)


(segy_SEGYData?delayedSEGYQ)[key_String?traceHeadersUnloadedKeyQ] := 
	segy[[1, 3, -1]]


(segy_SEGYData?delayedSEGYQ)[key_String?traceHeadersUnloadedKeyQ, k_String?(stringEQ["file"])] := 
	segy[[1, 3, -1, 1, -1]]


(segy_SEGYData?delayedSEGYQ)[key_String?traceHeadersUnloadedKeyQ, k_String?(stringEQ["positions"])] := 
	segy[[1, 3, -1, 2, -1]]


(segy_SEGYData?delayedSEGYQ)[key_String?traceHeadersUnloadedKeyQ, i_Integer] := 
	Block[{ud = segy[[1, 3, -1]]}, ud[[-1, -1]] = ud[[-1, -1, i]]; ud]


(segy_SEGYData?delayedSEGYQ)[key_String?traceHeadersUnloadedKeyQ, i: {__Integer} | Span[__]] := 
	Block[{ud = segy[[1, 3, -1]]}, ud[[-1, -1]] = ud[[-1, -1, i]]; ud]


(* TracesUnloaded *)


(segy_SEGYData?delayedSEGYQ)[key_String?tracesUnloadedKeyQ] := 
	segy[[1, 4, -1]]


(segy_SEGYData?delayedSEGYQ)[key_String?tracesUnloadedKeyQ, k_String?(stringEQ["file"])] := 
	segy[[1, 4, -1, 1, -1]]


(segy_SEGYData?delayedSEGYQ)[key_String?tracesUnloadedKeyQ, k_String?(stringEQ["SamplesFormatCode"])] := 
	segy[[1, 4, -1, 2, -1]]


(segy_SEGYData?delayedSEGYQ)[key_String?tracesUnloadedKeyQ, k_String?stringEQ["NumberOfSamplesForReel"]] := 
	segy[[1, 4, -1, 2, -1]]


(segy_SEGYData?delayedSEGYQ)[key_String?tracesUnloadedKeyQ, k_String?(stringEQ["Positions"])] := 
	segy[[1, 4, -1, 2, -1]]


(segy_SEGYData?delayedSEGYQ)[key_String?tracesUnloadedKeyQ, i_Integer] := 
	Block[{ud = segy[[1, 4, -1]]}, ud[[-1, -1]] = ud[[-1, -1, i]]; ud]


(segy_SEGYData?delayedSEGYQ)[key_String?tracesUnloadedKeyQ, i: {__Integer} | Span[__]] := 
	Block[{ud = segy[[1, 4, -1]]}, ud[[-1, -1]] = ud[[-1, -1, i]]; ud]


(* ::Section:: *)
(*SEGYData setters*)


SEGYData /: Set[name_Symbol, segy_SEGYData] := 
	Module[{}, 
		
		(*clear name*)
		ClearAll[name];
		
		(* put object to the variable *)
		Internal`LocalizedBlock[{SEGYData}, name = segy]; 
		
		(* rewrite *)
		name /: Set[name, newValue_] := 
			(ClearAll[name]; name = newValue);
		
		(*TextHeader*)
		
		(*setter: name["textheader"] = "new text header"*)
		name /: Set[name[_?textHeaderKeyQ], newTextHeader_String] := 
			name[[1, 1, 2]] = newTextHeader;
		
		(*BinaryHeader*)
		
		(*setter: name["binaryheader"] = {jobid, linenumber, .. <<25 int values>>}*)
		name /: Set[name[_?binaryHeaderKeyQ], newBinaryHeaderValues: {__Integer}] /; 
			Length[newBinaryHeaderValues] === Length[$SEGYBinaryHeaderSpetification] := 
			name[[1, 2, 2, All, 2]] = newBinaryHeaderValues;
			
		(*setter: name["binaryheader"] = {"jobid" -> 1, "linenumber" -> 1, ..}*)
		name /: Set[name[_?binaryHeaderKeyQ], newBinaryHeaderRules: {(_String -> _Integer)..}] /; 
			SubsetQ[ToLowerCase[$SEGYBinaryHeaderSpetification[[All, 1, 2]]], ToLowerCase[newBinaryHeaderRules[[All, 1]]]] := 
			Module[{
				positions = Map[
					First[FirstPosition[ToLowerCase[$SEGYBinaryHeaderSpetification[[All, 1, 2]]], #]]&, 
					ToLowerCase[newBinaryHeaderRules[[All, 1]]]
				]
			}, 
				name[[1, 2, 2, positions, 2]] = newBinaryHeaderRules[[All, 2]];
				newBinaryHeaderRules
			]; 
			
		(*setter: name["binaryheader", "jobid"] = 1*)
		name /: Set[name[_?binaryHeaderKeyQ, key_String], newValue_Integer] /; 
			MemberQ[ToLowerCase[$SEGYBinaryHeaderSpetification[[All, 1, 2]]], ToLowerCase[key]] := 
			(name["BinaryHeader"] = {key -> newValue}; newValue); 
		
		(*setter: name["binaryheader", {"jobid", "linenumber"}] = {1, 1}*)
		name /: Set[name[_?binaryHeaderKeyQ, keys: {__String}], newValues: {__Integer}] /; 
			SubsetQ[ToLowerCase[$SEGYBinaryHeaderSpetification[[All, 1, 2]]], ToLowerCase[keys]] && 
			Length[keys] === Length[newValues] := 
			(name["BinaryHeader"] = MapThread[Rule, {keys, newValues}]; newValues); 
		
		(*TraceHeaders*)
		
		(*setter: name["traceheaders"] = {{1, 2, 3, .. <<>> ,.. }, {.. }, ..}*)
		name /: Set[name[_?traceHeadersKeyQ], newTraceHeadersValues: {{__Integer}.. }] /; 
			(memorySEGYQ[name] || headerSEGYQ[name]) && 
			MatrixQ[newTraceHeadersValues] && 
			Length[newTraceHeadersValues[[1]]] === Length[$SEGYTraceHeaderSpecification] := 
			name[[1, 3, 2, 2]] = newTraceHeadersValues; 
		
		(*setter: name["traceheaders", 1] = {1, 2, 3, .. <<>> ,.. }*)
		name /: Set[name[_?traceHeadersKeyQ, i_Integer], newTraceHeaderValues: {__Integer}] /; 
			memorySEGYQ[name] && 
			i =!= 0 && 
			Length[newTraceHeaderValues] === Length[$SEGYTraceHeaderSpecification] := 
			name[[1, 3, 2, 2, i]] = newTraceHeaderValues; 
		
		(*setter: name["traceheaders", {1, 2}] = {{1, 2, 3, .. <<>> ,.. }, {..}}*)
		name /: Set[name[_?traceHeadersKeyQ, i: {__Integer} | _Span | All], newTraceHeadersValues: {{__Integer}.. }] /; 
			memorySEGYQ[name] && 
			MatrixQ[newTraceHeadersValues] && 
			Dimensions[newTraceHeadersValues] === Dimensions[name["traceheaders", i]] := 
			name[[1, 3, 2, 2, i]] = newTraceHeadersValues; 
		
		(*setter: name["traceheaders", 1, "xs"] = 1*)
		name /: Set[name[_?traceHeadersKeyQ, i_Integer, key_String], newTraceHeaderValue_Integer] /; 
			memorySEGYQ[name] && 
			i =!= 0 && 
			MemberQ[ToLowerCase[Keys[$SEGYTraceHeaderSpecification]], ToLowerCase[key]] := 
			name[[1, 3, 2, 2, i, Position[ToLowerCase[Keys[$SEGYTraceHeaderSpecification]], ToLowerCase[key]][[1, 1]]]] = newTraceHeaderValue; 
		
		(*setter: name["traceheaders", 1, {"xs", "ys"}] = 1*)
		name /: Set[name[_?traceHeadersKeyQ, i_Integer, keys: {__String}], newTraceHeadersValues: {__Integer}] /; 
			memorySEGYQ[name] && 
			i =!= 0 && 
			SubsetQ[ToLowerCase[Keys[$SEGYTraceHeaderSpecification]], ToLowerCase[keys]] := 
			name[[1, 3, 2, 2, i, Table[Position[ToLowerCase[Keys[$SEGYTraceHeaderSpecification]], ToLowerCase[key]][[1, 1]], {key, keys}]]] = newTraceHeadersValues; 
		
		(*setter: name["traceheaders", {1, 2}, "xs"] = {1, 2}*)
		name /: Set[name[_?traceHeadersKeyQ, i: {__Integer} | _Span | All, key_String], newTraceHeadersValues: {__Integer}] /; 
			memorySEGYQ[name] && 
			Length[name["traceheaders", i, key]] === Length[newTraceHeadersValues] && 
			MemberQ[ToLowerCase[Keys[$SEGYTraceHeaderSpecification]], ToLowerCase[key]] := 
			name[[1, 3, 2, 2, i, Position[ToLowerCase[Keys[$SEGYTraceHeaderSpecification]], ToLowerCase[key]][[1, 1]]]] = newTraceHeadersValues; 
		
		(*setter: name["traceheaders", {1, 2}, {"xs", "ys"}] = 1*)
		name /: Set[name[_?traceHeadersKeyQ, i: {__Integer} | _Span | All, keys: {__String}], newTraceHeadersValues: {{__Integer}.. }] /; 
			memorySEGYQ[name] && 
			Dimensions[name["traceheaders", i, keys]] === Dimensions[newTraceHeadersValues] && 
			SubsetQ[ToLowerCase[Keys[$SEGYTraceHeaderSpecification]], ToLowerCase[keys]] := 
			name[[1, 3, 2, 2, i, Table[Position[ToLowerCase[Keys[$SEGYTraceHeaderSpecification]], ToLowerCase[key]][[1, 1]], {key, keys}]]] = newTraceHeadersValues; 
		
		(* TraceHeadersUnloaded*)
		
		(*setter: name["traceheadersunloaded"] = {"file" -> path}*)
		name /: Set[name[_?traceHeadersUnloadedKeyQ], rules: {(_String -> _).. }] /; 
			delayedSEGYQ[name] && 
			SubsetQ[ToLowerCase[name[[1, 3, 2, All, 1]]], ToLowerCase[rules[[All, 1]]]] && 
			AllTrue[Table[Head[name["traceheadersunloaded", rules[[i, 1]]]] === Head[rules[[i, 2]]], {i, 1, Length[rules]}], TrueQ] := 
			(name[[1, 3, 2, Table[Position[ToLowerCase[name[[1, 3, 2, All, 1]]], ToLowerCase[key]][[1, 1]], {key, rules[[All, 1]]}], 2]] = rules[[All, 2]]; rules); 
		
		(*setter: name["traceheadersunloaded", "file"] = path*)
		name /: Set[name[_?traceHeadersUnloadedKeyQ, key_String], value_] /; 
			delayedSEGYQ[name] := 
			(name["traceheadersunloaded"] = {key -> value}; value);
		
		(*setter: name["traceheadersunloaded", {"file", "positions"}] = {path, {1, 2, 3}}*)
		name /: Set[name[_?traceHeadersUnloadedKeyQ, keys: {__String}], values: {__}] /; 
			delayedSEGYQ[name] := 
			(name["traceheadersunloaded"] = MapThread[Rule, {keys, values}]; values); 
		
		(*Traces*)
		
		(*setter: name["traces"] = {{1, 2, 3, .. <<>>, ..}, ..}*)
		name /: Set[name[_?tracesKeyQ], newTracesValues: {{__Integer}.. } | {{__Real}.. }] /; 
			(memorySEGYQ[name] || headerSEGYQ[name]) && 
			MatrixQ[newTracesValues] := 
			name[[1, 4, 2]] = newTracesValues; 
		
		(*setter: name["traces", 1] = {1, 2, 3, .. <<>>, ..}*)
		name /: Set[name[_?tracesKeyQ, i_Integer], newTraceValues: {__Integer} | {__Real}] /; 
			memorySEGYQ[name] && 
			i =!= 0 && 
			Length[name["traces", i]] === Length[newTraceValues] && 
			Head[newTraceValues[[1]]] === Head[name["traces", i, 1]] := 
			name[[1, 4, 2, i]] = newTraceValues; 
			
		(*setter: name["traces", {1, 2, ..}] = {{1, 2, .. <<>>, ..}, {..}, ..}*)
		name /: Set[name[_?tracesKeyQ, i: {__Integer} | _Span | All], newTracesValues: {{__Integer}.. } | {{__Real}.. }] /; 
			memorySEGYQ[name] && 
			MatrixQ[newTracesValues] && 
			Dimensions[name["traces", i]] == Dimensions[newTracesValues] && 
			Head[newTracesValues[[1]]] === Head[name["traces", i, 1]] := 
			name[[1, 4, 2, i]] = newTracesValues; 
		
		(*setter: name["traces", 1, 1] = 1*)
		name /: Set[name[_?tracesKeyQ, i_Integer, j_Integer], newTraceValue: _Integer | _Real] /; 
			memorySEGYQ[name] && 
			i =!= 0 && 
			j =!= 0 && 
			Head[newTraceValue] === Head[name["traces", i, j]] := 
			name[[1, 4, 2, i, j]] = newTraceValue; 
			
		(*setter: name["traces", {1, 2}, 1] = {1, 2}*)
		name /: Set[name[_?tracesKeyQ, i: {__Integer} | _Span | All, j_Integer], newTracesValues: {__Integer} | {__Real}] /; 
			memorySEGYQ[name] && 
			Length[name["traces", i, j]] == Length[newTracesValues] && 
			Head[newTracesValues[[1]]] === Head[name["traces", i, j][[1]]] := 
			name[[1, 4, 2, i, j]] = newTracesValues; 
		
		(*setter: name["traces", 1, {1, 2, 3}] = {1, 2, 3}*)
		name /: Set[name[_?tracesKeyQ, i_Integer, j: {__Integer} | _Span | All], newTraceValues: {__Integer} | {__Real}] /; 
			memorySEGYQ[name] && 
			i =!= 0 && 
			Length[newTraceValues] === Length[name["traces", i, j]] && 
			Head[newTraceValues[[1]]] === Head[name["traces", i, j][[1]]] := 
			name[[1, 4, 2, i, j]] = newTraceValues; 
		
		(*setter: name["traces", {1, 2}, {1, 2}] = {{1, 2}, {1, 2}}*)
		name /: Set[name[_?tracesKeyQ, i: {__Integer} | _Span | All, j: {__Integer} | _Span | All], newTracesValues: {{__Integer}.. } | {{__Real}.. }] /; 
			memorySEGYQ[name] && 
			Dimensions[name["traces", i, j]] == Dimensions[newTracesValues] && 
			Head[newTracesValues[[1, 1]]] === Head[name["traces", i, j][[1, 1]]] := 
			name[[1, 4, 2, i, j]] = newTracesValues;
		
		(* TracesUnloaded *)
		
		(*setter: name["tracesunloaded"] = {"file" -> path}*)
		name /: Set[name[_?tracesUnloadedKeyQ], rules: {(_String -> _).. }] /; 
			delayedSEGYQ[name] && 
			SubsetQ[ToLowerCase[name[[1, 4, 2, All, 1]]], ToLowerCase[rules[[All, 1]]]] && 
			AllTrue[Table[Head[name["tracesunloaded", rules[[i, 1]]]] === Head[rules[[i, 2]]], {i, 1, Length[rules]}], TrueQ] := 
			(name[[1, 4, 2, Table[Position[ToLowerCase[name[[1, 4, 2, All, 1]]], ToLowerCase[key]][[1, 1]], {key, rules[[All, 1]]}], 2]] = rules[[All, 2]]; rules); 
		
		(*setter: name["tracesunloaded"] = {"file" -> path}*)
		name /: Set[name[_?tracesUnloadedKeyQ, key_String], value_] /; 
			delayedSEGYQ[name] := 
			(name["tracesunloaded"] = {key -> value}; value);
		
		(*setter: name["tracesunloaded", {"file", "positions"}] = {path, {1, 2, 3}}*)
		name /: Set[name[_?tracesUnloadedKeyQ, keys: {__String}], values: {__}] /; 
			delayedSEGYQ[name] := 
			(name["tracesunloaded"] = MapThread[Rule, {keys, values}]; values); 
		
		(* ERROR *)
		
		name /: Set[name[_], _] := 
			(Message[SEGYData::seterr]; Null);
		
		(*return value*)
		Return[name]
	]


(* ::Section:: *)
(*SEGYData format*)


$segyFormatIcon = 
	Hyperlink[
		Import[FileNameJoin[{DirectoryName[$InputFileName], "Images", "SEGLogo.png"}]], 
		"https://seg.org/Portals/0/SEG/News%20and%20Resources/Technical%20Standards/seg_y_rev1.pdf"
	]


segyFormatAbove[segy_SEGYData?memorySEGYQ] := 
	{
		{
			BoxForm`SummaryItem[{"Loading: ", "Memory"}], 
			BoxForm`SummaryItem[{"DataSize: ", Quantity[ByteCount[segy]/1024.0^2, "Megabytes"]}],  
			SpanFromLeft
		}, 
		{
			BoxForm`SummaryItem[{"Format: ", Row[{segy["BinaryHeader", "SamplesFormatCode"], ": ", Head[segy[[1, -1, -1, 1, 1]]]}]}], 
			BoxForm`SummaryItem[{"NTraces: ", Row[{segy["BinaryHeader", "NumberDataTraces"], ": ", Length[segy[[1, -1, -1]]]}]}], 
			BoxForm`SummaryItem[{"TraceLen: ", Row[{segy["BinaryHeader", "NumberOfSamplesForReel"], ": ", Length[segy[[1, -1, -1, 1]]]}]}], 
			BoxForm`SummaryItem[{"TimeStep: ", segy["BinaryHeader", "IntervalReelRecord"]}], 
			SpanFromLeft
		}
	}


segyFormatAbove[segy_SEGYData?delayedSEGYQ] := 
	{
		{
			BoxForm`SummaryItem[{"Loading: ", "Delayed"}], 
			BoxForm`SummaryItem[{"FileName: ", FileNameTake[segy["TracesUnloaded", "File"]]}], 
			BoxForm`SummaryItem[{"FileSize: ", Quantity[FileByteCount[segy["TracesUnloaded", "File"]]/1024.0^2, "Megabytes"]}], 
			SpanFromLeft
		}, 
		{
			BoxForm`SummaryItem[{"Format: ", Row[{#, ": ", If[MatchQ[#, 2 | 3 | 8], Integer, Real]}]&[segy["BinaryHeader", "SamplesFormatCode"]]}], 
			BoxForm`SummaryItem[{"NTraces: ", segy["BinaryHeader", "NumberDataTraces"]}], 
			BoxForm`SummaryItem[{"TraceLen: ", segy["BinaryHeader", "NumberOfSamplesForReel"]}], 
			BoxForm`SummaryItem[{"TimeStep: ", segy["BinaryHeader", "IntervalReelRecord"]}], 
			SpanFromLeft
		}
	}


segyFormatAbove[segy_SEGYData?headerSEGYQ] := 
	{
		{
			BoxForm`SummaryItem[{"Loading: ", "Header"}], 
			SpanFromLeft
		}, 
		{
			BoxForm`SummaryItem[{"Format: ", Row[{#, ": ", If[MatchQ[#, 2 | 3 | 8], Integer, Real]}]&[segy["BinaryHeader", "SamplesFormatCode"]]}], 
			BoxForm`SummaryItem[{"NTraces: ", Row[{segy["BinaryHeader", "NumberDataTraces"]}]}], 
			BoxForm`SummaryItem[{"TraceLen: ", Row[{segy["BinaryHeader", "NumberOfSamplesForReel"]}]}], 
			BoxForm`SummaryItem[{"TimeStep: ", segy["BinaryHeader", "IntervalReelRecord"]}], 
			SpanFromLeft
		}
	}


segyFormatAbove[segy_SEGYData] := 
	{
		{
			BoxForm`SummaryItem[{"Error", ""}], 
			SpanFromLeft
		}, 
		{
			BoxForm`SummaryItem[{"Message: ", "Not valid SEGYData"}], 
			SpanFromLeft
		}
	}


segyFormatBelow[segy: _SEGYData?memorySEGYQ | _SEGYData?delayedSEGYQ | _SEGYData?headerSEGYQ] := 
	{
		{BoxForm`SummaryItem[{"TextHeader: \n", segy["TextHeader"]}], SpanFromLeft}, 
		{BoxForm`SummaryItem[{"BinaryHeader: \n", ToString[Column[Map[Row[{#[[1]], ": ", #[[2]]}]&, segy["BinaryHeader"]]]]}], SpanFromLeft}
	}


segyFormatBelow[segy_SEGYData] := 
	{}


SEGYData /: MakeBoxes[segy_SEGYData, form: StandardForm | TraditionalForm] := 
	BoxForm`ArrangeSummaryBox[
		SEGYData, segy, 
		$segyFormatIcon, segyFormatAbove[segy], segyFormatBelow[segy], 
		form, "Interpretable" -> Automatic
	]


(* ::Section:: *)
(*LAS Def*)


SetAttributes[lasInfoLineParse, Listable]


lasInfoLineParse[line_String] := StringCases[line, key__ ~~ {".", "." ~~ _} ~~ Whitespace.. ~~ value__ ~~ ":" :> {key -> value}]


lasGetVersion[versioninfo: {__String}] := Association[Flatten[DeleteCases[lasInfoLineParse[versioninfo], {}]]]


lasVersionStartQ[line_String] := StringMatchQ[line, "~VERSION INFORMATION"]


lasGetWell[wellheader: {__String}] := Association[Flatten[DeleteCases[lasInfoLineParse[wellheader], {}]]]


lasWellStartQ[line_String] := StringMatchQ[line, "~WELL INFORMATION"]


(*Info*)


lasInfoStartQ[___] := 
	False


lasInfoStartQ[line_String] := 
	StringMatchQ[line, "~CURVE INFORMATION"]


lasGetInfo[curveheader: {__String}] := 
	Association[Flatten[DeleteCases[lasInfoLineParse[curveheader], {}]]]


(*Data*)


lasDataStartQ[___] := 
	False


lasDataStartQ[line_String] := 
	StringMatchQ[line, "~A " ~~ __]


lasGetData[headers: {__String}, data: {__String}] := 
	{
		Flatten[StringSplit[StringTrim[StringTrim[headers, "~A "]], Whitespace..]],
		ImportString[StringRiffle[data, "\n"], "Table"]
	};


(*LASImport*)


LASImport[path_String?FileExistsQ] /; StringMatchQ[FileExtension[path], "las"] := 
	Module[
		{lines = ReadList[path, String], version = {}, well = {}, curve = {}, headers = {}, data = {}, line = "", current = "", i = 0}, 

		While[Not[lasDataStartQ[line]], 
			i = i + 1;
			line = lines[[i]];
			If[
				lasVersionStartQ[line] || lasWellStartQ[line] || lasInfoStartQ[line] || lasDataStartQ[line], 
				current = line
			];
			Which[
				lasVersionStartQ[current], AppendTo[version, line], 
				lasWellStartQ[current], AppendTo[well, line], 
				lasInfoStartQ[current], AppendTo[curve, line], 
				lasDataStartQ[current], AppendTo[headers, line]
			]; 
			data = lines[[i + 1 ;; -1]];
		];
				
		(*Return*)
		LASData[<|
			"Version" -> lasGetVersion[version],
			"Well" -> lasGetWell[well],
			"Info" -> lasGetInfo[curve],
			"Data" -> lasGetData[headers, data]
		|>]
	]


(* ::Section:: *)
(*LASData*)


LASData[data_Association][key_String] /; 
	MemberQ[ToLowerCase[Keys[data]], ToLowerCase[key]]:= 
	First[KeySelect[data, StringMatchQ[#, key, IgnoreCase -> True]&]]


(* ::Section:: *)
(*LASData representation*)


$lasFormatIcon = 
	None


lasFormatAbove[las_LASData] := 
	{
		{BoxForm`SummaryItem[{"Version: ", las[[1]]["Version", "VERS"]}], BoxForm`SummaryItem[{"Wrap: ",  las[[1]]["Version", "WRAP"]}], SpanFromLeft}
	}


lasFormatBelow[las_LASData] := 
	{}
	(*here we have to add common information about file content*)
	(*KeyValueMap[{BoxForm`SummaryItem[{#1 <> ": ",  #2}]}&, las[[1, 2]]]*)
	


LASData /: MakeBoxes[las_LASData, form: StandardForm | TraditionalForm] := 
	BoxForm`ArrangeSummaryBox[
		LASData, las, 
		$lasFormatIcon, lasFormatAbove[las], lasFormatBelow[las], 
		form, "Interpretable" -> Automatic
	]


(* ::Section:: *)
(*ZMAP*)


ZMAPGridCommenStartQ[___] := False


ZMAPGridCommenStartQ[line_String] := StringMatchQ[line, "!" ~~ ___ ~~ EndOfString]


ZMAPGridHeaderStartQ[line_String] := StringMatchQ[line, "@GRID_HEADER" ~~ ___]


ZMAPGridDataStartQ[line_String] := StringMatchQ[line, Verbatim["@ "] ~~ ___ ~~ EndOfString]


ZMAPGridComment[gridcomment: {__String}] := 
	StringRiffle[gridcomment, "\n"]


ZMAPGridHeader[gridheader: {__String}]:=
	Block[{header},
		header = Map[StringSplit[#, ","]&, gridheader];
		<|
			"nanvalue" -> Internal`StringToDouble[header[[2,2]]],
			"nrows" -> Round@Internal`StringToDouble[header[[3,1]]],
			"ncols" -> Round@Internal`StringToDouble[header[[3,2]]],
			"minx" -> Internal`StringToDouble[header[[3,3]]],
			"maxx" -> Internal`StringToDouble[header[[3,4]]],
			"miny" -> Internal`StringToDouble[header[[3,5]]],
			"maxy" -> Internal`StringToDouble[header[[3,6]]]
		|>
	]


ZMAPGridData[data: {__Real}, header_Association] := 
	ArrayReshape[data, {header["ncols"],header["nrows"]}]


ZMAPGridImport[path_String?FileExistsQ] /; 
	StringMatchQ[FileExtension[path],"zmap"] := 
	Module[{
		stream, line, 
		comment = {}, header = {}, data = {}, 
		isheader = False, flag = True, position, headerValues
	}, 
		stream = OpenRead[path];
	
		While[flag, 
			line = ReadLine[stream]; 
			If[ZMAPGridCommenStartQ[line], AppendTo[comment, line]];
			If[ZMAPGridHeaderStartQ[line], isheader = True]; 
			If[isheader, AppendTo[header, line]]; 
			If[ZMAPGridDataStartQ[line], flag = False; Break[]];
		];

		position = StringLength[StringRiffle[Join[comment, header], "\n"]];
		SetStreamPosition[stream, position + 1]; 
		data = ReadList[stream, Real]; 
		
		Close[stream];

		headerValues = ZMAPGridHeader[header];

		(*Return*)
		<|
			"comment" -> ZMAPGridComment[comment], 
			"header" -> ZMAPGridHeader[header], 
			"data" -> ZMAPGridData[data, headerValues]
		|>
	]


(* ::Section:: *)
(*Irap*)


IrapIdQ[___] := False


IrapIdQ[line_String] := StringMatchQ[line, StartOfString ~~ "" ~~ ___ ~~ EndOfString]


IrapGridHeader[gridheader : {__String}] := 
	Block[{header}, header = Map[StringSplit[#] &, gridheader];
		<|
			"nanvalue" -> -999,
			"nrows" -> Round@Internal`StringToDouble[header[[1, 2]]], 
			"ncols" -> Round@Internal`StringToDouble[header[[3, 1]]],
			"rotangle" -> Internal`StringToDouble[header[[3, 2]]],
			"rotox" -> Internal`StringToDouble[header[[3, 3]]], 
   "rotoy" -> Internal`StringToDouble[header[[3, 4]]],
   "minx" -> Internal`StringToDouble[header[[2, 1]]], 
   "maxx" -> Internal`StringToDouble[header[[2, 2]]], 
   "miny" -> Internal`StringToDouble[header[[2, 3]]], 
   "maxy" -> Internal`StringToDouble[header[[2, 4]]],
   "stepx" -> Internal`StringToDouble[header[[1, 3]]],
   "stepy" -> Internal`StringToDouble[header[[1, 4]]]|>]


IrapGridData[data : {__Real}, header_Association] := 
  ArrayReshape[data, {header["ncols"], header["nrows"]}];


IrapGridImport[path_String?FileExistsQ] /; 
  StringMatchQ[FileExtension[path], "irap"] := 
 Module[{stream, line, linenum = 1, header = {}, data = {}, 
    position, headerValues}, stream = OpenRead[path];
  While[linenum <= 4,
   line = ReadLine[stream];
   header = AppendTo[header, line];
   linenum = linenum + 1];
  
  position = StringLength[StringRiffle[header, "\n"]];
  SetStreamPosition[stream, position + 1];
  data = ReadList[stream, Real];
  Close[stream];
  headerValues = IrapGridHeader[header];
  <|"header" -> IrapGridHeader[header], 
   "data" -> IrapGridData[data, headerValues]|>]


(* ::Section:: *)
(*End*)


End[] (*`Private`*)


(* ::Section:: *)
(*End package*)


EndPackage[] (*GeologyIO`*)
