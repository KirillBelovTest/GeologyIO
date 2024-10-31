(* ::Package:: *)

(* ::Title:: *)
(*SEGY*)


(* ::Section:: *)
(*Info*)


(* :Title: ZMap *)
(* :Context: GeologyIO`ZMap` *)
(* :Version: 0.0.2 *)
(* :Keywords: ZMAP; ZMap Grid; *)
(* :Authors: Anton Ekimenko; Kirill Belov *)
(* :Developer: Anton Ekimenko*)
(* :Email: KirillBelovTest@gmail.com *)
(* :Description: 
	
*)


(* ::Section:: *)
(*Code*)


BeginPackage["GeologyIO`ZMap`"]


ZMAPGridImport::usage = 
"ZMAPGridImport[\"path\"]"


Begin["`Private`"]


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


End[] (*`Private`*)


EndPackage[] (*GeologyIO`ZMap`*)
