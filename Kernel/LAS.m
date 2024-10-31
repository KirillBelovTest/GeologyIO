(* ::Package:: *)


BeginPackage["GeologyIO`LAS`"]


(* ::Section::Closed:: *)
(*Public names declaration*)


LASData::usage = 
"LASData[Version, Well, CurveInfo, CurveData]"


LASImport::usage = 
"LASImport[\"file.las\", options]"


LASExport::usage = 
"LASExport[\"file.las\", lasdata, options]"


(* ::Section::Closed:: *)
(*Private context*)


Begin["`Private`"]


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
				lasVersionStartQ[current], AppendTo[well, line], 
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


(* ::Section::Closed:: *)
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


(* ::Section::Closed:: *)
(*End private context*)


End[]


(* ::Section::Closed:: *)
(*End package*)


EndPackage[]