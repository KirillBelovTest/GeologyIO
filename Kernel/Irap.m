(* ::Package:: *)

(* ::Title:: *)
(*SEGY*)


(* ::Section:: *)
(*Info*)


(* :Title: Irap *)
(* :Context: GeologyIO`Irap` *)
(* :Version: 0.0.2 *)
(* :Keywords: Irap; Irap Grid; *)
(* :Authors: Anton Ekimenko; Kirill Belov *)
(* :Developer: Anton Ekimenko*)
(* :Email: KirillBelovTest@gmail.com *)
(* :Description: 
	
*)


(* ::Section:: *)
(*Code*)

BeginPackage["GeologyIO`Irap`"]


IrapGridImport::usage = 
"IrapGridImport[\"path\"]"


Begin["`Private`"]


IrapIdQ[___] := False

IrapIdQ[line_String] := StringMatchQ[line, StartOfString~~"" ~~ ___ ~~ EndOfString]




IrapGridHeader[gridheader : {__String}] := 
 Block[{header}, header = Map[StringSplit[#] &, gridheader];
  <|"nanvalue" -> -999,
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


End[] (*`Private`*)


EndPackage[] (*GeologyIO`Irap`*)
