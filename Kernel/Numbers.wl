(* :Package: *)

BeginPackage["KirillBelov`GeologyIO`Numbers`", {
	"LibraryLink`"
}]; 


(*Names*)


ToIBMFloat32::usage = 
"ToIBMFloat32[numbers] returns byte array in IBM Float 32 format."; 


FromIBMFloat32::usage = 
"ToIBMFloat32[byteArray] returns numbers from IBM Float 32 byte array."; 


Begin["`Private`"]; 


(*Public*)


ToIBMFloat32[numbers_?(VectorQ[#, NumericQ]&)] := 
If[Developer`PackedArrayQ[numbers], 
	toIBMFloat32[numbers, Length[numbers]], 
(*Else*)
	toIBMFloat32[Developer`ToPackedArray[numbers], Length[numbers]]
]; 


FromIBMFloat32[byteArray_?ByteArrayQ] := 
fromIBMFloat32[byteArray, Length[byteArray]]; 


(*Internal*)


toIBMFloat32 := toIBMFloat32 = 
LibraryFunctionLoad[$lib, "toIBMFloat32", {{_Real, 1, "Shared"}, Integer}, "ByteArray"]; 


fromIBMFloat32 := fromIBMFloat32 = 
LibraryFunctionLoad[$lib, "fromIBMFloat32", {{"ByteArray", "Shared"}, Integer}, {_Real, 1}]; 


(*Very Internal*)


$lib := $lib = 
With[{$libDir = FileNameJoin[{
			$directory, 
			"LibraryResources", 
			$SystemID <> "-v" <> ToString[$libraryLinkVersion]
	}]
}, 
	Internal`InheritedBlock[{$LibraryPath = Append[$LibraryPath, $libDir]}, 
		FindLibrary["numbers"]
	]
]; 


With[{$inputFileName = $InputFileName}, 
	$directory := $directory = DirectoryName[$inputFileName, 2]; 
]; 


$libraryLinkVersion := $libraryLinkVersion = 
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


End[(*`Private`*)]; 


EndPackage[(*KirillBelov`GeologyIO`Numbers`*)]; 