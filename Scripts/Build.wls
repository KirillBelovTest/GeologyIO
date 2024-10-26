#!/usr/bin/env wolframscript
(* ::Package:: *)

Get["CCompilerDriver`"]; 
Get["LibraryLink`"]; 


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


build[lib_String] := Block[{$directory, $libSrc, $libDir, $linkerOptions}, 
	$directory = DirectoryName[If[$InputFileName == "", 
		NotebookFileName[], 
		$InputFileName
	], 2]; 

	$libSrc = File[FileNameJoin[{
		$directory, 
		"Source", 
		lib <> ".c"
	}]]; 

	$libDir = FileNameJoin[{
		$directory, 
		"LibraryResources", 
		$SystemID <> "-v" <> ToString[$libraryLinkVersion]
	}]; 


	If[!FileExistsQ[$libDir], CreateDirectory[]];

	CreateLibrary[$libSrc, lib, 
		"TargetDirectory" -> $libDir, 
		"Debug" -> False
	]
]; 


build["numbers"]; 
