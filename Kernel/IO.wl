(* :Package: *)

BeginPackage["WLJS`GeologyIO`IO`", {
    "CCompilerDriver`",
    "LibraryLink`"
}];


GeologyIOOpenFile::usage =
"GeologyIOOpenFile[path] returns opened file.";


Begin["`Private`"];


GeologyIOOpenFile[path: _String | _File] :=
openFile[AbsoluteFileName[path]];


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


openFile =
LibraryFunctionLoad[$library, "openFile", {String}, Integer];


End[(*`Private`*)];


EndPackage[(*KirillBelov`GeologyIO`SEGY`*)];