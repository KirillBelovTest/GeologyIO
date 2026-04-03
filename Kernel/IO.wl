(* :Package: *)

BeginPackage["WLJS`GeologyIO`IO`", {
    "WLJS`GeologyIO`Library`",
    "CCompilerDriver`",
    "LibraryLink`"
}];


Begin["`Private`"];


openFile::usage =
"openFile[path] returns file stream pointer.";


openFile =
LibraryFunctionLoad[$GeologyIOLibrary, "openFile", {String}, Integer];


readByteArray::usage =
"readByteArray[stream, positions, counts, length] returns byte array.";


readByteArray =
LibraryFunctionLoad[$GeologyIOLibrary, "readByteArray", {Integer, {Integer, 1}, {Integer, 1}, Integer}, LibraryDataType[ByteArray]];


closeFile::usage =
"closeFile[stream] close file stream.";


closeFile =
LibraryFunctionLoad[$GeologyIOLibrary, "closeFile", {Integer}, "Void"];


End[(*`Private`*)];


EndPackage[(*KirillBelov`GeologyIO`SEGY`*)];