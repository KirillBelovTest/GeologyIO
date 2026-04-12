(* :Package: *)

BeginPackage["WLJS`GeologyIO`IO`", {
    "WLJS`GeologyIO`Library`",
    "CCompilerDriver`",
    "LibraryLink`"
}];


openFile::usage =
"openFile[\"path\"] returns file stream pointer.";


readByteArray::usage =
"readByteArray[file, {positions}, partSize] read and return byte array.";


writeByteArray::usage =
"writeByteArray[file, byteArray, {positions}, partSize] write byte array.";

closeFile::usage =
"closeFile[stream] close file stream.";


Begin["`Private`"];


openFile =
LibraryFunctionLoad[$GeologyIOLibrary, "openFile", {String}, Integer];


readByteArray =
LibraryFunctionLoad[$GeologyIOLibrary, "readByteArray", {Integer, {Integer, 1}, Integer}, LibraryDataType[ByteArray]];


writeByteArray =
LibraryFunctionLoad[$GeologyIOLibrary, "writeByteArray", {Integer, LibraryDataType[ByteArray], {Integer, 1}, Integer}, "Void"];


closeFile =
LibraryFunctionLoad[$GeologyIOLibrary, "closeFile", {Integer}, "Void"];


End[];


EndPackage[];
