(* :Package: *)

BeginPackage["KirillBelov`GeologyIO`EBCDIC`"]; 


(*Names*)


EBCDICToString::usage = 
"ToIBMFloat32[numbers] returns byte array in IBM Float 32 format."; 


StringToEBCDIC::usage = 
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


End[(*`Private`*)]; 


EndPackage[(*KirillBelov`GeologyIO`Numbers`*)]; 