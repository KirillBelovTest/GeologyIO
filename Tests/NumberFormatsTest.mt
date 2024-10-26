(* Wolfram Language Test file *)

(* Int 32 converter *)

Test[
	GeologyIO`SEGY`Private`toInt32["MVM"][{1 - 2^23, -1, 0, 1, 2^23}]
	,
	{255, 128, 0, 1, 255, 255, 255, 255, 0, 0, 0, 0, 0, 0, 0, 1, 0, 128, 0, 0}
	,
	TestID->"NumberFormatsTest-toInt32"
]

Test[
	GeologyIO`SEGY`Private`fromInt32["MVM"][{255, 128, 0, 1, 255, 255, 255, 255, 0, 0, 0, 0, 0, 0, 0, 1, 0, 128, 0, 0}]
	,
	{1 - 2^23, -1, 0, 1, 2^23}
	,
	TestID->"NumberFormatsTest-fromInt32"
]

(* Int 16 converter *)

Test[
	GeologyIO`SEGY`Private`toInt16["MVM"][{-2^15, -1, 0, 1, 2^15 - 1}]
	,
	{128, 0, 255, 255, 0, 0, 0, 1, 127, 255}
	,
	TestID->"NumberFormatsTest-toInt16"
]

Test[
	GeologyIO`SEGY`Private`fromInt16["MVM"][{128, 0, 255, 255, 0, 0, 0, 1, 127, 255}]
	,
	{-2^15, -1, 0, 1, 2^15 - 1}
	,
	TestID->"NumberFormatsTest-fromInt16"
]

(* Int 8 converter *)

Test[
	GeologyIO`SEGY`Private`toInt8["MVM"][{-2^7, -1, 0, 1, 2^7 - 1}]
	,
	{255, 128, 0, 1, 127}
	,
	TestID->"NumberFormatsTest-toInt8"
]

Test[
	GeologyIO`SEGY`Private`fromInt8["MVM"][{255, 128, 0, 1, 127}]
	,
	{-128, -1, 0, 1, 127}
	,
	TestID->"NumberFormatsTest-fromInt8"
]

(* IBM Float 32 converter*)

Test[
	GeologyIO`SEGY`Private`toIBMFloat32["MVM"][{-1.0, 0.0, 2.0}]
	,
	{192, 0, 0, 0, 0, 0, 0, 0, 65, 32, 0, 0}
	,
	TestID->"NumberFormatsTest-toIBMFloat32"
]

Test[
	GeologyIO`SEGY`Private`fromIBMFloat32["MVM"][{192, 0, 0, 0, 0, 0, 0, 0, 65, 32, 0, 0}]
	,
	{-1.0, 0.0, 2.0}
	,
	TestID->"NumberFormatsTest-fromIBMFloat32"
]