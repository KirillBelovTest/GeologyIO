(* ::Package:: *)

BeginPackage["WLJS`GeologyIO`IBM32Float`", {
    "CCompilerDriver`", "LibraryLink`"
}];


IBM32RealToByteArray::usaga =
"IBM32RealToByteArray[numbers] returns ByteArray[<>] with IBM 32 Float format.";


IBM32ByteArrayToReal::usaga =
"IBM32ByteArrayToReal[byteArray] returns List of Real numbers.";


Begin["`Private`"];


IBM32RealToByteArray[numbers: {__Real}] :=
toIBMFloat32[numbers, Length[numbers]];


IBM32ByteArrayToReal[byteArray_ByteArray] :=
fromIBMFloat32[byteArray, Length[byteArray]];


$directory =
DirectoryName[$InputFileName, 2];


$libraryLinkVersion =
LibraryVersionInformation[FindLibrary["demo"]]["WolframLibraryVersion"];


$libDir =
FileNameJoin[{
    $directory,
    "LibraryResources",
    $SystemID <> "-v" <> ToString[$libraryLinkVersion]
}];


$lib =
Block[{$LibraryPath = $libDir}, FindLibrary["geologyio"]];


toIBMFloat32 =
LibraryFunctionLoad[$lib, "toIBMFloat32", {{_Real, 1, "Constant"}, Integer}, "ByteArray"];


fromIBMFloat32 =
LibraryFunctionLoad[$lib, "fromIBMFloat32", {{"ByteArray", "Shared"}, Integer}, {_Real, 1}];


cToIBMFloat32 = Compile[{{numbers, _Real, 1}},
    Module[{
        number = 0.0,
        absNum = 0.0,
        rsign = 0,
        exp = 0,
        firstbyte = 0,
        mantissa = 0.0,
        fractbytes = {0, 0, 0},
        bytes = Table[0, {4 * Length[numbers]}]
    },
        Table[
            number = numbers[[i]];
            absNum = Abs[number];

            If[absNum == 0.0,
                bytes[[4i - 3 ;; 4i]] = {0, 0, 0, 0},

                If[absNum > 10.0^-75 && absNum < 10.0^75,
                    (* Sign bit *)
                    rsign = Boole[number < 0];

                    (* Calculate exponent (base 16) *)
                    exp = Floor[Log[16, absNum]];
                    mantissa = absNum / (16.0^exp);

                    (* Normalize mantissa to [1/16, 1) *)
                    If[mantissa >= 1.0,
                        mantissa = mantissa / 16.0;
                        exp = exp + 1;
                    ];

                    (* First byte: sign + (exponent + bias 64) *)
                    firstbyte = exp + 64 + rsign * 128;

                    (* Fraction: 24-bit mantissa *)
                    fractbytes = IntegerDigits[
                        Floor[mantissa * 256.0^3 + 0.5],
                        256,
                        3
                    ];

                    (* Store result *)
                    bytes[[4i - 3]] = firstbyte;
                    bytes[[4i - 2 ;; 4i]] = fractbytes;
                ]
            ],

            {i, 1, Length[numbers]}
        ];

        bytes
    ],
    CompilationTarget -> "C",
    RuntimeOptions -> "Speed"
];





cFromIBMFloat32 = Compile[{{bytes, _Integer, 1}},
    Module[{sign, exp, fract, firstByte},
        Table[
            firstByte = bytes[[i]];

            (* Sign: MSB = 1 for negative *)
            sign = If[firstByte >= 128, -1.0, 1.0];

            (* Exponent: 7 bits, bias 64, base 16 *)
            exp = 16.0^(BitAnd[firstByte, 127] - 64);

            (* Fraction: 24-bit mantissa normalized to [0, 1) *)
            fract = (
                bytes[[i + 1]] * 65536.0 +
                bytes[[i + 2]] * 256.0 +
                bytes[[i + 3]]
            ) / 16777216.0;

            (* Zero is represented by all bits zero *)
            If[firstByte == 0 && bytes[[i + 1]] == 0 &&
               bytes[[i + 2]] == 0 && bytes[[i + 3]] == 0,
                0.0,
                sign * exp * fract
            ],

            {i, 1, Length[bytes], 4}
        ]
    ],
    CompilationTarget -> "C",
    RuntimeOptions -> "Speed"
];


End[]; (*`Private`s*)


EndPackage[] (*WLJS`GeologyIO`IBM32Float`*)