(* ::Package:: *)

PacletObject[
  <|
    "Name" -> "KirillBelov/GeologyIO",
    "Description" -> "Importing and exporting for specific geology file formats",
    "Creator" -> "Kirill Belov <kirillbelovtest@gmail.com>",
    "URL" -> "https://resources.wolframcloud.com/PacletRepository/resources/KirillBelov/GeologyIO",
    "SourceControlURL" -> "https://github.com/KirillBelovTest/GeologyIO",
    "License" -> "MIT",
    "PublisherID" -> "KirillBelov",
    "Version" -> "1.0.0",
    "WolframVersion" -> "13.0+",
    "ActionURL" -> "$ACTION_URL$",
    "Dependencies" -> {},
    "Extensions" -> {
      {
        "Kernel",
        "Root" -> "Kernel",
        "Context" -> {
          {"KirillBelov`GeologyIO`", "GeologyIO.wl"}, 
          {"KirillBelov`GeologyIO`Numbers`", "Numbers.wl"}
        },
        "Symbols" -> {
          "KirillBelov`GeologyIO`Numbers`ToIBMFloat32", 
          "KirillBelov`GeologyIO`Numbers`FromIBMFloat32"
        }
      },
      {
        "Documentation",
        "Root" -> "Documentation",
        "Language" -> "English"
      },
      {
        "Asset",
        "Assets" -> {
          {"License", "./LICENSE"},
          {"ReadMe", "./README.md"},
          {"Images", "./Images"},
          {"Examples", "./Examples"}
        }
      }
    },
    "Icon" -> "Images/PacletIcon.png",
    "ReleaseDate" -> "$RELEASE_DATE$",
    "ReleaseID" -> "$RELEASE_ID$",
    "ReleaseURL" -> "$RELEASE_URL$"
  |>
]
