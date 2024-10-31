(* ::Package:: *)

(* GUI source file *)

Widget["Frame", {
	Widget["MenuBar", {
		Widget["Menu", {
			"Text" -> "File", 
			Widget["MenuItem", {
        		"Text" -> "Open", 
        		BindEvent["Action", 
					Script[
          				Widget["FileDialog", Name -> "openFileDialog"];
					
						SetPropertyValue[{"openFileDialog", "multiSelectionEnabled"}, False];
						SetPropertyValue[{"openFileDialog", "fileSelectionMode"}, PropertyValue[{"openFileDialog", "FILES_ONLY"}]]; 
						
						If[InvokeMethod[{"openFileDialog", "showOpenDialog"}, Null] === PropertyValue[{"openFileDialog", "APPROVE_OPTION"}], 
							segyImport[PropertyValue[{PropertyValue[{"openFileDialog", "selectedFile"}], "path"}]], 
							Null
						]
					]
				]
			}]
		}], 
		Widget["Menu", {
			"Text" -> "Data", 
			Widget["MenuItem", {
        		"Text" -> "ArrayPlot(Traces)", 
        		BindEvent["Action", 
        			Script[If[Head[SEGYViewer`segy] === GeologyIO`SEGY`SEGYData, 
        				SetPropertyValue[{"SEGYArrayPlot", "Data"}, ExportString[ArrayPlot[SEGYViewer`segy["traces"]], "GIF"]]]]
        		]
			}]
		}], 
		Widget["Menu", {
			"Text" -> "Fiter", 
			Widget["MenuItem", {
        		"Text" -> "Gaussian(5)", 
        		BindEvent["Action", 
        			Script[If[Head[SEGYViewer`segy] === GeologyIO`SEGY`SEGYData, 
        				SetPropertyValue[{"SEGYArrayPlot", "Data"}, ExportString[ArrayPlot[GaussianFilter[SEGYViewer`segy["traces"], 5]], "GIF"]]]]
        		]
			}]
		}]
	}],  
	Widget["ImageLabel", {
		"Data" -> Script[
			ExportString[Graphics[{White, Disk[]}], "GIF"]
		]
	}, Name -> "SEGYArrayPlot"], 
	Script[
		segyImport[path_String] := SEGYViewer`segy = GeologyIO`SEGY`SEGYImport[path];
	]
}]
