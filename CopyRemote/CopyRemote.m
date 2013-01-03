(* ::Package:: *)

(* ::Author:: Rolf Mertig *)

(* ::Version:: 1.0 *)


(* check for a setting in the Mathematica preferences *)
If[ ("AllowInternetUse" /. SystemInformation["Network"]) === False,
    Print["You have configured Mathematica to not access the internet. Too bad. 
	Please check the \"Allow Mathematica to use the Internet\" box in the
    Help \[FilledRightTriangle] Internet Connectivity dialog. Exiting now." ];
    Quit[]
];


BeginPackage["CopyRemote`",{"JLink`"}];

Clear[CopyRemote];
  
CopyRemote::usage = "CopyRemote[urlfile] copies a urlfile as FileNameTake[urlfile] to $TemporaryDirectory.
 CopyRemote[url, localdir] copies a file from an http location to localdir.";

ProxyHost::usage="ProxyHost is an option for CopyRemote.";
ProxyPort::usage="ProxyPort is an option for CopyRemote.";

URLFileByteSize::usage = "URLFileByteSize[file] gives the remote file size in Byte."
URLQ::usage = "URLQ[url] give True if url is reachable and False otherwise.";



badurl::dead = "The URL `1` does not exist or is not reachable.";

Begin["`Private`"];

(*
Example usage:
CopyRemote["http://www.mertig.com/mathdepot/buttons/ButtonTools.nb",
ToFileName[{$UserAddOnsDirectory,"SystemFiles","FrontEnd","Palettes"},
"ButtonTools.nb"]]
*)

(* This code is based on the GetRemote example in the JLink documentation *)

Options[CopyRemote] = {ProxyHost :> None, ProxyPort :> None, ProgressIndicator -> True};

CopyRemote[url_ /; Not[URLQ[url]],___] := Message[badurl::dead, url];

CopyRemote[url_String?URLQ, localdir_:Automatic, opts:OptionsPattern[]] /; AtomQ[localdir] := Block[{openStream, read, close, locfile}, 
    Needs["JLink`"]; Symbol["JLink`InstallJava"][];
    If[ StringQ[localdir], 
    	locfile = FileNameJoin[{DirectoryName[localdir] /. "" :> $TemporaryDirectory, FileNameTake[url]}],
        locfile = FileNameJoin[{$TemporaryDirectory, FileNameTake[url]}];
    ];
    If[OptionValue[ProgressIndicator], progress[url, locfile]];
    Symbol["JLink`JavaBlock"][
        Module[ {u, stream, numRead, outFile, buf, prxyHost, prxyPort},
            {prxyHost, prxyPort} = OptionValue/@{ProxyHost, ProxyPort};
            If[ StringQ[prxyHost],
                (* Set properties to force use of proxy. *)
                Symbol["JLink`SetInternetProxy"][prxyHost, prxyPort]
            ];
            u = Symbol["JLink`JavaNew"]["java.net.URL", url];
            stream = u@openStream[];
            If[ stream === $Failed,
                Return[$Failed]
            ];
            buf = Symbol["JLink`JavaNew"]["[B", 8192];
            outFile = OpenWrite[locfile, DOSTextFormat -> False];
            While[(numRead = stream@read[buf]) > 0,
             WriteString[outFile, FromCharacterCode[If[ # < 0,
                                                        #+256,
                                                        #
                                                    ]& /@ Take[Symbol["JLink`Val"][buf], numRead]]]
            ];
            stream@close[];
            Close[outFile] (* Close returns the filename *)
        ]
    ]
];

URLQ[link_String] :=
    Block[ {openConnection, getContentLength, getInputstream, check, 
      close, getInputStream},
        Needs["JLink`"];
        Symbol["JLink`InstallJava"][];
        Symbol["JLink`JavaBlock"][
         Module[ {url, urlcon},
             url = Symbol["JavaNew"]["java.net.URL", link];
             urlcon = url@openConnection[];
             Quiet[check = urlcon@getInputStream[]];
             If[ check === $Failed,
                 False,
                 check@close[];
                 True
             ]
         ]]
    ];

URLQ[h_/;Head[h] =!= String] = False;   


URLFileByteSize[link_String] :=
    Block[ {openConnection, getContentLength, getInputstream, check, 
      close, getInputStream},
        Needs["JLink`"];
        Symbol["JLink`InstallJava"][];
        Symbol["JLink`JavaBlock"][
         Module[ {url, urlcon, len},
             url = Symbol["JavaNew"]["java.net.URL", link];
             urlcon = url@openConnection[];
             Quiet[check = urlcon@getInputStream[]];
             If[ check === $Failed,
                 0,
                 len = urlcon@getContentLength[];
                 check@close[];
                 len
             ]
         ]]
    ];

progress[remotefile_String, localfile_String] := Module[{rfilesize = URLFileByteSize[remotefile]},
If[ Head[$FrontEnd]===System`FrontEndObject,
    PrintTemporary @  (* this way it does not get saved which is good *)
    Dynamic@Row[{"Downloading ", Round[rfilesize/1024.^2]," MB from ",
    If[ StringQ[Setting@#],
        #,
        " "
    ] &@remotefile, " ", 
    ProgressIndicator[
     Quiet[If[ ! NumberQ[#],
               0,
               #
           ] &@(Refresh[FileByteCount[localfile], 
         UpdateInterval -> .01]/rfilesize)]],
    " ", If[ ! NumberQ[Setting@#],
             0,
             #
         ] &@
     Refresh[FileByteCount[localfile]/1024.^2, UpdateInterval -> .02], 
    " MByte"
    }],
    Print["Downloading ", remotefile,"   please wait "]
]];
    
End[];
EndPackage[];
(*
Test[
URLQ["http://mathematica.stackexchange.com"],
True
]

*)

(*
Test[
URLQ["http://www.mertig.cddm"]
,
False
]

CopyRemote["http://www.mertig.com/mathdepot/CopyRemote.m","CopyRemote.m"]
*)
