(* Mathematica Package *)

(* :Title: CopyRemote *)

(* :Context: CopyRemote` *)

(* :Author:
        Rolf Mertig
        rolf@mertig.com
*)

(* :Package Version: 2.0 *)

(* :Mathematica Versions: 7 - 9 *)

(* :Copyright: Rolf Mertig, 2002 - 2013.  *)

(* :Installation:

   This package can be invoked without local installation by 
   Import["http://www.mertig.com/mathdepot/CopyRemote.m"];

*)

(* :Discussion:

	CopyRemote copies a file from a url to a local directory.
	The code is based on the GetRemote example from the JLink documentation 
	Furthermore OpenRemote, URLQ and URLFileByteSize are implemented.
	
*)
	
(* "Example usage:

CopyRemote["http://functions.wolfram.com/NB/Hypergeometric2F1.nb"]

copies the notebook to $TemporaryDirectory

To also open the notebook, do:
OpenRemote["http://functions.wolfram.com/NB/Hypergeometric2F1.nb"]

This copies a palette to the right place:

Notice: 
Since with default Options[CopyRemote]  "%20" will be replaced by " ", so this:
NotebookOpen @ 
CopyRemote["https://dl.dropbox.com/u/38623/SE%20Uploader.nb",
	       FileNameJoin[{$UserAddOnsDirectory,"SystemFiles","FrontEnd","Palettes"}]]
will put "SE Uploader.nb" into the Palettes directory 
 
 
 An alternative is to use
 Import["http://www.mertig.com/mathdepot/Installer.m"];
 InstallPalette["https://dl.dropbox.com/u/38623/SE%20Uploader.nb"]

*)
      

(* :Keywords: projects, install *)
(* check for a setting in the Mathematica preferences *)
If[ ("AllowInternetUse" /. SystemInformation["Network"]) === False,
    Print["You have configured Mathematica to not access the internet. Too bad. 
	Please check the \"Allow Mathematica to use the Internet\" box in the
    Help \[FilledRightTriangle] Internet Connectivity dialog. " ];
];


BeginPackage["CopyRemote`",{"JLink`"}];


list = { CopyRemote, OpenRemote, URLFileByteSize(*, URLQ *)};
Unprotect @@ list;
ClearAll @@ list;
  
CopyRemote::usage = "CopyRemote[urlfile] copies a urlfile as FileNameTake[urlfile] to $TemporaryDirectory.
 CopyRemote[url, localdir] copies a file from an http location to localdir.";
 
OpenRemote::usage= "OpenRemote[urfile] is a utility function for basically SystemOpen[CopyRemote[urlfile]]."

ProxyHost::usage="ProxyHost is an option for CopyRemote.";
ProxyPort::usage="ProxyPort is an option for CopyRemote.";

URLFileByteSize::usage = "URLFileByteSize[file] gives the remote file size in Byte."
URLQ::usage = "URLQ[url] give True if url is reachable and False otherwise.";

badurl::dead = "The URL `1` does not exist or is not reachable.";

Begin["`Private`"];



(* the option StringReplace is to unescape URL-file artifacts like %20 *)
Options[CopyRemote] = {ProxyHost :> None, ProxyPort :> None, ProgressIndicator -> True, StringReplace -> {"%20"->" "}};
Options[OpenRemote] = Options[CopyRemote];

(* CopyRemote might return $Failed, therefore, only open the result if it was successful : *)
OpenRemote[args__] :=
        Replace[CopyRemote[args], (s_String?FileExistsQ) :> SystemOpen[s]];

filename = Function[{f,s}, StringReplace[ FileNameTake[f], s]];

(* use $TemporaryDirectory if no second argument is given *)
CopyRemote[url_?URLQ, opts:OptionsPattern[]] :=
	CopyRemote[url, $TemporaryDirectory, FileNameJoin[{$TemporaryDirectory, filename[url, OptionValue[StringReplace]]}], opts];

(* create directory if it does not exist *)
CopyRemote[url_?URLQ, dir_String /; ( (*!StringMatchQ[FileNameTake@dir, "*.*"] && *)FileType[dir] === None), more___
          ]  /; (FileNameTake[url]=!=FileNameTake[dir])  :=
 Module[{cdir}, 
 	cdir = CreateDirectory[dir]; 
 	If[cdir  === $Failed, Throw[$Failed]]; 
 	CopyRemote[url, dir, more] 
 	];
 	
(* since it will not work, redefine natural calls like
   CopyRemote["http://www.mertig.com/mathdepot/CopyRemote.m", "CopyRemote.m"] to mean
   CopyRemote["http://www.mertig.com/mathdepot/CopyRemote.m", $TemporaryDirectory]
*)
   
CopyRemote[url_?URLQ, dir_String /; (FileType[dir] === None), more___
          ]  /; (FileNameTake[url] === FileNameTake[dir])  :=
 CopyRemote[url, DirectoryName[dir] /. "" :> $TemporaryDirectory];

(* if the directory exists, use it and get the filename from the url filename *)
CopyRemote[url_?URLQ, 
           dir_String /; FileType[dir] === Directory, 
           opts:OptionsPattern[]
] := CopyRemote[url, dir, filename[url, OptionValue[StringReplace] ], opts];
        
(*  a NotebookClose function which does nothing if $Notebooks is False *)
closenb = Function[locnb, If[ $Notebooks && StringMatchQ[locnb, "*.nb", IgnoreCase -> True],
            Select[Notebooks[], 
            ToFileName[ "FileName" /. NotebookInformation[#]] === locnb &] /. {n_NotebookObject} :> NotebookClose[n]
        ]];

CopyRemote[url_?URLQ, 
	       localdir_String?DirectoryQ, 
	       locfile_String, 
	       opts:OptionsPattern[]
	       ] := Block[{openStream, read, close, locfilefull}, 
    Needs["JLink`"]; Symbol["JLink`InstallJava"][]; (* using Symbol here enables an .mx saveable package, or to put this into a ButtonFunction, etc.  *)
    (* locfile can be a notebook. If it is open, close it *)
    locfilefull = If[DirectoryName[locfile]==="", FileNameJoin[{localdir, locfile}], locfile];
    closenb @ locfilefull;
    (* This code is based on the GetRemote example in the JLink documentation *)
   	  Function[j, 
        If[OptionValue[ProgressIndicator], 
   	  	Monitor[j, progress[url, locfilefull]],
   	    j], HoldFirst][
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
            buf = Symbol["JLink`JavaNew"]["[B", 8192]; (* ] *)
            outFile = OpenWrite[locfilefull, DOSTextFormat -> False];
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
             ]
];


(* does the URL exists or not *)
URLQ[link_String] := URLQ[link] = 
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


(* find out how big the remote file is *)
URLFileByteSize[link_?URLQ] :=
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
                 $Failed,
                 len = urlcon@getContentLength[];
                 check@close[];
                 len
             ]
         ]]
    ];
    
    
   
$progressupdateinterval = .6;
myroundMB[rfs_?NumberQ]:= Round[100 rfs /1024.^2]/ 100.;
myroundMB[_]:=" ";

progress[remotefile_?URLQ, localfile_String] :=
    Module[ {rfilesize = URLFileByteSize[remotefile]},
        If[ $Notebooks,
            Row[{"Transferring ", myroundMB[rfilesize]," MB from ",
            If[ StringQ[Setting@#],
                #,
                " "
            ] &@remotefile, " ", 
            ProgressIndicator[
             Quiet[If[ ! NumberQ[#],
                       0,
                       #
                   ] &@(Refresh[FileByteCount[localfile], 
                    TrackedSymbols -> {},
                 UpdateInterval -> $progressupdateinterval]/rfilesize)]],
            " ", If[ ! NumberQ[Setting@#],
                     0,
                     #
                 ] &@
             Refresh[FileByteCount[localfile]/1024.^2, 
                    TrackedSymbols -> {},
             	  UpdateInterval -> $progressupdateinterval], 
            " MByte"
            }], 
            Print["Downloading ", remotefile,"   please wait "]
        ]
    ];
    
With[{list = list},   
     SetAttributes[list, ReadProtected];   
     Protect @ list;
];
    
End[];
EndPackage[];
(*
Test[
URLQ["http://mathematica.stackexchange.com"],
True
]

Test[
URLQ["http://www.mertig.cddm"]
,
False
]

Test[URLFileByteSize[
  "http://www.nist.gov/images/banner_graphics/homepage_banner.jpg"],
  44850]


CopyRemote["http://www.mertig.com/mathdepot/CopyRemote.m"]

*)
