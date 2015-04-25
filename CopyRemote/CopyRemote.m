(* Mathematica Package *)

(* :Title: CopyRemote *)

(* :Context: CopyRemote` *)

(* :Author:
        Rolf Mertig 
        GluonVision GmbH
        http://www.mertig.com
*)

(* :Package Version: 3.0 *)

(* :Mathematica Versions: 7 - 10 *)

(* :License: LGPL *)

(* :Copyright: Rolf Mertig, 2002 - 2015.  *)

(* :Installation:

   This package can be invoked without local installation by
   Import @ "https://raw.githubusercontent.com/rolfmertig/copyremote/master/CopyRemote/CopyRemote.m";

*)

(* :Discussion:

	CopyRemote copies a file from a url to a local directory.
	The code is based on the GetRemote example from the JLink documentation 
	Furthermore OpenRemote, URLQ and URLFileByteCount are implemented.
	
*)

(* "Example usage:

	CopyRemote["http://functions.wolfram.com/NB/Hypergeometric2F1.nb"]

copies the notebook to $TemporaryDirectory


To also open the notebook, do:
	OpenRemote["http://functions.wolfram.com/NB/Hypergeometric2F1.nb"]

This copies a palette to the right place:

    NotebookOpen @ CopyRemote["https://www.mertig.com/mathdepot/ButtonTools.nb",
    FileNameJoin[{$UserAddOnsDirectory,"SystemFiles","FrontEnd","Palettes"}]]
     
*)


(* :Keywords: projects, install *)


BeginPackage["CopyRemote`", {"JLink`"}];

(* enable updating without reloading. URLQ is memoizing, so leave it unprotected *)
Unprotect @@ { CopyRemote, OpenRemote, URLFileByteCount};
ClearAll @@ { CopyRemote, OpenRemote, URLFileByteCount};
ClearAll[URLQ];


CopyRemote::usage = "CopyRemote[urlfile] copies a urlfile as URLFileNameTake[urlfile] to $TemporaryDirectory.
 CopyRemote[url, localfile] copies a file from an url to localfile.";

OpenRemote::usage = "OpenRemote[urfile] is a utility function for basically SystemOpen[CopyRemote[urlfile]]."

ProxyHost::usage = "ProxyHost is an option for CopyRemote.";
ProxyPort::usage = "ProxyPort is an option for CopyRemote.";

URLFileByteCount::usage = "URLFileByteCount[file] gives the remote file size in bytes."

URLFileNameTake::usage = "URLFileNameTake[file] is like FileNameTake but skips everything after a ?";

URLQ::usage = "URLQ[url] give True if url is reachable and False otherwise.";

CopyRemote::failed = "The transfer of `1` did not succeed. Please try again.";

Begin["`Private`"];

(* the option StringReplace is to unescape URL-file artifacts like %20 *)
(* if Print is set to True then a Monitor shows up displaying the percentage of tranfer *)
Options[CopyRemote] = {ProxyHost :> None, ProxyPort :> None, Print -> True, StringReplace -> {"%20" -> " "}};
Options[OpenRemote] = Options[CopyRemote];

(* CopyRemote might return $Failed, therefore, only open the result if it was successful : *)
OpenRemote[args__] :=
    Module[ {cr},
      Replace[cr = CopyRemote[args], (s_String?FileExistsQ) :> SystemOpen[s]];
      cr
    ];


URLFileNameTake[s_String] :=
    StringSplit[FileNameTake[s], "&"] // First;
filename = Function[{f, s}, StringReplace[ URLFileNameTake[f], s]];

(* use $TemporaryDirectory if no second argument is given *)
CopyRemote[url_?URLQ, opts : OptionsPattern[]] :=
    CopyRemote[url, $TemporaryDirectory, FileNameJoin[{$TemporaryDirectory, filename[url, OptionValue[StringReplace]]}], opts];

(* create directory if it does not exist *)
CopyRemote[url_?URLQ, file_String /; (
  (DirectoryName[file] =!= "") && (FileType[DirectoryName[file]] =!= Directory)
), opts___?OptionQ_
] :=
    Module[ {cdir},
      Catch[
        cdir = CreateDirectory[DirectoryName @ file];
        If[ cdir === $Failed,
          Throw[$Failed]
        ];
        CopyRemote[url, cdir, FileNameTake[file], opts]
      ]
    ];

(* since it will put files in Directory[] otherwise, redefine natural calls like
   CopyRemote["http://www.mertig.com/mathdepot/CopyRemote.m", "CopyRemote.m"] to mean
   CopyRemote["http://www.mertig.com/mathdepot/CopyRemote.m", FileNameJoin[{$TemporaryDirectory, "CopyRemote.m"}]]
*)

CopyRemote[url_?URLQ, file_String /; (FileType[file] === None), more___
] /; (file === FileNameTake[file]) :=
    CopyRemote[url, $TemporaryDirectory, file];

(* go from 2 argumetn to three argument form: *)
CopyRemote[url_?URLQ, file_String /; (
  (DirectoryName[file] =!= "") && (FileType[DirectoryName[file]] === Directory)
), opts___?OptionQ
] :=
    CopyRemote[url, DirectoryName[file], file, opts];

(* if the directory exists, use it and get the filename from the url filename *)
CopyRemote[url_?URLQ,
  dir_String /; FileType[dir] === Directory,
  opts : OptionsPattern[]
] :=
    CopyRemote[url, dir, filename[url, OptionValue[StringReplace] ], opts];

(*  a NotebookClose function which does nothing if $Notebooks is False *)
closenb = Function[locnb, If[ $Notebooks && StringMatchQ[locnb, "*.nb", IgnoreCase -> True],
  Select[Notebooks[],
    ToFileName[ "FileName" /. NotebookInformation[#]] === locnb &] /. {n_NotebookObject} :> NotebookClose[n]
]];

CopyRemote[url_?URLQ,
  localdir_String?DirectoryQ,
  locfile_String,
  opts : OptionsPattern[]
] :=
    Catch @ Block[ {openStream, read, close, locfilefull, locfiletmp, outFile, rfilesize},
      Needs["JLink`"];
      Symbol["JLink`InstallJava"][]; (* using Symbol here enables an .mx saveable package, or to put this into a ButtonFunction, etc.  *)
      locfilefull = If[ DirectoryName[locfile] === "",
        FileNameJoin[{localdir, locfile}],
        locfile
      ];
      (* download to a temporary file , in case the download fails.
                   copy locfiletmp to locfilefull only if download succeeded *)
      (* This code is based on the GetRemote example in the JLink documentation *)
      rfilesize = URLFileByteCount[url];
      If[ !IntegerQ[rfilesize],
        Message[CopyRemote::failed, url];
        Throw[$Failed]
      ];
      (* temporary file *)
      outFile = OpenWrite[DOSTextFormat -> False];
      locfiletmp =
          Function[j, If[ OptionValue[Print],
            Monitor[j, progress[url, outFile[[1]], rfilesize]],
            j
          ],
            HoldFirst
          ][
            Symbol["JLink`JavaBlock"][
              Module[ {u, stream, numRead, buf, prxyHost, prxyPort},
                {prxyHost, prxyPort} = OptionValue /@ {ProxyHost, ProxyPort};
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
                While[(numRead = stream@read[buf]) > 0,
                  WriteString[outFile, FromCharacterCode[If[ # < 0,
                    # + 256,
                    #
                  ]& /@ Take[Symbol["JLink`Val"][buf], numRead]]]
                ];
                stream@close[];
                Close[outFile]
              (* Close returns the filename *)
              ]
            ]
          ];
      (*check if the transfer was successfull: *)
      (* TODO: add MD5sum check here somehow *)
      (* sometimes rfilesize is -1 ... : *)
      If[ rfilesize > 0,
        If[ FileByteCount[locfiletmp] =!= rfilesize,
          Message[CopyRemote::failed, url];
          Throw[$Failed]
        ]
      ];
      (* locfilefull can be a notebook. If it is open, close it *)
      closenb @ locfilefull;
      If[ FileExistsQ[locfilefull],
        DeleteFile[locfilefull]
      ];
      RenameFile[locfiletmp, locfilefull]
    ];


(* does the URL exists or not *)
URLQ[link_String] :=
    URLQ[link] = (* memoize links, to save time *)
        Catch @ Block[ {openConnection, getContentLength, getInputstream, check,
          close, getInputStream},
          If[ (!StringMatchQ[StringTrim@link, "http://*"] ) &&
              (!StringMatchQ[StringTrim@link, "https://*"] ),
            Throw[False]
          ];
          checknetwork[]; (* maybe not necessary, but well ... *)
          Needs["JLink`"];
          Symbol["JLink`InstallJava"][];
          Symbol["JLink`JavaBlock"][
            Module[ {url, urlcon},
              url = Symbol["JavaNew"]["java.net.URL", StringTrim@link];
              urlcon = url@openConnection[];
              Quiet[check = urlcon@getInputStream[]];
              If[ check === $Failed,
                False,
                check@close[];
                True
              ]
            ]]
        ];
URLQ[h_ /; Head[h] =!= String] = False;


(* find out how big the remote file is *)
URLFileByteCount[link_?URLQ] :=
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
myroundMB[rfs_?NumberQ] :=
    Round[100 rfs / 1024.^2] / 100.;
myroundMB[_] :=
    " ";

Clear[progress];
progress[remotefile_?URLQ, localfile_String, rfilesize_Integer] :=
    If[ $Notebooks,
      Row[{"Copied ",
        ProgressIndicator[
          Quiet[If[ ! NumberQ[#],
            0,
            #
          ] &@(Refresh[FileByteCount[localfile],
            TrackedSymbols -> {},
            UpdateInterval -> $progressupdateinterval] / rfilesize)],
          Background -> Orange, ImageSize -> {42, 15}
        ],
        " ", If[ ! NumberQ[Setting@#],
          0,
          #
        ] &@
            Refresh[Round[100 FileByteCount[localfile] / rfilesize],
              TrackedSymbols -> {},
              UpdateInterval -> $progressupdateinterval
            ],
        " % of ",
        myroundMB[rfilesize],
        " MB ",
        "from ",
        remotefile,
        " to ", localfile
      }],
      Print["Transferring ", remotefile, "   please wait "]
    ];

(* check for a setting in the Mathematica preferences *)
checknetwork[] :=
    If[ ("AllowInternetUse" /. SystemInformation["Network"]) === False,
      Print["You have configured Mathematica to not access the internet. Too bad.
	Please check the \"Allow Mathematica to use the Internet\" box in the
    Help \[FilledRightTriangle] Internet Connectivity dialog. " ];
      Throw[$Failed]
    ];

With[ {list = { CopyRemote, OpenRemote, URLFileByteCount}},
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

Test[URLFileByteCount[
  "http://www.nist.gov/images/banner_graphics/homepage_banner.jpg"],
  44850]


CopyRemote["http://www.mertig.com/mathdepot/CopyRemote.m"]

*)
