(* ::Package:: *)

(* check for a setting in the Mathematica preferences *)
If[ ("AllowInternetUse" /. SystemInformation["Network"]) === False,
    Print["You have configured Mathematica to not access the internet. Too bad. 
	Please check the \"Allow Mathematica to use the Internet\" box in the
    Help \[FilledRightTriangle] Internet Connectivity dialog. Exiting now." ];
    Quit[]
];


BeginPackage["CopyRemote`",{"JLink`"}]
  
CopyRemote::usage = "CopyRemote[url, localfilename] copies a file from an http location to localfilename."

ProxyHost::usage="ProxyHost is an option for CopyRemote.";
ProxyPort::usage="ProxyPort is an option for CopyRemote."

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

Options[CopyRemote] = {ProxyHost :> None, ProxyPort :> None};

CopyRemote[url_ /; Not[URLQ[url]],___] := Message[badurl::dead, url];

CopyRemote[url_String?URLQ, localfile_:Automatic, opts:OptionsPattern[]] /; (Length[localfile]===0) := Block[{openStream, read, close},
    Needs["JLink`"]; Symbol["JLink`InstallJava"][];
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
            If[ StringQ[localfile],
                outFile = OpenWrite[localfile, DOSTextFormat -> False],
                outFile = OpenTemporary[DOSTextFormat->False];
            ];
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
    
URLQ[url_String] := Block[{client, get, resp, jn, executeMethod, releaseConnection}, 
	Needs["JLink`"];Symbol["JLink`InstallJava"][];
    jn = Symbol["JLink`JavaNew"]; 
    Quiet[client = jn["org.apache.commons.httpclient.HttpClient"]; 
      get = jn["org.apache.commons.httpclient.methods.GetMethod", url]; 
      resp = client[executeMethod[get]]; 
      get[releaseConnection[]]; resp === 200]]; 
URLQ[h_/;Head[h] =!= String] = False;   
    
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
