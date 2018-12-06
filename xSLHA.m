(* ::Package:: *)

(* ---------------------------------------------------------------------- *)
(* A Mathematica SLHA reader   *)
(* by Florian Staub (florian.staub@gmail.com)                      *)
(* ---------------------------------------------------------------------  *)




Off[General::spell];


Options[xSLHA`Read]={separator->None};

xSLHA`Read[file_,options___]:=ReadSpectrumFile[file,separator/.{options}/.Options[xSLHA`Read]];

ReadSpectrumFile[file_,separator_]:=Block[{i,j,k,spc,allFiles,count,line,sline,in,lenS},
If[FileExistsQ[file]==False,
	File::DoesntExist="File `` does not exist";
	Message[File::DoesntExist,file];
	Interrupt[];
];

If[separator=!=None,
	allFiles={};
	count=1;
	lenS=StringLength[separator];
];

spc={};
in=OpenRead[file];
xSLHA`Init;
While[True,
line=ReadLine[in];
If[line===EndOfFile,
	Break[];,
	line=ToUpperCase[StringTrim[line]];
	];
If[StringTake[line,{1}]=="#",	Continue[]];
If[separator=!=None && StringLength[line]>=lenS,
	If[StringTake[line,{1,lenS}]===separator,
		xSLHA`flush;
		spc=Flatten[{xSLHA`blocks,xSLHA`br,xSLHA`br1L,{{WIDTH,xSLHA`widths}},{{WIDTH1L,xSLHA`widths1L}},XS->xSLHA`xsections},1];
		spc=Flatten[MakeSubNum[spc]];
		If[spc=!={},allFiles=Join[allFiles,{spc}]];
		count++;
		xSLHA`Init;
		spc={};
		Continue[];	
	];
];
	
	sline=StringSplit[line];
	Switch[sline[[1]],
		"BLOCK",
			xSLHA`Flush;
			xSLHA`StartBlock[sline];,
		"DECAY",
			xSLHA`Flush;
			xSLHA`StartDecay[sline];,
		"XSECTION",
			xSLHA`Flush;
			xSLHA`StartXsection[sline];,
		"DECAY1L",
			xSLHA`Flush;
			xSLHA`StartDecay1L[sline];,
		_,
			xSLHA`ParseLine[sline];
			];		
];
xSLHA`Flush;
Close[in];

spc=Flatten[{xSLHA`blocks,xSLHA`br,xSLHA`br1L,{{WIDTH,xSLHA`widths}},{{WIDTH1L,xSLHA`widths1L}},XS->xSLHA`xsections},1];
spc=Flatten[MakeSubNum[spc]];
If[separator===None,
Return[spc];,
If[spc=!={},allFiles=Join[allFiles,{spc}]];
Return[allFiles];
];
];

MyToExpression[string_]:=
If[NumericQ[ToExpression[string]],
   Return[ToExpression[StringReplace[string,"E"->"*^"]]];,
   Return[string];
   ]

xSLHA`ParseLine[line_]:=Block[{},
If[FreeQ[line,"#"],cline=line;,cline=line[[1;;Position[line,"#"][[1,1]]-1]];];
If[xSLHA`readingBlock,
	If[xSLHA`readingHBfermion,
		xSLHA`entries=Join[xSLHA`entries,{ToExpression/@cline[[3;;-1]]->{MyToExpression[cline[[1]]],MyToExpression[cline[[2]]]}}];,
		If[xSLHA`readingHBscalar,
			xSLHA`entries=Join[xSLHA`entries,{ToExpression/@cline[[2;;-1]]->MyToExpression[cline[[1]]]}];,
			xSLHA`entries=Join[xSLHA`entries,{ToExpression/@cline[[1;;-2]]->MyToExpression[cline[[-1]]]}];
		];
	];
];		
If[xSLHA`readingDecay || xSLHA`readingDecay1L,
	xSLHA`entries=Join[xSLHA`entries,{Sort[ToExpression/@cline[[3;;-1]]]->MyToExpression[cline[[1]]]}];
]; 
];

xSLHA`Init:=Block[{},
(*
      xSLHA`blocks=<||>;
      xSLHA`br=<||>;
      xSLHA`widths=<||>;
      xSLHA`br1L=<||>;
      xSLHA`widths1L=<||>;
      xSLHA`xsections=<||>;
      *)
      xSLHA`blocks={};
      xSLHA`br={};
      xSLHA`widths={};
      xSLHA`br1L={};
      xSLHA`widths1L={};
      xSLHA`xsections={};

      xSLHA`blockName=None;
      xSLHA`entries={};
      xSLHA`readingBlock=False;
      xSLHA`readingDecay=False;
      xSLHA`readingDecay1L=False;
      xSLHA`readingXsection=False;
      xSLHA`readingHBfermion=False;
      xSLHA`readingHBboson=False;
      xSLHA`decay1L=False;
      xSLHA`decayPart=0;
];

(*
xSLHA`Flush:=Block[{},
    If[Length[xSLHA`entries]>0,
     If[xSLHA`readingBlock, xSLHA`blocks[xSLHA`blockName]=xSLHA`entries;,
       If[xSLHA`readingDecay, xSLHA`decays[xSLHA`decayPart]=xSLHA`entries;,
        If[xSLHA`readingDecay1L, xSLHA`decays1L[xSLHA`decayPart]=xSLHA`entries;,
          If[xSLHA`readingXsection, xSLHA`xsections[xSLHA`XShead]=xSLHA`entries;  ];
        ];
       ];
      ];
    ];
];
*)

xSLHA`Flush:=Block[{},
    If[Length[xSLHA`entries]>0,
     If[xSLHA`readingBlock, xSLHA`blocks=Join[xSLHA`blocks,{{xSLHA`blockName,xSLHA`entries}}];,
       If[xSLHA`readingDecay, xSLHA`br=Join[xSLHA`br,{{BR[xSLHA`decayPart],xSLHA`entries}}];,
        If[xSLHA`readingDecay1L, xSLHA`br1L=Join[xSLHA`br1L,{{BR1L[xSLHA`decayPart],xSLHA`entries}}];,
          If[xSLHA`readingXsection, xSLHA`xsections=Join[xSLHA`xsections,{{xSLHA`blockName,xSLHA`entries}}];  ];
        ];
       ];
      ];
    ];
];

xSLHA`StartBlock[line_]:=Block[{},
xSLHA`entries={};
xSLHA`blockName=ToExpression[line[[2]]];
xSLHA`readingHBfermion=(xSLHA`blockName==="HIGGSBOUNDSINPUTHIGGSCOUPLINGSFERMIONS");
xSLHA`readingHBscalar=(xSLHA`blockName==="HIGGSBOUNDSINPUTHIGGSCOUPLINGSBOSONS");
{xSLHA`readingBlock,xSLHA`readingDecay,xSLHA`readingDecay1L, xSLHA`readingXsection}={True,False,False,False};
];

xSLHA`StartDecay[line_]:=Block[{},
xSLHA`entries={};
xSLHA`decayPart=ToExpression[line[[2]]];
xSLHA`widths=Join[xSLHA`widths,{{xSLHA`decayPart}->ToExpression[line[[3]]]}];
{xSLHA`readingBlock,xSLHA`readingDecay,xSLHA`readingDecay1L, xSLHA`readingXsection}={False,True,False,False};
];

xSLHA`StartDecay1L[line_]:=Block[{},
xSLHA`entries={};
xSLHA`decayPart=ToExpression[line[[2]]];
xSLHA`widths1L=Join[xSLHA`widths1L,{{xSLHA`decayPart}->ToExpression[line[[3]]]}];
{xSLHA`readingBlock,xSLHA`readingDecay,xSLHA`readingDecay1L, xSLHA`readingXsection}={False,False,True,False};
];


MakeSubNum[spc_]:=Table[(#[[1]]@@#[[2,i,1]])->#[[2,i,2]],{i,1,Length[#[[2]]]}]&/@spc;
BR1L[a_][b__]:=BR1L[a]@@Sort[{b}]/;OrderedQ[{b}]==False
BR1L[a_][b__]:=BR1L[a]@@Sort[{b}]/;OrderedQ[{b}]==False

Options[xSLHA`ReadSmall]={separator->None, entries->{}};
Options[xSLHA`ReadDir]={entries->{}};

xSLHA`ReadSmall[file_,options___]:=xSLHA`ReadSmallFunc[file,entries/.{options}/.Options[xSLHA`ReadSmall],separator/.{options}/.Options[xSLHA`ReadSmall]];
xSLHA`ReadSmallFunc[file_,entries_,sep_]:=Block[{string,i,out},
	If[entries=={},
		Return[xSLHA`Read[file,separator->sep]];
	];	
	string="--regexp=\""<>sep<>"\" --regexp=\"Block\" ";
	For[i=1,i<=Length[entries],
		string=string<>"--regexp=\""<>entries[[i]]<>"\" ";
	i++;];
	Run["rm temp_read_small.spc"];
	Run["cat "<>file<>" | grep -i "<>string<>" > temp_read_small.spc"];
	out=xSLHA`Read["temp_read_small.spc",separator->sep];
	Run["rm temp_read_small.spc"];
	Return[out];
];

xSLHA`ReadDir[file_,options___]:=xSLHA`ReadDirFunc[file,entries/.{options}/.Options[xSLHA`ReadSmall]];
xSLHA`ReadDirFunc[dir_,entr_]:=Block[{out},
	Run["rm temp_read_dir.spc"];
	Run["cat "<>dir<>"/* > temp_read_dir.spc"];
	out=xSLHA`ReadSmall["temp_read_dir.spc",entries->entr,separator->"BLOCK SPINFO"];
	Run["rm temp_read_dir.spc"];
	Return[out];
];




FindValue[block_,entry_,data_,app_]:=Block[{pos,blockname},
If[Head[block]===Symbol,
blockname=ToUpperCase[ToString[block]];,
blockname = ToUpperCase[block];
];

pos=Position[data,blockname];
If[pos=!={},
If[Head[app]===Integer,
pos = pos[[app,1]];,
pos=Last[pos][[1]];
];
temp=DeleteCases[Drop[Extract[data,pos],1],{}];
If[entry===None, Return[temp];];
If[Head[entry]===List,
pos=Cases[NIX[temp][[1]],Join[entry,{_}]];
If[pos=={},Return[None];];
Return[Last[pos[[1]]]];,
pos=Position[Transpose[temp][[1]],entry];
If[pos=={},Return[None];];
Return[temp[[pos[[1,1]]]][[2]]];
];,
Return[None];
];
];



Options[ShowValue]={Appearance->Last};
Options[ShowBlock]={Appearance->Last};
Options[ShowBR]={Appearance->Last};

ShowValue[block_,entry_,opt___]:=FindValue[ToUpperCase[ToString[block]],entry,LesHouchesInput,Appearance/.{opt}/.Options[ShowValue]];
ShowBlock[block_,opt___]:=FindValue[ToUpperCase[ToString[block]],None,LesHouchesInput,Appearance/.{opt}/.Options[ShowBlock]];
ShowWidth[block_]:=FindValue[ToUpperCase[ToString[block]],WIDTH,LesHouchesInput,Last];
ShowBR[field_,particles_]:=FindValue[ToUpperCase[ToString[field]],FinalParticles@@particles,LesHouchesInput,Last];
