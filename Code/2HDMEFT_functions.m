(* Modified from SmeftFR v3.0 package https://www.fuw.edu.pl/smeft/ *)
(* Auxiliary functions to simplify and transform 2HDMMEFT Lagrangian if needed *)

NumberToString = Function[{x},
(* string display format for floating point numbers *)
Module[{tmp,tmp1,tmp2},

If[ NumberQ[x],
  If[ Head[x] === Real || Head[x] === Rational,
    tmp1 = MantissaExponent[N[x]];
    tmp  = ToString[tmp1[[1]]] <> "*^" <> ToString[tmp1[[2]]];
  ];
  If[ Head[x] === Integer,
    tmp1 = MantissaExponent[x];
    tmp  = ToString[tmp1[[1]]] <> "*^" <> ToString[tmp1[[2]]];
  ];
  If[ Head[x] === Complex,
    tmp1 = MantissaExponent[N[Re[x]]];
    tmp1 = ToString[tmp1[[1]]] <> "*^" <> ToString[tmp1[[2]]];
    tmp = If[ Im[x] < 0, " - ", " + "];
    tmp2 = MantissaExponent[N[Abs[Im[x]]]];
    tmp2 = ToString[tmp2[[1]]] <> "*^" <> ToString[tmp2[[2]]];
    tmp  = tmp1 <> tmp <> tmp2 <> "*I";
  ];
,
  Print[ "NumberToString function called with argument ",x];
  Print[ "Argument is not a number, please correct!"];
  Abort[];
];

tmp

]
];

CombineCommonPowers = Function[{expr,par,n},
(* rearranges power expansion into a_0 + a_1 par^1 + ... a_n par^n *)
Module[{tmp,fact,aa},
tmp =  expr /. par -> 0 // Simplify; 
If[ n > 0,
  For[aa=1, aa < n+1, aa++,
    fact = Coefficient[expr, par^aa]; 
    tmp = tmp + par^aa If[ aa==1, Dim6CoefficientsSimplify[fact], Simplify[fact, TimeConstraint->5] ]; 
  ];
];

tmp

]
];


SU3Simplify = Function[{expr},
(*** SU(3) generators T^a_bc T^a_de reduction ***)
Module[{tmp,aa,y,z,y1,z1},

expr //. T[aa_,y_,z_] T[aa_,y1_,z1_] ->
      1/2 ( IndexDelta[y,z1] IndexDelta[z,y1] -
      1/3 IndexDelta[y,z] IndexDelta[y1,z1] ) // Expand

]
];
 
 
(* Goldstone couplings and mass replacement *)
 
GoldstoneCouplings = Function[{ver},
Module[{vert},
vert = ver;
Do[ If[ Length[vert[[kk, 1]]] == 3 && FreeQ[vert[[kk, 2]], alphah] == True && MemberQ[{h, G0, GP, GPbar}, vert[[kk, 1]][[3, 1]]] == True,
              vert[[kk]] = vert[[kk]] /. RuleMe // Simplify;
      ];
         
    If[ Length[vert[[kk, 1]]] == 4 && FreeQ[vert[[kk, 2]], alphah] == True &&
        (MemberQ[{h, G0, GP, GPbar}, vert[[kk, 1]][[3, 1]]] == True && MemberQ[{h, G0, GP, GPbar}, vert[[kk, 1]][[4, 1]]] == True),
              vert[[kk]] = vert[[kk]] /. RuleMe // Simplify;
      ], {kk, 1, Length[vert]}];
       
    vert
    ]
];



SymmetrizeNeutrino2Current = Function[{x},
(* symmetrize vertices for Majorana neutrinos *)
Module[{aa,bb,cc,swap,i,frule,symlist,tmp},

symlist = {};
tmp = x;
       
For[i=1, i < Length[tmp]+1, i++,
(* find symmetrized form of vlbar vl vertices *)
  If[ SubsetQ[ Flatten[ Take[ tmp[[i,1]], All, 1] ], {vlbar,vl} ],
    Clear[swap];
(* swap external indices *)
    frule = tmp[[i]] /. Ext[i1] -> swap;
    frule = frule /. Ext[i2] -> Ext[i1];
    frule = frule /. swap -> Ext[i2];
      
(* but swap back external spin indices *)
    frule = frule /. Index[Spin,Ext[i1]] -> Index[Spin,swap];
    frule = frule /. Index[Spin,Ext[i2]] -> Index[Spin,Ext[i1]];
    frule = frule /. Index[Spin,swap]    -> Index[Spin,Ext[i2]];
    frule = frule /. TensDot[SlashedP[aa_], ProjM][bb_,cc_] ->    TensDot[SlashedP[aa], swap][bb,cc];
    frule = frule /. TensDot[SlashedP[aa_], ProjP][bb_,cc_] ->  - TensDot[SlashedP[aa], ProjM][bb,cc];
    frule = frule /. TensDot[SlashedP[aa_], swap][bb_,cc_]  ->  - TensDot[SlashedP[aa], ProjP][bb,cc];
    frule = frule /. TensDot[Ga[aa_], ProjM][bb_,cc_] ->    TensDot[Ga[aa], swap][bb,cc];
    frule = frule /. TensDot[Ga[aa_], ProjP][bb_,cc_] ->  - TensDot[Ga[aa], ProjM][bb,cc];
    frule = frule /. TensDot[Ga[aa_], swap][bb_,cc_]  ->  - TensDot[Ga[aa], ProjP][bb,cc];

    AppendTo[symlist,frule];
  ];
(*  change vl vl and vlbar vlbar vertices into vlbar vl, they are identical for Majorana neutrinos *)
  If[ Flatten[ Take[ tmp[[i,1]], All, 1] ][[1;;2]] === {vl,vl}, tmp[[i,1,1,1]] = vlbar ];
  If[ Flatten[ Take[ tmp[[i,1]], All, 1] ][[1;;2]] === {vlbar,vlbar}, tmp[[i,1,2,1]] = vl ];
(*  change lbar vlbar vertex into lbar vl, identical for Majorana neutrinos *)
  If[ Flatten[ Take[ tmp[[i,1]], All, 1] ][[1;;2]] === {lbar,vlbar}, tmp[[i,1,2,1]] = vl ];
(*  change l vl vertex into vlbar l, identical for Majorana neutrinos *)
  If[ Flatten[ Take[ tmp[[i,1]], All, 1] ][[1;;2]] === {l,vl}, tmp[[i,1,1,1]] = vlbar; tmp[[i,1,2,1]] = l ];

];

MergeVertices[tmp,symlist] // Simplify

]
];


 (* symmetrization of fermionic WC's. Following D. Straub notation of WC "classes":

 Class      Type
 0          Scalar object
 1          2F general 3x3 matrix
                 [    "lphi111", "lphi122", "lphi121", "lphi112",
                      "lphi211", "lphi222", "lphi221", "lphi212",
                      "dphi111", "dphi122", "dphi121", "dphi112",
                      "dphi211", "dphi222", "dphi221", "dphi212",
                      "uphi111", "uphi122", "uphi121", "uphi112",
                      "uphi211", "uphi222", "uphi221", "uphi212",
                      
                      "lBphi1", "lBphi2", "lWphi1", "lWphi2",
                      "dBphi1", "dBphi2", "dWphi1", "dWphi2", "dGphi1", "dGphi2",
                      "uBphi1", "uBphi2", "uWphi1", "uWphi2", "uGphi1", "uGphi2",
                      
                      "phiud11", "phiud22", "phiud21",
                 ]
  
 2          2F Hermitian/non-Hermitian matrix
                 [    "phie11", "phie22", "phil1l1", "phil221", "phil113", "phil223",
                      "phid11", "phid22", "phiu11", "phiu22",
                      "phiq111", "phiq221", "phiq113", "phiq223",
                      "phie12", "phil121", "phil123",
                      "phid12", "phiu12",
                      "phiq121", "phiq123" ]
  
 3          2F symmetric matrix
                 ["vvphi11", "vvphi22", "vvphi12" ]
  
 4          4F two identical ffbar currents
                 ["ll", "qq1", "qq3", "uu", "dd"]
 5          4F two independent ffbar currents
                 ["lq1", "lq3", "eu", "ed", "ud1", "ud8", "le", "lu", "ld", "qe", "qu1",
                  "qd1", "qu8", "qd8"]
 6          4F two identical ffbar currents - special case ["ee"]
 7          4F Baryon-number-violating - special case ["qqu"]
 8          4F Baryon-number-violating - special case ["qqq"]
 9          4F general 3x3x3x3 object
                 ["ledq", "quqd1", "quqd8", "lequ1", "lequ3", "duq", "duu"]

 *)

 Tensor2Class = {
     "lphi111" -> 1, "lphi122" -> 1, "lphi121" -> 1, "lphi112" -> 1,
     "lphi211" -> 1, "lphi222" -> 1, "lphi221" -> 1, "lphi212" -> 1,
     "dphi111" -> 1, "dphi122" -> 1, "dphi121" -> 1, "dphi112" -> 1,
     "dphi211" -> 1, "dphi222" -> 1, "dphi221" -> 1, "dphi212" -> 1,
     "uphi111" -> 1, "uphi122" -> 1, "uphi121" -> 1, "uphi112" -> 1,
     "uphi211" -> 1, "uphi222" -> 1, "uphi221" -> 1, "uphi212" -> 1,
     "lBphi1" -> 1, "lBphi2" -> 1, "lWphi1" -> 1, "lWphi2" -> 1,
     "dBphi1" -> 1, "dBphi2" -> 1, "dWphi1" -> 1, "dWphi2" -> 1, "dGphi1" -> 1, "dGphi2" -> 1,
     "uBphi1" -> 1, "uBphi2" -> 1, "uWphi1" -> 1, "uWphi2" -> 1, "uGphi1" -> 1, "uGphi2" -> 1,
     "phiud11" -> 1, "phiud22" -> 1, "phiud21" -> 1,
     "phie11" -> 2, "phie22" -> 2, "phil1l1" -> 2, "phil221" -> 2, "phil113" -> 2, "phil223" -> 2,
     "phid11" -> 2, "phid22" -> 2, "phiu11" -> 2, "phiu22" -> 2,
     "phiq111" -> 2, "phiq221" -> 2, "phiq113" -> 2, "phiq223" -> 2,
     "phie12" -> 2, "phil121" -> 2, "phil123" -> 2,
     "phid12" -> 2, "phiu12" -> 2,
     "phiq121" -> 2, "phiq123" -> 2,
     "vvphi11" -> 3, "vvphi22" -> 3, "vvphi12" -> 3 };


 Tensor4Class = {
     "ll" -> 4, "qq1" -> 4, "qq3" -> 4,
     "lq1" -> 5, "lq3" -> 5,
     "ee" -> 6, "uu" -> 4, "dd" -> 4, "eu" -> 5,
     "ed" -> 5, "ud1" -> 5, "ud8" -> 5, "le" -> 5, "lu" -> 5, "ld" -> 5,
     "qe" -> 5, "qu1" -> 5, "qd1" -> 5,
     "qu8" -> 5, "qd8" -> 5, "ledq" -> 9, "quqd1" -> 9,
     "quqd8" -> 9, "lequ1" -> 9, "lequ3" -> 9, "duq" -> 9,
     "qqu" -> 7, "qqq" -> 8, "duu" -> 9 };

 (* indices of non-redundant tensorial fermionic WC's. Last element in
 each sublist - False for complex and true for real entries *)


Tensor2WCSymmetrize = Function[{WC,cat},
(* symmetrization of 3x3 WC matrices *)
Module[{tmp},

tmp = WC;

If[cat == 2,(* hermitian 3x3 matrix *)
  tmp[[2,1]] = Conjugate[tmp[[1,2]]];
  tmp[[3,1]] = Conjugate[tmp[[1,3]]];
  tmp[[3,2]] = Conjugate[tmp[[2,3]]];
];

If[cat == 3,(* symmetric 3x3 matrix *)
  tmp[[2,1]] = tmp[[1,2]];
  tmp[[3,1]] = tmp[[1,3]];
  tmp[[3,2]] = tmp[[2,3]];
];

tmp

]
]

Tensor4WCSymmetrize = Function[{WC,cat},
(* symmetrization of 3x3x3x3 WC matrices, according to their classes defined above *)
Module[{WCR,WCI,tmp},

tmp = WC;

If[ cat == 4 || cat == 5 || cat == 6,

WCR = Re[WC];
WCI = Im[WC];

If[cat == 4,(* two identical XX currents *)
(* Real part *)
  WCR[[1,1,2,1]] = WCR[[1,1,1,2]];
  WCR[[1,1,3,1]] = WCR[[1,1,1,3]];
  WCR[[1,1,3,2]] = WCR[[1,1,2,3]];
  WCR[[1,2,1,1]] = WCR[[1,1,1,2]];
  WCR[[1,3,1,1]] = WCR[[1,1,1,3]];
  WCR[[1,3,1,2]] = WCR[[1,2,1,3]];
  WCR[[1,3,2,1]] = WCR[[1,2,3,1]];
  WCR[[2,1,1,1]] = WCR[[1,1,1,2]];
  WCR[[2,1,1,2]] = WCR[[1,2,2,1]];
  WCR[[2,1,1,3]] = WCR[[1,2,3,1]];
  WCR[[2,1,2,1]] = WCR[[1,2,1,2]];
  WCR[[2,1,2,2]] = WCR[[1,2,2,2]];
  WCR[[2,1,2,3]] = WCR[[1,2,3,2]];
  WCR[[2,1,3,1]] = WCR[[1,2,1,3]];
  WCR[[2,1,3,2]] = WCR[[1,2,2,3]];
  WCR[[2,1,3,3]] = WCR[[1,2,3,3]];
  WCR[[2,2,1,1]] = WCR[[1,1,2,2]];
  WCR[[2,2,1,2]] = WCR[[1,2,2,2]];
  WCR[[2,2,1,3]] = WCR[[1,3,2,2]];
  WCR[[2,2,2,1]] = WCR[[1,2,2,2]];
  WCR[[2,2,3,1]] = WCR[[1,3,2,2]];
  WCR[[2,2,3,2]] = WCR[[2,2,2,3]];
  WCR[[2,3,1,1]] = WCR[[1,1,2,3]];
  WCR[[2,3,1,2]] = WCR[[1,2,2,3]];
  WCR[[2,3,1,3]] = WCR[[1,3,2,3]];
  WCR[[2,3,2,1]] = WCR[[1,2,3,2]];
  WCR[[2,3,2,2]] = WCR[[2,2,2,3]];
  WCR[[2,3,3,1]] = WCR[[1,3,3,2]];
  WCR[[3,1,1,1]] = WCR[[1,1,1,3]];
  WCR[[3,1,1,2]] = WCR[[1,2,3,1]];
  WCR[[3,1,1,3]] = WCR[[1,3,3,1]];
  WCR[[3,1,2,1]] = WCR[[1,2,1,3]];
  WCR[[3,1,2,2]] = WCR[[1,3,2,2]];
  WCR[[3,1,2,3]] = WCR[[1,3,3,2]];
  WCR[[3,1,3,1]] = WCR[[1,3,1,3]];
  WCR[[3,1,3,2]] = WCR[[1,3,2,3]];
  WCR[[3,1,3,3]] = WCR[[1,3,3,3]];
  WCR[[3,2,1,1]] = WCR[[1,1,2,3]];
  WCR[[3,2,1,2]] = WCR[[1,2,3,2]];
  WCR[[3,2,1,3]] = WCR[[1,3,3,2]];
  WCR[[3,2,2,1]] = WCR[[1,2,2,3]];
  WCR[[3,2,2,2]] = WCR[[2,2,2,3]];
  WCR[[3,2,2,3]] = WCR[[2,3,3,2]];
  WCR[[3,2,3,1]] = WCR[[1,3,2,3]];
  WCR[[3,2,3,2]] = WCR[[2,3,2,3]];
  WCR[[3,2,3,3]] = WCR[[2,3,3,3]];
  WCR[[3,3,1,1]] = WCR[[1,1,3,3]];
  WCR[[3,3,1,2]] = WCR[[1,2,3,3]];
  WCR[[3,3,1,3]] = WCR[[1,3,3,3]];
  WCR[[3,3,2,1]] = WCR[[1,2,3,3]];
  WCR[[3,3,2,2]] = WCR[[2,2,3,3]];
  WCR[[3,3,2,3]] = WCR[[2,3,3,3]];
  WCR[[3,3,3,1]] = WCR[[1,3,3,3]];
  WCR[[3,3,3,2]] = WCR[[2,3,3,3]];
(* Imaginary part *)
  WCI[[1,1,1,1]] = 0;
  WCI[[1,1,2,1]] = - WCI[[1,1,1,2]];
  WCI[[1,1,2,2]] = 0;
  WCI[[1,1,3,1]] = - WCI[[1,1,1,3]];
  WCI[[1,1,3,2]] = - WCI[[1,1,2,3]];
  WCI[[1,1,3,3]] = 0;
  WCI[[1,2,1,1]] = WCI[[1,1,1,2]];
  WCI[[1,2,2,1]] = 0;
  WCI[[1,3,1,1]] = WCI[[1,1,1,3]];
  WCI[[1,3,1,2]] = WCI[[1,2,1,3]];
  WCI[[1,3,2,1]] = - WCI[[1,2,3,1]];
  WCI[[1,3,3,1]] = 0;
  WCI[[2,1,1,1]] = - WCI[[1,1,1,2]];
  WCI[[2,1,1,2]] = 0;
  WCI[[2,1,1,3]] = - WCI[[1,2,3,1]];
  WCI[[2,1,2,1]] = - WCI[[1,2,1,2]];
  WCI[[2,1,2,2]] = - WCI[[1,2,2,2]];
  WCI[[2,1,2,3]] = - WCI[[1,2,3,2]];
  WCI[[2,1,3,1]] = - WCI[[1,2,1,3]];
  WCI[[2,1,3,2]] = - WCI[[1,2,2,3]];
  WCI[[2,1,3,3]] = - WCI[[1,2,3,3]];
  WCI[[2,2,1,1]] = 0;
  WCI[[2,2,1,2]] = WCI[[1,2,2,2]];
  WCI[[2,2,1,3]] = WCI[[1,3,2,2]];
  WCI[[2,2,2,1]] = - WCI[[1,2,2,2]];
  WCI[[2,2,2,2]] = 0;
  WCI[[2,2,3,1]] = - WCI[[1,3,2,2]];
  WCI[[2,2,3,2]] = - WCI[[2,2,2,3]];
  WCI[[2,2,3,3]] = 0;
  WCI[[2,3,1,1]] = WCI[[1,1,2,3]];
  WCI[[2,3,1,2]] = WCI[[1,2,2,3]];
  WCI[[2,3,1,3]] = WCI[[1,3,2,3]];
  WCI[[2,3,2,1]] = - WCI[[1,2,3,2]];
  WCI[[2,3,2,2]] = WCI[[2,2,2,3]];
  WCI[[2,3,3,1]] = - WCI[[1,3,3,2]];
  WCI[[2,3,3,2]] = 0;
  WCI[[3,1,1,1]] = - WCI[[1,1,1,3]];
  WCI[[3,1,1,2]] = WCI[[1,2,3,1]];
  WCI[[3,1,1,3]] = 0;
  WCI[[3,1,2,1]] = - WCI[[1,2,1,3]];
  WCI[[3,1,2,2]] = - WCI[[1,3,2,2]];
  WCI[[3,1,2,3]] = - WCI[[1,3,3,2]];
  WCI[[3,1,3,1]] = - WCI[[1,3,1,3]];
  WCI[[3,1,3,2]] = - WCI[[1,3,2,3]];
  WCI[[3,1,3,3]] = - WCI[[1,3,3,3]];
  WCI[[3,2,1,1]] = - WCI[[1,1,2,3]];
  WCI[[3,2,1,2]] = WCI[[1,2,3,2]];
  WCI[[3,2,1,3]] = WCI[[1,3,3,2]];
  WCI[[3,2,2,1]] = - WCI[[1,2,2,3]];
  WCI[[3,2,2,2]] = - WCI[[2,2,2,3]];
  WCI[[3,2,2,3]] = 0;
  WCI[[3,2,3,1]] = - WCI[[1,3,2,3]];
  WCI[[3,2,3,2]] = - WCI[[2,3,2,3]];
  WCI[[3,2,3,3]] = - WCI[[2,3,3,3]];
  WCI[[3,3,1,1]] = 0;
  WCI[[3,3,1,2]] = WCI[[1,2,3,3]];
  WCI[[3,3,1,3]] = WCI[[1,3,3,3]];
  WCI[[3,3,2,1]] = - WCI[[1,2,3,3]];
  WCI[[3,3,2,2]] = 0;
  WCI[[3,3,2,3]] = WCI[[2,3,3,3]];
  WCI[[3,3,3,1]] = - WCI[[1,3,3,3]];
  WCI[[3,3,3,2]] = - WCI[[2,3,3,3]];
  WCI[[3,3,3,3]] = 0;
];

If[cat == 5,(* two independent XX currents *)
(* Real part *)
  WCR[[1,1,2,1]] = WCR[[1,1,1,2]];
  WCR[[1,1,3,1]] = WCR[[1,1,1,3]];
  WCR[[1,1,3,2]] = WCR[[1,1,2,3]];
  WCR[[2,1,1,1]] = WCR[[1,2,1,1]];
  WCR[[2,1,1,2]] = WCR[[1,2,2,1]];
  WCR[[2,1,1,3]] = WCR[[1,2,3,1]];
  WCR[[2,1,2,1]] = WCR[[1,2,1,2]];
  WCR[[2,1,2,2]] = WCR[[1,2,2,2]];
  WCR[[2,1,2,3]] = WCR[[1,2,3,2]];
  WCR[[2,1,3,1]] = WCR[[1,2,1,3]];
  WCR[[2,1,3,2]] = WCR[[1,2,2,3]];
  WCR[[2,1,3,3]] = WCR[[1,2,3,3]];
  WCR[[2,2,2,1]] = WCR[[2,2,1,2]];
  WCR[[2,2,3,1]] = WCR[[2,2,1,3]];
  WCR[[2,2,3,2]] = WCR[[2,2,2,3]];
  WCR[[3,1,1,1]] = WCR[[1,3,1,1]];
  WCR[[3,1,1,2]] = WCR[[1,3,2,1]];
  WCR[[3,1,1,3]] = WCR[[1,3,3,1]];
  WCR[[3,1,2,1]] = WCR[[1,3,1,2]];
  WCR[[3,1,2,2]] = WCR[[1,3,2,2]];
  WCR[[3,1,2,3]] = WCR[[1,3,3,2]];
  WCR[[3,1,3,1]] = WCR[[1,3,1,3]];
  WCR[[3,1,3,2]] = WCR[[1,3,2,3]];
  WCR[[3,1,3,3]] = WCR[[1,3,3,3]];
  WCR[[3,2,1,1]] = WCR[[2,3,1,1]];
  WCR[[3,2,1,2]] = WCR[[2,3,2,1]];
  WCR[[3,2,1,3]] = WCR[[2,3,3,1]];
  WCR[[3,2,2,1]] = WCR[[2,3,1,2]];
  WCR[[3,2,2,2]] = WCR[[2,3,2,2]];
  WCR[[3,2,2,3]] = WCR[[2,3,3,2]];
  WCR[[3,2,3,1]] = WCR[[2,3,1,3]];
  WCR[[3,2,3,2]] = WCR[[2,3,2,3]];
  WCR[[3,2,3,3]] = WCR[[2,3,3,3]];
  WCR[[3,3,2,1]] = WCR[[3,3,1,2]];
  WCR[[3,3,3,1]] = WCR[[3,3,1,3]];
  WCR[[3,3,3,2]] = WCR[[3,3,2,3]];
(* Imaginary part *)
  WCI[[1,1,1,1]] = 0;
  WCI[[1,1,2,1]] = - WCI[[1,1,1,2]];
  WCI[[1,1,2,2]] = 0;
  WCI[[1,1,3,1]] = - WCI[[1,1,1,3]];
  WCI[[1,1,3,2]] = - WCI[[1,1,2,3]];
  WCI[[1,1,3,3]] = 0;
  WCI[[2,1,1,1]] = - WCI[[1,2,1,1]];
  WCI[[2,1,1,2]] = - WCI[[1,2,2,1]];
  WCI[[2,1,1,3]] = - WCI[[1,2,3,1]];
  WCI[[2,1,2,1]] = - WCI[[1,2,1,2]];
  WCI[[2,1,2,2]] = - WCI[[1,2,2,2]];
  WCI[[2,1,2,3]] = - WCI[[1,2,3,2]];
  WCI[[2,1,3,1]] = - WCI[[1,2,1,3]];
  WCI[[2,1,3,2]] = - WCI[[1,2,2,3]];
  WCI[[2,1,3,3]] = - WCI[[1,2,3,3]];
  WCI[[2,2,1,1]] = 0;
  WCI[[2,2,2,1]] = - WCI[[2,2,1,2]];
  WCI[[2,2,2,2]] = 0;
  WCI[[2,2,3,1]] = - WCI[[2,2,1,3]];
  WCI[[2,2,3,2]] = - WCI[[2,2,2,3]];
  WCI[[2,2,3,3]] = 0;
  WCI[[3,1,1,1]] = - WCI[[1,3,1,1]];
  WCI[[3,1,1,2]] = - WCI[[1,3,2,1]];
  WCI[[3,1,1,3]] = - WCI[[1,3,3,1]];
  WCI[[3,1,2,1]] = - WCI[[1,3,1,2]];
  WCI[[3,1,2,2]] = - WCI[[1,3,2,2]];
  WCI[[3,1,2,3]] = - WCI[[1,3,3,2]];
  WCI[[3,1,3,1]] = - WCI[[1,3,1,3]];
  WCI[[3,1,3,2]] = - WCI[[1,3,2,3]];
  WCI[[3,1,3,3]] = - WCI[[1,3,3,3]];
  WCI[[3,2,1,1]] = - WCI[[2,3,1,1]];
  WCI[[3,2,1,2]] = - WCI[[2,3,2,1]];
  WCI[[3,2,1,3]] = - WCI[[2,3,3,1]];
  WCI[[3,2,2,1]] = - WCI[[2,3,1,2]];
  WCI[[3,2,2,2]] = - WCI[[2,3,2,2]];
  WCI[[3,2,2,3]] = - WCI[[2,3,3,2]];
  WCI[[3,2,3,1]] = - WCI[[2,3,1,3]];
  WCI[[3,2,3,2]] = - WCI[[2,3,2,3]];
  WCI[[3,2,3,3]] = - WCI[[2,3,3,3]];
  WCI[[3,3,1,1]] = 0;
  WCI[[3,3,2,1]] = - WCI[[3,3,1,2]];
  WCI[[3,3,2,2]] = 0;
  WCI[[3,3,3,1]] = - WCI[[3,3,1,3]];
  WCI[[3,3,3,2]] = - WCI[[3,3,2,3]];
  WCI[[3,3,3,3]] = 0;
];

If[cat == 6,(* two identical XX currents special case Cee *)
(* Real part *)
  WCR[[1,1,2,1]] = WCR[[1,1,1,2]];
  WCR[[1,1,3,1]] = WCR[[1,1,1,3]];
  WCR[[1,1,3,2]] = WCR[[1,1,2,3]];
  WCR[[1,2,1,1]] = WCR[[1,1,1,2]];
  WCR[[1,2,2,1]] = WCR[[1,1,2,2]];
  WCR[[1,2,3,1]] = WCR[[1,1,2,3]];
  WCR[[1,3,1,1]] = WCR[[1,1,1,3]];
  WCR[[1,3,1,2]] = WCR[[1,2,1,3]];
  WCR[[1,3,2,1]] = WCR[[1,1,2,3]];
  WCR[[1,3,2,2]] = WCR[[1,2,2,3]];
  WCR[[1,3,3,1]] = WCR[[1,1,3,3]];
  WCR[[1,3,3,2]] = WCR[[1,2,3,3]];
  WCR[[2,1,1,1]] = WCR[[1,1,1,2]];
  WCR[[2,1,1,2]] = WCR[[1,1,2,2]];
  WCR[[2,1,1,3]] = WCR[[1,1,2,3]];
  WCR[[2,1,2,1]] = WCR[[1,2,1,2]];
  WCR[[2,1,2,2]] = WCR[[1,2,2,2]];
  WCR[[2,1,2,3]] = WCR[[1,2,3,2]];
  WCR[[2,1,3,1]] = WCR[[1,2,1,3]];
  WCR[[2,1,3,2]] = WCR[[1,2,2,3]];
  WCR[[2,1,3,3]] = WCR[[1,2,3,3]];
  WCR[[2,2,1,1]] = WCR[[1,1,2,2]];
  WCR[[2,2,1,2]] = WCR[[1,2,2,2]];
  WCR[[2,2,1,3]] = WCR[[1,2,2,3]];
  WCR[[2,2,2,1]] = WCR[[1,2,2,2]];
  WCR[[2,2,3,1]] = WCR[[1,2,2,3]];
  WCR[[2,2,3,2]] = WCR[[2,2,2,3]];
  WCR[[2,3,1,1]] = WCR[[1,1,2,3]];
  WCR[[2,3,1,2]] = WCR[[1,2,2,3]];
  WCR[[2,3,1,3]] = WCR[[1,3,2,3]];
  WCR[[2,3,2,1]] = WCR[[1,2,3,2]];
  WCR[[2,3,2,2]] = WCR[[2,2,2,3]];
  WCR[[2,3,3,1]] = WCR[[1,2,3,3]];
  WCR[[2,3,3,2]] = WCR[[2,2,3,3]];
  WCR[[3,1,1,1]] = WCR[[1,1,1,3]];
  WCR[[3,1,1,2]] = WCR[[1,1,2,3]];
  WCR[[3,1,1,3]] = WCR[[1,1,3,3]];
  WCR[[3,1,2,1]] = WCR[[1,2,1,3]];
  WCR[[3,1,2,2]] = WCR[[1,2,2,3]];
  WCR[[3,1,2,3]] = WCR[[1,2,3,3]];
  WCR[[3,1,3,1]] = WCR[[1,3,1,3]];
  WCR[[3,1,3,2]] = WCR[[1,3,2,3]];
  WCR[[3,1,3,3]] = WCR[[1,3,3,3]];
  WCR[[3,2,1,1]] = WCR[[1,1,2,3]];
  WCR[[3,2,1,2]] = WCR[[1,2,3,2]];
  WCR[[3,2,1,3]] = WCR[[1,2,3,3]];
  WCR[[3,2,2,1]] = WCR[[1,2,2,3]];
  WCR[[3,2,2,2]] = WCR[[2,2,2,3]];
  WCR[[3,2,2,3]] = WCR[[2,2,3,3]];
  WCR[[3,2,3,1]] = WCR[[1,3,2,3]];
  WCR[[3,2,3,2]] = WCR[[2,3,2,3]];
  WCR[[3,2,3,3]] = WCR[[2,3,3,3]];
  WCR[[3,3,1,1]] = WCR[[1,1,3,3]];
  WCR[[3,3,1,2]] = WCR[[1,2,3,3]];
  WCR[[3,3,1,3]] = WCR[[1,3,3,3]];
  WCR[[3,3,2,1]] = WCR[[1,2,3,3]];
  WCR[[3,3,2,2]] = WCR[[2,2,3,3]];
  WCR[[3,3,2,3]] = WCR[[2,3,3,3]];
  WCR[[3,3,3,1]] = WCR[[1,3,3,3]];
  WCR[[3,3,3,2]] = WCR[[2,3,3,3]];
(* Imaginary part *)
  WCI[[1,1,1,1]] = 0;
  WCI[[1,1,2,1]] = - WCI[[1,1,1,2]];
  WCI[[1,1,2,2]] = 0;
  WCI[[1,1,3,1]] = - WCI[[1,1,1,3]];
  WCI[[1,1,3,2]] = - WCI[[1,1,2,3]];
  WCI[[1,1,3,3]] = 0;
  WCI[[1,2,1,1]] = WCI[[1,1,1,2]];
  WCI[[1,2,2,1]] = 0;
  WCI[[1,2,3,1]] = - WCI[[1,1,2,3]];
  WCI[[1,3,1,1]] = WCI[[1,1,1,3]];
  WCI[[1,3,1,2]] = WCI[[1,2,1,3]];
  WCI[[1,3,2,1]] = WCI[[1,1,2,3]];
  WCI[[1,3,2,2]] = WCI[[1,2,2,3]];
  WCI[[1,3,3,1]] = 0;
  WCI[[1,3,3,2]] = WCI[[1,2,3,3]];
  WCI[[2,1,1,1]] = - WCI[[1,1,1,2]];
  WCI[[2,1,1,2]] = 0;
  WCI[[2,1,1,3]] = WCI[[1,1,2,3]];
  WCI[[2,1,2,1]] = - WCI[[1,2,1,2]];
  WCI[[2,1,2,2]] = - WCI[[1,2,2,2]];
  WCI[[2,1,2,3]] = - WCI[[1,2,3,2]];
  WCI[[2,1,3,1]] = - WCI[[1,2,1,3]];
  WCI[[2,1,3,2]] = - WCI[[1,2,2,3]];
  WCI[[2,1,3,3]] = - WCI[[1,2,3,3]];
  WCI[[2,2,1,1]] = 0;
  WCI[[2,2,1,2]] = WCI[[1,2,2,2]];
  WCI[[2,2,1,3]] = WCI[[1,2,2,3]];
  WCI[[2,2,2,1]] = - WCI[[1,2,2,2]];
  WCI[[2,2,2,2]] = 0;
  WCI[[2,2,3,1]] = - WCI[[1,2,2,3]];
  WCI[[2,2,3,2]] = - WCI[[2,2,2,3]];
  WCI[[2,2,3,3]] = 0;
  WCI[[2,3,1,1]] = WCI[[1,1,2,3]];
  WCI[[2,3,1,2]] = WCI[[1,2,2,3]];
  WCI[[2,3,1,3]] = WCI[[1,3,2,3]];
  WCI[[2,3,2,1]] = - WCI[[1,2,3,2]];
  WCI[[2,3,2,2]] = WCI[[2,2,2,3]];
  WCI[[2,3,3,1]] = - WCI[[1,2,3,3]];
  WCI[[2,3,3,2]] = 0;
  WCI[[3,1,1,1]] = - WCI[[1,1,1,3]];
  WCI[[3,1,1,2]] = - WCI[[1,1,2,3]];
  WCI[[3,1,1,3]] = 0;
  WCI[[3,1,2,1]] = - WCI[[1,2,1,3]];
  WCI[[3,1,2,2]] = - WCI[[1,2,2,3]];
  WCI[[3,1,2,3]] = - WCI[[1,2,3,3]];
  WCI[[3,1,3,1]] = - WCI[[1,3,1,3]];
  WCI[[3,1,3,2]] = - WCI[[1,3,2,3]];
  WCI[[3,1,3,3]] = - WCI[[1,3,3,3]];
  WCI[[3,2,1,1]] = - WCI[[1,1,2,3]];
  WCI[[3,2,1,2]] = WCI[[1,2,3,2]];
  WCI[[3,2,1,3]] = WCI[[1,2,3,3]];
  WCI[[3,2,2,1]] = - WCI[[1,2,2,3]];
  WCI[[3,2,2,2]] = - WCI[[2,2,2,3]];
  WCI[[3,2,2,3]] = 0;
  WCI[[3,2,3,1]] = - WCI[[1,3,2,3]];
  WCI[[3,2,3,2]] = - WCI[[2,3,2,3]];
  WCI[[3,2,3,3]] = - WCI[[2,3,3,3]];
  WCI[[3,3,1,1]] = 0;
  WCI[[3,3,1,2]] = WCI[[1,2,3,3]];
  WCI[[3,3,1,3]] = WCI[[1,3,3,3]];
  WCI[[3,3,2,1]] = - WCI[[1,2,3,3]];
  WCI[[3,3,2,2]] = 0;
  WCI[[3,3,2,3]] = WCI[[2,3,3,3]];
  WCI[[3,3,3,1]] = - WCI[[1,3,3,3]];
  WCI[[3,3,3,2]] = - WCI[[2,3,3,3]];
  WCI[[3,3,3,3]] = 0;
];

tmp = WCR + I WCI;

];

If[cat == 7,(* B- violating special case qque *)
  tmp[[2,1,1,1]] = tmp[[1,2,1,1]];
  tmp[[2,1,1,2]] = tmp[[1,2,1,2]];
  tmp[[2,1,1,3]] = tmp[[1,2,1,3]];
  tmp[[2,1,2,1]] = tmp[[1,2,2,1]];
  tmp[[2,1,2,2]] = tmp[[1,2,2,2]];
  tmp[[2,1,2,3]] = tmp[[1,2,2,3]];
  tmp[[2,1,3,1]] = tmp[[1,2,3,1]];
  tmp[[2,1,3,2]] = tmp[[1,2,3,2]];
  tmp[[2,1,3,3]] = tmp[[1,2,3,3]];
  tmp[[3,1,1,1]] = tmp[[1,3,1,1]];
  tmp[[3,1,1,2]] = tmp[[1,3,1,2]];
  tmp[[3,1,1,3]] = tmp[[1,3,1,3]];
  tmp[[3,1,2,1]] = tmp[[1,3,2,1]];
  tmp[[3,1,2,2]] = tmp[[1,3,2,2]];
  tmp[[3,1,2,3]] = tmp[[1,3,2,3]];
  tmp[[3,1,3,1]] = tmp[[1,3,3,1]];
  tmp[[3,1,3,2]] = tmp[[1,3,3,2]];
  tmp[[3,1,3,3]] = tmp[[1,3,3,3]];
  tmp[[3,2,1,1]] = tmp[[2,3,1,1]];
  tmp[[3,2,1,2]] = tmp[[2,3,1,2]];
  tmp[[3,2,1,3]] = tmp[[2,3,1,3]];
  tmp[[3,2,2,1]] = tmp[[2,3,2,1]];
  tmp[[3,2,2,2]] = tmp[[2,3,2,2]];
  tmp[[3,2,2,3]] = tmp[[2,3,2,3]];
  tmp[[3,2,3,1]] = tmp[[2,3,3,1]];
  tmp[[3,2,3,2]] = tmp[[2,3,3,2]];
  tmp[[3,2,3,3]] = tmp[[2,3,3,3]];
];

If[cat == 8,(* B- violating special case qqql *)
  tmp[[2,1,1,1]] = tmp[[1,1,2,1]];
  tmp[[2,1,1,2]] = tmp[[1,1,2,2]];
  tmp[[2,1,1,3]] = tmp[[1,1,2,3]];
  tmp[[2,2,1,1]] = tmp[[1,2,2,1]];
  tmp[[2,2,1,2]] = tmp[[1,2,2,2]];
  tmp[[2,2,1,3]] = tmp[[1,2,2,3]];
  tmp[[3,1,1,1]] = tmp[[1,1,3,1]];
  tmp[[3,1,1,2]] = tmp[[1,1,3,2]];
  tmp[[3,1,1,3]] = tmp[[1,1,3,3]];
  tmp[[3,1,2,1]] = tmp[[2,3,1,1]] + tmp[[2,1,3,1]] - tmp[[1,3,2,1]];
  tmp[[3,1,2,2]] = tmp[[2,3,1,2]] + tmp[[2,1,3,2]] - tmp[[1,3,2,2]];
  tmp[[3,1,2,3]] = tmp[[2,3,1,3]] + tmp[[2,1,3,3]] - tmp[[1,3,2,3]];
  tmp[[3,2,1,1]] = tmp[[1,3,2,1]] + tmp[[1,2,3,1]] - tmp[[2,3,1,1]];
  tmp[[3,2,1,2]] = tmp[[1,3,2,2]] + tmp[[1,2,3,2]] - tmp[[2,3,1,2]];
  tmp[[3,2,1,3]] = tmp[[1,3,2,3]] + tmp[[1,2,3,3]] - tmp[[2,3,1,3]];
  tmp[[3,2,2,1]] = tmp[[2,2,3,1]];
  tmp[[3,2,2,2]] = tmp[[2,2,3,2]];
  tmp[[3,2,2,3]] = tmp[[2,2,3,3]];
  tmp[[3,3,1,1]] = tmp[[1,3,3,1]];
  tmp[[3,3,1,2]] = tmp[[1,3,3,2]];
  tmp[[3,3,1,3]] = tmp[[1,3,3,3]];
  tmp[[3,3,2,1]] = tmp[[2,3,3,1]];
  tmp[[3,3,2,2]] = tmp[[2,3,3,2]];
  tmp[[3,3,2,3]] = tmp[[2,3,3,3]];
];

tmp

]
];


