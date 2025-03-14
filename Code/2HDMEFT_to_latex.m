(* latex output generator *)
(* Modified from SmeftFR v3.0 package https://www.fuw.edu.pl/smeft/ *)
 
  
 (* latex output auxiliary variables *)

  FieldType = {h1, h2, h3, h, H, A0, HP, HPbar, G0, GP, GPbar,
               G, Z, A, W, Wbar,
               l, vl, dq, uq, lbar, vlbar, dqbar, uqbar,
               ghWp, ghWm, ghZ, ghA, ghG,
               ghWpbar, ghWmbar, ghZbar, ghAbar, ghGbar,
               lc, vlc, dqc, uqc, lcbar, vlcbar, dqcbar, uqcbar};
  
  (* hL stands for light Higgs so the program does not write H over h *)
  (* A0 stands for CP-odd additional Higgs not to be confused with the photon A *)
  
  InFieldName = {"h1", "h2", "h3", "hL", "H", "A0", "HP", "HP", "G0","GP", "GP",
                 "g", "Z", "A", "W", "W",
                 "e", "v", "dq", "uq", "ec", "vc", "dqc", "uqc",
                 "etaP", "etaM", "etaZ", "etaA", "etaG",
                 "etabarP", "etabarM", "etabarZ", "etabarA", "etabarG",
                 "ec", "v", "dq", "uq", "ec", "v", "dq", "uq"};

  OutFieldName = {"h1", "h2", "h3", "hL", "H", "A0", "HP", "HP", "G0", "GP", "GP",
                  "g", "Z", "A", "W", "W",
                  "e", "v", "dq", "uq", "ec", "vc", "dqc", "uqc",
                  "etaP", "etaM", "etaZ", "etaA", "etaG",
                  "etabarP", "etabarM", "etabarZ", "etabarA", "etabarG",
                  "ec", "v", "dq", "uq", "ec", "v", "dq", "uq"};

  InFieldName  = Rule @@@ Partition[Riffle[FieldType, InFieldName],2];
  OutFieldName = Rule @@@ Partition[Riffle[FieldType, OutFieldName],2];

  FieldTex = {"h_1", "h_2", "h_3", "h", "H", "A^0", "H^+", "H^-", "G^0", "G^+", "G^-",
              "g", "Z", "A", "W^+", "W^-",
              "e", "\\nu", "d", "u", " e", "\\nu", " d", " u",
             "\\eta^+", "\\eta^-", "\\eta_Z", "\\eta_A", "\\eta_G",
             "\\bar\\eta^+", "\\bar\\eta^-", "\\bar\\eta_Z", "\\bar\\eta_A", "\\bar\\eta_G",
             "(e^c)", "(\\nu^c)", "(d^c)", "(u^c)", "(e^c)", "(\\nu^c)", "(d^c)", "(u^c)"};

  LineType =  {1,1,1,1,1,1,2,2,1,2,2,3,4,4,7,7,
                  6,6,6,6,5,5,5,5,
                  8,8,8,8,8,
                  9,9,9,9,9,
                  6,6,6,6,5,5,5,5};


 (* auxiliary functions for generation of diagrams with latex version of Feynman rules *)


 Dim6VertexName = Function[{plist},
 (* find vertex name tag *)
 Module[{tmp,f,fc},
   f = {"v","e","uq","dq"};
   fc = {"vc","ec","uqc","dqc"};
   tmp = plist[[All,1]];
   tmp[[1]] = tmp[[1]] /. OutFieldName;
   tmp[[2]] = tmp[[2]] /. InFieldName;
   tmp[[3]] = tmp[[3]] /. OutFieldName;
   If [Length[tmp] > 3, tmp[[4]] = tmp[[4]] /. InFieldName];
   tmp = tmp /. OutFieldName;
   If [Length[tmp] == 4 && MemberQ[fc,tmp[[1]]] && MemberQ[f,tmp[[2]]] &&
       MemberQ[fc,tmp[[3]]] && MemberQ[f,tmp[[4]]],
     tmp[[1]] = StringDrop[tmp[[1]],-1];
     tmp[[3]] = StringDrop[tmp[[3]],-1];
     tmp[[2]] = tmp[[2]] <> "c";
     tmp[[4]] = tmp[[4]] <> "c";
   ];

   StringJoin[ Sort[ tmp ] ]
 ]

 ];


 FieldNameAndIndex = Function[{pos, i, DiagType},
 Module[{flav, ind, is},

 flav  = {0,0,0,0,0,0,0,0,0,0,0,3,4,4,4,4,1,1,2,2,1,1,2,2,0,0,0,0,5,0,0,0,0,5,1,1,2,2,1,1,2,2};
 is = ToString[i];

 ind = Switch[flav[[pos]],
          0, "",
          1, "^{f_" ~~ is ~~ "}" ~~ If[DiagType == 2, "_{s_" ~~ is ~~ "}",""],
          2, "^{f_" ~~ is ~~ "}" ~~ Switch[ DiagType,
                            1, "_{m_" ~~ is ~~ "}",
                            2, "_{m_" ~~ is ~~ " s_" ~~ is ~~ "}",
                            _, "" ],
          3, "^{a_" ~~ is ~~ "}_{\\mu_" ~~ is ~~ "}",
          4, "_{\\mu_" ~~ is ~~ "}",
          5, "^{a_" ~~ is ~~ "}"
 ];

 "" ~~ FieldTex[[pos]] ~~ ind ~~ ""

 ]
 (*end of FieldNameAndIndex *)
 ]

 
 (* Independent lists of WC for formatting preferences *)
 
Yuk = {
    Ye1[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Ye1[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Ye2[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Ye2[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    
    Yd1[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Yd1[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Yd2[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Yd2[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    
    Yu1[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Yu1[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Yu2[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Yu2[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    
    Ye1[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Ye1[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    Ye2[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Ye2[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    
    Yd1[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Yd1[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    Yd2[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Yd2[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    
    Yu1[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Yu1[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    Yu2[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Yu2[Index[Generation, Generation$1], Index[Generation, Ext[1]]]]
};

 WCvv = {
    Cvvphi11[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Cvvphi11[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Cvvphi22[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Cvvphi22[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Cvvphi12[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Cvvphi12[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    
    Cvvphi11[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Cvvphi11[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    Cvvphi22[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Cvvphi22[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    Cvvphi12[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Cvvphi12[Index[Generation, Generation$1], Index[Generation, Ext[1]]]]
    
};
 
 WCpsi2phi3 = {
    Clphi111[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Clphi111[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Clphi122[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Clphi122[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Clphi112[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Clphi112[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Clphi121[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Clphi121[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    
    Clphi211[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Clphi211[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Clphi222[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Clphi222[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Clphi212[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Clphi212[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Clphi221[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Clphi221[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    
    Clphi111[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Clphi111[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    Clphi122[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Clphi122[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    Clphi112[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Clphi112[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    Clphi121[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Clphi121[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    
    Clphi211[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Clphi211[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    Clphi222[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Clphi222[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    Clphi212[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Clphi212[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    Clphi221[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Clphi221[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    
    Cdphi111[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Cdphi111[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Cdphi122[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Cdphi122[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Cdphi112[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Cdphi112[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Cdphi121[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Cdphi121[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    
    Cdphi211[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Cdphi211[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Cdphi222[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Cdphi222[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Cdphi212[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Cdphi212[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Cdphi221[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Cdphi221[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    
    Cdphi111[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Cdphi111[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    Cdphi122[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Cdphi122[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    Cdphi112[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Cdphi112[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    Cdphi121[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Cdphi121[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    
    Cdphi211[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Cdphi211[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    Cdphi222[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Cdphi222[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    Cdphi212[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Cdphi212[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    Cdphi221[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Cdphi221[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    
    Cuphi111[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Cuphi111[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Cuphi122[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Cuphi122[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Cuphi112[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Cuphi112[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Cuphi121[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Cuphi121[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    
    Cuphi211[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Cuphi211[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Cuphi222[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Cuphi222[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Cuphi212[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Cuphi212[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Cuphi221[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Cuphi221[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    
    Cuphi111[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Cuphi111[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    Cuphi122[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Cuphi122[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    Cuphi112[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Cuphi112[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    Cuphi121[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Cuphi121[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    
    Cuphi211[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Cuphi211[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    Cuphi222[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Cuphi222[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    Cuphi212[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Cuphi212[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    Cuphi221[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Cuphi221[Index[Generation, Generation$1], Index[Generation, Ext[1]]]]
    
};
 
 WCpsi2Xphi = {
    ClBphi1[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[ClBphi1[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    ClBphi2[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[ClBphi2[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    ClWphi1[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[ClWphi1[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    ClWphi2[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[ClWphi2[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    
    ClBphi1[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[ClBphi1[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    ClBphi2[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[ClBphi2[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    ClWphi1[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[ClWphi1[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    ClWphi2[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[ClWphi2[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    
    CdBphi1[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[CdBphi1[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    CdBphi2[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[CdBphi2[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    CdWphi1[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[CdWphi1[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    CdWphi2[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[CdWphi2[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    CdGphi1[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[CdGphi1[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    CdGphi2[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[CdGphi2[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    
    CuBphi1[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[CuBphi1[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    CuBphi2[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[CuBphi2[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    CuWphi1[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[CuWphi1[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    CuWphi2[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[CuWphi2[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    CuGphi1[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[CuGphi1[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    CuGphi2[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[CuGphi2[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    
    CdBphi1[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[CdBphi1[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    CdBphi2[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[CdBphi2[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    CdWphi1[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[CdWphi1[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    CdWphi2[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[CdWphi2[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    CdGphi1[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[CdGphi1[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    CdGphi2[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[CdGphi2[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    
    CuBphi1[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[CuBphi1[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    CuBphi2[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[CuBphi2[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    CuWphi1[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[CuWphi1[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    CuWphi2[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[CuWphi2[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    CuGphi1[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[CuGphi1[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    CuGphi2[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[CuGphi2[Index[Generation, Generation$1], Index[Generation, Ext[1]]]]
    
};
 
 WCpsi2phi2DherL = {

    Cphil111[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Cphil221[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Cphil113[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Cphil223[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    
    Cphil113[Index[Generation, Ext[1]], Index[Generation, Generation$1]],
    Cphil223[Index[Generation, Ext[1]], Index[Generation, Generation$1]],
    Cphil113[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Cphil223[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    
    Cphil111[Index[Generation, Generation$1], Index[Generation, Generation$2]],
    Cphil221[Index[Generation, Generation$1], Index[Generation, Generation$2]],
    Cphil113[Index[Generation, Generation$1], Index[Generation, Generation$2]],
    Cphil223[Index[Generation, Generation$1], Index[Generation, Generation$2]],
    
    Cphiq111[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Cphiq221[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Cphiq113[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Cphiq223[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    
    Cphiq113[Index[Generation, Ext[1]], Index[Generation, Generation$1]],
    Cphiq223[Index[Generation, Ext[1]], Index[Generation, Generation$1]],
    Cphiq113[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Cphiq223[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    
    Cphiq111[Index[Generation, Generation$1], Index[Generation, Generation$2]],
    Cphiq221[Index[Generation, Generation$1], Index[Generation, Generation$2]],
    Cphiq113[Index[Generation, Generation$1], Index[Generation, Generation$2]],
    Cphiq223[Index[Generation, Generation$1], Index[Generation, Generation$2]]
    
};
 
 WCpsi2phi2DherR = {
    Cphie11[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Cphie22[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    
    Cphid11[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Cphid22[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],

    Cphiu11[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Cphiu22[Index[Generation, Ext[1]], Index[Generation, Ext[2]]]
    
};
 
 WCpsi2phi2DnherL = {

    Cphil121[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Cphil121[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Cphil123[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Cphil123[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    
    Cphil121[Index[Generation, Ext[1]], Index[Generation, Generation$1]],
    Conjugate[Cphil121[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    Cphil123[Index[Generation, Ext[1]], Index[Generation, Generation$1]],
    Conjugate[Cphil123[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    
    Cphil121[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Cphil121[Index[Generation, Ext[2]], Index[Generation, Generation$1]]],
    Cphil123[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Cphil123[Index[Generation, Ext[2]], Index[Generation, Generation$1]]],

    Cphil121[Index[Generation, Generation$1], Index[Generation, Generation$2]],
    Conjugate[Cphil121[Index[Generation, Generation$2], Index[Generation, Generation$1]]],
    Cphil123[Index[Generation, Generation$1], Index[Generation, Generation$2]],
    Conjugate[Cphil123[Index[Generation, Generation$2], Index[Generation, Generation$1]]],
        
    
    Cphiq121[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Cphiq121[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Cphiq123[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Cphiq123[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
        
    Cphiq121[Index[Generation, Ext[1]], Index[Generation, Generation$1]],
    Conjugate[Cphiq121[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    Cphiq123[Index[Generation, Ext[1]], Index[Generation, Generation$1]],
    Conjugate[Cphilq23[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
                
    Cphiq121[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Cphiq121[Index[Generation, Ext[2]], Index[Generation, Generation$1]]],
    Cphiq123[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Cphiq123[Index[Generation, Ext[2]], Index[Generation, Generation$1]]],
                        
    Cphiq121[Index[Generation, Generation$1], Index[Generation, Generation$2]],
    Conjugate[Cphiq121[Index[Generation, Generation$2], Index[Generation, Generation$1]]],
    Cphiq123[Index[Generation, Generation$1], Index[Generation, Generation$2]],
    Conjugate[Cphiq123[Index[Generation, Generation$2], Index[Generation, Generation$1]]]
    
};
 
 WCpsi2phi2DnherR = {
    
    Cphie12[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Cphie12[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    
    Cphid12[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Cphid12[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Cphiu12[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Cphiu12[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    
    Cphiud11[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Cphiud11[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Cphiud22[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Cphiud22[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Cphiud21[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Cphiud21[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]]
    
};
 
 WCpsi4 = { cll, cqq1, clq1, cee, c uu, ceu, cud1, cle, clu, cqu1, cqd1, cledq, cquqd1, cquqd8, clequ1, clequ3, cduq, cqqu, cqqq, cduu
    
};
 
 
 WCX3 = { cG, cGtil, cW, cWtil };
 
 WCphi6her = {
    cphi111111, cphi111122, cphi222211, cphi222222, cphi112112, cphi222112
};
 
 
 WCphi6nher = {
    cphi111121, cphi222221, cphi112121, cphi222121, cphi212121, cphi212112, cphi112221
};
 
 
 WCphi4D2her = {
    cphidpar1111, cphiD1111, cphidpar2222, cphiD2222, cphidpar1122, cphiD1122,
    cphidpar2112, cphiD2112
    
};
 
 WCphi4D2nher = {
    cphidpar2121, cphiD2121, cphidpar2111, cphiD2111, cphidpar2122, cphiD2122
};
 
 WCX2phi2hernt = {
    cphiG11, cphiG22, cphiB11, cphiB22,
    cphiW11, cphiW22, cphiWB11, cphiWB22
};
 
 WCX2phi2hert = {
    cphiGtil11, cphiGtil22, cphiBtil11, cphiBtil22,
    cphiWtil11, cphiWtil22, cphiWBtil11, cphiWBtil22
};
 
 WCX2phi2nhernt = {
    cphiG21, cphiB21, cphiW21, cphiWB21
};
 
 WCX2phi2nhert = {
    cphiGtil21, cphiBtil21, cphiWtil21, cphiWBtil21
};
 
 (* Field redefinitions of scalars *)
 WCcpv = { Jc, Kc, Lc, Nc };
  
 factorCollectHF = {WCpsi2phi2DherL, WCpsi2phi2DherR};
 factorCollectHCF = {WCcpv, Yuk, WCvv, WCpsi2phi3, WCpsi2Xphi, WCpsi2phi2DnherL, WCpsi2phi2DnherR};
 
 factorCollectHBS = {WCX3, WCphi6her, WCphi4D2her, WCX2phi2hernt, WCX2phi2hert};
 factorCollectHCBS = {WCcpv, WCphi6nher, WCphi4D2nher, WCX2phi2nhernt, WCX2phi2nhert};
 
trigSimp = { 
    Cos[_]^2 + Sin[_]^2 -> 1,
    -Cos[_]^2 - Sin[_]^2 -> -1,
    Cos[x_]^2 - Sin[x_]^2 -> Cos[2 x],
    2 Sin[x_] Cos[x_]-> Sin[2 x],
    Sin[x_] Cos[y_] + Cos[x_] Sin[y_] -> Sin[x + y],
    Sin[x_] Cos[y_] - Cos[x_] Sin[y_] -> Sin[x - y],
    Cos[x_] Cos[y_] + Sin[x_] Sin[y_] -> Cos[x - y],
    Cos[x_] Cos[y_] - Sin[x_] Sin[y_] -> Cos[x + y]};
 
 mList = {
    IndexDelta[Index[Colour, Ext[1]], Index[Colour, Ext[2]]],
    IndexDelta[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    IndexDelta[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    
    Upmns[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Upmns[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Upmns[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Upmns[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    Upmns[Index[Generation, Ext[1]], Index[Generation, Generation$1]],
    Conjugate[Upmns[Index[Generation, Ext[2]], Index[Generation, Generation$1]]],
    Upmns[Index[Generation, Ext[2]], Index[Generation, Generation$2]],
    Conjugate[Upmns[Index[Generation, Ext[1]], Index[Generation, Generation$2]]],
    
    Vckm[Index[Generation, Ext[1]], Index[Generation, Ext[2]]],
    Conjugate[Vckm[Index[Generation, Ext[2]], Index[Generation, Ext[1]]]],
    Vckm[Index[Generation, Generation$1], Index[Generation, Ext[2]]],
    Conjugate[Vckm[Index[Generation, Generation$1], Index[Generation, Ext[1]]]],
    Vckm[Index[Generation, Ext[1]], Index[Generation, Generation$1]],
    Conjugate[Vckm[Index[Generation, Ext[2]], Index[Generation, Generation$1]]],
    Vckm[Index[Generation, Ext[2]], Index[Generation, Generation$2]],
    Conjugate[Vckm[Index[Generation, Ext[1]], Index[Generation, Generation$2]]]
};
 
 (* Formatting algorithm for different types of contributions that are hermitian, non-hermitian, and dimention-4 terms *)
 
organizeFactors = Function[{expr, fieldList, WCList, key},
     Module[{i, cfactor, factoredNum, coeff, lorentz, sm, sm1, sm2, ind, res},
            
            
    If[Simplify[expr] =!= 0,
       
       coeff = {};
       lorentz = {};

       expr1 =  Simplify[expr, Trig -> False] /. {Cos[beta]^2 + Sin[beta]^2 -> 1};
       exprList = List@@expr1;

       (* Sorts array such that all WC/Yukawa terms are in the last entry in order to extract common factor *)
       Do[ If[ StringContainsQ[ToString[ TeXForm[exprList[[j]]] ], "\\hat{C}"] == True ||
               StringContainsQ[ToString[ TeXForm[exprList[[j]]] ], "\\text{y}"] == True ||
               StringContainsQ[ToString[ TeXForm[exprList[[j]]] ], "X_"] == True,
                tp = exprList[[-1]];
                exprList[[-1]] = exprList[[j]];
                exprList[[j]] = tp], {j, Length[exprList]}];
    
       (* If still remains, then there is no common factor present and coefficient is 1 *)

       If[ StringContainsQ[ToString[ TeXForm[exprList[[1]]] ], "\\hat{C}"] == True, 
           cfactor = 1;
         ,
           cfactor = Times @@ Delete[exprList, -1];
         ];
       
    If[ key == 1,
        
            (* Sorting algorithm to put Hermitian conjugate terms and like terms together *)

            If[ (ToString[fieldList[[1, 1]]] == "lbar" && ToString[fieldList[[2, 1]]] == "l") ||
                (ToString[fieldList[[1, 1]]] == "vlbar" && ToString[fieldList[[2, 1]]] == "vl") ||
                (ToString[fieldList[[1, 1]]] == "uqbar" && ToString[fieldList[[2, 1]]] == "uq") ||
                (ToString[fieldList[[1, 1]]] == "dqbar" && ToString[fieldList[[2, 1]]] == "dq"),
                   sm = Collect[expr, WCList];
                   sm1 = List@@sm;
                   sm1 = Simplify[sm1 / cfactor, Trig -> False];

                
                   ind = {};
                
                   Do[ If[ FreeQ[sm1[[j]], WCList[[i]]] == False, AppendTo[ind, j] ], {i, Length[WCList]}, {j, Length[sm1]} ];

                   sm2 = {};
                   For[i = 1, i <= Length[sm1], i++, AppendTo[sm2, sm1[[ ind[[i]] ]]] ];
                
                   sm = Together[sm2];

                   tmp = 0;
        
                If[StringCount[ ToString[ TeXForm[expr] ], "\\hat{C}"] != 1,
                
                   For[i = 1, i < Length[sm], i += 2, tmp += Together[Simplify[sm[[i]] + sm[[i + 1]], Trig -> False]]];
                    factoredNum = tmp;
                   
                   ,
                   
                   factoredNum = expr / cfactor;
                   
                  ];
                
                If[ StringContainsQ[ToString[ TeXForm[ cfactor ] ], "M"] &&
                    (ToString[fieldList[[1, 1]]] == "dqbar" && ToString[fieldList[[2, 1]]] == "dq" && ToString[fieldList[[3, 1]]] == "G0") ||
                    (ToString[fieldList[[1, 1]]] == "uqbar" && ToString[fieldList[[2, 1]]] == "uq" && ToString[fieldList[[3, 1]]] == "G0"),

                        cfactor = FactorTerms[ Simplify[ReleaseHold[cfactor], Trig -> False] ];
                                            
                    ];
            ,
                factoredNum = Simplify[expr / cfactor, Trig -> False] // Expand;
                
                
              ];
       
       ,

        factoredNum = Simplify[expr / cfactor, Trig -> False];
        
        
      ];
            

            cfactor = cfactor //. trigSimp // ReleaseHold;
            factoredNum = factoredNum //. trigSimp // ReleaseHold;
            factoredNum = factoredNum /. {IndexDelta[Index[Colour, Ext[1]], Index[Colour, Ext[1]]] -> 1,
                                          IndexDelta[Index[Spin, Ext[1]], Index[Spin, Ext[1]]] -> 1,
                                          IndexDelta[Index[Generation, Ext[1]], Index[Generation, Ext[1]]] -> 1};
       
            coeff = AppendTo[coeff, cfactor];
            lorentz = AppendTo[lorentz, factoredNum];
       
            coeff = ToString[ TeXForm[ # ] ] & /@ coeff;
                   
           (* remove blank spaces *)
       
            coeff = StringReplace[coeff, (StartOfString ~~ Whitespace) | (Whitespace ~~ EndOfString) :> ""];
            For[ i=1, i < Length[coeff]+1, i++,
              If [ coeff[[i]] == "1",  coeff[[i]] = " " ];
              If [ coeff[[i]] == "-1" || coeff[[i]] == "- 1",  coeff[[i]] = "-" ];
            ];
       
       (* set nicely formatted first sign in the line *)
            coeff = If[ StringTake[#,1] == "-", " - " <> StringDrop[#, 1], " + " <> # ] & /@ coeff;
            lorentz = ToString[ TeXForm[ # ] ] & /@ lorentz;
       
            (* Nice formatting for field redefinitions from Higgs sector *)
       
            If[StringContainsQ[lorentz[[1]], "M"] == True,
                tmpL = lorentz[[1]];
                tmpL = StringReplace[tmpL, "\\right)\\right) \\left(" -> "\\right) \\right) \\right. \\\\ \n&  \\left. \\times \\left("];
                lorentz[[1]] = StringJoin[tmpL];
       
            ];
       
            If[key == 0 && ToString[fieldList[[3, 1]]] == "Z" && Length[fieldList] == 3,
               tmpL = lorentz[[1]];
               tmpL = StringReplace[tmpL, "P_-{}_{\\text{s}_1,\\text{s}_2}+" ~~ Shortest[x__] ~~ "P_+{}_{\\text{s}_1,\\text{s}_2}" -> "P_-{}_{\\text{s}_1,\\text{s}_2} \\right. \\\\ \n&  \\left. +" ~~ x ~~ "P_+{}_{\\text{s}_1,\\text{s}_2}"];
               tmpL = StringReplace[tmpL, "P_-{}_{\\text{s}_1,\\text{s}_2}-" ~~ Shortest[x__] ~~ "P_+{}_{\\text{s}_1,\\text{s}_2}" -> "P_-{}_{\\text{s}_1,\\text{s}_2} \\right. \\\\ \n&  \\left. -" ~~ x ~~ "P_+{}_{\\text{s}_1,\\text{s}_2}"];
               lorentz[[1]] = StringJoin[tmpL];
  
              ];
       
            If[ WCList == WCX2phi2nher || WCList == WCphi4D2nher,
                tmpL = lorentz[[1]];
                tmpL = StringReplace[tmpL, "+" -> "tx+"];
                tmpL = StringReplace[tmpL, "-" -> "tx-"];
                tmpL = StringSplit[tmpL, {"tx"}];

                For[ii = 2, ii < Length[tmpL], ii += 2, tmpL[[ii]] = tmpL[[ii]] <> " \\right. \\\\ \n& \\left. "  ];

                lorentz[[1]] = StringJoin[tmpL];

              ];
       

            If[ WCList == WCpsi2phi3,
                tmpL = lorentz[[1]];
                tmpL = StringReplace[tmpL, "}+" -> "}tx+"];
                tmpL = StringReplace[tmpL, "}-" -> "}tx-"];
                
                If[ (ToString[fieldList[[1, 1]]] == "dqbar" && ToString[fieldList[[2, 1]]] == "uq" ||
                     ToString[fieldList[[1, 1]]] == "uqbar" && ToString[fieldList[[2, 1]]] == "dq"),
                    tmpL = StringReplace[tmpL, "*+" -> "*tx+"];
                    tmpL = StringReplace[tmpL, "*-" -> "*tx-"];
                ];

                tmpL = StringSplit[tmpL, {"tx"}];

                For[ii = 2, ii < Length[tmpL], ii += 2, tmpL[[ii]] = tmpL[[ii]] <> " \\right. \\\\ \n& \\left. "  ];

                lorentz[[1]] = StringJoin[tmpL];

              ];
  
 
            (* Nice formatting for charged currents by every second term *)
       
             If[ WCList == WCpsi2Xphi,
                 tmpL = lorentz[[1]];
                 
                 If[ (ToString[fieldList[[1, 1]]] == "vlbar" && ToString[fieldList[[2, 1]]] == "l" ||
                      ToString[fieldList[[1, 1]]] == "lbar" && ToString[fieldList[[2, 1]]] == "vl" ||
                      ToString[fieldList[[1, 1]]] == "dqbar" && ToString[fieldList[[2, 1]]] == "uq" ||
                      ToString[fieldList[[1, 1]]] == "uqbar" && ToString[fieldList[[2, 1]]] == "dq"),
                     
                     tmpL = StringReplace[tmpL, "}+" -> "}tx+"];
                     tmpL = StringReplace[tmpL, "}-" -> "}tx-"];
                     tmpL = StringReplace[tmpL, "*+" -> "*tx+"];
                     tmpL = StringReplace[tmpL, "*-" -> "*tx-"];
                     tmpL = StringSplit[tmpL, {"tx"}];
                 ];

                For[jj = 2, jj < Length[tmpL], jj += 2, tmpL[[jj]] = tmpL[[jj]] <> " \\right. \\\\ \n& \\left. "  ];
       
                lorentz[[1]] = StringJoin[tmpL];
       
                ];

       If[ WCList == WCpsi2Xphi || WCList == WCX3,
           
           tmpL = lorentz[[1]];
           tmpL = StringReplace[tmpL, "\\hat{C}_{" ~~ Shortest[x__] ~~ "}_{" ~~ Shortest[y__] ~~ "}" -> "\\hat{C}_{" ~~ x ~~ "}^{\\quad}_{" ~~ y ~~ "}"];
           lorentz[[1]] = StringJoin[tmpL];
  
           ];
       
       
       Do[ If[ MemberQ[{h1, h2, h3}, fieldList[[kk, 1]]] == True,
               
               tmpL = lorentz[[1]];

               tmpL = StringReplace[tmpL, "}+" -> "}tx+"];
               tmpL = StringReplace[tmpL, "}-" -> "}tx-"];
               tmpL = StringReplace[tmpL, "*+" -> "*tx+"];
               tmpL = StringReplace[tmpL, "*-" -> "*tx-"];
               tmpL = StringSplit[tmpL, {"tx"}];

               For[jj = 4, jj < Length[tmpL], jj += 4, tmpL[[jj]] = tmpL[[jj]] <> " \\right. \\\\ \n& \\left. "  ];
 
               lorentz[[1]] = StringJoin[tmpL];
               
               Break[];
             ], {kk, 1, Length[fieldList]} ];

              
            lorentz[[1]] = List @@ " & \\left(" <> lorentz[[1]] <> "\\right)";

            res = Riffle[coeff, lorentz];
       
            tmpX = res;
            tmpX = StringReplace[tmpX, ")" -> "\\right)"];
            tmpX = StringReplace[tmpX, "\\right\\right)" -> "\\right)"];
            tmpX = StringReplace[tmpX, "(" -> "\\left("];
            tmpX = StringReplace[tmpX, "\\left\\left(" -> "\\left("];
            res = StringJoin[tmpX];
            res = "\\\\ \n& \\begin{aligned}" <> res <> "\\end{aligned}";

       , res = "";
      
      ];
            res
    ]
 ]
 
 
ToLaTeXForm = Function[{expr, fieldList, DiagType},
Module[{i, ii, terms, rTerms, herTerms, nherTerms, totalExpr},
    
totalListF = Join[factorCollectHCF, factorCollectHF];
totalListBS = Join[factorCollectHBS, factorCollectHCBS];
                  
totalExpr = "";
rTerms = expr;
       

If[DiagType == 0 || DiagType == 2,

For[i = 1, i <= Length[totalListF], i++,
    totalTerms = "";
    terms = 0;
    
    Do[ If[ FreeQ[List@@rTerms[[k]], totalListF[[i, j]]] == False, terms += rTerms[[k]] ], {j, Length[totalListF[[i]]]}, {k, Length[rTerms]}];
    
    herTerms = 0;
    nherTerms = 0;
        
    Do[ If[ FreeQ[totalListF[[i]], factorCollectHCF[[k]]] == False, nherTerms = terms], {k, Length[factorCollectHCF]} ];
    
    herTerms = terms - nherTerms;
    
    (* dimension-four Hermitian terms are what remain *)

    rTerms -= terms;
    
    totalTerms = organizeFactors[herTerms, fieldList, totalListF[[i]], 0] <> organizeFactors[nherTerms, fieldList, totalListF[[i]], 1];
    
    totalExpr = totalExpr <> totalTerms;

   ];
   
   ,
 
   For[ii = 1, ii <= Length[totalListBS], ii++,
       totalTerms = "";
       terms = 0;
       
       Do[ If[ FreeQ[List@@rTerms[[k]], totalListBS[[ii, j]]] == False, terms += rTerms[[k]] ], {j, Length[totalListBS[[ii]]]}, {k, Length[rTerms]}];
       
       herTerms = 0;
       nherTerms = 0;
           
       Do[ If[ FreeQ[totalListBS[[ii]], factorCollectHCBS[[k]]] == False, nherTerms = terms], {k, Length[factorCollectHCBS]} ];
       
       herTerms = terms - nherTerms;
       
       
       (* dimension-four Hermitian terms are what remain *)

       rTerms -= terms;
       
       totalTerms = organizeFactors[herTerms, fieldList, totalListBS[[ii]], 0] <> organizeFactors[nherTerms, fieldList, totalListBS[[ii]], 1];
       
       totalExpr = totalExpr <> totalTerms;

      ];
   
  ];

totalExpr = organizeFactors[rTerms, fieldList, "", 0] <> totalExpr;
totalExpr
 

]
(* end of ToLaTexForm *)
]




ToLaTeX = Function[{expr, DiagType, verbose, fieldList},
Block[{SIG},Module[{tmp,res,x,y,v,z,a,b,c,i,s1,s2,s3,s4,sprod},

tmp = expr /. LambdaCutoff -> 1;

tmp = tmp  /. TensDot[Ga[x_], SlashedP[y_], z_][s1_, s2_] ->
              FV[y, x] z[s1,s2] - I SIG[x, \[Nu], z, s1, s2] FV[y, \[Nu]]  /.
              TensDot[SlashedP[y_], Ga[x_], z_][s1_, s2_] ->
              FV[y, x] z[s1,s2] + I SIG[x, \[Nu], z, s1, s2] FV[y, \[Nu]]  // Expand;

tmp = tmp /. TensDot[Ga[x_], Ga[y_], z_][s1_, s2_] -> ME[x, y] z[s1,s2] - I SIG[x, y, z, s1, s2] // Expand;

tmp = tmp /. SIG[Index[Lorentz, mu$2], Index[Lorentz, mu$1], x_, s1_, s2_] ->
           - SIG[Index[Lorentz, mu$1], Index[Lorentz, mu$2], x, s1, s2];

tmp = tmp /. SIG[Index[Lorentz, Ext[a_]], Index[Lorentz, Ext[b_]], x_, y_, z_] ->
    If[a <= b,  SIG[Index[Lorentz, Ext[a]], Index[Lorentz, Ext[b]], x, y, z],
              - SIG[Index[Lorentz, Ext[b]], Index[Lorentz, Ext[a]], x, y, z] ];
    
tmp = tmp /. Eps[x_, y_, v_, z_] FV[a_, y_] FV[b_, v_] FV[c_, z_] ->
    Eps[x, y, v, z] Switch[{a > b, a > c, b > c},
	   {False, False, False} ,  FV[a, y] FV[b, v] FV[c, z],
	   {False, False, True}, -  FV[a, y] FV[b, z] FV[c, v],
	   {True, False, False}, -  FV[a, v] FV[b, y] FV[c, z],
	   {True, True, True},   -  FV[a, z] FV[b, v] FV[c, y],
	   {True, True, False},     FV[a, z] FV[b, y] FV[c, v],
	   {False, True, True},     FV[a, v] FV[b, z] FV[c, y]];

tmp = tmp /. Eps[x_, y_, v_, z_] FV[s1_, v_] FV[s2_, z_] ->
    Eps[x, y, v, z] If[s2 >= s1,  FV[s1, v] FV[s2, z], - FV[s1, z] FV[s2, v] ];

tmp = tmp /. SP[x_,y_] -> sprod[x,y];
                   
        
tmp = ToLaTeXForm[tmp, fieldList, DiagType];

tmp = StringReplace[tmp,"Generation$\$$"->"g_"];
tmp = StringReplace[tmp,"Gluon$\$$"->"b_"];
                   
tmp = StringReplace[tmp,"mu$\$$1"->"\\mu"];
tmp = StringReplace[tmp,"mu$\$$2"->"\\nu"];
tmp = StringReplace[tmp, "{$\\alpha $1}" -> "{$\\lambda$}"];
tmp = StringReplace[tmp, "{$\\beta $1}" -> "{$\\rho$}"];
tmp = StringReplace[tmp, "{$\\gamma $1}" -> "{$\\sigma$}"];
tmp = StringReplace[tmp, "{$\\delta $1}" -> "{$\\chi$}"];


tmp = StringReplace[tmp,"{}"->""];
tmp = StringReplace[tmp, "\\text{" ~~ Shortest[x__] ~~ "}" -> x];
tmp = StringReplace[tmp,"\\text" -> ""];

tmp = StringReplace[tmp,"Mh"->"m_h"];
tmp = StringReplace[tmp,"MH"->"m_H"];
tmp = StringReplace[tmp,"MA"->"m_A"];
tmp = StringReplace[tmp,"MHP"->"m_{H^{\\pm}}"];
tmp = StringReplace[tmp,"MW"->"m_W"];
tmp = StringReplace[tmp,"MZ"->"m_Z"];


tmp = StringReplace[tmp," ^"->"^"];
tmp = StringReplace[tmp,"^ "->"^"];
tmp = StringReplace[tmp," _"->"_"];
tmp = StringReplace[tmp,"_ "->"_"];
                   

tmp = StringReplace[tmp,"\Lambda^2"->" "];
tmp = StringReplace[tmp,"\Lambda"->" "];
                   

tmp = StringReplace[tmp,"  "->" "];
tmp = StringReplace[tmp," _"->"_"];
tmp = StringReplace[tmp,"_ "->"_"];

tmp = StringReplace[tmp,"P_-" -> "{\mathcal P}_{L}"];
tmp = StringReplace[tmp,"P_+" -> "{\mathcal P}_{R}"];

tmp = StringReplace[tmp,"+"->" + "];
tmp = StringReplace[tmp,"-"->" - "];
                   
(* Masses *)
                   
tmp = StringReplace[tmp, "fml\\left(" ~~ Shortest[x__] ~~ "," ~~ Shortest[y__] ~~ "\\right)"->
		    "m_{l_{" ~~ x ~~ "}} \\delta_{" ~~ x ~~ " " ~~ y ~~ "}" ];
tmp = StringReplace[tmp, "fmv\\left(" ~~ Shortest[x__] ~~ "," ~~ Shortest[y__] ~~ "\\right)"->
		    "m_{\\nu_{" ~~ x ~~ "}} \\delta_{" ~~ x ~~ " " ~~ y ~~ "}" ];
tmp = StringReplace[tmp, "fmu\\left(" ~~ Shortest[x__] ~~ "," ~~ Shortest[y__] ~~ "\\right)"->
		    "m_{u_{" ~~ x ~~ "}} \\delta_{" ~~ x ~~ " " ~~ y ~~ "}" ];
tmp = StringReplace[tmp, "fmd\\left(" ~~ Shortest[x__] ~~ "," ~~ Shortest[y__] ~~ "\\right)"->
		    "m_{d_{" ~~ x ~~ "}} \\delta_{" ~~ x ~~ " " ~~ y ~~ "}" ];

        
tmp = StringReplace[tmp, "{\\nu }" -> "{\\nu}"];
tmp = StringReplace[tmp, "m^{\\nu}_{" ~~ Shortest[x__] ~~ "," ~~ Shortest[y__] ~~"}" ->
		    "m_{\\nu_{" ~~ x ~~ "}} \\delta_{" ~~ x ~~ " " ~~ y ~~ "}" ];
tmp = StringReplace[tmp, "m^l_{" ~~ Shortest[x__] ~~ "," ~~ Shortest[y__] ~~"}" ->
		    "m_{l_{" ~~ x ~~ "}} \\delta_{" ~~ x ~~ " " ~~ y ~~ "}" ];
tmp = StringReplace[tmp, "m^u_{" ~~ Shortest[x__] ~~ "," ~~ Shortest[y__] ~~"}" ->
		    "m_{u_{" ~~ x ~~ "}} \\delta_{" ~~ x ~~ " " ~~ y ~~ "}" ];
tmp = StringReplace[tmp, "m^d_{" ~~ Shortest[x__] ~~ "," ~~ Shortest[y__] ~~"}" ->
		    "m_{d_{" ~~ x ~~ "}} \\delta_{" ~~ x ~~ " " ~~ y ~~ "}" ];

tmp = StringReplace[tmp, "{Subsuperscript}[p," ~~ x_ ~~ "," ~~ Shortest[y__] ~~ "]" ->
                         "p_{" ~~ x ~~ "}^{" ~~ y ~~ "}"];
tmp = StringReplace[tmp, "{Subsuperscript}\\left[p," ~~ x_ ~~ "," ~~ Shortest[y__] ~~ "\\right]" ->
                         "p_{" ~~ x ~~ "}^{" ~~ y ~~ "}"];
tmp = StringReplace[tmp, "sprod$\\$$" ~~ Shortest[z___] ~~ "(" ~~ Shortest[x__] ~~ "," ~~ Shortest[y__] ~~ ")" ->
                         "p_{" ~~ x ~~ "}\\nobreak\\cdot\\nobreak{}p_{" ~~ y ~~ "}"];

tmp = StringReplace[tmp,"$"->" "]; 

tmp = StringReplace[tmp, "m_" ~~ x_ ~~ "_{" ~~ Shortest[y__] ~~ "}" ->
                         "m_{" ~~ x ~~ "_{" ~~ y ~~ "}}"];

tmp = StringReplace[tmp, "SlashedP\\left(" ~~ Shortest[x__] ~~ "\\right)" ->
                         "\\slashed{p}_{" ~~ x ~~ "}"];
(* tmp = StringReplace[tmp,"SIG" ~~ Shortest[x__] ~~ "\\left(" -> "SIG\\left("];*)
If[DiagType == 2, 
(*   tmp = StringReplace[tmp, "\\gamma^{\\mu}.P_" ~~ Shortest[x_] ~~ "_{s_" ~~ Shortest[s1_] ~~
		       ",s_" ~~ Shortest[s2_] ~~ "} \\gamma^{\\mu}.P_" ~~ Shortest[y_] ~~
		       "_{s_" ~~ Shortest[s3_] ~~ ",s_" ~~ Shortest[s4_] ~~ "}" ->
		       "(\\gamma^{\\mu}.P_" ~~ x ~~ ")_{s_" ~~ s1 ~~ " s_" ~~ s2 ~~
		       "} (\\gamma_{\\mu}.P_" ~~ y ~~ ")_{s_" ~~ s3 ~~ " s_" ~~ s4 ~~ "}"];*)
(* with the replacement in line below all Lorentz indices are always up... *)
   tmp = StringReplace[tmp, "\\gamma^{\\mu}.P_" ~~ Shortest[x_] ~~ "_{s_" ~~ Shortest[s1_] ~~
		       ",s_" ~~ Shortest[s2_] ~~ "}" ->
		       "(\\gamma^{\\mu}.P_" ~~ x ~~ ")_{s_" ~~ s1 ~~ " s_" ~~ s2 ~~ "}"];  
   tmp = StringReplace[tmp, "SIG\\left(\\mu,\\nu,P_" ~~ Shortest[x_] ~~ ",s_" ~~ Shortest[s1_] ~~
		       ",s_" ~~ Shortest[s2_] ~~ "\\right) SIG\\left(\\mu,\\nu,P_" ~~
		       Shortest[y_] ~~ ",s_" ~~ Shortest[s3_] ~~ ",s_" ~~ Shortest[s4_] ~~ "\\right)" ->
		       "(\\sigma^{\\mu\\nu} P_" ~~ x ~~ ")_{s_" ~~ s1 ~~ " s_" ~~ s2 ~~
		       "} (\\sigma_{\\mu\\nu} P_" ~~ y ~~ ")_{s_" ~~ s3 ~~ " s_" ~~ s4 ~~ "}" ]; 
];

                   
tmp = StringReplace[tmp, "SIG\\left(" ~~ Shortest[x__] ~~ "," ~~ Shortest[y__] ~~ "," ~~
                                         Shortest[z__] ~~ "," ~~ Shortest[v__] ~~ "\\right)" ->
                         "\\sigma^{" ~~ x ~~ " " ~~ y ~~ "} " ~~ z ~~ " "];
                   
                   
tmp = StringReplace[tmp, "\\eta_{" ~~ Shortest[x__] ~~ "}" -> "g_{" ~~ x ~~ "}"];
                                      

If [DiagType == 0,
  tmp = StringReplace[tmp,"\\frac{\\delta_{m_1,m_2}}" -> "\\frac{1}"];
  tmp = StringReplace[tmp,"\\delta_{m_1,m_2}" -> ""];
];
If [DiagType == 0 || DiagType == 1,
  tmp = StringReplace[tmp,"\\frac{\\delta_{s_1,s_2}}" -> "\\frac{1}"];
  tmp = StringReplace[tmp,"\\delta_{s_1,s_2}" -> ""];
  tmp = StringReplace[tmp,"_{s_1,s_2}" -> ""];
];
      
                
(* Trig formatting *)
                   
tmp = StringReplace[tmp, "\\sin \\left(" ~~ Shortest[x__] ~~ "\\right)" -> "s_{" ~~ x ~~ "}"];
tmp = StringReplace[tmp, "\\cos \\left(" ~~ Shortest[x__] ~~ "\\right)" -> "c_{" ~~ x ~~ "}"];
tmp = StringReplace[tmp, "\\tan \\left(" ~~ Shortest[x__] ~~ "\\right)" -> "t_{" ~~ x ~~ "}"];
tmp = StringReplace[tmp, "\\cot \\left(" ~~ Shortest[x__] ~~ "\\right)" -> "ct_{" ~~ x ~~ "}"];
                   
tmp = StringReplace[tmp, "\\sin^" ~~ Shortest[x__] ~~ "\\left(" ~~ Shortest[y__] ~~ "\\right)" -> "s_{" ~~ y ~~ "}^{" ~~ x ~~ "}"];
tmp = StringReplace[tmp, "\\cos^" ~~ Shortest[x__] ~~ "\\left(" ~~ Shortest[y__] ~~ "\\right)" -> "c_{" ~~ y ~~ "}^{" ~~ x ~~ "}"];
tmp = StringReplace[tmp, "\\tan^" ~~ Shortest[x__] ~~ "\\left(" ~~ Shortest[y__] ~~ "\\right)" -> "t_{" ~~ y ~~ "}^{" ~~ x ~~ "}"];
tmp = StringReplace[tmp, "\\cot^" ~~ Shortest[x__] ~~ "\\left(" ~~ Shortest[y__] ~~ "\\right)" -> "ct_{" ~~ y ~~ "}^{" ~~ x ~~ "}"];
                   
tmp = StringReplace[tmp, "P_"~~ Shortest[x_] ~~ "_{s_" ~~ Shortest[s1_] ~~ ",s_" ~~ Shortest[s2_] ~~ "}" ->
                        "\\left(P_"~~ x ~~ "\\right)_{s_" ~~ s1 ~~ " s_" ~~ s2 ~~ "}"];
                   
                   
tmp = StringReplace[tmp, "m_{" ~~ Shortest[x__] ~~ "}_{" ~~ Shortest[y__] ~~ "}"
                    -> "m_{" ~~ x ~~ "_{" ~~ y ~~ "}}"];

tmp = StringReplace[tmp, "M_{" ~~ Shortest[x__] ~~ "}^2^2" -> "\\left( M_{" ~~ x ~~ "}^2 \\right)^2"];
                      
tmp = StringReplace[tmp, ".{"->"{"];
tmp = StringReplace[tmp, "}."->"}"];
tmp = StringReplace[tmp, ","->" "];
tmp = StringReplace[tmp, "'^2" ->"^{\\prime 2}"];
tmp = StringReplace[tmp, "'^3" ->"^{\\prime 3}"];
tmp = StringReplace[tmp, "'^4" ->"^{\\prime 4}"];
tmp = StringReplace[tmp, "'^5" ->"^{\\prime 5}"];
tmp = StringReplace[tmp, "'^6" ->"^{\\prime 6}"];
                   
tmp = StringReplace[tmp, "p_" ~~ Shortest[x_] ~~ "^{" ~~ Shortest[y__] ~~ "}" -> "p_{" ~~ x ~~ " " ~~ y ~~ "}"];
tmp = StringReplace[tmp, "p_{" ~~ Shortest[x_] ~~ "\\right}" -> "p_{" ~~ x ~~ "}"];

If[StringContainsQ[tmp, "\\epsilon"] == True,
   tmp = StringReplace[tmp, "p_{" ~~ Shortest[x__] ~~ "\\mu}" -> "p_{" ~~ x ~~ "}^{\\mu}"];
   tmp = StringReplace[tmp, "p_{" ~~ Shortest[x__] ~~ "\\nu}" -> "p_{" ~~ x ~~ "}^{\\nu}"];
   tmp = StringReplace[tmp, "p_{" ~~ Shortest[x__] ~~ "\\lambda }" -> "p_{" ~~ x ~~ "}^{\\lambda}"];
   tmp = StringReplace[tmp, "p_{" ~~ Shortest[x__] ~~ "\\rho }" -> "p_{" ~~ x ~~ "}^{\\rho}"];
   tmp = StringReplace[tmp, "p_{" ~~ Shortest[x__] ~~ "\\sigma }" -> "p_{" ~~ x ~~ "}^{\\sigma}"];
   tmp = StringReplace[tmp, "p_{" ~~ Shortest[x__] ~~ "\\chi }" -> "p_{" ~~ x ~~ "}^{\\chi}"];
   , None
  ];
        
If[DiagType == 0,
   tmp = StringReplace[tmp, "\\hat{y}_" ~~ Shortest[x__] ~~ "^{" ~~ Shortest[y__] ~~ "}_{" ~~ Shortest[z__] ~~ "}" -> "\\hat{y}_{" ~~ x ~~ ", " ~~ z ~~ "}^{" ~~ y ~~ "}"];
                   
   tmp = StringReplace[tmp, "\\hat{C}_{" ~~ Shortest[x__] ~~ "}^{" ~~ Shortest[y__] ~~ "}_{" ~~ Shortest[z__] ~~ "}" -> "\\hat{C}_{" ~~ x ~~ ", " ~~ z ~~ "}^{" ~~ y ~~ "}"];
   
   tmp = StringReplace[tmp, "}^*" -> "*}"];
   tmp = StringReplace[tmp, "U_{" ~~ Shortest[x__] ~~ "*}" -> "U_{" ~~ x ~~ "}^*"];
   tmp = StringReplace[tmp, "V_{" ~~ Shortest[x__] ~~ "*}" -> "V_{" ~~ x ~~ "}^*"];

   ,
   
   tmp = StringReplace[tmp, "\\hat{C}_{" ~~ Shortest[x__] ~~ "}^{" ~~ Shortest[y__] ~~ "}^*" -> "\\hat{C}_{" ~~ x ~~ "}^{" ~~ y ~~ "*}"];
   
  ];

                    
tmp = StringReplace[tmp,  "\\left(" ~~ Shortest[x__] ~~ " +  \\right)" -> "\\left(" ~~ x ~~ " + 1 \\right)"];
                   
(* For removing empty denominators *)
                   
tmp = StringReplace[tmp, "\\frac{" ~~ Shortest[x__] ~~ "}{ }" -> "" ~~ x ~~ ""];

tmp = StringReplace[tmp, "\\left(\\right)" -> ""];
tmp = StringReplace[tmp, "+  1" -> " + "];
tmp = StringReplace[tmp, "-  1" -> " - "];
                   
tmp = StringDrop[tmp, {1, 4}];
             
If[DiagType == 0,
    tmp = StringReplace[tmp, ") + " ~~ Shortest[x__] ~~ "(" -> ") \\right. \\\\ \n& \\left. + " ~~ x ~~ "("];
    tmp = StringReplace[tmp, ") - " ~~ Shortest[x__] ~~ "(" -> ") \\right. \\\\ \n& \left. - " ~~ x ~~ "("];
    tmp = StringReplace[tmp, "\\right) + " ~~ Shortest[x__] ~~ "\\left(" -> "\\right) \\right. \\\\ \n& \\left. + " ~~ x ~~ "\\left("];
    tmp = StringReplace[tmp, "\\right) - " ~~ Shortest[x__] ~~ "\\left(" -> "\\right) \\right. \\\\ \n& \\left. - " ~~ x ~~ "\\left("];
   ,

 (*  tmp = StringReplace[tmp, "\\right) " ~~ Shortest[x_] ~~ " s_{" ~~ Shortest[y__] ~~ "} \\left(" -> "\\right) \\right. \\\\ \n& \\left." ~~ x ~~ "s_{" ~~ y ~~ "}\\left("];
                       
   tmp = StringReplace[tmp, "\\right) " ~~ Shortest[x_] ~~ " s_{" ~~ Shortest[y__] ~~ "} \\left(" -> "\\right) \\right. \\\\ \n& \\left." ~~ x ~~ "s_{" ~~ y ~~ "}\\left("];
   
   tmp = StringReplace[tmp, "\\right) " ~~ Shortest[x_] ~~ " c_{" ~~ Shortest[y__] ~~ "} \\left(" -> "\\right) \\right. \\\\ \n& \\left." ~~ x ~~ "s_{" ~~ y ~~ "}\\left("];
                       
   tmp = StringReplace[tmp, "\\right) " ~~ Shortest[x_] ~~ " c_{" ~~ Shortest[y__] ~~ "} \\left(" -> "\\right) \\right. \\\\ \n& \\left." ~~ x ~~ "s_{" ~~ y ~~ "}\\left("];
 
   tmp = StringReplace[tmp, "\\right) " ~~ Shortest[x_] ~~ " \\left(p" -> "\\right) \\right. \\\\ \n& \\left. " ~~ x ~~ " \\left(p"];
   tmp = StringReplace[tmp, "\\right) " ~~ Shortest[x_] ~~ " \\left(g" -> "\\right) \\right. \\\\ \n& \\left. " ~~ x ~~ " \\left(g"];
   tmp = StringReplace[tmp, "\\right) " ~~ Shortest[x_] ~~ " \\left(\hat" -> "\\right) \\right. \\\\ \n& \\left. " ~~ x ~~ " \\left(\hat"];
*)
   tmp = StringReplace[tmp, "{B \\Phi \\tilde{W}}" -> "{\\Phi B \\tilde{W}}"];

  ];


                   
(* Tricky formatting for Higgs field redefinitions for charged sector if needed *)
tmp = StringReplace[tmp, "\\hat{y}^{" ~~ Shortest[x__] ~~ "}_{e, g_1 f_1}^*" ~~ Shortest[y__] ~~ "\\hat{y}^{" ~~ Shortest[z__] ~~ "}_{e, g_1 f_1}^*" ~~ Shortest[w__] ~~ "\\left(\\frac{\\Delta " -> "\\hat{y}^{" ~~ x ~~ "}_{e, g_1 f_1}^*" ~~ y ~~ "\\hat{y}^{" ~~ z ~~ "}_{e, g_1 f_1}^* \\right. \\\\ \n&  \\left." ~~ w ~~ "\\left(\\frac{\\Delta "];

tmp = StringReplace[tmp, "\\hat{y}^{" ~~ Shortest[x__] ~~ "}_{e, g_1 f_2}" ~~ Shortest[y__] ~~ "\\hat{y}^{" ~~ Shortest[z__] ~~ "}_{e, g_1 f_2}" ~~ Shortest[w__] ~~ "\\left(\\frac{\\Delta " -> "\\hat{y}^{" ~~ x ~~ "}_{e, g_1 f_2}" ~~ y ~~ "\\hat{y}^{" ~~ z ~~ "}_{e, g_1 f_2} \\right. \\\\ \n&  \\left." ~~ w ~~ "\\left(\\frac{\\Delta "];

tmp = StringReplace[tmp, "\\hat{y}^{" ~~ Shortest[x__] ~~ "}_{d, g_1 f_1}^*" ~~ Shortest[y__] ~~ "\\hat{y}^{" ~~ Shortest[z__] ~~ "}_{d, g_1 f_1}^*" ~~ Shortest[w__] ~~ "\\left(\\frac{\\Delta " -> "\\hat{y}^{" ~~ x ~~ "}_{d, g_1 f_1}^*" ~~ y ~~ "\\hat{y}^{" ~~ z ~~ "}_{d, g_1 f_1}^* \\right. \\\\ \n&  \\left." ~~ w ~~ "\\left(\\frac{\\Delta "];

tmp = StringReplace[tmp, "\\hat{y}^{" ~~ Shortest[x__] ~~ "}_{d, g_1 f_2}" ~~ Shortest[y__] ~~ "\\hat{y}^{" ~~ Shortest[z__] ~~ "}_{d, g_1 f_2}" ~~ Shortest[w__] ~~ "\\left(\\frac{\\Delta " -> "\\hat{y}^{" ~~ x ~~ "}_{d, g_1 f_2}" ~~ y ~~ "\\hat{y}^{" ~~ z ~~ "}_{d, g_1 f_2} \\right. \\\\ \n&  \\left." ~~ w ~~ "\\left(\\frac{\\Delta "];

tmp = StringReplace[tmp, "\\hat{y}^{" ~~ Shortest[x__] ~~ "}_{u, g_1 f_1}^*" ~~ Shortest[y__] ~~ "\\hat{y}^{" ~~ Shortest[z__] ~~ "}_{u, g_1 f_1}^*" ~~ Shortest[w__] ~~ "\\left(\\frac{\\Delta " -> "\\hat{y}^{" ~~ x ~~ "}_{u, g_1 f_1}^*" ~~ y ~~ "\\hat{y}^{" ~~ z ~~ "}_{u, g_1 f_1}^* \\right. \\\\ \n&  \\left." ~~ w ~~ "\\left(\\frac{\\Delta "];

tmp = StringReplace[tmp, "\\hat{y}^{" ~~ Shortest[x__] ~~ "}_{u, g_1 f_2}" ~~ Shortest[y__] ~~ "\\hat{y}^{" ~~ Shortest[z__] ~~ "}_{u, g_1 f_2}" ~~ Shortest[w__] ~~ "\\left(\\frac{\\Delta " -> "\\hat{y}^{" ~~ x ~~ "}_{u, g_1 f_2}" ~~ y ~~ "\\hat{y}^{" ~~ z ~~ "}_{u, g_1 f_2} \\right. \\\\ \n&  \\left." ~~ w ~~ "\\left(\\frac{\\Delta "];
                   
                   
If[verbose,
   Print[tmp];
   Print[ ];
];

tmp <> "\n"

]]
(* end of ToLaTex *)
]

 
DrawFeynmanDiagram = Function[{lineList, fieldList, cfile, frule, vname},

    propType = lineList /.
                    {1 -> "scalar",
                     2 -> "charged scalar",
                     3 -> "gluon",
                     4 -> "boson",
                     5 -> "anti fermion",
                     6 -> "fermion",
                     7 -> "charged boson",
                     8 -> "ghost",
                     9 -> "ghost"};

                   
     particleLabel = fieldList;

     nPoint = Length[frule[[1]]];

     WriteString[cfile, "{\\small \\begin{flalign}\n"];
     WriteString[cfile, "& \\begin{aligned}\n"];
     WriteString[cfile, "\\begin{tikzpicture}\n"];
     WriteString[cfile, "\\begin{feynman}\n"];
     WriteString[cfile, "\\vertex (a0);\n"];
     
     If[nPoint == 3,
     
        WriteString[cfile, "\\vertex at ($(a0) + 2.0*(-1.0 cm, 0.0 cm)$) (a1){\("<> particleLabel[[1]] <>"\)};\n"];
        WriteString[cfile, "\\vertex at ($(a0) + 2.0*(0.5 cm, 0.866 cm)$) (a2){\("<> particleLabel[[2]] <>"\)};\n"];
        WriteString[cfile, "\\vertex at ($(a0) + 2.0*(0.5 cm, -0.866 cm)$) (a3){\("<> particleLabel[[3]] <>"\)};\n"];
        WriteString[cfile, "\\diagram* {\n"];
        WriteString[cfile, "(a1) -- ["<> propType[[1]] <>", thick] (a0);\n"];
        WriteString[cfile, "(a2) -- ["<> propType[[2]] <>", thick] (a0);\n"];
        WriteString[cfile, "(a3) -- ["<> propType[[3]] <>", thick] (a0);\n"];
        
       , None];
        
     If[nPoint == 4,
        
        WriteString[cfile, "\\vertex at ($(a0) + 2.0*(-1.0 cm, 0.0 cm)$) (a1){\("<> particleLabel[[1]] <>"\)};\n"];
        WriteString[cfile, "\\vertex at ($(a0) + 2.0*(0.0 cm, 1.0 cm)$) (a2){\("<> particleLabel[[2]] <>"\)};\n"];
        WriteString[cfile, "\\vertex at ($(a0) + 2.0*(1.0 cm, 0.0 cm)$) (a3){\("<> particleLabel[[3]] <>"\)};\n"];
        WriteString[cfile, "\\vertex at ($(a0) + 2.0*(0.0 cm, -1.0 cm)$) (a4){\("<> particleLabel[[4]] <>"\)};\n"];
        WriteString[cfile, "\\diagram* {\n"];
        WriteString[cfile, "(a1) -- ["<> propType[[1]] <>", thick] (a0);\n"];
        WriteString[cfile, "(a2) -- ["<> propType[[2]] <>", thick] (a0);\n"];
        WriteString[cfile, "(a3) -- ["<> propType[[3]] <>", thick] (a0);\n"];
        WriteString[cfile, "(a4) -- ["<> propType[[4]] <>", thick] (a0);\n"];
        
       , None];
     
     If[nPoint == 5,
        
        WriteString[cfile, "\\vertex at ($(a0) + 2.0*(-1.0 cm, 0.0 cm)$) (a1){\("<> particleLabel[[1]] <>"\)};\n"];
        WriteString[cfile, "\\vertex at ($(a0) + 2.0*(-0.309 cm, 0.951 cm)$) (a2){\("<> particleLabel[[2]] <>"\)};\n"];
        WriteString[cfile, "\\vertex at ($(a0) + 2.0*(0.809 cm, 0.588 cm)$) (a3){\("<> particleLabel[[3]] <>"\)};\n"];
        WriteString[cfile, "\\vertex at ($(a0) + 2.0*(0.809 cm, -0.588 cm)$) (a4){\("<> particleLabel[[4]] <>"\)};\n"];
        WriteString[cfile, "\\vertex at ($(a0) + 2.0*(-0.309 cm, -0.951 cm)$) (a5){\("<> particleLabel[[5]] <>"\)};\n"];
        WriteString[cfile, "\\diagram* {\n"];
        WriteString[cfile, "(a1) -- ["<> propType[[1]] <>", thick] (a0);\n"];
        WriteString[cfile, "(a2) -- ["<> propType[[2]] <>", thick] (a0);\n"];
        WriteString[cfile, "(a3) -- ["<> propType[[3]] <>", thick] (a0);\n"];
        WriteString[cfile, "(a4) -- ["<> propType[[4]] <>", thick] (a0);\n"];
        WriteString[cfile, "(a5) -- ["<> propType[[5]] <>", thick] (a0);\n"];
        
       , None];
     
     If[nPoint == 6,
        
        WriteString[cfile, "\\vertex at ($(a0) + 2.0*(-1.0 cm, 0.0 cm)$) (a1){\("<> particleLabel[[1]] <>"\)};\n"];
        WriteString[cfile, "\\vertex at ($(a0) + 2.0*(-0.5 cm, 0.809 cm)$) (a2){\("<> particleLabel[[2]] <>"\)};\n"];
        WriteString[cfile, "\\vertex at ($(a0) + 2.0*(0.5 cm, 0.809 cm)$) (a3){\("<> particleLabel[[3]] <>"\)};\n"];
        WriteString[cfile, "\\vertex at ($(a0) + 2.0*(1.0 cm, 0.0 cm)$) (a4){\("<> particleLabel[[4]] <>"\)};\n"];
        WriteString[cfile, "\\vertex at ($(a0) + 2.0*(0.5 cm, -0.809 cm)$) (a5){\("<> particleLabel[[5]] <>"\)};\n"];
        WriteString[cfile, "\\vertex at ($(a0) + 2.0*(-0.5 cm, -0.809 cm)$) (a6){\("<> particleLabel[[6]] <>"\)};\n"];
        WriteString[cfile, "\\diagram* {\n"];
        WriteString[cfile, "(a1) -- ["<> propType[[1]] <>", thick] (a0);\n"];
        WriteString[cfile, "(a2) -- ["<> propType[[2]] <>", thick] (a0);\n"];
        WriteString[cfile, "(a3) -- ["<> propType[[3]] <>", thick] (a0);\n"];
        WriteString[cfile, "(a4) -- ["<> propType[[4]] <>", thick] (a0);\n"];
        WriteString[cfile, "(a5) -- ["<> propType[[5]] <>", thick] (a0);\n"];
        WriteString[cfile, "(a6) -- ["<> propType[[6]] <>", thick] (a0);\n"];

       , None];
        
        
     WriteString[cfile, "};\n"];
     WriteString[cfile, "\\end{feynman}\n"];
     WriteString[cfile, "\\end{tikzpicture}\n"];
     WriteString[cfile, "\\end{aligned} &\n"];
     WriteString[cfile, "\\begin{split}\n"];
     WriteString[cfile, "\\input vertices/"~~vname[[1]]~~"/"~~vname[[2]]~~".tex\n" ];
     WriteString[cfile, "\\end{split}\n"];
     WriteString[cfile, "\\end{flalign}}\n"];

]
 

DrawDiagram = Function[{cfile, np, feynrule, DiagType, verbose, vname},
Module[{i, vfile, frule, f, fbar},

frule = feynrule;
       
f = {(*vl,*)l,uq,dq};
fbar =  {(*vlbar,*)lbar,uqbar,dqbar};
       
(* Shall we replace clashing arrows with ^C fields with inverted fermion flow? *)
If [ MemberQ[ f, frule[[1,1,1]] ], 
  frule[[1,1,1]] = ToExpression[ ToString[ frule[[1,1,1]] ] <> "cbar"]; 
];
If [ MemberQ[ fbar, frule[[1,2,1]] ], 
  frule[[1,2,1]] = ToExpression[ StringDrop[ ToString[ frule[[1,2,1]] ], -3] <> "c"]; 
];
If [ MemberQ[ f, frule[[1,3,1]] ], 
  frule[[1,3,1]] = ToExpression[ ToString[ frule[[1,3,1]] ] <> "cbar"]; 
];
If [ np > 3 && MemberQ[ fbar, frule[[1,4,1]] ], 
  frule[[1,4,1]] = ToExpression[ StringDrop[ ToString[ frule[[1,4,1]] ], -3] <> "c"]; 
];
       

LineList = {};
FieldList = {};

For[i = 1,i < np + 1, i++,
    
    pos = Position[ FieldType, frule[[1]][[i]][[1]] ][[1]][[1]];
    
    AppendTo[LineList, LineType[[pos]]];
    AppendTo[FieldList, FieldNameAndIndex[pos,i,DiagType]];
];
       

(* Condition which omits conjugate vertices (ebar nu and dbar u) and electroweak vertices involving only the down-type vertices except for the photon and the Z (since they are identical to charged lepton vertices upon relacing e -> d and l -> q in the labelings)
 
   Refer to Appendix (or Supplemental material )
 
 *)
       
(*
 
If[ (ToString[frule[[1, 1, 1]]] == "lbar" && ToString[frule[[1, 2, 1]]] == "vl") ||
    (ToString[frule[[1, 1, 1]]] == "dqbar" && ToString[frule[[1, 2, 1]]] == "uq") ||
    (ToString[frule[[1, 2, 1]]] == "lc") ||
    (ToString[frule[[1, 1, 1]]] == "dqbar" && ToString[frule[[1, 2, 1]]] == "dq" && Length[frule[[1]]] == 3 && 
     (ToString[frule[[1, 3, 1]]] != "G" && ToString[frule[[1, 3, 1]]] != "A" && ToString[frule[[1, 3, 1]]] != "Z" )) ||
    (ToString[frule[[1, 1, 1]]] == "dqbar" && ToString[frule[[1, 2, 1]]] == "dq" && Length[frule[[1]]] == 4 &&
     (ToString[frule[[1, 3, 1]]] != "G" && ToString[frule[[1, 4, 1]]] != "G" )) ||
    (ToString[frule[[1, 1, 1]]] == "dqbar" && ToString[frule[[1, 2, 1]]] == "dq" && Length[frule[[1]]] == 5 &&
     (ToString[frule[[1, 3, 1]]] != "G" && ToString[frule[[1, 4, 1]]] != "G" && ToString[frule[[1, 5, 1]]] != "G" ))
    , None,
    
    (* Put following lines here *)

];
 
*)
       
    DrawFeynmanDiagram[LineList, FieldList, cfile, frule, vname];
       
    WriteString[cfile, "\\bigskip\n\n"];
       
    vfile = OpenWrite[ FileNameJoin[{THDMEFT$Path, "Output", "Latex", "Vertices", vname[[1]], vname[[2]] <> ".tex"}] ];
    
    WriteString[vfile, ToLaTeX[frule[[2]], DiagType, verbose, frule[[1]] ] ];
    Close[vfile];

(* end of DrawDiagram *)
];


DrawSector = Function[{cfile,frset,verbose,DiagType,DiagClass},
(* draws diagrams for chosen sector of Lagrangian *)		
Module[{i,j,vlist,vtype,vcount,vname},

vlist = {};
vcount = 0;

For[j=3,j<7,j++,
  For[i=1,i < Length[frset]+1,i++, 
    vtype = Dim6VertexName[ frset[[i,1]] ];
    If[ ! MemberQ[vlist, vtype] && Length[frset[[i]][[1]]] == j && frset[[i]][[2]] =!= 0,
    If[ verbose, Print[vtype] ];
          
    If[ AlignmentLimit == "yes",
        vname = FileNameJoin[{THDMEFT$Path, "Output", "Latex", "Vertices", DiagClass, "Type" <> ModelKey <> "Alignment"}];
        ,
        vname = FileNameJoin[{THDMEFT$Path, "Output", "Latex", "Vertices", DiagClass, "Type" <> ModelKey}];
      ];
          
      If [ ! DirectoryQ[vname],  CreateDirectory[vname] ];
    
    If[ AlignmentLimit == "yes",
        vname = {StringJoin[{DiagClass, "/Type" <> ModelKey <> "Alignment"}], vtype};
        ,
        vname = {StringJoin[{DiagClass, "/Type" <> ModelKey}], vtype};
      ];
          
      WriteString[cfile, "%Vertex " <> vtype <> "\n"];
          
      DrawDiagram[cfile, j, frset[[i]], DiagType, verbose, vname ];
          
          
      vlist = AppendTo[vlist, vtype];
      vcount = vcount + 1;
    ];
  ];
];

vcount

]
(* end of DrawSector *)
]




THDMEFTToLatex[ OptionsPattern[{ FullDocument -> True, ScreenOutput -> False}] ] :=
(* master routine of latex generation code *)
Module[{i, j, k, frules, cfile, VertCount, vlist, olist, ghostname, tmp},

tmp = Complement[{ OptionValue[FullDocument], OptionValue[ScreenOutput]}, {True,False}];
If[ tmp =!= {},
  Print["Options FullDocument and ScreenOutput of THDMEFTToLatex can be only True or False, value(s) ", tmp, " not allowed, please correct!"];
  Abort[]
];


vlist = # <> "Vertices" <> "" & /@ {"LeptonScalar", "LeptonGaugeScalar", "QuarkScalar",
"QuarkGaugeScalar", "Scalar", "Gauge", "ScalarGauge", "FourFermion"};
       
       
(* 0 = fermion; 1 = scalar; 2 = four-fermion; 3 = gauge *)
       
olist = {0,0,0,0,1,3,3,2};

Print[ Style["Generating Latex file with Feynman rules...", Bold] ];
VertCount = 0;

(* open file and create header *)
cfile = OpenWrite[ FileNameJoin[{THDMEFT$Path, "Output", "Latex", "2HDMEFT_feynman_rules.tex"}] ];

Print["Latex form of Feynman rules will be stored in file ",
  Style[ FileNameJoin[{THDMEFT$Path, "Output", "Latex", "2HDMEFT_feynman_rules.tex"}], Bold]];
Print["Line breaking/factoring of equations may not be perfect."];

WriteString[cfile, "%Automatically generated Feynman rules file for 2HDMEFT\n\n"];
If[OptionValue[FullDocument],
   WriteString[cfile, "\\documentclass[11pt]{article}\n"];
   WriteString[cfile, "\\usepackage{amsmath, amssymb}\n"];
   WriteString[cfile, "\\usepackage{comment}\n"];
   WriteString[cfile, "\\usepackage{slashed}\n"];
   WriteString[cfile, "\\usepackage{breqn}\n"];
   WriteString[cfile, "\\setkeys{breqn}{breakdepth={10}}\n"];
   WriteString[cfile, "\\usepackage{tikz}\n"];
   WriteString[cfile, "\\usepackage[compat=1.1.0]{tikz-feynman}\n"];
   
   WriteString[cfile, "\\textwidth = 16cm\n\n"];
   WriteString[cfile, "\\textheight = 24cm\n\n"];
   WriteString[cfile, "\\topmargin=-2cm\n"];
   WriteString[cfile, "\\oddsidemargin=0cm\n"];
   WriteString[cfile, "\\evensidemargin=\\oddsidemargin\n"];

   WriteString[cfile, "\\begin{document}\n\n"];

   (* Turn off page number *)
   (*
   WriteString[cfile, "\\pagenumbering{gobble}\n\n"];
   *)
];

For[i=1, i < Length[vlist] + 1, i++, 

  frules = Expand[ ToExpression[ vlist[[i]] ] ];

(* skip gluon vertices with more than 4 legs *)
    (* FIX *)
    
  If[ MemberQ[ {"GluonSelfVertices", "GluonSelfVerticesLatex"}, vlist[[i]] ],
    For[j=1, j < Length[frules] + 1, j++,
      If[ Length[ frules[[j,1]] ] > 4 , frules[[j,2]] = LongExpressionNotDisplayed ];
    ];
  ];
    
    
  VertCount = VertCount + DrawSector[cfile,frules,OptionValue[ScreenOutput],olist[[i]],vlist[[i]]];
    
    
];
    
If[OptionValue[FullDocument],
   WriteString[cfile, "\\end{document}\n"];
];

Close[ FileNameJoin[{THDMEFT$Path, "Output", "Latex", "2HDMEFT_feynman_rules.tex"}] ];

Print["Latex output ready, total number of vertices drawn = ", VertCount];

      ];
