This FeynRules package allows the user to calculate Feynman rules in the most general 2HDM Effective Field Theory at mass dimension six. 

Please refer to the following works for notation, conventions, and the operators spanned in the 2HDM effective field theory:

[1] R. Dermisek and K. Hermanek, ``Two-Higgs-doublet model effective field theory,''
Phys. Rev. D 110, no.3, 035026 (2024) [arXiv:2405.20511 [hep-ph]], 

[2] R. Dermisek and K. Hermanek, ``Feynman Rules in the Two-Higgs Doublet Model Effective Field Theory,''
[arXiv:2411.07337 [hep-ph]]


(******************** Model-specifics ********************)

The model default is a CP-conserving type-II 2HDM EFT in the alignment limit (see below).


(********* Writing UFO files **********)
The user may be interested in just a subset of effective operators rather than the entire set, which could take a long time to fully compute. To only generate UFO files for individual operator classes defined in the 2HDM EFT:

In the "2HDMEFT_vertex_calculation.fr" file beginning on line 160, comment out the operators the user would like to study. 

The "ReleaseHold" command is required since the "Hold" is to keep the Feynman rules more compact when converted into LaTeX, but will give errors if not removed when converted into a UFO file. 


(********* Gauge-fixing and ghost fields **********)

In this code as well as for presenting results in Ref. [2], we choose to not include the gauge-fixing procedure and ghosts. We are interested in presenting results for modified vertices and contact terms in the present of higher-dimensional operators and are not concerned with loop effects implemented in the FeynArts package. 

In terms of determining the masses of the Goldstone bosons in the R-xi gauge, one would replace the masses mG -> sqrt{xi_Z} mZ and mGpm -> sqrt{xi_W} mW in the "2HDMEFT_fields.fr" file for gauge parameters xi_Z and xi_W. The default masses are mG = mZ and mGpm = mW (gauge parameters = 1).


(********* Four-fermion/triple gauge operators **********)

Since we are interested in calculating the effects of a 2HDM EFT, we choose to limit the discussion of four-fermion operators and X^3, since they are not affected in the 2HDMEFT other than the former possibly being zero due to Z2 symmetries. 

To include these operators, first go into the "2HDMEFT_Lagrangian.fr" field and uncomment these two Lagrangians in the end of line 183.

To increase the computational time for implementing the model, the four-fermion operators are commented out in the "2HDMEFT_parameters.fr" file lines 5273 - 9298. Please uncomment them if needed. 


See "Types of 2HDMs" below for more details.


(********* Dimension-five lepton-violating operators **********)

We include lepton-violating operators expressed in the form of Weinberg operators for the 2HDM since they are consistent with the expansion order. We also assume that the neutrino is Majorana, meaning that these operators contribute to the neutrino's mass. 


(********* Mixed kinetic term **********)

For generality, we included all calculations that involve eta, assuming we work in the basis where it is generated at loop level. Thus, all corrections of eta^2 and eta*O(1/lambda^2) are neglected.


(********* CP-violation in the scalar sector **********)

Generally with complex couplings especially in the scalar sector, the theory strongly violates charge-parity (CP) symmetry. This mixes the four degrees of freedom from the scalar doublets in both the kinetic terms and in the mass squared matrix. These terms are generated from the imaginary parts of non-Hermitian terms in the renormalizable potential, phi^4 D^2, and phi^6 operators.

Given that one eigenvalue is exactly zero, we can rotate to the basis where this eigenstate is diagonal, which mixes the three remaining scalars together (A, rho1, rho2) by three rotation angles (hat{alpha}, hat{xi}, hat{omega}) into the diagonal basis (h3, h2, h1).

- In the "2HDM_model_file.fr" file, one sets the variable "CPConservation" to "no". 

However, if the user wishes to work in the theory in where CP is conserved at least in the scalar sector, then the eigenstates h3 -> A0 (heavy CP-odd Higgs boson), h2 -> H (heavy CP-even Higgs boson) and h1 -> h (light CP-even Higgs boson). 

- In the "2HDM_model_file.fr" file, one sets the variable "CPConservation" to "yes". 

Additionally, in order to consistently expand to 1/LambdaCutoff^2, one would need to expand the eigenvalues and diagonaliation angles in this theory. This can be achieved by

- Diagonalization angles: go into the "2HDMEFT_rules_functions.fr" file lines 28-49 

- Uncomment the section which expands to the appropriate order and updates the parameters in the overall program


- Mass squared eigenvalues: go into the "2HDMEFT_rules_functions.fr" file lines 71-85 

- Uncomment the section which expands to the appropriate order and updates the parameters in the overall program



(********** Types of 2HDMs **********)

Different types of 2HDMs are defined based on a discrete Z2 parity symmetry on the doublets: Phi1 -> - Phi1 and Phi2 -> + Phi2, as well as how these doublets couple to the fermions. 

These symmetries restrict which operators are allowed from the general 2HDM. There are 76 common operators at dimension-six between the four types. However, there are model-specific operators based on

Type-I: Phi2 couples to the (u, d, e) sectors only (62 operators)

Type-II: Phi2 couples to the u-sector; Phi1 couples to the (d, e) sectors (48 operators)

Type-X (lepton-specific): Phi2 couples to the (u, d) sectors and only Phi1 to e (52 operators)

Type-Y (flipped): Phi2 couples to the (u, e) sectors and only Phi1 to d (50 operators)

In all types we assume the Z2 symmetry is softly broken, meaning m12^2 remains while lambda_6 and lambda_7 vanish. Note that m12^2 and lambda_5 can be complex.


An important feature of any of the types of 2HDM is that one of the Yukawa matrices is restricted. This allows for the couplings of fermions to the Goldstone bosons or the SM Higgs (in the alignment limit) to be written exactly in terms of the physical mass of the fermion. This is implemented in the code. 


- In the "2HDM_model_file.fr" file, one sets the variable "ModelKey" to "I", "II", "X", or "Y" for each specific type of model. Otherwise, the default is "General".


 (********** Alignment Limit **********)

In the renormalizable 2HDM, it is known that the difference between the vacuum and CP-even diagonalization angle alpha can be evaluated in the alignment limit ( beta - alpha -> pi/2), where couplings to the light CP-even eigenstate h are SM like. However, in Ref. [1] and more specifically in [2], it is argued that in the effective theory no couplings of the scalars are SM-like, and evaluating beta - hat{alpha} -> pi/2 (hat{alpha} diagonalizes the CP-even scalars in the effective theory) is only approximate. This means that the SM degrees of freedom are approximately separated from new fields. This is what is meant by "alignment limit" and is implemented in the code. 

- In the "2HDM_model_file.fr" file, one sets the variable "AlignmentLimit" to "yes".


(********** Diagonalization **********)

Fermions: 

Diagonalization is done by unitary matrices U_L,R, where the fields transform as psi_L,R -> hat{psi}_{L,R}. Given some UV complete theory, these matrices can be found however, in the context of the effective field theory, they are all set to the identity. 

Diagonalization to the physical basis redefines the Wilson coefficients to ones with hats, where the definitions are provided in both references. Hat denote the physical basis for both fermions and scalars.

Due to the diagonaliztion of the fermion sector via U_L,R and the redefinition of Wilson coefficients, operators which contain left-left or right-right currents with a charged mediator (W^pm, G^pm, or H^pm)cannot be diagonalized by these matrices. As a result, they give rise to the PMNS or CKM matrices in the lepton or quark sectors, respectively and are defined as

U = Upmns = U_L^{e dagger} U_L^{nu}

V = Vckm = U_L^{u dagger} U_L^{d},

in which each of the matrix values are experimentally known. 


Scalars: 

Diagonalization is done via rotation angles hat{beta}, hat{alpha}, hat{betapm}, hat{xi}, and hat{omega}

hat{beta} rotates the 4x4 scalar mass matrix to the basis where the eigenstate of zero mass is identified as the neutral Goldstone G and the orthogonal one is the would-be CP-odd A0. This results in a 3x3 sub matrix that needs to be diagonalized. 

hat{alpha} rotates the resulting 3x3 scalar mass matrix to the basis where the sub matrix containing the rho fields are diagonal (the basis containing the would-be CP even scalars H and h).

The parameters of the matrix after partial diagonalization by hat{alpha} are given in Ref. [2] as well as the eigenvalues of the 3x3 matrix. 

The remaining angles hat{xi} and hat{omega} rotate the {A, H, h} fields into physical eigenstates {h3, h2, h1} with mh3^2 => mh2^2 => mh1^2. 

hat{betapm} diagonalizes the charged scalars, which is only ever a 2x2 matrix. The eigenstate with zero eigenvalue is identified as the charged Goldstone Gpm and the orthogonal eigenstate is the charged Higgs Hpm. 

NOTE: hat{beta} and hat{betapm} are not the same as the vacuum angle, beta. beta does not diagonalize the would-be CP-odd scalars or the charged scalars as in the tree-level 2HDM potential; they receive modifications from dimension-six operators contributing to the field redefinitions and mass matrices. See the references for details.


(********** EFT Expansion **********)

Although in the theory several parameters and variables are exact, such as the dressed gauge couplings or the eigenvalues of the mass squared matrix, for consistency the theory must be expanded to leading order 1 / LambdaCutoff^2. 

Hence, after all computations of diagonalization and field redefinitions, the program expands the theory to leading order. Note that the cutoff scale is explicit to aide Mathematica in expanding these relations, however, in the listed references and LaTeX output of the Feynman rules, the scale is absorbed into the Wilson coefficient. 



