(* ::Package:: *)

(* ========================================================================= *)
(* RELATIVITY TOOLKIT ENGINE (v1.0)                                          *)
(* Author: Brian Beckman                                                     *)
(* Based on: "A Relativist's Toolkit" by Eric Poisson                        *)
(* ========================================================================= *)

BeginPackage["RelativityToolkit`"];

(* --- EXPORTED SYMBOLS (The Public Interface) --- *)
(* Defining usage messages prevents Context collisions. *)
(* When a user loads this package, these symbols become available globally. *)

valence::usage = "valence[expr] returns the {up, down} indices of a tensor expression.";
CanonicalizeIndices::usage = "CanonicalizeIndices[expr] simplifies dummy indices to standard forms.";
TensorForm::usage = "TensorForm[expr] displays the tensor with canonicalized indices.";

(* The Core Vocabulary *)
U::usage = "U[idx] represents an Up (contravariant) index.";
D::usage = "D[idx] represents a Down (covariant) index.";
Partials::usage = "Partials[num, den] represents a partial derivative operator.";

(* Standard Tensors *)
g::usage = "g[D[mu], D[nu]] represents the metric tensor.";
delta::usage = "delta[U[mu], D[nu]] represents the Kronecker delta.";
x::usage = "x[U[mu]] represents a coordinate vector.";
p::usage = "p[D[mu]] represents a momentum covector.";

(* User Convenience Symbols (Optional, but helpful) *)
A::usage = "A is a generic tensor symbol.";
B::usage = "B is a generic tensor symbol.";
T::usage = "T is a generic tensor symbol.";

(* Configuration & Rules *)
metricRules::usage = "metricRules contains replacement rules for raising/lowering indices.";
robustTransformRules::usage = "robustTransformRules contains rules for coordinate transformations.";


(* ========================================================================= *)
(* PRIVATE IMPLEMENTATION                                                    *)
(* ========================================================================= *)
Begin["`Private`"];

(* 1. VALENCE LOGIC -------------------------------------------------------- *)

(* Helpers *)
contractValence[{u_List, d_List}] := Module[{common},
  common = Intersection[u, d];
  {DeleteElements[u, common], DeleteElements[d, common]}
];

(* Base Cases *)
valence[sym_Symbol] := {{}, {}};
valence[_?NumericQ] := {{}, {}};
valence[] := Null;

(* Atoms *)
valence[_[U[alpha_]]] := {{alpha}, {}};
valence[_[D[alpha_]]] := {{}, {alpha}};

(* Metric & Delta *)
valence[g[D[mu_], D[nu_]]] := {{}, {mu, nu}};
valence[g[U[mu_], U[nu_]]] := {{mu, nu}, {}};
valence[delta[U[mu_], D[nu_]]] := {{mu}, {nu}};

(* Partials *)
valence[Partials[num_, den_]] := Module[{vn, vd, vdFlipped},
  vn = valence[num];
  vd = valence[den];
  vdFlipped = {vd[[2]], vd[[1]]}; (* Denom flips variance *)
  {Join[vn[[1]], vdFlipped[[1]]], Join[vn[[2]], vdFlipped[[2]]]}
];

(* Derivatives *)
valence[Derivative[1][f_][arg_]] := Module[{u, d}, 
  {u, d} = valence[arg]; 
  {d, u} (* Gradient flips variance *)
];
valence[Derivative[_][_][f_] /; (valence[f] =!= {{}, {}})] := valence[f];

(* Arithmetic *)
valence[(h : _[U[_]] | _[D[_]])[___]] := valence[h];
valence[prod_Times] := contractValence[Fold[Join[#1, valence[#2], 2] &, {{}, {}}, List @@ prod]];
valence[sum_Plus] := Module[{vs},
  vs = valence /@ (List @@ sum);
  If[Apply[SameQ, vs], First[vs], Print["Valence Mismatch"]; {{}, {}}]
];
valence[Power[b_, _]] := valence[b];
valence[h_[a_, b_, rest___]] := Join[valence[h[a]], valence[h[b, rest]], 2];
valence[___] := {{}, {}};


(* 2. DISPLAY RULES -------------------------------------------------------- *)

(* Unprotect to allow modification of System symbols inside our context *)
Unprotect[MakeBoxes];

MakeBoxes[Partials[num_, den_], StandardForm] := 
  FractionBox[RowBox[{"\[PartialD]", MakeBoxes[num, StandardForm]}], 
              RowBox[{"\[PartialD]", MakeBoxes[den, StandardForm]}]];

MakeBoxes[delta[U[up_], D[down_]], StandardForm] := 
  SubsuperscriptBox["\[Delta]", MakeBoxes[down, StandardForm], MakeBoxes[up, StandardForm]];

(* Generic formatting for U/D wrappers *)
MakeBoxes[h_[indices__], StandardForm] /; 
  (h =!= delta) && (h =!= Partials) && 
  MatchQ[{indices}, { (U[_] | D[_]) .. }] := 
 Module[{formattedScripts},
  formattedScripts = {indices} /. {
     U[i_] :> SuperscriptBox["", MakeBoxes[i, StandardForm]],
     D[i_] :> SubscriptBox["", MakeBoxes[i, StandardForm]]
  };
  RowBox[Prepend[formattedScripts, MakeBoxes[h, StandardForm]]]
 ]

Protect[MakeBoxes];


(* 3. ALGEBRAIC SIMPLIFICATION --------------------------------------------- *)

CanonicalizeTerm[term_] := 
  Module[{indices, counts, dummies, replacementRules},
   indices = Cases[term, (U | D)[i_] :> i, Infinity];
   counts = Tally[indices];
   dummies = Select[counts, Last[#] == 2 &][[All, 1]];
   
   (* Renaming scheme: i1, i2, i3... *)
   replacementRules = MapIndexed[
     #1 -> Symbol["\[FormalI]" <> ToString[First[#2]]] &, 
     dummies
   ];
   term /. replacementRules
  ];

CanonicalizeIndices[expr_Plus] := Total[CanonicalizeIndices /@ List @@ expr];
CanonicalizeIndices[expr_Equal] := Equal @@ (CanonicalizeIndices /@ List @@ expr);
CanonicalizeIndices[expr_] := CanonicalizeTerm[expr];


(* 4. PHYSICS RULES -------------------------------------------------------- *)

metricRules = {
   (* Lowering *)
   g[D[mu_], D[nu_]] * vec_[U[nu_]] :> vec[D[mu]],
   vec_[U[nu_]] * g[D[mu_], D[nu_]] :> vec[D[mu]],
   g[D[nu_], D[mu_]] * vec_[U[nu_]] :> vec[D[mu]], (* Symmetry *)
   
   (* Raising *)
   g[U[mu_], U[nu_]] * covec_[D[nu_]] :> covec[U[mu]],
   covec_[D[nu_]] * g[U[mu_], U[nu_]] :> covec[U[mu]],
   g[U[nu_], U[mu_]] * covec_[D[nu_]] :> covec[U[mu]], (* Symmetry *)

   (* Inverse *)
   g[U[mu_], U[alpha_]] * g[D[alpha_], D[nu_]] :> delta[U[mu], D[nu]],
   g[D[alpha_], D[nu_]] * g[U[mu_], U[alpha_]] :> delta[U[mu], D[nu]]
};

robustTransformRules = {
   A_[U[primed_]] :> Module[{fresh = Unique["\[Mu]"]}, 
     Partials[x[U[primed]], x[U[fresh]]] * A[U[fresh]]],
   p_[D[primed_]] :> Module[{fresh = Unique["\[Nu]"]}, 
     Partials[x[U[fresh]], x[U[primed]]] * p[D[fresh]]]
};

(* 5. PRETTY PRINTING ------------------------------------------------------ *)

TensorForm[expr_] := Module[{canonExpr, indexMap, prettyIndices},
   canonExpr = CanonicalizeIndices[expr];
   prettyIndices = {\[Lambda], \[Kappa], \[Rho], \[Sigma], \[Mu], \[Nu], \[Tau], \[Eta], \[Chi], \[Psi]};
   indexMap = Table[Symbol["\[FormalI]" <> ToString[n]] -> prettyIndices[[n]], {n, Length[prettyIndices]}];
   HoldForm[Evaluate[canonExpr /. indexMap]]
];

End[]; (* End Private *)
EndPackage[]; (* End Package *)
