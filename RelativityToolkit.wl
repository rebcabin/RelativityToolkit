(* ::Package:: *)

BeginPackage["RelativityToolkit`"];

(* --- EXPORTED SYMBOLS --- *)

valence::usage = "valence[expr] returns the {up, down} indices.";
CanonicalizeIndices::usage = "CanonicalizeIndices[expr] simplifies dummy indices.";
TensorForm::usage = "TensorForm[expr] displays the tensor in standard notation.";

(* The Core Vocabulary *)
\[ScriptCapitalU]::usage = "\[ScriptCapitalU][idx] represents an Up (contravariant) index.";
\[ScriptCapitalD]::usage = "\[ScriptCapitalD][idx] represents a Down (covariant) index.";

Partials::usage = "Partials[num, den] represents a partial derivative.";

(* Standard Tensors *)
g::usage = "g[\[ScriptCapitalD][mu], \[ScriptCapitalD][nu]] represents the metric tensor.";
delta::usage = "delta[\[ScriptCapitalU][mu], \[ScriptCapitalD][nu]] represents the Kronecker delta.";
x::usage = "x[\[ScriptCapitalU][mu]] represents a coordinate vector.";
p::usage = "p[\[ScriptCapitalD][mu]] represents a momentum covector.";

(* Convenience *)
A::usage = "A is a generic tensor symbol.";
B::usage = "B is a generic tensor symbol.";
T::usage = "T is a generic tensor symbol.";

metricRules::usage = "metricRules contains replacement rules for raising/lowering.";
robustTransformRules::usage = "robustTransformRules contains rules for coordinate transformations.";

Begin["`Private`"];

(* 1. VALENCE LOGIC -------------------------------------------------------- *)

contractValence[{u_List, d_List}] := Module[{common},
  common = Intersection[u, d];
  {DeleteElements[u, common], DeleteElements[d, common]}
];

valence[sym_Symbol] := {{}, {}};
valence[_?NumericQ] := {{}, {}};
valence[] := Null;

(* Atoms - Matching Script Letters *)
valence[_[\[ScriptCapitalU][alpha_]]] := {{alpha}, {}};
valence[_[\[ScriptCapitalD][alpha_]]] := {{}, {alpha}};

(* Metric & Delta *)
valence[g[\[ScriptCapitalD][mu_], \[ScriptCapitalD][nu_]]] := {{}, {mu, nu}};
valence[g[\[ScriptCapitalU][mu_], \[ScriptCapitalU][nu_]]] := {{mu, nu}, {}};
valence[delta[\[ScriptCapitalU][mu_], \[ScriptCapitalD][nu_]]] := {{mu}, {nu}};

(* Partials *)
valence[Partials[num_, den_]] := Module[{vn, vd, vdFlipped},
  vn = valence[num];
  vd = valence[den];
  vdFlipped = {vd[[2]], vd[[1]]}; 
  {Join[vn[[1]], vdFlipped[[1]]], Join[vn[[2]], vdFlipped[[2]]]}
];

(* Derivatives *)
valence[Derivative[1][f_][arg_]] := Module[{u, d}, {u, d} = valence[arg]; {d, u}];
valence[Derivative[_][_][f_] /; (valence[f] =!= {{}, {}})] := valence[f];

(* Recursion *)
valence[(h : _[\[ScriptCapitalU][_]] | _[\[ScriptCapitalD][_]])[___]] := valence[h];
valence[prod_Times] := contractValence[Fold[Join[#1, valence[#2], 2] &, {{}, {}}, List @@ prod]];
valence[sum_Plus] := Module[{vs},
  vs = valence /@ (List @@ sum);
  If[Apply[SameQ, vs], First[vs], Print["Valence Mismatch"]; {{}, {}}]
];
valence[Power[b_, _]] := valence[b];
valence[h_[a_, b_, rest___]] := Join[valence[h[a]], valence[h[b, rest]], 2];
valence[___] := {{}, {}};


(* 2. DISPLAY RULES -------------------------------------------------------- *)

Unprotect[MakeBoxes];

MakeBoxes[Partials[num_, den_], StandardForm] := 
  FractionBox[RowBox[{"\[PartialD]", MakeBoxes[num, StandardForm]}], 
              RowBox[{"\[PartialD]", MakeBoxes[den, StandardForm]}]];

MakeBoxes[delta[\[ScriptCapitalU][up_], \[ScriptCapitalD][down_]], StandardForm] := 
  SubsuperscriptBox["\[Delta]", MakeBoxes[down, StandardForm], MakeBoxes[up, StandardForm]];

(* Generic Tensors - Explicitly matching the Exported Script Symbols *)
MakeBoxes[h_[indices__], StandardForm] /; 
  (h =!= delta) && (h =!= Partials) && 
  MatchQ[{indices}, { (_[\[ScriptCapitalU]] | _[\[ScriptCapitalD]] | \[ScriptCapitalU][_] | \[ScriptCapitalD][_]) .. }] := 
 Module[{formattedScripts},
  formattedScripts = {indices} /. {
     \[ScriptCapitalU][i_] :> SuperscriptBox["", MakeBoxes[i, StandardForm]],
     \[ScriptCapitalD][i_] :> SubscriptBox["", MakeBoxes[i, StandardForm]]
  };
  RowBox[Prepend[formattedScripts, MakeBoxes[h, StandardForm]]]
 ]

Protect[MakeBoxes];


(* 3. ALGEBRAIC SIMPLIFICATION --------------------------------------------- *)

CanonicalizeTerm[term_] := 
  Module[{indices, counts, dummies, replacementRules},
   indices = Cases[term, (\[ScriptCapitalU] | \[ScriptCapitalD])[i_] :> i, Infinity];
   counts = Tally[indices];
   dummies = Select[counts, Last[#] == 2 &][[All, 1]];
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

(* 4. PHYSICS RULES -------------------------------------------------------- *)

metricRules = {
   (* --- LOWERING RULES (g_uv A^v -> A_u) --- *)
   
   (* Standard: Contract the second index *)
   g[\[ScriptCapitalD][mu_], \[ScriptCapitalD][nu_]] * vec_[\[ScriptCapitalU][nu_]] :> vec[\[ScriptCapitalD][mu]],
   vec_[\[ScriptCapitalU][nu_]] * g[\[ScriptCapitalD][mu_], \[ScriptCapitalD][nu_]] :> vec[\[ScriptCapitalD][mu]],
   
   (* Symmetry: Contract the first index *)
   g[\[ScriptCapitalD][nu_], \[ScriptCapitalD][mu_]] * vec_[\[ScriptCapitalU][nu_]] :> vec[\[ScriptCapitalD][mu]],
   vec_[\[ScriptCapitalU][nu_]] * g[\[ScriptCapitalD][nu_], \[ScriptCapitalD][mu_]] :> vec[\[ScriptCapitalD][mu]], (* NEW *)

   (* --- RAISING RULES (g^uv A_v -> A^u) --- *)

   (* Standard: Contract the second index *)
   g[\[ScriptCapitalU][mu_], \[ScriptCapitalU][nu_]] * covec_[\[ScriptCapitalD][nu_]] :> covec[\[ScriptCapitalU][mu]],
   covec_[\[ScriptCapitalD][nu_]] * g[\[ScriptCapitalU][mu_], \[ScriptCapitalU][nu_]] :> covec[\[ScriptCapitalU][mu]],

   (* Symmetry: Contract the first index *)
   g[\[ScriptCapitalU][nu_], \[ScriptCapitalU][mu_]] * covec_[\[ScriptCapitalD][nu_]] :> covec[\[ScriptCapitalU][mu]],
   covec_[\[ScriptCapitalD][nu_]] * g[\[ScriptCapitalU][nu_], \[ScriptCapitalU][mu_]] :> covec[\[ScriptCapitalU][mu]], (* NEW *)

   (* --- INVERSE IDENTITY --- *)
   g[\[ScriptCapitalU][mu_], \[ScriptCapitalU][alpha_]] * g[\[ScriptCapitalD][alpha_], \[ScriptCapitalD][nu_]] :> delta[\[ScriptCapitalU][mu], \[ScriptCapitalD][nu]],
   g[\[ScriptCapitalD][alpha_], \[ScriptCapitalD][nu_]] * g[\[ScriptCapitalU][mu_], \[ScriptCapitalU][alpha_]] :> delta[\[ScriptCapitalU][mu], \[ScriptCapitalD][nu]]
};

robustTransformRules = {
   A_[\[ScriptCapitalU][primed_]] :> Module[{fresh = Unique["\[Mu]"]}, 
     Partials[x[\[ScriptCapitalU][primed]], x[\[ScriptCapitalU][fresh]]] * A[\[ScriptCapitalU][fresh]]],
   p_[\[ScriptCapitalD][primed_]] :> Module[{fresh = Unique["\[Nu]"]}, 
     Partials[x[\[ScriptCapitalU][fresh]], x[\[ScriptCapitalU][primed]]] * p[\[ScriptCapitalD][fresh]]]
};


(* 5. PRETTY PRINTING ------------------------------------------------------ *)

TensorForm[expr_] := Module[{canonExpr, indexMap, prettyIndices},
   canonExpr = CanonicalizeIndices[expr];
   prettyIndices = {\[Lambda], \[Kappa], \[Rho], \[Sigma], \[Mu], \[Nu], \[Tau], \[Eta], \[Chi], \[Psi]};
   indexMap = Table[Symbol["\[FormalI]" <> ToString[n]] -> prettyIndices[[n]], {n, Length[prettyIndices]}];
   HoldForm[Evaluate[canonExpr /. indexMap]]
];

End[];
EndPackage[];
