(* ::Package:: *)

BeginPackage["RelativityToolkit`"];

(* --- Exported Symbols --- *)
valence::usage = "valence[expr] returns the index structure.";
CanonicalizeIndices::usage = "Renames dummy indices.";
TensorForm::usage = "Displays with Greek indices.";
Partials::usage = "Represents a partial derivative.";

(* Export Formatting Symbols to encourage correct context usage *)
\[ScriptCapitalU]::usage = "Up index wrapper.";
\[ScriptCapitalD]::usage = "Down index wrapper.";
\[Delta]::usage = "Kronecker Delta.";

(* Tensor Symbols *)
x::usage = "Coordinate vector.";
p::usage = "Covector.";
g::usage = "Metric tensor.";
u::usage = "Velocity.";
A::usage = "Generic tensor.";
B::usage = "Generic tensor.";
T::usage = "Generic tensor.";

(* Rules *)
metricRules::usage = "Raising/Lowering rules.";
simpleCommaRules::usage = "Comma notation rules.";
robustTransformRules::usage = "Transformation rules.";

(* Predicates *)
upQ::usage = "Check for Up indices.";
downQ::usage = "Check for Down indices.";

Begin["`Private`"];

(* ========================================================== *)
(* 1. HELPER: CONTEXT-AGNOSTIC CHECK                          *)
(* ========================================================== *)
(* Checks if a head is the fancy U or D, ignoring context *)
IsUpHead[h_Symbol] := StringEndsQ[SymbolName[h], "ScriptCapitalU"];
IsUpHead[_] := False;

IsDownHead[h_Symbol] := StringEndsQ[SymbolName[h], "ScriptCapitalD"];
IsDownHead[_] := False;

(* ========================================================== *)
(* 2. VALENCE LOGIC                                           *)
(* ========================================================== *)

(* Logic helpers *)
upQ[x_] := upQ[valence[x]];
downQ[x_] := downQ[valence[x]];
upQ[{{__}, {}}] := True;
upQ[___] := False;
downQ[{{}, {__}}] := True;
downQ[___] := False;

contractValence[{u_List, d_List}] := Module[{common},
  common = Intersection[u, d];
  {DeleteElements[u, common], DeleteElements[d, common]}
];

valence[x_Symbol] := {{}, {}};
valence[_?NumericQ] := {{}, {}};
valence[] := Null;

(* Context-Agnostic Atoms *)
valence[_[h_[\[Alpha]_]]] /; IsUpHead[h] := {{\[Alpha]}, {}};
valence[_[h_[\[Alpha]_]]] /; IsDownHead[h] := {{}, {\[Alpha]}};

(* Metric & Delta *)
(* For known symbols, we can stick to specific valence definitions *)
valence[g[d1_[\[Mu]_], d2_[\[Nu]_]]] /; IsDownHead[d1] && IsDownHead[d2] := {{}, {\[Mu], \[Nu]}};
valence[g[u1_[\[Mu]_], u2_[\[Nu]_]]] /; IsUpHead[u1] && IsUpHead[u2] := {{\[Mu], \[Nu]}, {}};
valence[\[Delta][u_[\[Mu]_], d_[\[Nu]_]]] /; IsUpHead[u] && IsDownHead[d] := {{\[Mu]}, {\[Nu]}};

(* Partials *)
valence[Partials[num_, den_]] := Module[{vn, vd, vdFlipped},
  vn = valence[num];
  vd = valence[den];
  vdFlipped = {vd[[2]], vd[[1]]}; 
  {Join[vn[[1]], vdFlipped[[1]]], Join[vn[[2]], vdFlipped[[2]]]}
];

(* Recursive Rules *)
valence[(h : _[head_] /; (IsUpHead[head] || IsDownHead[head]))[___]] := valence[h];
valence[Derivative[_][_][f_] /; (valence[f] =!= {{}, {}})] := valence[f];
valence[Derivative[1][f_][arg_]] := With[{v = valence[arg]}, If[v === {{}, {}}, {{}, {}}, Reverse[v]]];

valence[prod_Times] := contractValence[Fold[Join[#1, valence[#2], 2] &, {{}, {}}, List @@ prod]];
valence[sum_Plus] := Module[{vs}, vs = valence /@ (List @@ sum); If[Apply[SameQ, vs], First[vs], Print["Valence Mismatch"]; {{}, {}}]];
valence[Power[b_, _]] := valence[b];
valence[h_[a_, b_, rest___]] := Join[valence[h[a]], valence[h[b, rest]], 2];
valence[___] := {{}, {}};


(* ========================================================== *)
(* 3. DISPLAY RULES (ROBUST)                                  *)
(* ========================================================== *)

Unprotect[MakeBoxes];
Quiet[MakeBoxes[Partials[_,_], StandardForm] =.];
Quiet[MakeBoxes[\[Delta][__], StandardForm] =.];

(* Partials *)
MakeBoxes[Partials[num_, den_], StandardForm] := 
  FractionBox[
   RowBox[{"\[PartialD]", MakeBoxes[num, StandardForm]}],
   RowBox[{"\[PartialD]", MakeBoxes[den, StandardForm]}]
  ];

(* Delta (Vertical Stack) *)
(* Matches any head that looks like U or D *)
MakeBoxes[\[Delta][u_[up_], d_[down_]], StandardForm] /; IsUpHead[u] && IsDownHead[d] := 
  SubsuperscriptBox["\[Delta]", MakeBoxes[down, StandardForm], MakeBoxes[up, StandardForm]];

MakeBoxes[\[Delta][d_[down_], u_[up_]], StandardForm] /; IsUpHead[u] && IsDownHead[d] := 
  SubsuperscriptBox["\[Delta]", MakeBoxes[down, StandardForm], MakeBoxes[up, StandardForm]];

(* Generic Tensor Formatting *)
(* Condition: The expression is NOT Delta, and ALL arguments match the U/D pattern *)
MakeBoxes[h_[indices__], StandardForm] /; 
  (h =!= \[Delta]) && 
  (h =!= Partials) &&
  AllTrue[{indices}, Function[idx, MatchQ[idx, _[__]] && (IsUpHead[Head[idx]] || IsDownHead[Head[idx]])]] := 
 Module[{formattedScripts},
  formattedScripts = {indices} /. {
     idx_ /; IsUpHead[Head[idx]] :> SuperscriptBox["", MakeBoxes[First[idx], StandardForm]],
     idx_ /; IsDownHead[Head[idx]] :> SubscriptBox["", MakeBoxes[First[idx], StandardForm]]
  };
  RowBox[Prepend[formattedScripts, MakeBoxes[h, StandardForm]]]
 ]

Protect[MakeBoxes];


(* ========================================================== *)
(* 4. ALGEBRAIC SIMPLIFICATION                                *)
(* ========================================================== *)

CanonicalizeTerm[term_] := 
  Module[{indices, counts, dummies, replacementRules, rawIndices},
   (* Find anything matching _[i] where Head is U/D *)
   rawIndices = Cases[term, x_ /; MatchQ[x, _[_]] && (IsUpHead[Head[x]] || IsDownHead[Head[x]]), Infinity];
   indices = First /@ rawIndices;
   
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


(* ========================================================== *)
(* 5. PRETTY PRINTING                                         *)
(* ========================================================== *)

TensorForm[expr_] := Module[{canonExpr, indexMap, prettyIndices},
   canonExpr = CanonicalizeIndices[expr];
   prettyIndices = {\[Lambda], \[Kappa], \[Rho], \[Sigma], \[Mu], \[Nu], \[Tau], \[Eta], \[Chi], \[Psi]};
   indexMap = Table[Symbol["\[FormalI]" <> ToString[n]] -> prettyIndices[[n]], {n, Length[prettyIndices]}];
   HoldForm[Evaluate[canonExpr /. indexMap]]
];


(* ========================================================== *)
(* 6. PHYSICS RULES                                           *)
(* ========================================================== *)
(* Note: We keep specific patterns here for safety, assuming package symbols are used for calculation *)

metricRules = {
   g[d1_[\[Mu]_], d2_[\[Nu]_]] * vec_[u1_[\[Nu]_]] /; IsDownHead[d1] && IsDownHead[d2] && IsUpHead[u1] :> vec[d1[\[Mu]]],
   vec_[u1_[\[Nu]_]] * g[d1_[\[Mu]_], d2_[\[Nu]_]] /; IsDownHead[d1] && IsDownHead[d2] && IsUpHead[u1] :> vec[d1[\[Mu]]],
   
   g[u1_[\[Mu]_], u2_[\[Nu]_]] * covec_[d1_[\[Nu]_]] /; IsUpHead[u1] && IsUpHead[u2] && IsDownHead[d1] :> covec[u1[\[Mu]]],
   covec_[d1_[\[Nu]_]] * g[u1_[\[Mu]_], u2_[\[Nu]_]] /; IsUpHead[u1] && IsUpHead[u2] && IsDownHead[d1] :> covec[u1[\[Mu]]],

   g[u1_[\[Mu]_], u2_[\[Alpha]_]] * g[d1_[\[Alpha]_], d2_[\[Nu]_]] /; IsUpHead[u1] && IsUpHead[u2] && IsDownHead[d1] && IsDownHead[d2] :> \[Delta][u1[\[Mu]], d2[\[Nu]]],
   g[d1_[\[Alpha]_], d2_[\[Nu]_]] * g[u1_[\[Mu]_], u2_[\[Alpha]_]] /; IsUpHead[u1] && IsUpHead[u2] && IsDownHead[d1] && IsDownHead[d2] :> \[Delta][u1[\[Mu]], d2[\[Nu]]]
};

simpleCommaRules = {
   Derivative[1][f_][x_[u_[\[Alpha]_]][t_]] /; IsUpHead[u] :> Subscript[f, ","][\[ScriptCapitalD][\[Alpha]]]
};

robustTransformRules = {
   A_[u_[primedIndex_]] /; IsUpHead[u] :> 
    Module[{freshIndex},
     freshIndex = Unique["\[Mu]"]; 
     Partials[x[u[primedIndex]], x[u[freshIndex]]] * A[u[freshIndex]]
     ],
   p_[d_[primedIndex_]] /; IsDownHead[d] :> 
    Module[{freshIndex},
     freshIndex = Unique["\[Nu]"]; 
     Partials[x[\[ScriptCapitalU][freshIndex]], x[\[ScriptCapitalU][primedIndex]]] * p[d[freshIndex]]
     ]
};

End[];
EndPackage[];
