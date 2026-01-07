(* ::Package:: *)

BeginPackage["RelativityToolkit`"];

(* --- Exported Symbols (Public Interface) --- *)
valence::usage = "valence[expr] returns a list {{up_indices}, {down_indices}}.";
CanonicalizeIndices::usage = "CanonicalizeIndices[expr] renames dummy indices.";
TensorForm::usage = "TensorForm[expr] displays the expression with Greek indices.";
Partials::usage = "Partials[num, den] represents a partial derivative.";

(* EXPORT THE INDEX WRAPPERS SO THEY MATCH NOTEBOOK INPUT *)
\[ScriptCapitalU]::usage = "Wrapper for Contravariant (Up) indices.";
\[ScriptCapitalD]::usage = "Wrapper for Covariant (Down) indices.";
\[Delta]::usage = "Kronecker Delta symbol.";

(* Common Tensor Symbols *)
x::usage = "Coordinate vector symbol.";
p::usage = "Covector symbol.";
g::usage = "Metric tensor symbol.";
u::usage = "Velocity vector symbol.";
A::usage = "Generic vector/tensor symbol.";
B::usage = "Generic vector/tensor symbol.";
T::usage = "Generic tensor symbol.";

(* Rules *)
metricRules::usage = "Rules for index raising/lowering.";
simpleCommaRules::usage = "Rules to convert derivatives to comma notation.";
robustTransformRules::usage = "Rules for coordinate transformations.";

(* Predicates *)
upQ::usage = "Returns True if indices are Up.";
downQ::usage = "Returns True if indices are Down.";

Begin["`Private`"];

(* ========================================================== *)
(* 1. VALENCE LOGIC                                           *)
(* ========================================================== *)

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

(* Use the Exported Symbols in Patterns *)
valence[_[\[ScriptCapitalU][\[Alpha]_]]] := {{\[Alpha]}, {}};
valence[_[\[ScriptCapitalD][\[Alpha]_]]] := {{}, {\[Alpha]}};

valence[g[\[ScriptCapitalD][\[Mu]_], \[ScriptCapitalD][\[Nu]_]]] := {{}, {\[Mu], \[Nu]}};
valence[g[\[ScriptCapitalU][\[Mu]_], \[ScriptCapitalU][\[Nu]_]]] := {{\[Mu], \[Nu]}, {}};
valence[\[Delta][\[ScriptCapitalU][\[Mu]_], \[ScriptCapitalD][\[Nu]_]]] := {{\[Mu]}, {\[Nu]}};

valence[Partials[num_, den_]] := Module[{vn, vd, vdFlipped},
  vn = valence[num];
  vd = valence[den];
  vdFlipped = {vd[[2]], vd[[1]]}; 
  {Join[vn[[1]], vdFlipped[[1]]], Join[vn[[2]], vdFlipped[[2]]]}
];

valence[(h : _[\[ScriptCapitalU][_]] | _[\[ScriptCapitalD][_]])[___]] := valence[h];
valence[Derivative[_][_][f_] /; (valence[f] =!= {{}, {}})] := valence[f];
valence[Derivative[1][f_][arg_]] := With[{v = valence[arg]},
  If[v === {{}, {}}, {{}, {}}, Reverse[v]]
];

valence[prod_Times] := contractValence[
  Fold[Join[#1, valence[#2], 2] &, {{}, {}}, List @@ prod]
];

valence[sum_Plus] := Module[{vs},
  vs = valence /@ (List @@ sum);
  If[Apply[SameQ, vs], First[vs], 
   Print["Valence Mismatch in Sum: ", vs]; {{}, {}}]
];

valence[Power[b_, _]] := valence[b];
valence[h_[a_, b_, rest___]] := Join[valence[h[a]], valence[h[b, rest]], 2];
valence[___] := {{}, {}};


(* ========================================================== *)
(* 2. DISPLAY RULES (MakeBoxes)                               *)
(* ========================================================== *)

Unprotect[MakeBoxes];

Quiet[MakeBoxes[h_[indices__], StandardForm] =.]; 
Quiet[MakeBoxes[\[Delta][__], StandardForm] =.];
Quiet[MakeBoxes[Partials[_,_], StandardForm] =.];

MakeBoxes[Partials[num_, den_], StandardForm] := 
  FractionBox[
   RowBox[{"\[PartialD]", MakeBoxes[num, StandardForm]}],
   RowBox[{"\[PartialD]", MakeBoxes[den, StandardForm]}]
  ];

MakeBoxes[\[Delta][\[ScriptCapitalU][up_], \[ScriptCapitalD][down_]], StandardForm] := 
  SubsuperscriptBox["\[Delta]", MakeBoxes[down, StandardForm], MakeBoxes[up, StandardForm]];

MakeBoxes[\[Delta][\[ScriptCapitalD][down_], \[ScriptCapitalU][up_]], StandardForm] := 
  SubsuperscriptBox["\[Delta]", MakeBoxes[down, StandardForm], MakeBoxes[up, StandardForm]];

(* Generic Tensor Rule *)
(* The pattern now matches the EXPORTED \[ScriptCapitalU] / \[ScriptCapitalD] *)
MakeBoxes[h_[indices__], StandardForm] /; 
  (h =!= \[Delta]) && 
  MatchQ[{indices}, { (\[ScriptCapitalU][_] | \[ScriptCapitalD][_]) .. }] := 
 Module[{formattedScripts},
  formattedScripts = {indices} /. {
     \[ScriptCapitalU][i_] :> SuperscriptBox["", MakeBoxes[i, StandardForm]],
     \[ScriptCapitalD][i_] :> SubscriptBox["", MakeBoxes[i, StandardForm]]
  };
  RowBox[Prepend[formattedScripts, MakeBoxes[h, StandardForm]]]
 ]

Protect[MakeBoxes];


(* ========================================================== *)
(* 3. ALGEBRAIC SIMPLIFICATION                                *)
(* ========================================================== *)

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


(* ========================================================== *)
(* 4. PRETTY PRINTING                                         *)
(* ========================================================== *)

TensorForm[expr_] := Module[{canonExpr, indexMap, prettyIndices},
   canonExpr = CanonicalizeIndices[expr];
   prettyIndices = {\[Lambda], \[Kappa], \[Rho], \[Sigma], \[Mu], \[Nu], \[Tau], \[Eta], \[Chi], \[Psi]};
   
   indexMap = Table[
     Symbol["\[FormalI]" <> ToString[n]] -> prettyIndices[[n]], 
     {n, Length[prettyIndices]}
   ];
   
   HoldForm[Evaluate[canonExpr /. indexMap]]
];


(* ========================================================== *)
(* 5. PHYSICS RULES                                           *)
(* ========================================================== *)

metricRules = {
   g[\[ScriptCapitalD][\[Mu]_], \[ScriptCapitalD][\[Nu]_]] * vec_[\[ScriptCapitalU][\[Nu]_]] :> vec[\[ScriptCapitalD][\[Mu]]],
   vec_[\[ScriptCapitalU][\[Nu]_]] * g[\[ScriptCapitalD][\[Mu]_], \[ScriptCapitalD][\[Nu]_]] :> vec[\[ScriptCapitalD][\[Mu]]],
   
   g[\[ScriptCapitalU][\[Mu]_], \[ScriptCapitalU][\[Nu]_]] * covec_[\[ScriptCapitalD][\[Nu]_]] :> covec[\[ScriptCapitalU][\[Mu]]],
   covec_[\[ScriptCapitalD][\[Nu]_]] * g[\[ScriptCapitalU][\[Mu]_], \[ScriptCapitalU][\[Nu]_]] :> covec[\[ScriptCapitalU][\[Mu]]],

   g[\[ScriptCapitalU][\[Mu]_], \[ScriptCapitalU][\[Alpha]_]] * g[\[ScriptCapitalD][\[Alpha]_], \[ScriptCapitalD][\[Nu]_]] :> \[Delta][\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Nu]]],
   g[\[ScriptCapitalD][\[Alpha]_], \[ScriptCapitalD][\[Nu]_]] * g[\[ScriptCapitalU][\[Mu]_], \[ScriptCapitalU][\[Alpha]_]] :> \[Delta][\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Nu]]]
};

simpleCommaRules = {
   Derivative[1][f_][x_[\[ScriptCapitalU][\[Alpha]_]][t_]] :> Subscript[f, ","][\[ScriptCapitalD][\[Alpha]]]
};

robustTransformRules = {
   A_[\[ScriptCapitalU][primedIndex_]] :> 
    Module[{freshIndex},
     freshIndex = Unique["\[Mu]"]; 
     Partials[x[\[ScriptCapitalU][primedIndex]], x[\[ScriptCapitalU][freshIndex]]] * A[\[ScriptCapitalU][freshIndex]]
     ],
   p_[\[ScriptCapitalD][primedIndex_]] :> 
    Module[{freshIndex},
     freshIndex = Unique["\[Nu]"]; 
     Partials[x[\[ScriptCapitalU][freshIndex]], x[\[ScriptCapitalU][primedIndex]]] * p[\[ScriptCapitalD][freshIndex]]
     ]
};

End[];
EndPackage[];
