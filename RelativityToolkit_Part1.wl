(* ::Package:: *)

BeginPackage["RelativityToolkit`"];

(* --- Exported Symbols (Public Interface) --- *)
valence::usage = "valence[expr] returns a list {{up_indices}, {down_indices}} representing the index structure of the expression. It acts as a type-checker for tensor operations.";
CanonicalizeIndices::usage = "CanonicalizeIndices[expr] renames dummy (summation) indices to a standard form (FormalI1, FormalI2...) to allow algebraic simplification and index coalescing.";
TensorForm::usage = "TensorForm[expr] displays the expression with canonicalized indices mapped to pretty Greek letters. Use this for final output, not for further computation.";
Partials::usage = "Partials[num, den] represents a partial derivative (Jacobian) for display and valence calculations.";

(* Common Tensor Symbols *)
x::usage = "Coordinate vector symbol.";
p::usage = "Covector symbol.";
g::usage = "Metric tensor symbol.";
u::usage = "Velocity vector symbol.";
A::usage = "Generic vector/tensor symbol.";
B::usage = "Generic vector/tensor symbol.";

(* Transformation & Logic Rules *)
metricRules::usage = "List of rules for index raising/lowering (e.g., g_uv A^v -> A_u) and the inverse metric identity.";
simpleCommaRules::usage = "List of rules to convert derivatives (f'[x]) into comma notation (f_,a).";
robustTransformRules::usage = "List of rules for coordinate transformations that use alpha-conversion (Unique indices) to prevent collisions.";

(* Helper Predicates *)
upQ::usage = "upQ[expr] returns True if the expression has only contiguous Up indices.";
downQ::usage = "downQ[expr] returns True if the expression has only contiguous Down indices.";

Begin["`Private`"];

(* ========================================================== *)
(* 1. VALENCE LOGIC (The Type Checker)                        *)
(* ========================================================== *)

(* Helpers *)
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

(* Base Cases *)
valence[x_Symbol] := {{}, {}};
valence[_?NumericQ] := {{}, {}};
valence[] := Null;

(* Tensor Atoms *)
valence[_[\[ScriptCapitalU][\[Alpha]_]]] := {{\[Alpha]}, {}};
valence[_[\[ScriptCapitalD][\[Alpha]_]]] := {{}, {\[Alpha]}};

(* Metric & Delta Specifics *)
valence[g[\[ScriptCapitalD][\[Mu]_], \[ScriptCapitalD][\[Nu]_]]] := {{}, {\[Mu], \[Nu]}};
valence[g[\[ScriptCapitalU][\[Mu]_], \[ScriptCapitalU][\[Nu]_]]] := {{\[Mu], \[Nu]}, {}};
valence[\[Delta][\[ScriptCapitalU][\[Mu]_], \[ScriptCapitalD][\[Nu]_]]] := {{\[Mu]}, {\[Nu]}};

(* Partials (Jacobians) *)
valence[Partials[num_, den_]] := Module[{vn, vd, vdFlipped},
  vn = valence[num];
  vd = valence[den];
  (* Denominator flips variance: Up->Down, Down->Up *)
  vdFlipped = {vd[[2]], vd[[1]]}; 
  {Join[vn[[1]], vdFlipped[[1]]], Join[vn[[2]], vdFlipped[[2]]]}
];

(* Structural Rules *)
(* 1. Passthrough for Tensor Functions x[U][t] *)
valence[(h : _[\[ScriptCapitalU][_]] | _[\[ScriptCapitalD][_]])[___]] := valence[h];

(* 2. Parameter Derivatives (Scalars) *)
valence[Derivative[_][_][f_] /; (valence[f] =!= {{}, {}})] := valence[f];

(* 3. Gradient/Chain Rule (f'[x[U]]) *)
valence[Derivative[1][f_][arg_]] := With[{v = valence[arg]},
  If[v === {{}, {}}, {{}, {}}, Reverse[v]]
];

(* Arithmetic Rules *)
valence[prod_Times] := contractValence[
  Fold[Join[#1, valence[#2], 2] &, {{}, {}}, List @@ prod]
];

valence[sum_Plus] := Module[{vs},
  vs = valence /@ (List @@ sum);
  If[Apply[SameQ, vs], First[vs], 
   Print["Valence Mismatch in Sum: ", vs]; {{}, {}}]
];

valence[Power[b_, _]] := valence[b];

(* Recursion for Multi-Index Tensors h[a, b] *)
valence[h_[a_, b_, rest___]] := Join[valence[h[a]], valence[h[b, rest]], 2];

(* Fallback *)
valence[___] := {{}, {}};


(* ========================================================== *)
(* 2. DISPLAY RULES (MakeBoxes)                               *)
(* ========================================================== *)

(* Clear old definitions to prevent shadowing if reloaded *)
Quiet[MakeBoxes[h_[indices__], StandardForm] =.]; 
Quiet[MakeBoxes[\[Delta][__], StandardForm] =.];
Quiet[MakeBoxes[Partials[_,_], StandardForm] =.];

(* Partials Display *)
MakeBoxes[Partials[num_, den_], StandardForm] := 
  FractionBox[
   RowBox[{"\[PartialD]", MakeBoxes[num, StandardForm]}],
   RowBox[{"\[PartialD]", MakeBoxes[den, StandardForm]}]
  ];

(* Specific Delta Rule (Vertical Stacking) *)
MakeBoxes[\[Delta][\[ScriptCapitalU][up_], \[ScriptCapitalD][down_]], StandardForm] := 
  SubsuperscriptBox["\[Delta]", MakeBoxes[down, StandardForm], MakeBoxes[up, StandardForm]];

MakeBoxes[\[Delta][\[ScriptCapitalD][down_], \[ScriptCapitalU][up_]], StandardForm] := 
  SubsuperscriptBox["\[Delta]", MakeBoxes[down, StandardForm], MakeBoxes[up, StandardForm]];

(* Generic Tensor Rule (Excluding Delta) *)
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


(* ========================================================== *)
(* 3. ALGEBRAIC SIMPLIFICATION (Canonicalization)             *)
(* ========================================================== *)

CanonicalizeTerm[term_] := 
  Module[{indices, counts, dummies, replacementRules},
   indices = Cases[term, (\[ScriptCapitalU] | \[ScriptCapitalD])[i_] :> i, Infinity];
   counts = Tally[indices];
   dummies = Select[counts, Last[#] == 2 &][[All, 1]];
   
   (* Map dummies to FormalI1, FormalI2... *)
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
(* 4. PRETTY PRINTING (TensorForm)                            *)
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
(* 5. PHYSICS RULES (Metric & Transformations)                *)
(* ========================================================== *)

metricRules = {
   (* Lowering *)
   g[\[ScriptCapitalD][\[Mu]_], \[ScriptCapitalD][\[Nu]_]] * vec_[\[ScriptCapitalU][\[Nu]_]] :> vec[\[ScriptCapitalD][\[Mu]]],
   vec_[\[ScriptCapitalU][\[Nu]_]] * g[\[ScriptCapitalD][\[Mu]_], \[ScriptCapitalD][\[Nu]_]] :> vec[\[ScriptCapitalD][\[Mu]]],
   
   (* Raising *)
   g[\[ScriptCapitalU][\[Mu]_], \[ScriptCapitalU][\[Nu]_]] * covec_[\[ScriptCapitalD][\[Nu]_]] :> covec[\[ScriptCapitalU][\[Mu]]],
   covec_[\[ScriptCapitalD][\[Nu]_]] * g[\[ScriptCapitalU][\[Mu]_], \[ScriptCapitalU][\[Nu]_]] :> covec[\[ScriptCapitalU][\[Mu]]],

   (* Inverse Identity *)
   g[\[ScriptCapitalU][\[Mu]_], \[ScriptCapitalU][\[Alpha]_]] * g[\[ScriptCapitalD][\[Alpha]_], \[ScriptCapitalD][\[Nu]_]] :> \[Delta][\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Nu]]],
   g[\[ScriptCapitalD][\[Alpha]_], \[ScriptCapitalD][\[Nu]_]] * g[\[ScriptCapitalU][\[Mu]_], \[ScriptCapitalU][\[Alpha]_]] :> \[Delta][\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Nu]]]
};

simpleCommaRules = {
   Derivative[1][f_][x_[\[ScriptCapitalU][\[Alpha]_]][t_]] :> Subscript[f, ","][\[ScriptCapitalD][\[Alpha]]]
};

robustTransformRules = {
   (* Vector Transformation with Alpha-Conversion *)
   A_[\[ScriptCapitalU][primedIndex_]] :> 
    Module[{freshIndex},
     freshIndex = Unique["\[Mu]"]; 
     Partials[x[\[ScriptCapitalU][primedIndex]], x[\[ScriptCapitalU][freshIndex]]] * A[\[ScriptCapitalU][freshIndex]]
     ],

   (* Covector Transformation with Alpha-Conversion *)
   p_[\[ScriptCapitalD][primedIndex_]] :> 
    Module[{freshIndex},
     freshIndex = Unique["\[Nu]"]; 
     Partials[x[\[ScriptCapitalU][freshIndex]], x[\[ScriptCapitalU][primedIndex]]] * p[\[ScriptCapitalD][freshIndex]]
     ]
};

End[]; (* End Private Context *)
EndPackage[];
