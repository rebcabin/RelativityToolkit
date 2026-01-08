(* ::Package::*)

(* =========================================================================*)
(* RELATIVITY TOOLKIT: PART 1*)
(* Formal Differential Geometry in the UD Calculus*)
(* =========================================================================*)

(* 1. CLEAN SLATE ----------------------------------------------------------*)(* \
Clear previous definitions to ensure a fresh environment. *)
ClearAll["RelativityToolkit`*"];
ClearAll["RelativityToolkit`Private`*"];

Unprotect[MakeBoxes];
Quiet[MakeBoxes[Partials[_, _], StandardForm] =.];
Quiet[MakeBoxes[\[Delta][__], StandardForm] =.];
Quiet[DownValues[MakeBoxes] =
   Select[
    DownValues[MakeBoxes], 
    FreeQ[#, \[ScriptCapitalU]] && FreeQ[#, \[ScriptCapitalD]] &]];
Protect[MakeBoxes];

ClearAll[
  valence,
  CanonicalizeIndices,
  TensorForm,
  Partials,
  upQ,
  downQ, x, p, g, u, A, B, T, P, Q, S,
  metricRules,
  simpleCommaRules,
  robustTransformRules];

(* =========================================================================*)
(* 2. VALENCE LOGIC (The Type Checker)*)
(* =========================================================================*)

(*---Predicates----------------------------------------------------------*)
upQ[x_] := upQ[valence[x]];
downQ[x_] := downQ[valence[x]];

upQ[{{__}, {}}] := True;
upQ[___] := False;

downQ[{{}, {__}}] := True;
downQ[___] := False;

(*---Helpers-------------------------------------------------------------*)
contractValence[{u_List, d_List}] :=
  Module[{common},
   common = Intersection[u, d];
   {DeleteElements[u, common],
    DeleteElements[d, common]}];

(*---Base Cases----------------------------------------------------------*)
valence[x_Symbol] := {{}, {}};
valence[_?NumericQ] := {{}, {}};
valence[] := Null;

(*---Tensor Atoms--------------------------------------------------------*)
(*Direct matching on Global symbols for inline usage*)
valence[_[\[ScriptCapitalU][\[Alpha]_]]] := {{\[Alpha]}, {}};
valence[_[\[ScriptCapitalD][\[Alpha]_]]] := {{}, {\[Alpha]}};

(*---Metric& Delta------------------------------------------------------*)
(*g and \[Delta]\
 are special global symbols for the metric and Kronecker delta *)
valence[g[\[ScriptCapitalD][\[Mu]_], \[ScriptCapitalD][\[Nu]_]]] := \
{{}, {\[Mu], \[Nu]}};
valence[g[\[ScriptCapitalU][\[Mu]_], \[ScriptCapitalU][\[Nu]_]]] := \
{{\[Mu], \[Nu]}, {}};
valence[\[Delta][\[ScriptCapitalU][\[Mu]_], \[ScriptCapitalD][\[Nu]_]]] \
:= {{\[Mu]}, {\[Nu]}};

(*---Partials (Jacobians)------------------------------------------------*)
valence[Partials[num_, den_]] :=
  Module[{vn, vd, vdFlipped},
   vn = valence[num];
   vd = valence[den];
   (*Denominator variance flips:Up->Down,Down->Up*)
   vdFlipped = {vd[[2]], vd[[1]]};
   {Join[vn[[1]], vdFlipped[[1]]], Join[vn[[2]], vdFlipped[[2]]]}];

(*---Differentiation-----------------------------------------------------*)
(*Gradient Rule:d(T)/dx^u flips the valence of x^u from Up to Down*)
valence[Derivative[1][f_][arg_]] :=
  Module[{u, d},
   {u, d} = valence[arg];
   If[{u, d} === {{}, {}},
    {{}, {}},
    {d, u}]];

(*Higher derivatives pass through if the function has valence*)
valence[Derivative[_][_][f_] /; (valence[f] =!= {{}, {}})] := 
  valence[f];

(*---Recursion & Arithmetic---------------------------------------------*)
(*Pass-through for Heads like x[U[mu]]*)
valence[(h : _[\[ScriptCapitalU][_]] | _[\[ScriptCapitalD][_]])[___]] \
:= valence[h];

valence[prod_Times] := contractValence[
   Fold[
    Join[#1, valence[#2], 2] &,
    {{}, {}},
    List @@ prod]];

valence[sum_Plus] :=
  Module[{vs},
   vs = valence /@ (List @@ sum);
   If[Apply[SameQ, vs],
    First[vs],
    Print["Valence Mismatch"]; {{}, {}}]];

valence[Power[b_, _]] := valence[b];

(*Multi-index recursion:T[a,b]->Join valences*)
valence[h_[a_, b_, rest___]] :=
  Join[
   valence[h[a]],
   valence[h[b, rest]], 2];

(*Fallback*)
valence[___] := {{}, {}};

(* =========================================================================*)
(* 3. DISPLAY RULES (Textbook Formatting)*)
(* =========================================================================*)

Unprotect[MakeBoxes];

(*---Partials------------------------------------------------------------*)
MakeBoxes[Partials[num_, den_], StandardForm] :=
  FractionBox[
   RowBox[{"\[PartialD]", MakeBoxes[num, StandardForm]}],
   RowBox[{"\[PartialD]", MakeBoxes[den, StandardForm]}]];

(*---Kronecker Delta-----------------------------------------------------*)
(*Enforce vertical stacking for Delta indices*)
MakeBoxes[\[Delta][\[ScriptCapitalU][up_], \[ScriptCapitalD][down_]], 
   StandardForm] :=
  SubsuperscriptBox["\[Delta]",
   MakeBoxes[down, StandardForm],
   MakeBoxes[up, StandardForm]];

MakeBoxes[\[Delta][\[ScriptCapitalD][down_], \[ScriptCapitalU][up_]], 
   StandardForm] :=
  SubsuperscriptBox["\[Delta]",
   MakeBoxes[down, StandardForm],
   MakeBoxes[up, StandardForm]];

(*---Generic Tensors-----------------------------------------------------*)
(*Matches ANY head h_[\
indices] where indices are wrapped in U[...] or D[...]*)
MakeBoxes[h_[indices__], 
   StandardForm] /; (h =!= \[Delta]) && (h =!= Partials) && 
   MatchQ[{indices}, {(\[ScriptCapitalU][_] | \[ScriptCapitalD][_]) ..}] \
:= Module[{formattedScripts}, formattedScripts = {indices} /. {
     \[ScriptCapitalU][i_] :> 
      SuperscriptBox["", MakeBoxes[i, StandardForm]],
     \[ScriptCapitalD][i_] :> 
      SubscriptBox["", MakeBoxes[i, StandardForm]]};
  RowBox[Prepend[formattedScripts, MakeBoxes[h, StandardForm]]]]

Protect[MakeBoxes];

(* =========================================================================*)
(* 4. ALGEBRAIC SIMPLIFICATION (Index Canonicalization)*)
(* =========================================================================*)

CanonicalizeTerm[term_] :=
  Module[{indices, counts, dummies, replacementRules},
   (*Identify all Up/Down indices*)
   indices = 
    Cases[term, (\[ScriptCapitalU] | \[ScriptCapitalD])[i_] :> i, 
     Infinity];
   (*Find pairs*)
   counts = Tally[indices];
   dummies = Select[counts, Last[#] == 2 &][[All, 1]];
   (*Create rules:mu->FormalI1,nu->FormalI2,etc.*)
   replacementRules = 
    MapIndexed[#1 -> Symbol["\[FormalI]" <> ToString[First[#2]]] &, 
     dummies];
   term /. replacementRules];

CanonicalizeIndices[expr_Plus] := 
  Total[CanonicalizeIndices /@ List @@ expr];
CanonicalizeIndices[expr_Equal] := 
  Equal @@ (CanonicalizeIndices /@ List @@ expr);
CanonicalizeIndices[expr_] := CanonicalizeTerm[expr];

(* =========================================================================*)
(* 5. PHYSICS RULES (Metric & Transformations)*)
(* =========================================================================*)

metricRules = {
   
   (*---LOWERING RULES (g_uv A^v->A_u)---*)
   (*Standard:Contract the second index (g_uv A^v)*)
   g[\[ScriptCapitalD][\[Mu]_], \[ScriptCapitalD][\[Nu]_]]*
     vec_[\[ScriptCapitalU][\[Nu]_]] :> vec[\[ScriptCapitalD][\[Mu]]],
   vec_[\[ScriptCapitalU][\[Nu]_]]*
     g[\[ScriptCapitalD][\[Mu]_], \[ScriptCapitalD][\[Nu]_]] :> 
    vec[\[ScriptCapitalD][\[Mu]]],
   (*Symmetry:Contract the first index (g_vu A^v)*)
   g[\[ScriptCapitalD][\[Nu]_], \[ScriptCapitalD][\[Mu]_]]*
     vec_[\[ScriptCapitalU][\[Nu]_]] :> vec[\[ScriptCapitalD][\[Mu]]],
   vec_[\[ScriptCapitalU][\[Nu]_]]*
     g[\[ScriptCapitalD][\[Nu]_], \[ScriptCapitalD][\[Mu]_]] :> 
    vec[\[ScriptCapitalD][\[Mu]]],
   
   (*---RAISING RULES (g^uv A_v->A^u)---*)
   (*Standard:Contract the second index (g^uv A_v)*)
   g[\[ScriptCapitalU][\[Mu]_], \[ScriptCapitalU][\[Nu]_]]*
     covec_[\[ScriptCapitalD][\[Nu]_]] :> 
    covec[\[ScriptCapitalU][\[Mu]]],
   covec_[\[ScriptCapitalD][\[Nu]_]]*
     g[\[ScriptCapitalU][\[Mu]_], \[ScriptCapitalU][\[Nu]_]] :> 
    covec[\[ScriptCapitalU][\[Mu]]],
   (*Symmetry:Contract the first index (g^vu A_v)*)
   g[\[ScriptCapitalU][\[Nu]_], \[ScriptCapitalU][\[Mu]_]]*
     covec_[\[ScriptCapitalD][\[Nu]_]] :> 
    covec[\[ScriptCapitalU][\[Mu]]],
   covec_[\[ScriptCapitalD][\[Nu]_]]*
     g[\[ScriptCapitalU][\[Nu]_], \[ScriptCapitalU][\[Mu]_]] :> 
    covec[\[ScriptCapitalU][\[Mu]]],
   
   (*---INVERSE IDENTITY (g^ua g_av->delta^u_v)---*)
   g[\[ScriptCapitalU][\[Mu]_], \[ScriptCapitalU][\[Alpha]_]]*
     g[\[ScriptCapitalD][\[Alpha]_], \[ScriptCapitalD][\[Nu]_]] :> \
\[Delta][\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Nu]]],
   g[\[ScriptCapitalD][\[Alpha]_], \[ScriptCapitalD][\[Nu]_]]*
     g[\[ScriptCapitalU][\[Mu]_], \[ScriptCapitalU][\[Alpha]_]] :> \
\[Delta][\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Nu]]]};

robustTransformRules = {
   (*Vector Transformation with Alpha-Conversion*)
   A_[\[ScriptCapitalU][primedIndex_]] :>
    Module[{freshIndex},
     freshIndex = Unique["\[Mu]"];
     Partials[x[\[ScriptCapitalU][primedIndex]], 
       x[\[ScriptCapitalU][freshIndex]]]*
      A[\[ScriptCapitalU][freshIndex]]],
   (*Covector Transformation with Alpha-Conversion*)
   p_[\[ScriptCapitalD][primedIndex_]] :>
    Module[{freshIndex},
     freshIndex = Unique["\[Nu]"];
     Partials[x[\[ScriptCapitalU][freshIndex]], 
       x[\[ScriptCapitalU][primedIndex]]]*
      p[\[ScriptCapitalD][freshIndex]]]};

(* =========================================================================*)
(* 6. PRETTY PRINTING (TensorForm)*)
(* =========================================================================*)

TensorForm[expr_] :=
  Module[{canonExpr, indexMap, prettyIndices},
   canonExpr = CanonicalizeIndices[expr];
   (*Standard pool of physics indices for display*)
   prettyIndices = {\[Lambda], \[Kappa], \[Rho], \[Sigma], \[Mu], \
\[Nu], \[Tau], \[Eta], \[Chi], \[Psi]};
   indexMap = 
    Table[Symbol["\[FormalI]" <> ToString[n]] -> 
      prettyIndices[[n]], {n, Length[prettyIndices]}];
   HoldForm[Evaluate[canonExpr /. indexMap]]];

Print["Relativity Toolkit Part 1 (Inline) Loaded Successfully."];
