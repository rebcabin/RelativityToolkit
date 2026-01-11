(* =========================================================================*)
(*RELATIVITY TOOLKIT ENGINE (Script Mode)*)
(*Version:1.4.3 (Feature:\
Covariant Derivative CD with Semicolon Notation)*)(* =========================================================================*)
RelativityToolkitVersion = "1.4.3";

(*1. CLEAN SLATE----------------------------------------------------------*)
Unprotect[MakeBoxes];
Quiet[DownValues[MakeBoxes] = 
   Select[DownValues[MakeBoxes], FreeQ[#, Partials] &]];
Quiet[DownValues[MakeBoxes] = 
   Select[DownValues[MakeBoxes], FreeQ[#, CD] &]]; (*Clean CD rules*)
Quiet[MakeBoxes[\[Delta][__], StandardForm] =.];
Quiet[MakeBoxes[\[CapitalGamma][__], StandardForm] =.];
Quiet[DownValues[MakeBoxes] = 
   Select[DownValues[MakeBoxes], 
    FreeQ[#, \[ScriptCapitalU]] && FreeQ[#, \[ScriptCapitalD]] &]];
Protect[MakeBoxes];

ClearAll[valence, CanonicalizeIndices, TensorForm, Partials, 
  CD, \[CapitalGamma], \[Delta], upQ, downQ, x, p, g, u, A, B, T, P, 
  Q, S, metricRules, robustTransformRules, differentiationRules, 
  ExpandDerivatives];

(* =========================================================================*)
(*2. VALENCE LOGIC (The Type Checker)*)
(* =========================================================================*)
upQ[x_] := upQ[valence[x]];
downQ[x_] := downQ[valence[x]];
upQ[{{__}, {}}] := True; upQ[___] := False;
downQ[{{}, {__}}] := True; downQ[___] := False;

contractValence[{u_List, d_List}] := 
  Module[{common}, common = Intersection[u, d];
   {DeleteElements[u, common], DeleteElements[d, common]}];

valence[x_Symbol] := {{}, {}};
valence[_?NumericQ] := {{}, {}};
valence[] := Null;

valence[_[\[ScriptCapitalU][\[Alpha]_]]] := {{\[Alpha]}, {}};
valence[_[\[ScriptCapitalD][\[Alpha]_]]] := {{}, {\[Alpha]}};

valence[g[\[ScriptCapitalD][\[Mu]_], \[ScriptCapitalD][\[Nu]_]]] := \
{{}, {\[Mu], \[Nu]}};
valence[g[\[ScriptCapitalU][\[Mu]_], \[ScriptCapitalU][\[Nu]_]]] := \
{{\[Mu], \[Nu]}, {}};
valence[\[Delta][\[ScriptCapitalU][\[Mu]_], \[ScriptCapitalD][\[Nu]_]]] \
:= {{\[Mu]}, {\[Nu]}};

(*Partials adds a denominator index*)
valence[Partials[num_, den_]] := 
  Module[{vn, vd, vdFlipped}, vn = valence[num];
   vd = valence[den];
   vdFlipped = {vd[[2]], vd[[1]]};
   {Join[vn[[1]], vdFlipped[[1]]], Join[vn[[2]], vdFlipped[[2]]]}];

(*CD behaves exactly like Partials for valence*)
valence[CD[num_, den_]] := valence[Partials[num, den]];

valence[\[CapitalGamma][\[ScriptCapitalU][a_], \[ScriptCapitalD][
     b_], \[ScriptCapitalD][c_]]] := {{a}, {b, c}};

valence[Derivative[1][f_][arg_]] := 
  Module[{u, d}, {u, d} = valence[arg]; {d, u}];
valence[Derivative[_][_][f_] /; (valence[f] =!= {{}, {}})] := 
  valence[f];

valence[(h : _[\[ScriptCapitalU][_]] | _[\[ScriptCapitalD][_]])[___]] \
:= valence[h];
valence[prod_Times] := 
  contractValence[
   Fold[Join[#1, valence[#2], 2] &, {{}, {}}, List @@ prod]];
valence[sum_Plus] := 
  Module[{vs}, vs = valence /@ (List @@ sum); 
   If[Apply[SameQ, vs], First[vs], Print["Valence Mismatch"]; {{}, {}}]];
valence[Power[b_, _]] := valence[b];
valence[h_[a_, b_, rest___]] := 
  Join[valence[h[a]], valence[h[b, rest]], 2];
valence[___] := {{}, {}};

(* =========================================================================*)
(*3. DISPLAY RULES*)
(* =========================================================================*)

Unprotect[MakeBoxes];

(*Semicolon Notation for CD*)
MakeBoxes[CD[num_, den_], StandardForm] := 
  If[MatchQ[den, x[\[ScriptCapitalU][_]]],(*YES:Format as Subscript[
   num,";k"]*)Replace[den, 
    x[\[ScriptCapitalU][idx_]] :> 
     SubscriptBox[MakeBoxes[num, StandardForm], 
      RowBox[{";", MakeBoxes[idx, StandardForm]}]]],(*Fallback*)
   RowBox[{"CD", "[", MakeBoxes[num, StandardForm], ",", 
     MakeBoxes[den, StandardForm], "]"}]];

MakeBoxes[Partials[num_, den_], StandardForm] := 
  If[MatchQ[num, Partials[_, _]],(*Case 1:Second Derivative (Nested)*)
   Replace[num, 
    Partials[top_, bot1_] :> FractionBox[RowBox[{SuperscriptBox["\[PartialD]",\
 "2"], MakeBoxes[top, StandardForm]}], 
      RowBox[{RowBox[{"\[PartialD]", MakeBoxes[den, StandardForm]}], 
        RowBox[{"\[PartialD]", 
          MakeBoxes[bot1, StandardForm]}]}]]],(*Case 2:
   Comma Notation Check*)
   If[MatchQ[den, x[\[ScriptCapitalU][_]]] && ! 
      MatchQ[num, x[\[ScriptCapitalU][_]]],(*YES (Fields):Comma*)
    Replace[den, 
     x[\[ScriptCapitalU][idx_]] :> 
      SubscriptBox[MakeBoxes[num, StandardForm], 
       RowBox[{",", MakeBoxes[idx, StandardForm]}]]],(*NO (Jacobians):
    Fraction*)
    FractionBox[RowBox[{"\[PartialD]", MakeBoxes[num, StandardForm]}],
      RowBox[{"\[PartialD]", MakeBoxes[den, StandardForm]}]]]];

(*Gamma& Delta*)
MakeBoxes[\[CapitalGamma][\[ScriptCapitalU][u_], \[ScriptCapitalD][
     d1_], \[ScriptCapitalD][d2_]], StandardForm] := 
  SubsuperscriptBox["\[CapitalGamma]", 
   RowBox[{MakeBoxes[d1, StandardForm], MakeBoxes[d2, StandardForm]}],
    MakeBoxes[u, StandardForm]];

MakeBoxes[\[Delta][\[ScriptCapitalU][up_], \[ScriptCapitalD][down_]], 
   StandardForm] := 
  SubsuperscriptBox["\[Delta]", MakeBoxes[down, StandardForm], 
   MakeBoxes[up, StandardForm]];
MakeBoxes[\[Delta][\[ScriptCapitalD][down_], \[ScriptCapitalU][up_]], 
   StandardForm] := 
  SubsuperscriptBox["\[Delta]", MakeBoxes[down, StandardForm], 
   MakeBoxes[up, StandardForm]];

(*Generic Tensors*)
MakeBoxes[h_[indices__], 
   StandardForm] /; (h =!= \[Delta]) && (h =!= Partials) && (h =!= 
     CD) && (h =!= \[CapitalGamma]) && 
   MatchQ[{indices}, {(_[\[ScriptCapitalU]] | _[\[ScriptCapitalD]] | \
\[ScriptCapitalU][_] | \[ScriptCapitalD][_]) ..}] := 
 Module[{formattedScripts}, 
  formattedScripts = {indices} /. {\[ScriptCapitalU][i_] :> 
      SuperscriptBox["", 
       MakeBoxes[i, StandardForm]], \[ScriptCapitalD][i_] :> 
      SubscriptBox["", MakeBoxes[i, StandardForm]]};
  RowBox[Prepend[formattedScripts, MakeBoxes[h, StandardForm]]]]

Protect[MakeBoxes];

(* =========================================================================*)
(*4. ALGEBRAIC SIMPLIFICATION& 5. PHYSICS RULES*)
(* =========================================================================*)
CanonicalizeTerm[term_] := 
  Module[{indices, counts, dummies, replacementRules}, 
   indices = 
    Cases[term, (\[ScriptCapitalU] | \[ScriptCapitalD])[i_] :> i, 
     Infinity];
   counts = Tally[indices];
   dummies = Select[counts, Last[#] == 2 &][[All, 1]];
   replacementRules = 
    MapIndexed[#1 -> Symbol["\[FormalI]" <> ToString[First[#2]]] &, 
     dummies];
   term /. replacementRules];

CanonicalizeIndices[expr_Plus] := 
  Total[CanonicalizeIndices /@ List @@ expr];
CanonicalizeIndices[expr_Equal] := 
  Equal @@ (CanonicalizeIndices /@ List @@ expr);
CanonicalizeIndices[expr_] := CanonicalizeTerm[expr];

metricRules = {g[\[ScriptCapitalD][mu_], \[ScriptCapitalD][nu_]]*
     vec_[\[ScriptCapitalU][nu_]] :> vec[\[ScriptCapitalD][mu]], 
   vec_[\[ScriptCapitalU][nu_]]*
     g[\[ScriptCapitalD][mu_], \[ScriptCapitalD][nu_]] :> 
    vec[\[ScriptCapitalD][mu]], 
   g[\[ScriptCapitalD][nu_], \[ScriptCapitalD][mu_]]*
     vec_[\[ScriptCapitalU][nu_]] :> vec[\[ScriptCapitalD][mu]], 
   vec_[\[ScriptCapitalU][nu_]]*
     g[\[ScriptCapitalD][nu_], \[ScriptCapitalD][mu_]] :> 
    vec[\[ScriptCapitalD][mu]], 
   g[\[ScriptCapitalU][mu_], \[ScriptCapitalU][nu_]]*
     covec_[\[ScriptCapitalD][nu_]] :> covec[\[ScriptCapitalU][mu]], 
   covec_[\[ScriptCapitalD][nu_]]*
     g[\[ScriptCapitalU][mu_], \[ScriptCapitalU][nu_]] :> 
    covec[\[ScriptCapitalU][mu]], 
   g[\[ScriptCapitalU][nu_], \[ScriptCapitalU][mu_]]*
     covec_[\[ScriptCapitalD][nu_]] :> covec[\[ScriptCapitalU][mu]], 
   covec_[\[ScriptCapitalD][nu_]]*
     g[\[ScriptCapitalU][nu_], \[ScriptCapitalU][mu_]] :> 
    covec[\[ScriptCapitalU][mu]], 
   g[\[ScriptCapitalU][mu_], \[ScriptCapitalU][\[Alpha]_]]*
     g[\[ScriptCapitalD][\[Alpha]_], \[ScriptCapitalD][
       nu_]] :> \[Delta][\[ScriptCapitalU][mu], \[ScriptCapitalD][nu]],
    g[\[ScriptCapitalD][\[Alpha]_], \[ScriptCapitalD][nu_]]*
     g[\[ScriptCapitalU][
       mu_], \[ScriptCapitalU][\[Alpha]_]] :> \[Delta][\
\[ScriptCapitalU][mu], \[ScriptCapitalD][nu]]};

robustTransformRules = {A_[\[ScriptCapitalU][primed_]] :> 
    Module[{fresh = Unique["\[Mu]"]}, 
     Partials[x[\[ScriptCapitalU][primed]], x[\[ScriptCapitalU][fresh]]]*
      A[\[ScriptCapitalU][fresh]]], 
   p_[\[ScriptCapitalD][primed_]] :> 
    Module[{fresh = Unique["\[Nu]"]}, 
     Partials[x[\[ScriptCapitalU][fresh]], x[\[ScriptCapitalU][primed]]]*
      p[\[ScriptCapitalD][fresh]]]};

TensorForm[expr_] := 
  Module[{canonExpr, indexMap, prettyIndices}, 
   canonExpr = CanonicalizeIndices[expr];
   prettyIndices = {\[Lambda], \[Kappa], \[Rho], \[Sigma], \[Mu], \
\[Nu], \[Tau], \[Eta], \[Chi], \[Psi]};
   indexMap = 
    Table[Symbol["\[FormalI]" <> ToString[n]] -> 
      prettyIndices[[n]], {n, Length[prettyIndices]}];
   HoldForm[Evaluate[canonExpr /. indexMap]]];

(* =========================================================================*)
(*6. CALCULUS ENGINE (Automatic Differentiation& CD)*)
(* =========================================================================*)

differentiationRules = {(*1. Leibniz Product Rule*)
   Partials[a_*b_, var_] :> 
    Partials[a, var]*b + 
     a*Partials[b, var],(*2. Chain Rule for Sums (Linearity)*)
   Partials[a_ + b_, var_] :> 
    Partials[a, var] + 
     Partials[b, 
      var],(*3. Chain Rule for Coordinates*)(*If differentiating w.r.\
t.a PRIMED index,switch to UNPRIMED*)
   Partials[expr_, x[\[ScriptCapitalU][idx_]]] /; 
     StringContainsQ[ToString[idx], "'"] :> 
    Module[{fresh = Unique["\[Sigma]"]}, 
     Partials[x[\[ScriptCapitalU][fresh]], x[\[ScriptCapitalU][idx]]]*
      Partials[expr, x[\[ScriptCapitalU][fresh]]]]};

ExpandDerivatives[expr_] := expr //. differentiationRules;

(*CD Definition (Contravariant Vector)*)
(*Expands to Partial+Gamma term with a fresh dummy index*)
CD[vec_[\[ScriptCapitalU][mu_]], x[\[ScriptCapitalU][nu_]]] := 
  Module[{lam = Unique["\[Lambda]"]}, 
   Partials[vec[\[ScriptCapitalU][mu]], 
     x[\[ScriptCapitalU][nu]]] + \[CapitalGamma][\[ScriptCapitalU][
       mu], \[ScriptCapitalD][nu], \[ScriptCapitalD][lam]]*
     vec[\[ScriptCapitalU][lam]]];
