(* =========================================================================*)
(* RELATIVITY TOOLKIT ENGINE (Script Mode) *)
(* Version: 1.5.3 (Feature: Riemann) *)
(* =========================================================================*)
RelativityToolkitVersion = "1.5.3";

(* 1. CLEAN SLATE ----------------------------------------------------------*)
Unprotect[MakeBoxes];
Quiet[DownValues[MakeBoxes] = 
   Select[DownValues[MakeBoxes], FreeQ[#, Partials] &]];
Quiet[DownValues[MakeBoxes] = 
   Select[DownValues[MakeBoxes], FreeQ[#, CD] &]];
Quiet[MakeBoxes[\[Delta][__], StandardForm] =.];
Quiet[MakeBoxes[\[CapitalGamma][__], StandardForm] =.];
Quiet[DownValues[MakeBoxes] = 
   Select[DownValues[MakeBoxes], 
    FreeQ[#, \[ScriptCapitalU]] && FreeQ[#, \[ScriptCapitalD]] &]];
Protect[MakeBoxes];

ClearAll[contractValence, valence, Partials, CanonicalizeTerm,
CanonicalizeIndices, TensorForm, 
  CD, \[CapitalGamma], \[Delta], upQ, downQ, 
  \[Lambda], \[Kappa], \[Rho], \[Sigma], \[Mu], \[Nu], \[Tau], \[Eta], \[Chi], \[Psi],
  x, p, g, u, A, B, T, 
  P, Q, S, metricRules, robustTransformRules, differentiationRules, 
  ExpandDerivatives];

(* =========================================================================*)
(* 2. VALENCE LOGIC (The Type Checker) *)
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

valence[g[\[ScriptCapitalD][\[Mu]_], \[ScriptCapitalD][\[Nu]_]]] := {{}, {\[Mu], \[Nu]}};
valence[g[\[ScriptCapitalU][\[Mu]_], \[ScriptCapitalU][\[Nu]_]]] := {{\[Mu], \[Nu]}, {}};
valence[\[Delta][\[ScriptCapitalU][\[Mu]_], \[ScriptCapitalD][\[Nu]_]]] := {{\[Mu]}, {\[Nu]}};

valence[Partials[num_, den_]] := 
  Module[{vn, vd, vdFlipped}, vn = valence[num];
   vd = valence[den];
   vdFlipped = {vd[[2]], vd[[1]]};
   {Join[vn[[1]], vdFlipped[[1]]], Join[vn[[2]], vdFlipped[[2]]]}];

valence[CD[num_, den_]] := valence[Partials[num, den]];

valence[\[CapitalGamma][\[ScriptCapitalU][a_], \[ScriptCapitalD][
     b_], \[ScriptCapitalD][c_]]] := {{a}, {b, c}};

valence[Derivative[1][f_][arg_]] := 
  Module[{u, d}, {u, d} = valence[arg]; {d, u}];
valence[Derivative[_][_][f_] /; (valence[f] =!= {{}, {}})] := 
  valence[f];

valence[(h : _[\[ScriptCapitalU][_]] | _[\[ScriptCapitalD][_]])[___]] := valence[h];

valence[prod_Times] := 
  contractValence[
   Fold[Join[#1, valence[#2], 2] &, {{}, {}}, List @@ prod]];

valence[sum_Plus] := 
  Module[{vs}, vs = valence /@ (List @@ sum); 
   If[Apply[SameQ, vs], 
   First[vs], 
   Print["Valence Mismatch"]; {{}, {}}]];
   
valence[Power[b_, _]] := valence[b];

valence[h_[a_, b_, rest___]] := 
  Join[valence[h[a]], valence[h[b, rest]], 2];
  
valence[___] := {{}, {}};

(* =========================================================================*)
(* 3. DISPLAY RULES *)
(* =========================================================================*)

Unprotect[MakeBoxes];

(*Semicolon Notation for CD*)
MakeBoxes[CD[num_, den_], StandardForm] := 
  If[MatchQ[den, x[\[ScriptCapitalU][_]]], 
   Replace[den, 
    x[\[ScriptCapitalU][idx_]] :> 
     SubscriptBox[MakeBoxes[num, StandardForm], 
      RowBox[{";", MakeBoxes[idx, StandardForm]}]]], 
   RowBox[{"CD", "[", MakeBoxes[num, StandardForm], ",", 
     MakeBoxes[den, StandardForm], "]"}]];

MakeBoxes[Partials[num_, den_], StandardForm] := 
  If[MatchQ[num, Partials[_, _]], 
   Replace[num, 
    Partials[top_, bot1_] :> 
     FractionBox[
      RowBox[{SuperscriptBox["\[PartialD]", "2"], 
        MakeBoxes[top, StandardForm]}], 
      RowBox[{RowBox[{"\[PartialD]", MakeBoxes[den, StandardForm]}], 
        RowBox[{"\[PartialD]", MakeBoxes[bot1, StandardForm]}]}]]], 
   If[MatchQ[den, x[\[ScriptCapitalU][_]]] && !MatchQ[num, x[\[ScriptCapitalU][_]]], 
    Replace[den, 
     x[\[ScriptCapitalU][idx_]] :> 
      SubscriptBox[MakeBoxes[num, StandardForm], 
       RowBox[{",", MakeBoxes[idx, StandardForm]}]]], 
    FractionBox[RowBox[{"\[PartialD]", MakeBoxes[num, StandardForm]}],
      RowBox[{"\[PartialD]", MakeBoxes[den, StandardForm]}]]]];

MakeBoxes[\[CapitalGamma][\[ScriptCapitalU][u_], \[ScriptCapitalD][d1_], \[ScriptCapitalD][d2_]], StandardForm] := 
  SubsuperscriptBox["\[CapitalGamma]", 
   RowBox[{MakeBoxes[d1, StandardForm], MakeBoxes[d2, StandardForm]}],
    MakeBoxes[u, StandardForm]];

MakeBoxes[\[Delta][\[ScriptCapitalU][up_], \[ScriptCapitalD][down_]], StandardForm] := 
  SubsuperscriptBox["\[Delta]", MakeBoxes[down, StandardForm], 
   MakeBoxes[up, StandardForm]];
   
MakeBoxes[\[Delta][\[ScriptCapitalD][down_], \[ScriptCapitalU][up_]], StandardForm] := 
  SubsuperscriptBox["\[Delta]", MakeBoxes[down, StandardForm], 
   MakeBoxes[up, StandardForm]];

MakeBoxes[h_[indices__], StandardForm] /; (h =!= \[Delta]) && 
(h =!= Partials) && 
(h =!= CD) && 
(h =!= \[CapitalGamma]) && 
MatchQ[{indices}, {(_[\[ScriptCapitalU]] | _[\[ScriptCapitalD]] | \[ScriptCapitalU][_] | \[ScriptCapitalD][_]) ..}] := 
 Module[{formattedScripts}, 
  formattedScripts = {indices} /. {\[ScriptCapitalU][i_] :> 
      SuperscriptBox["", 
       MakeBoxes[i, StandardForm]], \[ScriptCapitalD][i_] :> 
      SubscriptBox["", MakeBoxes[i, StandardForm]]};
  RowBox[Prepend[formattedScripts, MakeBoxes[h, StandardForm]]]]

Protect[MakeBoxes];

(* =========================================================================*)
(* 4. ALGEBRAIC SIMPLIFICATION *)
(* =========================================================================*)

CanonicalizeTerm[term_] := 
  Module[{indices, counts, dummies, replacementRules}, 
   indices = Cases[term, (\[ScriptCapitalU] | \[ScriptCapitalD])[i_] :> i, Infinity];
   counts = Tally[indices];
   dummies = Select[counts, Last[#] == 2 &][[All, 1]];
   replacementRules = MapIndexed[
     #1 -> Symbol["\[FormalI]" <> ToString[First[#2]]] &, 
     dummies];
   term /. replacementRules];

CanonicalizeIndices[expr_Plus] := 
  Total[CanonicalizeIndices /@ List @@ expr];
  
CanonicalizeIndices[expr_Equal] := 
  Equal @@ (CanonicalizeIndices /@ List @@ expr);

CanonicalizeIndices[expr_] := CanonicalizeTerm[expr];

robustTransformRules = {
  A_[\[ScriptCapitalU][primed_]] :> 
    Module[{fresh = Unique["\[Mu]"]}, 
     Partials[x[\[ScriptCapitalU][primed]], x[\[ScriptCapitalU][fresh]]]*A[\[ScriptCapitalU][fresh]]], 
  p_[\[ScriptCapitalD][primed_]] :> 
    Module[{fresh = Unique["\[Nu]"]}, 
     Partials[x[\[ScriptCapitalU][fresh]], x[\[ScriptCapitalU][primed]]]*p[\[ScriptCapitalD][fresh]]]};

(* DYNAMIC TENSOR FORM *)
TensorForm[expr_] := 
  Module[{canonExpr, formalPattern,formalIndices, usedSymbols, greekPool, availableGreek, indexMap},
   
   (* 1. Canonicalize first *)
   canonExpr = CanonicalizeIndices[expr];

(* 2. Define the Formal Pattern: Any symbol starting with \[FormalA]...\[FormalZ] *)
   (* This covers \[FormalS], \[FormalI]1, \[FormalI]2, etc. automatically *)
   formalPattern = CharacterRange["\[FormalA]", "\[FormalZ]"];

   (* 3. Identify ALL Formal Indices *)
   formalIndices = Sort @ DeleteDuplicates @ Cases[canonExpr, 
     s_Symbol /; StringStartsQ[SymbolName[s], formalPattern], 
     Infinity];

   (* 4. Identify symbols ALREADY in the expression *)
   usedSymbols = DeleteDuplicates @ Cases[canonExpr, _Symbol, Infinity];
   (* 4. Define our preferred Greek pool *)
   greekPool = {\[Lambda], \[Kappa], \[Rho], \[Sigma], \[Mu], \[Nu], \[Tau], \[Eta], \[Chi], \[Psi], \[Alpha], \[Beta], \[Gamma]};

   (* 5. Filter the pool: Remove symbols that are already used *)
   availableGreek = DeleteElements[greekPool, usedSymbols];

   (* 6. Create the Map *)
   (* Map the ordered found formals to the available Greek letters *)
   indexMap = Thread[formalIndices -> Take[availableGreek, UpTo[Length[formalIndices]]]];

   (* 7. Apply *)
   HoldForm[Evaluate[canonExpr /. indexMap]]
  ];
 
(* EXTRACT COEFFICIENT FIELD (Quotient-Theorem Tool) *)
ExtractCoefficient[expr_, field_Symbol] := 
 Module[{processTerm, expanded, termList},
  expanded = Expand[expr];
  termList = If[Head[expanded] === Plus, 
    List @@ expanded, 
    {expanded}];
  
  processTerm[term_] := Module[{canonTerm, dummyIndex, targetIdx, normalizedTerm},
    (* 1. Canonicalize locally *)
    canonTerm = CanonicalizeIndices[term];
    
    (* 2. Find the index attached to the field *)
    dummyIndex = Cases[canonTerm, field[_[idx_]] :> idx, Infinity];
    If[dummyIndex === {}, Return[0]];
    
    (* 3. NORMALIZE: Swap the vector's index to a Reserved Symbol (FormalS) *)
    (* This prevents it from colliding with internal dummies (i1, i2) in other terms *)
    targetIdx = First[dummyIndex];
    normalizedTerm = canonTerm /. targetIdx -> Symbol["\[FormalS]"];
    
    (* 4. Extract coefficient of A^S *)
    Coefficient[normalizedTerm, field[\[ScriptCapitalU][Symbol["\[FormalS]"]]]] + 
    Coefficient[normalizedTerm, field[\[ScriptCapitalD][Symbol["\[FormalS]"]]]]];

  Total[processTerm /@ termList]];

(* =========================================================================*)
(* 5. APPLICATION-SPECIFIC RULES *)
(*    User must explicitly apply these when wanted *)
(* =========================================================================*)

metricRules = {
  g[\[ScriptCapitalD][mu_], \[ScriptCapitalD][nu_]]*vec_[\[ScriptCapitalU][nu_]] :> vec[\[ScriptCapitalD][mu]], 
  vec_[\[ScriptCapitalU][nu_]]*g[\[ScriptCapitalD][mu_], \[ScriptCapitalD][nu_]] :> vec[\[ScriptCapitalD][mu]], 
  
  g[\[ScriptCapitalD][nu_], \[ScriptCapitalD][mu_]]*vec_[\[ScriptCapitalU][nu_]] :> vec[\[ScriptCapitalD][mu]], 
  vec_[\[ScriptCapitalU][nu_]]*g[\[ScriptCapitalD][nu_], \[ScriptCapitalD][mu_]] :> vec[\[ScriptCapitalD][mu]], 
  
  g[\[ScriptCapitalU][mu_], \[ScriptCapitalU][nu_]]*covec_[\[ScriptCapitalD][nu_]] :> covec[\[ScriptCapitalU][mu]], 
  covec_[\[ScriptCapitalD][nu_]]*g[\[ScriptCapitalU][mu_], \[ScriptCapitalU][nu_]] :> covec[\[ScriptCapitalU][mu]], 
  
  g[\[ScriptCapitalU][nu_], \[ScriptCapitalU][mu_]]*covec_[\[ScriptCapitalD][nu_]] :> covec[\[ScriptCapitalU][mu]], 
  covec_[\[ScriptCapitalD][nu_]]*g[\[ScriptCapitalU][nu_], \[ScriptCapitalU][mu_]] :> covec[\[ScriptCapitalU][mu]], 
  
  g[\[ScriptCapitalU][mu_], \[ScriptCapitalU][\[Alpha]_]]*g[\[ScriptCapitalD][\[Alpha]_], \[ScriptCapitalD][nu_]] :> \[Delta][\[ScriptCapitalU][mu], \[ScriptCapitalD][nu]],
  g[\[ScriptCapitalD][\[Alpha]_], \[ScriptCapitalD][nu_]]*g[\[ScriptCapitalU][mu_], \[ScriptCapitalU][\[Alpha]_]] :> \[Delta][\[ScriptCapitalU][mu], \[ScriptCapitalD][nu]],
  
  (* \[Delta] Contraction Rules (For Associativity) *)

  (* Rule: T^\[Beta] * Subscript[\[Delta]^\[Alpha], \[Beta]] -> T^\[Alpha] *)
  expr_ * \[Delta][\[ScriptCapitalU][b_], \[ScriptCapitalD][a_]] /; !FreeQ[expr, \[ScriptCapitalD][b]] :> 
    (expr /. \[ScriptCapitalD][b] -> \[ScriptCapitalD][a]),
  \[Delta][\[ScriptCapitalU][b_], \[ScriptCapitalD][a_]] * expr_ /; !FreeQ[expr, \[ScriptCapitalD][b]] :> 
    (expr /. \[ScriptCapitalD][b] -> \[ScriptCapitalD][a]),

  (* Rule: Subscript[T, \[Alpha] * ]Subscript[\[Delta]^\[Alpha], \[Beta]] -> Subscript[T, \[Beta]] *)
  expr_ * \[Delta][\[ScriptCapitalU][a_], \[ScriptCapitalD][b_]] /; !FreeQ[expr, \[ScriptCapitalU][b]] :> 
    (expr /. \[ScriptCapitalU][b] -> \[ScriptCapitalU][a]),
  \[Delta][\[ScriptCapitalU][a_], \[ScriptCapitalD][b_]] * expr_ /; !FreeQ[expr, \[ScriptCapitalU][b]] :> 
    (expr /. \[ScriptCapitalU][b] -> \[ScriptCapitalU][a]),

  (* Cleanup: Delta on itself *)
  \[Delta][\[ScriptCapitalU][a_], \[ScriptCapitalD][b_]] * \[Delta][\[ScriptCapitalU][b_], \[ScriptCapitalD][c_]] :> \[Delta][\[ScriptCapitalU][a], \[ScriptCapitalD][c]]

  };
  
(* Torsion-Free Geometry (General Relativity) *)
(* User must apply this manually: expr //. torsionRules *)
torsionRules = {
  \[CapitalGamma][u_, \[ScriptCapitalD][a_], \[ScriptCapitalD][b_]] :> 
   \[CapitalGamma][u, \[ScriptCapitalD][Sort[{a, b}][[1]]], \[ScriptCapitalD][Sort[{a, b}][[2]]]]
};

(* =========================================================================*)
(* 6. DIFFERENTIATION ENGINE *)
(* =========================================================================*)

differentiationRules = {
  Partials[a_*b_, var_] :> Partials[a, var]*b + a*Partials[b, var], 
  Partials[a_ + b_, var_] :> Partials[a, var] + Partials[b, var], 
  Partials[expr_, x[\[ScriptCapitalU][idx_]]] /; StringContainsQ[ToString[idx], "'"] :> 
    Module[{fresh = Unique["\[Sigma]"]}, 
     Partials[x[\[ScriptCapitalU][fresh]], x[\[ScriptCapitalU][idx]]]*
      Partials[expr, x[\[ScriptCapitalU][fresh]]]]};

ExpandDerivatives[expr_] := expr //. differentiationRules;

(* Schwarz's Theorem (Sort Partial Derivatives) *)
(* If variables are out of canonical order, swap them *)
Partials[Partials[f_, x[\[ScriptCapitalU][a_]]], x[\[ScriptCapitalU][b_]]] /; 
  !OrderedQ[{a, b}] := 
  Partials[Partials[f, x[\[ScriptCapitalU][b]]], x[\[ScriptCapitalU][a]]];

(* ========================================================================= *)
(* 7. COVARIANT DERIVATIVE                                                   *)
(* ========================================================================= *)

(* ========================================================================= *)
(* 7. COVARIANT DERIVATIVE (The Master Rule)                                 *)
(* ========================================================================= *)

(* CONFIGURATION: The Connection Symbol *)
(* Default is \[CapitalGamma], but user can change it to A, C, etc., for Electrodynamics, Yang-Mills, whatever *)
RelativityConnection = \[CapitalGamma];

SetConnection[sym_Symbol] := (
   RelativityConnection = sym;
   Print["Relativity Engine: Connection set to ", sym]
);

(* Rule 0: Connection-Binding (The "Gamma Glue") *)
(* DYNAMIC: Checks if prod contains the CURRENT RelativityConnection symbol *)
(* This treats 'Gamma * A' as a single atom to enforce correct scope *)
CD[prod_Times /; !FreeQ[prod, RelativityConnection], var_] := 
  Module[{up, down, partialTerm, upCorrections, downCorrections},
   
   {up, down} = valence[prod];
   partialTerm = Partials[prod, var];
   
   (* Apply Corrections using the CURRENT Connection Symbol *)
   upCorrections = Sum[
     Module[{lam = Unique["\[Lambda]"]},
      RelativityConnection[\[ScriptCapitalU][idx], \[ScriptCapitalD][var[[1,1]]], \[ScriptCapitalD][lam]] * (prod /. idx -> lam)
     ], {idx, up}];
     
   downCorrections = Sum[
     Module[{lam = Unique["\[Lambda]"]},
      RelativityConnection[\[ScriptCapitalU][lam], \[ScriptCapitalD][idx], \[ScriptCapitalD][var[[1,1]]]] * (prod /. idx -> lam)
     ], {idx, down}];
     
   partialTerm + upCorrections - downCorrections
  ];

(* Rule 1: Linearity *)
CD[a_ + b_, var_] := CD[a, var] + CD[b, var];

(* Rule 2: Product Rule *)
CD[a_ * b_, var_] := CD[a, var] * b + a * CD[b, var];

(* Rule 3: The General Tensor Rule (Master Rule) *)
CD[expr_, x[\[ScriptCapitalU][nu_]]] := 
 Module[{up, down, partialTerm, upCorrections, downCorrections},
  
  {up, down} = valence[expr];
  partialTerm = Partials[expr, x[\[ScriptCapitalU][nu]]];
  
  (* DYNAMIC: Generate corrections using RelativityConnection *)
  upCorrections = Sum[
    Module[{lam = Unique["\[Lambda]"]},
     RelativityConnection[\[ScriptCapitalU][idx], \[ScriptCapitalD][nu], \[ScriptCapitalD][lam]] * (expr /. idx -> lam)
    ], {idx, up}];
    
  downCorrections = Sum[
    Module[{lam = Unique["\[Lambda]"]},
     RelativityConnection[\[ScriptCapitalU][lam], \[ScriptCapitalD][idx], \[ScriptCapitalD][nu]] * (expr /. idx -> lam)
    ], {idx, down}];
    
  partialTerm + upCorrections - downCorrections
 ];

