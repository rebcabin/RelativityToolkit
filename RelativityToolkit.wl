(* ::Package:: *)

(* ========================================================================= *)
(* RELATIVITY TOOLKIT ENGINE (Script Mode)                                   *)
(* Version: 1.9.0 (Gauge Theory)                                             *)
(* ------------------------------------------------------------------------- *)
(* TODO: complete the removal of hard-coded \[CapitalGamma]. Currently, only torsionRules, *)
(* CD, and SetConnection know about RelativityConnection. There are several  *)
(* places in MakeBoxes and other display functions that hard-code \[CapitalGamma].         *)
(* ========================================================================= *)

Echo[RelativityToolkitVersion = "1.9.0", "Relativity Toolkit version :"];

(* CONFIGURATION: The Connection Symbol *)
(* Default is \[CapitalGamma]; change it to A, C, etc., for Electrodynamics, Yang-Mills, etc. *)

Echo[RelativityConnection = \[CapitalGamma], "Relativity Connection Symbol :"];

(* ========================================================================= *)
(* 1. CLEAN SLATE ---------------------------------------------------------- *)
(* ========================================================================= *)

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

Module[{symbols = Symbol /@ {
	"CalculateRicciComponent",
	"CalculateRiemannComponent",
	"CanonicalizeIndices",
	"CanonicalizeTerm",
	"CD",
	"ChristoffelsFromMetric",
	"Contract",
	"ContractAll",
	"contractValence",
	"differentiationRules",
	"downQ",
	"EvaluateUDPartials",
	"ExpandDerivatives",
	"ExtractCoefficient",
	"g", (* reserved for metric tensors (will be relaxed later ) *)
	"ruleLeviCivita",
	"MakeIndexer",
	"MatrixToUDRules",
	"metricDifferentiationRules",
	"metricRules",
	"noValence",
	"Partials",
	"RelativityConnection",
	"RelativityToolkitVersion",
	"robustTransformRules",
	"SetConnection",
	"TensorForm",
	"torsionRules",
	"upQ",
	"valence",
	"x", (* all coordinate functions are named x *)
	"\[ScriptCapitalD]", (* essential syntax *)
	"\[ScriptCapitalU]", (* essential syntax *)
	"\[CapitalGamma]", (* reserved for GR *)
	"\[Delta]"  (* reserved for the Kronecker symbol *)
	}},
ClearAll[Sequence[symbols]];
ClearAll[
  \[Lambda], \[Kappa], \[Rho], \[Sigma], \[Mu], \[Nu], \[Tau], \[Eta], \[Chi], \[Psi],
  x, p, g, u, A, B, T, 
  P, Q, S]];

(* ========================================================================= *)
(* 2. VALENCE LOGIC (The Type Checker)                                       *)
(* ========================================================================= *)

upQ[x_] := upQ[valence[x]];
downQ[x_] := downQ[valence[x]];
upQ[{{__}, {}}] := True; upQ[___] := False;
downQ[{{}, {__}}] := True; downQ[___] := False;

contractValence[{u_List, d_List}] := 
  Module[{common}, common = Intersection[u, d];
   {DeleteElements[u, common], DeleteElements[d, common]}];

noValence := {{}, {}}; (* := (set delayed) means fresh instance each time *)

valence[x_Symbol] := noValence;
valence[_?NumericQ] := noValence;
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

(* TODO: Opportunity for replacement by RelativityConnection *)
valence[\[CapitalGamma][\[ScriptCapitalU][a_], \[ScriptCapitalD][b_], \[ScriptCapitalD][c_]]] := {{a}, {b, c}};

valence[Derivative[1][f_][arg_]] := 
  Module[{u, d}, {u, d} = valence[arg]; {d, u}];

valence[Derivative[_][_][f_] /; (valence[f] =!= noValence)] := 
  valence[f];

valence[(h : _[\[ScriptCapitalU][_]] | _[\[ScriptCapitalD][_]])[___]] := valence[h];

valence[prod_Times] := 
  contractValence[
   Fold[Join[#1, valence[#2], 2] &, noValence, List @@ prod]];

valence[sum_Plus] := 
  Module[{vs}, vs = valence /@ (List @@ sum); 
   If[Apply[SameQ, vs], 
   First[vs], 
   Print["Valence Mismatch"]; noValence]];
   
valence[Power[b_, _]] := valence[b];

valence[h_[a_, b_, rest___]] := 
  Join[valence[h[a]], valence[h[b, rest]], 2];
  
valence[___] := noValence;

(* ========================================================================= *)
(* 3. DISPLAY RULES                                                          *)
(* ========================================================================= *)

Unprotect[MakeBoxes];

(* Semicolon Notation for CD *)
MakeBoxes[CD[num_, den_], StandardForm] := 
  If[MatchQ[den, x[\[ScriptCapitalU][_]]], 
   Replace[den, 
    x[\[ScriptCapitalU][idx_]] :> 
     SubscriptBox[MakeBoxes[num, StandardForm], 
      RowBox[{";", MakeBoxes[idx, StandardForm]}]]], 
   RowBox[{"CD", "[", MakeBoxes[num, StandardForm], ",", 
     MakeBoxes[den, StandardForm], "]"}]];

(* PARTIAL DERIVATIVES (The Comma Operator) *)

MakeBoxes[Partials[num_, den_], StandardForm] := 
  If[MatchQ[num, Partials[_, _]],
   
   (* CASE 1: Nested Derivative -> Fraction *)
   Replace[num, 
    Partials[top_, bot1_] :> 
     FractionBox[
      RowBox[{SuperscriptBox["\[PartialD]", "2"], MakeBoxes[top, StandardForm]}], 
      RowBox[{RowBox[{"\[PartialD]", MakeBoxes[den, StandardForm]}], 
        RowBox[{"\[PartialD]", MakeBoxes[bot1, StandardForm]}]}]]],
   
   (* CASE 2 & 3 *)
   If[MatchQ[den, x[\[ScriptCapitalU][_]]] && ! MatchQ[num, x[\[ScriptCapitalU][_]]],
    
    (* CASE 2: Comma Notation *)
    Replace[den, 
     x[\[ScriptCapitalU][idx_]] :> 
      SubscriptBox[
       (* Wrap in parentheses if Product/Sum/Power *)
       If[MemberQ[{Times, Plus, Power}, Head[num]], 
        RowBox[{"(", MakeBoxes[num, StandardForm], ")"}], 
        MakeBoxes[num, StandardForm]], 
       
       (* Comma and Index *)
       RowBox[{",", MakeBoxes[idx, StandardForm]}]]],
    
    (* CASE 3: Default -> Fraction *)
    FractionBox[
     RowBox[{"\[PartialD]", MakeBoxes[num, StandardForm]}], 
     RowBox[{"\[PartialD]", MakeBoxes[den, StandardForm]}]] ] ];

(* TODO: Potential replacement by RelativityConnection *)
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
(h =!= \[CapitalGamma]) && (* TODO: Potential replacement by RelativityConnection *)
MatchQ[{indices}, {(_[\[ScriptCapitalU]] | _[\[ScriptCapitalD]] | \[ScriptCapitalU][_] | \[ScriptCapitalD][_]) ..}] := 
 Module[{formattedScripts}, 
  formattedScripts = {indices} /. {\[ScriptCapitalU][i_] :> 
      SuperscriptBox["", 
       MakeBoxes[i, StandardForm]], \[ScriptCapitalD][i_] :> 
      SubscriptBox["", MakeBoxes[i, StandardForm]]};
  RowBox[Prepend[formattedScripts, MakeBoxes[h, StandardForm]]]]

Protect[MakeBoxes];

(* ========================================================================= *)
(* 4. ALGEBRAIC SIMPLIFICATION                                               *)
(* ========================================================================= *)

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
  Module[{canonExpr, formalPattern,formalIndices, 
          usedSymbols, greekPool, availableGreek, indexMap},
   
   (* 1. Canonicalize first *)
   canonExpr = CanonicalizeIndices[expr];

   (* 2. Define Formal Pattern: Any symbol starting with \[FormalA]...\[FormalZ] *)
   (* This covers \[FormalS], \[FormalI]1, \[FormalI]2, etc. *)
   formalPattern = CharacterRange["\[FormalA]", "\[FormalZ]"];

   (* 3. Identify ALL Formal Indices *)
   formalIndices = Sort @ DeleteDuplicates @ Cases[canonExpr, 
     s_Symbol /; StringStartsQ[SymbolName[s], formalPattern], 
     Infinity];

   (* 4a. Identify symbols ALREADY in the expression *)
   usedSymbols = DeleteDuplicates @ Cases[canonExpr, _Symbol, Infinity];
   
   (* 4b. Define our preferred Greek pool *)
   greekPool = {\[Lambda], \[Kappa], \[Rho], \[Sigma], \[Mu], \[Nu], \[Tau], \[Eta], \[Chi], \[Psi], \[Alpha], \[Beta], \[Gamma]};

   (* 5. Filter the pool: Remove symbols that are already used *)
   availableGreek = DeleteElements[greekPool, usedSymbols];

   (* 6. Map the ordered, found formals to the available Greek letters *)
   indexMap = Thread[formalIndices -> Take[availableGreek, UpTo[Length[formalIndices]]]];

   (* 7. Apply *)
   HoldForm[Evaluate[canonExpr /. indexMap]] ];
 
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

(* ========================================================================= *)
(* 5. APPLICATION-SPECIFIC RULES                                             *)
(*    User must explicitly apply these when wanted                           *)
(* ========================================================================= *)

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
  
  (* \[Delta]-Contraction Rules (For Associativity) *)

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
  RelativityConnection[u_, a_, b_] :> 
   RelativityConnection[u, Sort[{a, b}][[1]], Sort[{a, b}][[2]]]
};

ruleLeviCivita=\[CapitalGamma][\[ScriptCapitalU][\[Lambda]_],\[ScriptCapitalD][a_],\[ScriptCapitalD][b_]]:>
  Module[{\[Sigma]=Unique["\[Sigma]"]},
    1/2 g[\[ScriptCapitalU][\[Lambda]],\[ScriptCapitalU][\[Sigma]]]*
     (Partials[g[\[ScriptCapitalD][\[Sigma]],\[ScriptCapitalD][b]],x[\[ScriptCapitalU][a]]]+
      Partials[g[\[ScriptCapitalD][\[Sigma]],\[ScriptCapitalD][a]],x[\[ScriptCapitalU][b]]]-
      Partials[g[\[ScriptCapitalD][a],\[ScriptCapitalD][b]],x[\[ScriptCapitalU][\[Sigma]]]])];

metricDifferentiationRules={
  (*Metric Symmetry*)
  g[\[ScriptCapitalD][a_],\[ScriptCapitalD][b_]]/;!OrderedQ[{a,b}]:>g[\[ScriptCapitalD][b],\[ScriptCapitalD][a]],
  g[\[ScriptCapitalU][a_],\[ScriptCapitalU][b_]]/;!OrderedQ[{a,b}]:>g[\[ScriptCapitalU][b],\[ScriptCapitalU][a]],
  (*Permissive Contraction (All 4 Alignments)*)
  (*First-First*)
  g[\[ScriptCapitalD][s_],\[ScriptCapitalD][a_]]*g[\[ScriptCapitalU][s_],\[ScriptCapitalU][b_]]:>\[Delta][\[ScriptCapitalU][b],\[ScriptCapitalD][a]],
  (*Second-Second*)
  g[\[ScriptCapitalD][a_],\[ScriptCapitalD][s_]]*g[\[ScriptCapitalU][b_],\[ScriptCapitalU][s_]]:>\[Delta][\[ScriptCapitalU][b],\[ScriptCapitalD][a]],
  (*First-Second (Redundant but Safe)*)
  g[\[ScriptCapitalD][s_],\[ScriptCapitalD][a_]]*g[\[ScriptCapitalU][b_],\[ScriptCapitalU][s_]]:>\[Delta][\[ScriptCapitalU][b],\[ScriptCapitalD][a]],
  (*Second-First*)
  g[\[ScriptCapitalD][a_],\[ScriptCapitalD][s_]]*g[\[ScriptCapitalU][s_],\[ScriptCapitalU][b_]]:>\[Delta][\[ScriptCapitalU][b],\[ScriptCapitalD][a]],
  (*Delta Contractions*)
  \[Delta][\[ScriptCapitalU][bound_],\[ScriptCapitalD][free_]]*Partials[expr_,x[\[ScriptCapitalU][bound_]]]:>Partials[expr,x[\[ScriptCapitalU][free]]],
  \[Delta][\[ScriptCapitalU][bound_],\[ScriptCapitalD][free_]]*Partials[g[\[ScriptCapitalD][bound_],\[ScriptCapitalD][other_]],v_]:>Partials[g[\[ScriptCapitalD][free],\[ScriptCapitalD][other]],v],
  \[Delta][\[ScriptCapitalU][bound_],\[ScriptCapitalD][free_]]*Partials[g[\[ScriptCapitalD][other_],\[ScriptCapitalD][bound_]],v_]:>Partials[g[\[ScriptCapitalD][other],\[ScriptCapitalD][free]],v]};

(* ========================================================================= *)
(* 6. DIFFERENTIATION ENGINE                                                 *)
(* ========================================================================= *)

differentiationRules = {
  Partials[a_*b_, var_] :> Partials[a, var]*b + a*Partials[b, var], 
  Partials[a_ + b_, var_] :> Partials[a, var] + Partials[b, var], 
  Partials[Exp[arg_], var_] :> Exp[arg] * Partials[arg, var],
  Partials[n_, _] /; (NumericQ[n]) :> 0,
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
     
   partialTerm + upCorrections - downCorrections ];

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
    
  partialTerm + upCorrections - downCorrections ];

(* Rule 4: Mixed Geometric & Gauge Derivative *)
(* Usage: CD[field, coord, coupling, gaugeField] *)
(* Returns: GeometricCD - i * coupling * gaugeField_mu * field *)
CD[field_, var_, coupling_, gaugeBoson_Symbol] :=
  CD[field, var] - I * coupling * gaugeBoson[\[ScriptCapitalD][var[[1,1]]]] * field;
  
      
(* ========================================================================= *)
(* 8. RIEMANN CURVATURE TENSOR FROM METRIC                                   *)
(* ========================================================================= *)

(* Christoffel (\[CapitalGamma]) *)

(* Reminder: this is not a general connection, but one for torsion-free,   *)
(* symmetrics metrics, such as for relativistic gravitation.               *)
(* Send in metric and its inverse so as not to recompute inverse.          *)
(* The symbol for covariant gDD must not equal that for contravariant gUU. *)
(* \!\(
\*SubsuperscriptBox[\(\[CapitalGamma]\), \(\[Mu] \[Nu]\), \(\[Sigma]\)] \[Equal] \(
\*FractionBox[\(1\), \(2\)]\ \((g
\*SuperscriptBox[\(\), \(\[Sigma]\)]
\*SuperscriptBox[\(\), \(\[Lambda]\)])\)\ \((
\*SubscriptBox[\(g
\*SubscriptBox[\(\), \(\[Lambda]\)]
\*SubscriptBox[\(\), \(\[Mu]\)]\), \(, \(\[Nu]\)\)]\  + \ 
\*SubscriptBox[
SubscriptBox[
SubscriptBox[\(g\), \(\[Lambda]\)], \(\[Nu]\)], \(, \(\[Mu]\)\)]\  - \ 
\*SubscriptBox[
SubscriptBox[\(g\), \(\[VeryThinSpace]\(\[Mu]\[VeryThinSpace]\[Nu]\)\)], \(, \(\[Lambda]\)\)])\)\)\) *)
ChristoffelsFromMetric[gDD_, gUU_, \[Sigma]_, \[Mu]_, \[Nu]_]/;(gDD =!= gUU) := 
  Module[{\[Lambda] = Unique["\[Lambda]"]},
    (1/2) gUU[\[ScriptCapitalU][\[Sigma]],\[ScriptCapitalU][\[Lambda]]] 
      (Partials[gDD[\[ScriptCapitalD][\[Lambda]], \[ScriptCapitalD][\[Mu]]], x[\[ScriptCapitalU][\[Nu]]]] +
       Partials[gDD[\[ScriptCapitalD][\[Lambda]], \[ScriptCapitalD][\[Nu]]], x[\[ScriptCapitalU][\[Mu]]]] -
       Partials[gDD[\[ScriptCapitalD][\[Mu]], \[ScriptCapitalD][\[Nu]]], x[\[ScriptCapitalU][\[Lambda]]]])];
(* Riemann *)

(* Send in \[CapitalGamma]Get so as not to recompute it on every call. *)
(* e.g., \[CapitalGamma]Get=MakeIndexer[Schw\[CapitalGamma], {t, r, \[Theta], \[Phi]}] *)
(* Subscript[R^\[Mu], \[Lambda] \[Nu] \[Rho]] = Subscript[Subsuperscript[\[CapitalGamma], \[Lambda]\[Nu], \[Mu]], ,\[Rho]] - Subscript[Subsuperscript[\[CapitalGamma], \[Lambda]\[Rho], \[Mu]], ,\[Nu]] + \!\(
\*SubsuperscriptBox[\(\[CapitalGamma]\), \(\[Lambda] \[Nu]\), \(\[Kappa]\)]\ 
\*SubsuperscriptBox[\(\[CapitalGamma]\), \(\[Kappa] \[Rho]\), \(\[Mu]\)]\) - \!\(
\*SubsuperscriptBox[\(\[CapitalGamma]\), \(\[Lambda] \[Rho]\), \(\[Kappa]\)]\(\ \)
\*SubsuperscriptBox[\(\[CapitalGamma]\), \(\[Kappa] \[Nu]\), \(\[Mu]\)]\(\ \)\)*)
CalculateRiemannComponent[\[Mu]Up_, \[Lambda]Dn_, \[Nu]Dn_, \[Rho]Dn_, \[CapitalGamma]get_, coords_List] := 
  Module[{d\[CapitalGamma]1, d\[CapitalGamma]2, interact1, interact2},
  (* Derivative Terms: Subscript[\[CapitalGamma]^\[Mu], \[Lambda] \[Nu],\[Rho]]-Subscript[\[CapitalGamma]^\[Mu], \[Lambda] \[Rho],\[Nu]] *)
  d\[CapitalGamma]1 = D[\[CapitalGamma]get[\[Mu]Up, \[Lambda]Dn, \[Nu]Dn], \[Rho]Dn];
  d\[CapitalGamma]2 = D[\[CapitalGamma]get[\[Mu]Up, \[Lambda]Dn, \[Rho]Dn], \[Nu]Dn];
  
  (* Interaction Terms: \!\(
\*SubsuperscriptBox[\(\[CapitalGamma]\), \(\[Lambda] \[Nu]\), \(\[Kappa]\)]\ 
\*SubsuperscriptBox[\(\[CapitalGamma]\), \(\[Kappa] \[Rho]\), \(\[Mu]\)]\) - \!\(
\*SubsuperscriptBox[\(\[CapitalGamma]\), \(\[Lambda] \[Rho]\), \(\[Kappa]\)]\ 
\*SubsuperscriptBox[\(\[CapitalGamma]\), \(\[Kappa] \[Nu]\), \(\[Mu]\)]\) *)
  Module[{\[Kappa]},
    interact1 = Sum[\[CapitalGamma]get[\[Kappa], \[Lambda]Dn, \[Nu]Dn] * \[CapitalGamma]get[\[Mu]Up, \[Kappa], \[Rho]Dn], {\[Kappa], coords}];
    interact2 = Sum[\[CapitalGamma]get[\[Kappa], \[Lambda]Dn, \[Rho]Dn] * \[CapitalGamma]get[\[Mu]Up, \[Kappa], \[Nu]Dn], {\[Kappa], coords}]];
  
  Simplify[d\[CapitalGamma]1 - d\[CapitalGamma]2 + interact1 - interact2]
];

(* Ricci *)

(* Send in RGet so as not to recompute it each time *)
(* e.g., RGet = MakeIndexer[SchwRiemann, {t, r, \[Theta], \[Phi]}]; *)
(* Subscript[R,  \[Lambda] \[Rho]] = Subscript[R^ \[Kappa],  \[Lambda] \[Kappa] \[Rho] ]*)
CalculateRicciComponent[\[Lambda]_, \[Rho]_, RGet_, coords_]:=
  Module[{\[Kappa]},
    Sum[RGet[\[Mu], \[Lambda], \[Mu], \[Rho]], {\[Mu], coords}] // Simplify];
                                          
(* ========================================================================= *)
(* 9. COMPILER AND SUPPORT FUNCTIONS                                         *)
(* ========================================================================= *)

(* MatrixToUDRules *)
(* Use Case: Convert a Wolfram Matrix to 2-D UD Rules *)
(* Input: A square, 2D matrix 'mat' of components,
          a Symbol 'g' to affect, e.g. gDD or gUU,
          a valence indicator, \[ScriptCapitalU] or \[ScriptCapitalD]. 
          a List of coordinate symbols, e.g., {t, r, \[Theta], \[Phi]} *)
(* Notes: Works only for 'homovalent' symbols like gDD or gUU.
   Heterovalent symbols like gUD or gDU are not handled here. *)
(* Output: A list of rules like {gDD[\[ScriptCapitalD][0], \[ScriptCapitalD][0]] -> A[r], ...} *)

MatrixToUDRules[mat_, g_Symbol, UorD_ /; (UorD === \[ScriptCapitalU] || UorD === \[ScriptCapitalD]), coords_List] := 
  Module[{dim, rules},
    dim = Length[mat];
    rules = Flatten @ Table[
      g[UorD[coords[[i]]], UorD[coords[[j]]]] -> mat[[i, j]],
      {i, 1, dim}, {j, 1, dim}];
    (* Filter out zeros to keep the rule list short *)
    Select[rules, (#[[2]] =!= 0) &]];

(* Contraction *)

(* In imitation of Wolfram's Replace and ReplaceAll, we present    *)
(* contraction functions that Total a list of terms with bound     *)
(* indices (repeated up-down indices in a term) where the bound    *)
(* indices are replaced by all the coords, in order.               *)
(* Contract performs contraction on the first detected bound       *)
(* index and ContractAll performs contraction on all bound indices *)

(* Contract on the first bound index (repeated up-down in a term) *)
Contract[expr_, coords_List] :=
  Module[{canonicalExpr, oneShotScanner, result, PD},

    (* Convert "Partial w.r.t Upper" (x^i) to "lower inert operator" (PD_i) *)
    (* Explicitly creates the D[i] index needed for scanning. *)
    canonicalExpr = expr /. Partials[f_, x[\[ScriptCapitalU][i_]]] :> PD[f, \[ScriptCapitalD][i]];

    (* Find a bound index pair that is not a coordinate symbol. *)
    oneShotScanner[term_] := Module[{ups, downs, pairs, idx},
      ups = Cases[term, \[ScriptCapitalU][s_Symbol] :> s, Infinity];
      downs = Cases[term, \[ScriptCapitalD][s_Symbol] :> s, Infinity];
    
      (* Exclude coordinate names from contraction *)
      pairs = Complement[Intersection[ups, downs], coords];
    
      If[Length[pairs] === 0,
       term,
       (idx = First[pairs];
       (* Total up on the expansion over all coords *)
       Total[term /. idx -> # & /@ coords])]];
       
      (* Apply the scanner to all terms with the same bound index *)
      With[{ec = Expand[canonicalExpr]},
        result = If[Head[ec] === Plus,
          Map[oneShotScanner, ec],
          oneShotScanner[ec]]];
          
      (* Restore Partials from PD for downstream UD evaluation. *)
      result /. PD[f_, \[ScriptCapitalD][i_]] :> Partials[f, x[\[ScriptCapitalU][i]]]];
      
      
(* ContractAll: Contract on all bound indices (repeated up-down in a term) *)
ContractAll[expr_, coords_List] := Module[{
    canonicalExpr, recursiveScanner, result, PD},
  
  (* PHASE A: Canonicalize Valence *)
  (* Convert "Partial w.r.t Upper" (x^i) to "lower inert operator" (PD_i) *)
  (* Explicitly creates the D[i] index needed for scanning. *)
  canonicalExpr = expr /. Partials[f_, x[\[ScriptCapitalU][i_]]] :> PD[f, \[ScriptCapitalD][i]];
  
  (* PHASE B: The Recursive Scanner *)
  recursiveScanner[term_] := Module[{ups, downs, pairs, idx},
    ups = Cases[term, \[ScriptCapitalU][s_Symbol] :> s, Infinity];
    downs = Cases[term, \[ScriptCapitalD][s_Symbol] :> s, Infinity];
    
    (* Exclude coordinate names from contraction *)
    pairs = Complement[Intersection[ups, downs], coords];
    
    If[Length[pairs] === 0,
     term,
     (idx = First[pairs];
      (* Recurse on the expansion *)
      recursiveScanner[Total[term /. idx -> # & /@ coords]])]];
  
  (* Run Scanner on Expanded Expression *)
  With[{ec = Expand[canonicalExpr]},
    result = If[Head[ec] === Plus, 
       Map[recursiveScanner, ec], 
       recursiveScanner[ec]]];
  
  (* PHASE C: Restoration *)
  (* Convert internal PD back to Partials for UD. *)
  result /. PD[f_, \[ScriptCapitalD][i_]] :> Partials[f, x[\[ScriptCapitalU][i]]]];
  
  
(* Compile UD partials into Wolfram D expressions. *)
  
EvaluateUDPartials[expr_] := expr /. 
  With[{killBogusChainRule = {(\[ScriptCapitalU]|\[ScriptCapitalD])'[idx_] -> 0}},
    { 
      (* CASE: Standard UD Syntax *)
      Partials[f_, x[\[ScriptCapitalU][idx_]]] :> (D[f, idx] /. killBogusChainRule),
      
      (* CASE: Pretty-Printed Subscript Notation (The comma) *)
      Subscript[f_, {idx_}] :> (D[f, idx] /. killBogusChainRule),
      
      (* CASE: Derivatives of Numerical Partials *)
      Partials[n_, x[(\[ScriptCapitalU]|\[ScriptCapitalD])[_]]] /; NumberQ[n] :> 0,
      Partials[(E | I | \[Pi]), x[(\[ScriptCapitalU]|\[ScriptCapitalD])[_]]] :> 0
   }];

(* Use Case: Make a function that can retrieve components from a Wolfram array       *)
(*           that represents a tensor-like object using symbolic coordinate symbols. *)
(* Usage (example): \[CapitalGamma]get = MakeIndexer[\[CapitalGamma]table, {t, r, \[Theta], \[Phi]}];                        *)
(*                  \[CapitalGamma]get[r, t, t] ~~> A'[r]/(2 B[r]])                                *)
(* Input: `table` is a 1-indexed Wolfram array of any dimension containing           *)
(*                components of a tensor-like object such as \[CapitalGamma], g, or R.             *)
(*        `coords` is a list of coordinate symbols, such as {t, r, \[Theta], \[Phi]}.            *)
(*        The Length of coords must match the dimension of the array (unchecked).    *)
(* Returns: a function that looks up components in the array by coord symbols.       *)
MakeIndexer[table_List, coords_List] :=
  Module[{dispatcher = Dispatch[MapIndexed[(#1 -> #2[[1]])&, coords]]},
    (* double-delayed; single-delay evaluates `Lookup` too early *)
    table[[Sequence @@ (Lookup[dispatcher, {##}]&)[Sequence @@ {##}]]]&];
  




