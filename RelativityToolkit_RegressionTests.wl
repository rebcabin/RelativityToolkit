(* ========================================================================= *)
(* RELATIVITY TOOLKIT: REGRESSION SUITE                                      *)
(* Version: 1.3.0 (Script Mode)                                              *)
(* ========================================================================= *)

Print["\n================================================================"];
Print["RUNNING REGRESSION SUITE v1.3.0"];
Print["================================================================\n"];

(* --- TEST HELPER FUNCTIONS ----------------------------------------------- *)
PassCount = 0;
FailCount = 0;

AssertValence[expr_, expected_, label_] :=
  Module[{v = Echo@valence[expr]},
   If[v === expected,
    PassCount++;
    Print["[PASS] ", label, " -> ", v],
    FailCount++;
    Print["[FAIL] ", label, " \n\tExpected: ", expected, 
     "\n\tGot:      ", v]]];

AssertEqual[expr1_, expr2_, label_] :=
  Module[{c1 = Echo@CanonicalizeIndices[expr1], c2 = CanonicalizeIndices[expr2]},
   If[c1 === c2,
    PassCount++;
    Print["[PASS] ", label],
    FailCount++;
    Print["[FAIL] ", label, " \n\tLHS: ", c1, "\n\tRHS: ", c2]]];

(* ========================================================================= *)
Print["--- SECTION 1: TYPE CHECKING (VALENCE) ---"];
(* ========================================================================= *)

(* 1. Atoms *)
AssertValence[x[\[ScriptCapitalU][\[Mu]]], {{\[Mu]}, {}}, "Vector x^u"];
AssertValence[p[\[ScriptCapitalD][\[Nu]]], {{}, {\[Nu]}}, "Covector p_v"];
AssertValence[T[\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Nu]]], {{\[Mu]}, {\[Nu]}}, "Mixed Tensor T^u_v"];
AssertValence[T[\[ScriptCapitalD][\[Mu]], \[ScriptCapitalD][\[Nu]]], {{}, {\[Mu], \[Nu]}}, "Mixed Tensor T_uv"];
AssertValence[T[\[ScriptCapitalU][\[Mu]], \[ScriptCapitalU][\[Nu]]], {{\[Mu], \[Nu]}, {}}, "Mixed Tensor T^uv"];
AssertValence[T[\[ScriptCapitalD][\[Mu]], \[ScriptCapitalU][\[Nu]]], {{\[Nu]}, {\[Mu]}}, "Mixed Tensor T_u^v"];

(* 2. Contractions *)
AssertValence[x[\[ScriptCapitalU][\[Mu]]]*p[\[ScriptCapitalD][\[Mu]]], {{}, {}}, "Scalar Contraction x^u p_u"];
AssertValence[p[\[ScriptCapitalD][\[Mu]]]*x[\[ScriptCapitalU][\[Mu]]], {{}, {}}, "Scalar Contraction p_u x^u"];
AssertValence[T[\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Nu]]] * x[\[ScriptCapitalU][\[Nu]]], {{\[Mu]}, {}}, "Tensor-Vector T^u_v x^v -> x^u"];
AssertValence[T[\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Nu]]] * x[\[ScriptCapitalD][\[Mu]]], {{}, {\[Nu]}}, "Tensor-Vector T^u_v x_u -> x_n"];

(* 3. Arithmetic *)
AssertValence[(x[\[ScriptCapitalU][\[Mu]]]*p[\[ScriptCapitalD][\[Mu]]])^2, {{}, {}}, "Power of Scalar"];
AssertValence[5*x[\[ScriptCapitalU][\[Mu]]], {{\[Mu]}, {}}, "Scalar Multiplication"];

(* 4. Differentiation (The Gradient Test) *)
AssertValence[Derivative[1][f][x[\[ScriptCapitalU][\[Alpha]]]], {{}, {\[Alpha]}}, "Gradient d(f)/dx^a -> Down[a]"];

(* 5. Error Handling *)
Quiet[Module[{badSum = x[\[ScriptCapitalU][\[Mu]]] + p[\[ScriptCapitalD][\[Mu]]]}, 
   If[valence[badSum] === {{}, {}}, PassCount++;
    Print["[PASS] Mismatch Detection (Handled Gracefully)"], 
    FailCount++; Print["[FAIL] Mismatch Detection failed"]]]];

(* ========================================================================= *)
Print["\n--- SECTION 2: ALGEBRA (CANONICALIZATION) ---"];
(* ========================================================================= *)

(* 1. Simple Index Renaming *)
AssertEqual[
  x[\[ScriptCapitalU][\[Mu]]]*p[\[ScriptCapitalD][\[Mu]]],
  x[\[ScriptCapitalU][\[Nu]]]*p[\[ScriptCapitalD][\[Nu]]],
  "Dummy Renaming (A^u B_u == A^v B_v)"];

(* 2. Commutativity *)
AssertEqual[
  x[\[ScriptCapitalU][\[Mu]]]*p[\[ScriptCapitalD][\[Mu]]],
  p[\[ScriptCapitalD][\[Nu]]]*x[\[ScriptCapitalU][\[Nu]]],
  "Commutativity (A^u B_u == B_v A^v)"];

(* 3. Distributivity/Sums *)
AssertEqual[
  x[\[ScriptCapitalU][\[Mu]]]*p[\[ScriptCapitalD][\[Mu]]] + 
   x[\[ScriptCapitalU][\[Nu]]]*p[\[ScriptCapitalD][\[Nu]]],
  2*x[\[ScriptCapitalU][\[Rho]]]*p[\[ScriptCapitalD][\[Rho]]],
  "Index Coalescing (Summing identical terms)"];

(* 4. Multi-Index Tensors *)
AssertEqual[
  P[\[ScriptCapitalU][\[Mu]]]*Q[\[ScriptCapitalD][\[Mu]]]*   S[\[ScriptCapitalU][\[Nu]]]*T[\[ScriptCapitalD][\[Nu]]],
  S[\[ScriptCapitalU][\[Alpha]]]*T[\[ScriptCapitalD][\[Alpha]]]*   P[\[ScriptCapitalU][\[Beta]]]*Q[\[ScriptCapitalD][\[Beta]]],
  "Double Dummy Pairs (P^u Q_u S^v T_v)"];

(* 5. Negative Test *)
If[CanonicalizeIndices[
    A[\[ScriptCapitalU][\[Alpha]]]*B[\[ScriptCapitalU][\[Beta]]]] =!= 
   CanonicalizeIndices[
    A[\[ScriptCapitalU][\[Mu]]]*B[\[ScriptCapitalU][\[Mu]]]],
  PassCount++;
  Print["[PASS] Free Indices Preserved (Distinct indices do not merge)"],
  FailCount++;
  Print["[FAIL] Free Indices Incorrectly Merged"]];

(* ========================================================================= *)
Print["\n--- SECTION 3: PHYSICS (METRIC & TRANSFORMATIONS) ---"];
(* ========================================================================= *)

(* 1. Raising & Lowering *)
AssertEqual[
  (g[\[ScriptCapitalD][\[Mu]], \[ScriptCapitalD][\[Nu]]]*A[\[ScriptCapitalU][\[Nu]]]) /. metricRules,
  A[\[ScriptCapitalD][\[Mu]]],
  "Lowering Index (g_uv A^v -> A_u)"];

AssertEqual[
  (g[\[ScriptCapitalU][\[Mu]], \[ScriptCapitalU][\[Nu]]]*p[\[ScriptCapitalD][\[Nu]]]) /. metricRules,
  p[\[ScriptCapitalU][\[Mu]]],
  "Raising Index (g^uv p_v -> p^u)"];

(* 2. Inverse Identity *)
AssertEqual[
  (g[\[ScriptCapitalU][\[Mu]], \[ScriptCapitalU][\[Alpha]]]*g[\[ScriptCapitalD][\[Alpha]], \[ScriptCapitalD][\[Nu]]]) /. metricRules,
  \[Delta][\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Nu]]],
  "Inverse Metric (g^ua g_av -> delta^u_v)"];

(* 3. Alpha-Conversion (Collision Safety) *)
Module[{t1, t2, res, dummies, unique},
  t1 = A[\[ScriptCapitalU][Superscript["a", "\[Prime]"]]];
  t2 = B[\[ScriptCapitalU][Superscript["b", "\[Prime]"]]];
  res = (t1*t2) /. robustTransformRules;
  dummies = 
   Cases[res, (\[ScriptCapitalU] | \[ScriptCapitalD])[i_Symbol] :> i, 
    Infinity];
  unique = DeleteDuplicates[dummies];
  If[Length[unique] == 2,
    PassCount++;
    Print["[PASS] Alpha-Conversion (Generated distinct indices: ", unique, ")"],
    FailCount++;
    Print["[FAIL] Alpha-Conversion (Collision detected: ", unique, ")"]];];

(* ========================================================================= *)
Print["\n--- METRIC EQUIVALENCE (A^u B_u == A_v B^v) ---"];
(* ========================================================================= *)

termUp = A[\[ScriptCapitalU][\[Mu]]]*B[\[ScriptCapitalD][\[Mu]]]; 
termDown = A[\[ScriptCapitalD][\[Nu]]]*B[\[ScriptCapitalU][\[Nu]]];

If[CanonicalizeIndices[termUp] =!= CanonicalizeIndices[termDown],
  PassCount++;
  Print["[PASS] Structural Distinction (A^u B_u != A_v B^v initially)"],
  FailCount++;
  Print["[FAIL] Structural Distinction failed"]];

expandRule = A[\[ScriptCapitalD][idx_]] :>
   Module[{fresh = Unique["\[Alpha]"]},
    g[\[ScriptCapitalD][idx], \[ScriptCapitalD][fresh]]*Hold[A[\[ScriptCapitalU][fresh]]]];

termExpanded = termDown /. expandRule;
termReduced = termExpanded /. metricRules;
termFinal = ReleaseHold[termReduced];

AssertEqual[termFinal, termUp, 
  "Metric Equivalence (A_v B^v reduces to A^u B_u)"];
  
(* ========================================================================= *)
Print["\n--- SECOND DERIVATIVE Box Structure ---"];
(* ========================================================================= *)

(* 1. EXECUTE (Direct Injection) *)
(* We pass the structure DIRECTLY to MakeBoxes to bypass variable evaluation issues *)
boxStructure = MakeBoxes[Partials[Partials[f, x], y], StandardForm];

(* 2. VALIDATION *)
(* Check for FractionBox *)
hasFraction = MatchQ[boxStructure, FractionBox[_, _]];

(* Check for Superscript "2" (Robust against encoding differences) *)
hasSquaredSym = !FreeQ[boxStructure, SuperscriptBox[_, "2"]];

If[hasFraction && hasSquaredSym,
  PassCount++;
  Print["[PASS] formatting rule detected (Found FractionBox + ^2)"],
  (* ELSE *)
  FailCount++;
  Print["[FAIL] formatting rule NOT detected."];
  Print["Expected: FractionBox with SuperscriptBox[_, \"2\"]"];
  Print["Found:"];
  Print[boxStructure] 
];

(* ========================================================================= *)
(* SUMMARY                                                                   *)
(* ========================================================================= *)
Print["\n----------------------------------------------------------------"];
Print["REGRESSION SUITE COMPLETE"];
Print["PASSED: ", PassCount];
Print["FAILED: ", FailCount];
If[FailCount == 0,
  Print["STATUS: GREEN (Ready for Publication)"],
  Print["STATUS: RED (Fix bugs before publishing)"]];
Print["----------------------------------------------------------------"];
