(* ========================================================================= *)
(* RELATIVITY TOOLKIT: REGRESSION SUITE                                      *)
(* ========================================================================= *)

Print["\n================================================================"];
Print["RUNNING REGRESSION SUITE v" <> RelativityToolkitVersion];
Print["================================================================\n"];

(* --- TEST HELPERS ------------------------------------------------------- *)
ClearAll[AssertValence, AssertEqual, PassCount, FailCount, pass, fail];
PassCount = 0;
FailCount = 0;
pass[label_, v_]:=
  (PassCount++;
   Print["[", Style["PASS", Green], "] ", label, " \[LongRightArrow] ", v]);
fail[label_, expected_, v_]:=
  (FailCount++;
   Print["[", Style["FAIL", Red], "] ", label, 
     "\n\tExpected ", expected,
     "\n\tGot ", v]);

AssertValence[expr_, expected_, label_] :=
  Module[{v = valence[expr]},
   If[v === expected,
    pass[label, v],
    fail[label, expected, v]]];

AssertEqual[expr1_, expr2_, label_] :=
  Module[{c1 = CanonicalizeIndices[expr1], c2 = CanonicalizeIndices[expr2]},
   If[c1 === c2,
    pass[label, c1],
    fail[label, c1, c2]]];
    
AssertTrue[expr_, label_] :=
  If[expr, pass[label, True], fail[label, True, False]];

(* ========================================================================= *)
Print["--- SECTION 1: TYPE CHECKING (VALENCE) ---"];
(* ========================================================================= *)

(* 1. Atoms *)
AssertValence[x[\[ScriptCapitalU][\[Mu]]], {{\[Mu]}, {}}, "Vector \!\(\*SuperscriptBox[\(x\), \(\[Mu]\)]\)"];
AssertValence[p[\[ScriptCapitalD][\[Nu]]], {{}, {\[Nu]}}, "Covector \!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)"];
AssertValence[T[\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Nu]]], {{\[Mu]}, {\[Nu]}}, "Mixed Tensor \!\(\*SubscriptBox[SuperscriptBox[\(T\), \(\[Mu]\)], \(\[Nu]\)]\)"];
AssertValence[T[\[ScriptCapitalD][\[Mu]], \[ScriptCapitalD][\[Nu]]], {{}, {\[Mu], \[Nu]}}, "Mixed Tensor \!\(\*SubscriptBox[\(T\), \(\[Mu]\[Nu]\)]\)"];
AssertValence[T[\[ScriptCapitalU][\[Mu]], \[ScriptCapitalU][\[Nu]]], {{\[Mu], \[Nu]}, {}}, "Mixed Tensor \!\(\*SuperscriptBox[\(T\), \(\[Mu]\[Nu]\)]\)"];
AssertValence[T[\[ScriptCapitalD][\[Mu]], \[ScriptCapitalU][\[Nu]]], {{\[Nu]}, {\[Mu]}}, "Mixed Tensor \!\(\*SuperscriptBox[SubscriptBox[\(T\), \(\[Mu]\)], \(\[Nu]\)]\)"];

(* 2. Contractions *)
AssertValence[x[\[ScriptCapitalU][\[Mu]]]*p[\[ScriptCapitalD][\[Mu]]], {{}, {}}, "Scalar Contraction \!\(\*SuperscriptBox[\(x\), \(\[Mu]\)]\) \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)"];
AssertValence[p[\[ScriptCapitalD][\[Mu]]]*x[\[ScriptCapitalU][\[Mu]]], {{}, {}}, "Scalar Contraction \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\) \!\(\*SuperscriptBox[\(x\), \(\[Mu]\)]\)"];
AssertValence[T[\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Nu]]] * x[\[ScriptCapitalU][\[Nu]]], {{\[Mu]}, {}}, "Tensor-Vector \!\(\*SubscriptBox[SuperscriptBox[\(T\), \(\[Mu]\)], \(\[Nu]\)]\) \!\(\*SuperscriptBox[\(x\), \(\[Nu]\)]\) -> \!\(\*SuperscriptBox[\(x\), \(\[Mu]\)]\)"];
AssertValence[T[\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Nu]]] * x[\[ScriptCapitalD][\[Mu]]], {{}, {\[Nu]}}, "Tensor-Vector \!\(\*SubscriptBox[SuperscriptBox[\(T\), \(\[Mu]\)], \(\[Nu]\)]\) \!\(\*SuperscriptBox[\(x\), \(\[Mu]\)]\) -> \!\(\*SubscriptBox[\(x\), \(\[Nu]\)]\)"];

(* 3. Arithmetic *)
AssertValence[(x[\[ScriptCapitalU][\[Mu]]]*p[\[ScriptCapitalD][\[Mu]]])^2, {{}, {}}, "Power of Scalar"];
AssertValence[5*x[\[ScriptCapitalU][\[Mu]]], {{\[Mu]}, {}}, "Scalar Multiplication"];

(* 4. Differentiation (The Gradient Test) *)
AssertValence[Derivative[1][f][x[\[ScriptCapitalU][\[Alpha]]]], {{}, {\[Alpha]}}, "Gradient d(f)/\!\(\*SuperscriptBox[\(dx\), \(\[Alpha]\)]\) -> Down[\[Alpha]]"];

(* 5. Error Handling *)
Quiet[Module[{badSum = x[\[ScriptCapitalU][\[Mu]]] + p[\[ScriptCapitalD][\[Mu]]]}, 
   If[Echo[valence[badSum], "expect error message above"] === {{}, {}}, 
    pass["Mismatch Handled Gracefully", {{}, {}}],
    fail["Mismatch Detection failed", {{}, {}}, valence[badSum]]]]];

(* ========================================================================= *)
Print["\n--- SECTION 2: ALGEBRA (CANONICALIZATION) ---"];
(* ========================================================================= *)

(* 1. Simple Index Renaming *)
AssertEqual[
  x[\[ScriptCapitalU][\[Mu]]]*p[\[ScriptCapitalD][\[Mu]]],
  x[\[ScriptCapitalU][\[Nu]]]*p[\[ScriptCapitalD][\[Nu]]],
  "Dummy Renaming (\!\(\*SuperscriptBox[\(A\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(B\), \(\[Mu]\)]\) == \!\(\*SuperscriptBox[\(A\), \(\[Nu]\)]\)\!\(\*SubscriptBox[\(B\), \(\[Nu]\)]\))"];

(* 2. Commutativity *)
AssertEqual[
  x[\[ScriptCapitalU][\[Mu]]]*p[\[ScriptCapitalD][\[Mu]]],
  p[\[ScriptCapitalD][\[Nu]]]*x[\[ScriptCapitalU][\[Nu]]],
  "Commutativity (\!\(\*SuperscriptBox[\(A\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(B\), \(\[Mu]\)]\) == \!\(\*SubscriptBox[\(B\), \(\[Nu]\)]\)\!\(\*SuperscriptBox[\(A\), \(\[Nu]\)]\))"];

(* 3. Distributivity/Sums *)
AssertEqual[
  x[\[ScriptCapitalU][\[Mu]]]*p[\[ScriptCapitalD][\[Mu]]] + 
   x[\[ScriptCapitalU][\[Nu]]]*p[\[ScriptCapitalD][\[Nu]]],
  2*x[\[ScriptCapitalU][\[Rho]]]*p[\[ScriptCapitalD][\[Rho]]],
  "Index Coalescing (\!\(\*SuperscriptBox[\(A\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(B\), \(\[Mu]\)]\) + \!\(\*SuperscriptBox[\(A\), \(\[Nu]\)]\)\!\(\*SubscriptBox[\(B\), \(\[Nu]\)]\) == 2 \!\(\*SuperscriptBox[\(A\), \(\[Rho]\)]\)\!\(\*SubscriptBox[\(B\), \(\[Rho]\)]\))"];

(* 4. Multi-Index Tensors *)
AssertEqual[
  P[\[ScriptCapitalU][\[Mu]]]*Q[\[ScriptCapitalD][\[Mu]]]* S[\[ScriptCapitalU][\[Nu]]]*T[\[ScriptCapitalD][\[Nu]]],
  S[\[ScriptCapitalU][\[Alpha]]]*T[\[ScriptCapitalD][\[Alpha]]]* P[\[ScriptCapitalU][\[Beta]]]*Q[\[ScriptCapitalD][\[Beta]]],
  "Double Dummy Pairs (\!\(\*SuperscriptBox[\(P\), \(\[Mu]\)]\) \!\(\*SubscriptBox[\(Q\), \(\[Mu]\)]\) \!\(\*SuperscriptBox[\(S\), \(\[Nu]\)]\) \!\(\*SubscriptBox[\(T\), \(\[Nu]\)]\))"];

(* 5. Negative Test *)
Module[{
   is1 = CanonicalizeIndices[A[\[ScriptCapitalU][\[Alpha]]] * B[\[ScriptCapitalU][\[Beta]]]],
   is2 = CanonicalizeIndices[A[\[ScriptCapitalU][\[Mu]]] * B[\[ScriptCapitalU][\[Mu]]]]},
  If[is1 =!= is2,
    pass["Free Indices Preserved", {is1, is2}],
    fail["Free Indices Incorrectly Merged", is1, is2]]];

(* ========================================================================= *)
Print["\n--- SECTION 3: PHYSICS (METRIC & TRANSFORMATIONS) ---"];
(* ========================================================================= *)

(* 1. Raising & Lowering *)
AssertEqual[
  (g[\[ScriptCapitalD][\[Mu]], \[ScriptCapitalD][\[Nu]]]*A[\[ScriptCapitalU][\[Nu]]]) /. metricRules,
  A[\[ScriptCapitalD][\[Mu]]],
  "Lowering Index (\!\(\*SubscriptBox[\(g\), \(\[Mu]\[Nu]\)]\) \!\(\*SuperscriptBox[\(A\), \(\[Nu]\)]\) \[Rule] \!\(\*SubscriptBox[\(A\), \(\[Mu]\)]\))"];

AssertEqual[
  (g[\[ScriptCapitalU][\[Mu]], \[ScriptCapitalU][\[Nu]]]*p[\[ScriptCapitalD][\[Nu]]]) /. metricRules,
  p[\[ScriptCapitalU][\[Mu]]],
  "Raising Index (\!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\) \!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\) \[Rule] \!\(\*SuperscriptBox[\(p\), \(\[Mu]\)]\))"];

(* 2. Inverse Identity *)
AssertEqual[
  (g[\[ScriptCapitalU][\[Mu]], \[ScriptCapitalU][\[Alpha]]]*g[\[ScriptCapitalD][\[Alpha]], \[ScriptCapitalD][\[Nu]]]) /. metricRules,
  \[Delta][\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Nu]]],
  "Inverse Metric (\!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Alpha]\)]\) \!\(\*SubscriptBox[\(g\), \(\[Alpha]\[Nu]\)]\) \[Rule] \!\(\*SubsuperscriptBox[\(\[Delta]\), \(\[Nu]\), \(\[Mu]\)]\))"];

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
    pass["Alpha-Conversion: distinct indices)", unique],
    fail["Alpha-Conversion: collision", 2, Length[unique]]]];

(* ========================================================================= *)
Print["\n--- SECTION 4: METRIC EQUIVALENCE (\!\(\*SuperscriptBox[\(A\), \(\[Mu]\)]\) \!\(\*SubscriptBox[\(B\), \(\[Mu]\)]\) == \!\(\*SubscriptBox[\(A\), \(\[Nu]\)]\) \!\(\*SuperscriptBox[\(B\), \(\[Nu]\)]\)) ---"];
(* ========================================================================= *)

Echo[termUp = A[\[ScriptCapitalU][\[Mu]]] * B[\[ScriptCapitalD][\[Mu]]], "termUp: "]; 
Echo[termDown = A[\[ScriptCapitalD][\[Nu]]] * B[\[ScriptCapitalU][\[Nu]]], "termDown: "];

Module[{
   is1 = CanonicalizeIndices[termUp], 
   is2 = CanonicalizeIndices[termDown]},
If[is1 =!= CanonicalizeIndices[termDown],
  pass["Structural Distinction \!\(\*SuperscriptBox[\(A\), \(\[Mu]\)]\) \!\(\*SubscriptBox[\(B\), \(\[Mu]\)]\) != \!\(\*SubscriptBox[\(A\), \(\[Nu]\)]\) \!\(\*SuperscriptBox[\(B\), \(\[Nu]\)]\) initially", {is1, is2}],
  fail["Structural Distinction Failed", is1, is2]]];

expandRule = A[\[ScriptCapitalD][idx_]] :>
   Module[{fresh = Unique["\[Alpha]"]},
    g[\[ScriptCapitalD][idx], \[ScriptCapitalD][fresh]] * Hold[A[\[ScriptCapitalU][fresh]]]];

Print[Style["\[ScriptCapitalU]\[ScriptCapitalD] will rearrange terms", Gray]];
Echo[termDownExpanded = termDown /. expandRule, "termDown expanded: "];
Echo[termDownReduced = termDownExpanded /. metricRules, "termDown reduced: "];
Echo[termFinal = ReleaseHold[termDownReduced], "termDown final: "];

AssertEqual[termFinal, termUp, 
  "Metric Equivalence (\!\(\*SubscriptBox[\(A\), \(\[Nu]\)]\) \!\(\*SuperscriptBox[\(B\), \(\[Nu]\)]\) reduces to \!\(\*SuperscriptBox[\(A\), \(\[Mu]\)]\) \!\(\*SubscriptBox[\(B\), \(\[Mu]\)]\))"];
  
(* ========================================================================= *)
Print["\n--- SECTION 5: SECOND DERIVATIVE Box Structure ---"];
(* ========================================================================= *)

(* 1. EXECUTE (Direct Injection) *)
(* Pass the structure DIRECTLY to MakeBoxes to bypass variable evaluation issues *)
Echo[boxStructure = MakeBoxes[Partials[Partials[f, x], y], StandardForm], 
  "box structure: "];

(* 2. VALIDATION *)
(* Check for FractionBox *)
Echo[hasFraction = MatchQ[boxStructure, FractionBox[_, _]], 
  "has fraction: "];

(* Check for Superscript "2" (Robust against encoding differences) *)
Echo[hasSquaredSym = !FreeQ[boxStructure, SuperscriptBox[_, "2"]],
  "has squared symbol: "];

If[hasFraction && hasSquaredSym,
  pass["Found FractionBox + ^2", True],
  fail["formatting rule NOT detected", True, False]];

Print["\n--- SECTION 6: CALCULUS ENGINE ---"];

(* Test Leibniz Product Rule *)
(* Input: d(f*g)/dx *)
(* Expected: (df/dx)g + f(dg/dx) *)
Echo[testProduct = ExpandDerivatives[Partials[f * g, x[\[ScriptCapitalU][\[Mu]]]]],
  "test product: "];
Echo[expectedProduct = Partials[f, x[\[ScriptCapitalU][\[Mu]]]] * g + f * Partials[g, x[\[ScriptCapitalU][\[Mu]]]],
  "expected product: "];
AssertEqual[testProduct, expectedProduct, "Leibniz Product Rule"];

(* Covariant Derivative Expansion *)
(* Input: CD[ A^u, x^v ] *)
(* Expected: Partial[A^u, x^v] + (Something containing Gamma) *)
Echo[testCD = CD[A[\[ScriptCapitalU][\[Mu]]], x[\[ScriptCapitalU][\[Nu]]]], 
  "CD[A[\[ScriptCapitalU][\[Mu]]], x[\[ScriptCapitalU][\[Nu]]]] : "];
(* LOGIC: It must be a Sum. One part is Partials. The other part MUST contain Gamma. *)
AssertTrue[MatchQ[testCD, Plus[Partials[_, _], _]] && !FreeQ[testCD, \[CapitalGamma]],
  "Covariant Derivative Expansion"];
(*If[MatchQ[testCD, Plus[Partials[_, _], _]] && !FreeQ[testCD, \[CapitalGamma]],
   PassCount++;
   Print["[PASS] CD Expansion Structure (Partial + Connection)"],
   FailCount++;
   Print["[FAIL] CD Expansion Structure\n   Found: ", testCD]
];*)

(* Test Chain Rule (Linearity) *)
(* Input: d(A+B)/dx *)
Echo[testLinearity = ExpandDerivatives[Partials[A + B, x[\[ScriptCapitalU][\[Mu]]]]],
  "(A + B\!\(\*SubscriptBox[\()\), \(, \(\[Mu]\)\)]\) : "];
AssertTrue[MatchQ[testLinearity, Plus[Partials[A, _], Partials[B, _]]],
  "Differentiation Linearity"];

(* ========================================================================= *)
(* SUMMARY                                                                   *)
(* ========================================================================= *)

Print["\n----------------------------------------------------------------"];
Print["REGRESSION SUITE COMPLETE"];
Print["PASSED: ", PassCount];
Print["FAILED: ", FailCount];
If[FailCount == 0,
  Print[Style["STATUS: GREEN (Ready for Publication)", Green]],
  Print[Style["STATUS: RED (Fix bugs before publishing)", Red]]];
Print["----------------------------------------------------------------"];
