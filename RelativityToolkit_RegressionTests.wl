(* ::Package:: *)

(* ::Title:: *)
(*Relativity Toolkit Regression Suite*)


(* ========================================================================= *)
(* RELATIVITY TOOLKIT: REGRESSION SUITE                                      *)
(* ========================================================================= *)

Print["\n================================================================"];
Print["RUNNING REGRESSION SUITE v" <> RelativityToolkitVersion];
Print["================================================================\n"];



(* ::Chapter::Closed:: *)
(*Test Harness*)


(* --- TEST HARNESS ------------------------------------------------------- *)
ClearAll[AssertValence, AssertEqual, PassCount, FailCount, pass, fail];
PassCount = 0;
FailCount = 0;

pass[label_, v_]:=
  (PassCount++;
   Print["[", Style["PASS", Green], "] ",
     NumberForm[PassCount+FailCount, 4, NumberPadding->{" ",""}], ": ",
     label, " \[LongRightArrow] ", v]);
   
fail[label_, expected_, actual_]:=
  (FailCount++;
   Print["[", Style["FAIL", Red], "] ", 
     NumberForm[PassCount+FailCount, 4, NumberPadding->{" ",""}], ": ",
     label, 
     "\n\tExpected ", expected,
     "\n\tGot ", actual]);

AssertValence[expr_, expected_, label_String:""] :=
  Module[{v = valence[expr]},
   If[v === expected,
    pass[label, v],
    fail[label, expected, v]]];

AssertEqual[actual_, expected_, label_String:""] :=
  Module[{
    canAct = CanonicalizeIndices[actual], 
    canExp = CanonicalizeIndices[expected]},
   If[canAct === canExp,
    pass[label, canAct],
    fail[label, canExp, canAct]]];
    
AssertTrue[expr_, label_String:""] :=
  If[expr, pass[label, True], fail[label, True, False]];

AssertFalse[expr_, label_String:""] := AssertTrue[!expr, label];

SetAttributes[AssertMatchQ, HoldAll];
AssertMatchQ[expr_, pattern_, label_String:""]:=
	Module[{result = expr},
		If[MatchQ[result, pattern],
		    pass[label, True],
		    fail[label, HoldForm[pattern], result]];
		result];
		
SetAttributes[AssertNotMatchQ, HoldAll];
AssertNotMatchQ[expr_, pattern_, label_String:""]:=
	Module[{result = expr},
		If[MatchQ[result, pattern],
		    fail[label, HoldForm[pattern], result],
		    pass[label, True]];
		result];

SetAttributes[AssertProtected, HoldAll];
AssertProtected[e_, label_String:""] :=
  AssertMatchQ[Attributes @@ {e}, {___, Protected, ___}, label];



(* ::Chapter::Closed:: *)
(*Type Checking*)


(* ========================================================================= *)
Print["--- SECTION 1: TYPE CHECKING (VALENCE) ---"];
(* ========================================================================= *)

(* 1. Atoms *)
AssertValence[x[\[ScriptCapitalU][\[Mu]]], {{\[Mu]}, {}}, "Vector (\!\(\*SuperscriptBox[\(x\), \(\[Mu]\)]\))"];
AssertValence[p[\[ScriptCapitalD][\[Nu]]], {{}, {\[Nu]}}, "Covector (\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\))"];
AssertValence[T[\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Nu]]], {{\[Mu]}, {\[Nu]}}, "Mixed Tensor (\!\(\*SubscriptBox[SuperscriptBox[\(T\), \(\[Mu]\)], \(\[Nu]\)]\))"];
AssertValence[T[\[ScriptCapitalD][\[Mu]], \[ScriptCapitalD][\[Nu]]], {{}, {\[Mu], \[Nu]}}, "Mixed Tensor (\!\(\*SubscriptBox[\(T\), \(\[Mu]\[Nu]\)]\))"];
AssertValence[T[\[ScriptCapitalU][\[Mu]], \[ScriptCapitalU][\[Nu]]], {{\[Mu], \[Nu]}, {}}, "Mixed Tensor (\!\(\*SuperscriptBox[\(T\), \(\[Mu]\[Nu]\)]\))"];
AssertValence[T[\[ScriptCapitalD][\[Mu]], \[ScriptCapitalU][\[Nu]]], {{\[Nu]}, {\[Mu]}}, "Mixed Tensor (\!\(\*SubsuperscriptBox[\(T\), \(\[Mu]\), \(\[Nu]\)]\))"];
AssertValence[T, noValence, "Bare Symbol T"];
AssertValence[T[], noValence, "Bare Call T[]"];
AssertValence[Null, noValence, "Trap (Null)"];

(* 2. Contractions (tests contractValence) *)
AssertValence[x[\[ScriptCapitalU][\[Mu]]]*p[\[ScriptCapitalD][\[Mu]]], {{}, {}}, "Scalar Contraction (\!\(\*SuperscriptBox[\(x\), \(\[Mu]\)]\) \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\))"];
AssertValence[p[\[ScriptCapitalD][\[Mu]]]*x[\[ScriptCapitalU][\[Mu]]], {{}, {}}, "Scalar Contraction (\!\(\*SuperscriptBox[\(x\), \(\[Mu]\)]\) \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\))"];
AssertValence[T[\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Nu]]] * x[\[ScriptCapitalU][\[Nu]]], {{\[Mu]}, {}}, "Tensor-Vector (\!\(\*SuperscriptBox[\(x\), \(\[Nu]\)]\) \!\(\*SubscriptBox[SuperscriptBox[\(T\), \(\[Mu]\)], \(\[Nu]\)]\) \[Rule] \!\(\*SuperscriptBox[\(x\), \(\[Mu]\)]\))"];
AssertValence[T[\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Nu]]] * x[\[ScriptCapitalD][\[Mu]]], {{}, {\[Nu]}}, "Tensor-Vector (\!\(\*SuperscriptBox[\(x\), \(\[Mu]\)]\) \!\(\*SubscriptBox[SuperscriptBox[\(T\), \(\[Mu]\)], \(\[Nu]\)]\) \[Rule] \!\(\*SubscriptBox[\(x\), \(\[Nu]\)]\))"];

(* 3. Arithmetic *)
AssertValence[(x[\[ScriptCapitalU][\[Mu]]] * p[\[ScriptCapitalD][\[Mu]]])^2, {{}, {}}, "Power of Scalar"];
AssertValence[5 * x[\[ScriptCapitalU][\[Mu]]], {{\[Mu]}, {}}, "Scalar Multiplication"];

(* 4. Differentiation (The Gradient Test) *)
AssertValence[Derivative[1][f][x[\[ScriptCapitalU][\[Alpha]]]], {{}, {\[Alpha]}}, "Gradient d(f)/\!\(\*SuperscriptBox[\(dx\), \(\[Alpha]\)]\) -> Down[\[Alpha]]"];

(* 5. Error Handling *)
Quiet[Module[{badSum = x[\[ScriptCapitalU][\[Mu]]] + p[\[ScriptCapitalD][\[Mu]]]}, 
   If[Echo[valence[badSum], "expect error message above"] === {{}, {}}, 
    pass["Mismatch Handled Gracefully", {{}, {}}],
    fail["Mismatch Detection failed", {{}, {}}, valence[badSum]]]]];



(* ::Chapter::Closed:: *)
(*Canonicalization*)


(* ========================================================================= *)
Print["\n--- SECTION 2: ALGEBRAIC CANONICALIZATION ---"];
(* ========================================================================= *)

(* 1. Index Renaming *)
AssertEqual[
  x[\[ScriptCapitalU][\[Mu]]] * p[\[ScriptCapitalD][\[Mu]]],
  x[\[ScriptCapitalU][\[Nu]]] * p[\[ScriptCapitalD][\[Nu]]],
  "Dummy Renaming (\!\(\*SuperscriptBox[\(A\), \(\[Mu]\)]\) \!\(\*SubscriptBox[\(B\), \(\[Mu]\(\\\ \)\)]\) \[Equal]\!\(\*SuperscriptBox[\(A\), \(\[Nu]\)]\) \!\(\*SubscriptBox[\(B\), \(\[Nu]\)]\))"];

(* 2. Commutativity *)
AssertEqual[
  x[\[ScriptCapitalU][\[Mu]]] * p[\[ScriptCapitalD][\[Mu]]],
  p[\[ScriptCapitalD][\[Nu]]] * x[\[ScriptCapitalU][\[Nu]]],
  "Commutativity (\!\(\*SuperscriptBox[\(A\), \(\[Mu]\)]\) \!\(\*SubscriptBox[\(B\), \(\[Mu]\)]\) \[Equal] \!\(\*SubscriptBox[\(B\), \(\[Nu]\)]\) \!\(\*SuperscriptBox[\(A\), \(\[Nu]\)]\))"];

(* 3. Distributivity/Sums *)
AssertEqual[
  x[\[ScriptCapitalU][\[Mu]]] * p[\[ScriptCapitalD][\[Mu]]] + x[\[ScriptCapitalU][\[Nu]]] * p[\[ScriptCapitalD][\[Nu]]],
  2 * x[\[ScriptCapitalU][\[Rho]]] * p[\[ScriptCapitalD][\[Rho]]],
  "Index Coalescing (\!\(\*SuperscriptBox[\(A\), \(\[Mu]\)]\) \!\(\*SubscriptBox[\(B\), \(\[Mu]\)]\) + \!\(\*SuperscriptBox[\(A\), \(\[Nu]\)]\) \!\(\*SubscriptBox[\(B\), \(\[Nu]\)]\) \[Equal] 2 \!\(\*SuperscriptBox[\(A\), \(\[Rho]\)]\) \!\(\*SubscriptBox[\(B\), \(\[Rho]\)]\))"];

(* 4. Multi-Index Tensors *)
AssertEqual[
  P[\[ScriptCapitalU][\[Mu]]] * Q[\[ScriptCapitalD][\[Mu]]] * S[\[ScriptCapitalU][\[Nu]]] * T[\[ScriptCapitalD][\[Nu]]],
  S[\[ScriptCapitalU][\[Alpha]]] * T[\[ScriptCapitalD][\[Alpha]]] * P[\[ScriptCapitalU][\[Beta]]] * Q[\[ScriptCapitalD][\[Beta]]],
  "Double Dummy Pairs (\!\(\*SuperscriptBox[\(P\), \(\[Mu]\)]\) \!\(\*SuperscriptBox[\(S\), \(\[Nu]\)]\) \!\(\*SubscriptBox[\(Q\), \(\[Mu]\)]\) \!\(\*SubscriptBox[\(T\), \(\[Nu]\)]\))"];

(* 5. Negative Test *)
Module[{
   is1 = CanonicalizeIndices[A[\[ScriptCapitalU][\[Alpha]]] * B[\[ScriptCapitalU][\[Beta]]]],
   is2 = CanonicalizeIndices[A[\[ScriptCapitalU][\[Mu]]] * B[\[ScriptCapitalU][\[Mu]]]]},
  If[is1 =!= is2,
    pass["Free Indices Preserved", {is1, is2}],
    fail["Free Indices Incorrectly Merged", is1, is2]]];



(* ::Chapter::Closed:: *)
(*Physics with the Metric*)


(* ========================================================================= *)
Print["\n--- SECTION 3: PHYSICS, METRIC & TRANSFORMATIONS ---"];
(* ========================================================================= *)



(* ::Section:: *)
(*Raising and Lowering*)


(* 1. Raising & Lowering *)
AssertEqual[
  (g[\[ScriptCapitalD][\[Mu]], \[ScriptCapitalD][\[Nu]]] * A[\[ScriptCapitalU][\[Nu]]]) /. metricRules,
  A[\[ScriptCapitalD][\[Mu]]],
  "Lowering Index (\!\(\*SuperscriptBox[\(A\), \(\[Nu]\)]\) \!\(\*SubscriptBox[\(g\), \(\[Mu]\[Nu]\)]\) \[Rule] \!\(\*SubscriptBox[\(A\), \(\[Mu]\)]\))"];

AssertEqual[
  (g[\[ScriptCapitalU][\[Mu]], \[ScriptCapitalU][\[Nu]]] * p[\[ScriptCapitalD][\[Nu]]]) /. metricRules,
  p[\[ScriptCapitalU][\[Mu]]],
  "Raising Index (\!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\) \!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\) \[Rule] \!\(\*SuperscriptBox[\(p\), \(\[Mu]\)]\))"];

(* 2. Inverse Identity *)
AssertEqual[
  (g[\[ScriptCapitalU][\[Mu]], \[ScriptCapitalU][\[Alpha]]] * g[\[ScriptCapitalD][\[Alpha]], \[ScriptCapitalD][\[Nu]]]) /. metricRules,
  \[Delta][\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Nu]]],
  "Inverse Metric (\!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Alpha]\)]\) \!\(\*SubscriptBox[\(g\), \(\[Alpha]\[Nu]\)]\) \[Rule] \!\(\*SubsuperscriptBox[\(\[Delta]\), \(\[Nu]\), \(\[Mu]\)]\))"];



(* ::Section:: *)
(*Alpha Conversion*)


(* 3. Alpha-Conversion (Collision Safety) *)
Module[{t1, t2, res, dummies, unique},
    t1 = A[\[ScriptCapitalU][Superscript["a", "\[Prime]"]]];
    t2 = B[\[ScriptCapitalU][Superscript["b", "\[Prime]"]]];
  res = (t1*t2) /. robustTransformRules;
  dummies = Cases[res, (\[ScriptCapitalU] | \[ScriptCapitalD])[i_Symbol] :> i, Infinity];
  unique = DeleteDuplicates[dummies];
  If[Length[unique] == 2,
    pass["Alpha-Conversion: distinct indices)", unique],
    fail["Alpha-Conversion: collision", 2, Length[unique]]]];
    


(* ::Section:: *)
(*Contraction & Associativity*)


(* 4. Associativity of Contraction (Triple Metric Sandwich) *)
(*  g_\[Mu]\[Alpha] g^\[Alpha]\[Beta] g_\[Beta]\[Nu] \[LongRightArrow] g_\[Mu]\[Nu] *)
Module[{termSandwich},
  termSandwich = g[\[ScriptCapitalD][\[Mu]], \[ScriptCapitalD][\[Alpha]]] * g[\[ScriptCapitalU][\[Alpha]], \[ScriptCapitalU][\[Beta]]] * g[\[ScriptCapitalD][\[Beta]], \[ScriptCapitalD][\[Nu]]];
  AssertEqual[termSandwich //. metricRules, g[\[ScriptCapitalD][\[Mu]], \[ScriptCapitalD][\[Nu]]], 
    "Triple Metric Sandwich (g.g.g -> g)"]];

(* 5. Delta Contracting on Gamma *)
(* \[CapitalGamma]^r_ab \[Delta]^s_r] \[LongRightArrow] \[CapitalGamma]^s_ab *)
Module[{gammaTerm = \[CapitalGamma][\[ScriptCapitalU][r], \[ScriptCapitalD][a], \[ScriptCapitalD][b]] * \[Delta][\[ScriptCapitalU][s], \[ScriptCapitalD][r]]},
  AssertEqual[gammaTerm //. metricRules, \[CapitalGamma][\[ScriptCapitalU][s], \[ScriptCapitalD][a], \[ScriptCapitalD][b]], 
    "Delta-Gamma Contraction"]];
    


(* ::Section:: *)
(*Metric Differentiation*)


Print["\n=== Metric Differentiation Rules ==="];

(* ========================================================================= *)
(* Metric Differentiation & Robust Contraction                               *)
(* Verify:                                                                   *)
(* 1. Permissive Contraction (Index alignment)                               *)
(* 2. Metric Symmetry (g_ab = g_ba)                                          *)
(* 3. Delta-on-Derivative Chain Rule                                         *)
(* ========================================================================= *)

allMetricRules = (metricRules ~Join~ metricDifferentiationRules);

(* ------------------------------------------------------------------------- *)
(* 1. Permissive Contraction ("Index Flip")                                  *)
test1Expr = g[\[ScriptCapitalD][mu], \[ScriptCapitalD][lam]] * g[\[ScriptCapitalU][mu], \[ScriptCapitalU][nu]];
test1Result = test1Expr //. allMetricRules // Simplify;
test1Expected = \[Delta][\[ScriptCapitalU][nu], \[ScriptCapitalD][lam]];

AssertEqual[test1Result, test1Expected, "Permissive Contraction"]

(* ------------------------------------------------------------------------- *)
(* 2. Metric Symmetry inside Derivatives                                     *)
(* Problem: d(g_uv) - d(g_vu) should be 0.                                   *)
test2Expr = Partials[g[\[ScriptCapitalD][\[Mu]], \[ScriptCapitalD][\[Nu]]], x[\[ScriptCapitalU][\[Rho]]]] - 
            Partials[g[\[ScriptCapitalD][\[Nu]], \[ScriptCapitalD][\[Mu]]], x[\[ScriptCapitalU][\[Rho]]]];
test2Result = test2Expr //. allMetricRules // Simplify;

AssertEqual[test2Result, 0, "Metric Symmetry in Derivatives"]

(* ------------------------------------------------------------------------- *)
(* 3. Delta Chain Rule                                                       *)
(* Problem: delta^s_m * d/dx^s must become d/dx^m                            *)
test3Expr = \[Delta][\[ScriptCapitalU][\[Sigma]], \[ScriptCapitalD][\[Mu]]] * Partials[f[x], x[\[ScriptCapitalU][\[Sigma]]]];
test3Result = test3Expr //. allMetricRules // Simplify;
test3Expected = Partials[f[x], x[\[ScriptCapitalU][\[Mu]]]];

AssertEqual[test3Result, test3Expected, "Delta Chain Rule"]



(* ::Section:: *)
(*Metric Compatibility*)


(* ------------------------------------------------------------------------- *)
(* 4, Full Metric Compatibility                                              *)
(* Problem: CD[g] must vanish identically for Levi-Civita connection.        *)
rawCD = CD[g[\[ScriptCapitalD][\[Mu]], \[ScriptCapitalD][\[Nu]]], x[\[ScriptCapitalU][\[Rho]]]];
compatibilityResult = ((rawCD /. ruleLeviCivita) // Expand) //. allMetricRules;

AssertEqual[compatibilityResult, 0, "Metric Compatibility"]

(* ------------------------------------------------------------------------- *)
(* 5. Levi-Civita Dummy Index Safety (The "Gamma Squared" Test)              *)
(* Problem: Gamma * Gamma must generate two DISTINCT summation indices.      *)
Module[{gammaProd, expandedProd, dummies},
  gammaProd = \[CapitalGamma][\[ScriptCapitalU][mu], \[ScriptCapitalD][a], \[ScriptCapitalD][b]] * \[CapitalGamma][\[ScriptCapitalU][nu], \[ScriptCapitalD][c], \[ScriptCapitalD][d]];
  
  expandedProd = gammaProd /. ruleLeviCivita;
  
  (* Extract all summation indices generated in the expansion *)
  dummies = Cases[expandedProd, g[\[ScriptCapitalU][_], \[ScriptCapitalU][s_]] :> s, Infinity];
  
  (* Expect 2 dummies. If Unique works, they must NOT be equal. *)
  If[Length[dummies] == 2 && dummies[[1]] =!= dummies[[2]],
    pass["Levi-Civita Index Safety (Unique indices generated)", dummies],
    fail["Levi-Civita Index Safety", "Distinct Indices", dummies]  ];  ];



(* ::Chapter::Closed:: *)
(*Metric Equivalence*)


(* ========================================================================= *)
Print["\n--- SECTION 4: METRIC EQUIVALENCE (\!\(\*SuperscriptBox[\(A\), \(\[Mu]\)]\) \!\(\*SubscriptBox[\(B\), \(\[Mu]\)]\) \[Equal] \!\(\*SuperscriptBox[\(B\), \(\[Nu]\)]\) \!\(\*SubscriptBox[\(A\), \(\[Nu]\)]\)) ---"];
(* ========================================================================= *)

Module[{termUp, termDown},
  termUp = A[\[ScriptCapitalU][\[Mu]]] * B[\[ScriptCapitalD][\[Mu]]]; (* A^\[Mu] Subscript[B, \[Mu]] *)
  termDown = A[\[ScriptCapitalD][\[Nu]]] * B[\[ScriptCapitalU][\[Nu]]]; (* Subscript[A, \[Nu]] B^\[Nu] *)

  Module[{
      is1 = CanonicalizeIndices[termUp], 
      is2 = CanonicalizeIndices[termDown]},
    If[is1 =!= is2,
      pass["Structural Distinction (\!\(\*SuperscriptBox[\(A\), \(\[Mu]\)]\) \!\(\*SubscriptBox[\(B\), \(\[Mu]\)]\) \[NotEqual] \!\(\*SuperscriptBox[\(B\), \(\[Nu]\)]\) \!\(\*SubscriptBox[\(A\), \(\[Nu]\)]\)) initially", {is1, is2}],
      fail["Structural Distinction Failed", is1, is2]]];

  Module[{expandRule, termDownExpanded, termDownReduced, termFinal},
    (* Subscript[A, \[Mu]] \[Rule] Subscript[g,  \[Mu] foo] A^foo *)
    expandRule = A[\[ScriptCapitalD][idx_]] :>
       Module[{fresh = Unique["\[Alpha]"]},
        g[\[ScriptCapitalD][idx], \[ScriptCapitalD][fresh]] * Hold[A[\[ScriptCapitalU][fresh]]]];

    (* Subscript[A, \[Nu]] B^\[Nu] \[Rule] Subscript[g,  \[Nu] foo] A^foo B^\[Nu] *)
    termDownExpanded = termDown /. expandRule;
    (* A^foo Subscript[B, foo] *)
    termDownReduced = termDownExpanded /. metricRules;
    (* A^foo Subscript[B, foo] *)
    termFinal = ReleaseHold[termDownReduced];

   (* A^foo Subscript[B, foo] == A^\[Mu] Subscript[B, \[Mu]]*)
    AssertEqual[termFinal, termUp, 
      "Metric Equivalence (\!\(\*SubscriptBox[\(A\), \(\[Nu]\)]\) \!\(\*SuperscriptBox[\(B\), \(\[Nu]\)]\) reduces to \!\(\*SuperscriptBox[\(A\), \(\[Mu]\)]\) \!\(\*SubscriptBox[\(B\), \(\[Mu]\)]\))"]]];
  


(* ::Chapter::Closed:: *)
(*Boxes for Second Derivative*)


(* ========================================================================= *)
Print["\n--- SECTION 5: SECOND DERIVATIVE BOX STRUCTURE ---"];
(* ========================================================================= *)

Module[{boxStructure, hasFraction, hasSquaredSym},

  (* 1. EXECUTE (Direct Injection) *)
  (* Pass the structure DIRECTLY to MakeBoxes to bypass evaluation issues *)
  boxStructure = MakeBoxes[Partials[Partials[f, x], y], StandardForm];

  (* 2. VALIDATION *)
  (* Check for FractionBox *)
  hasFraction = MatchQ[boxStructure, FractionBox[_, _]];

  (* Check for Superscript "2" (Robust against encoding differences) *)
  hasSquaredSym = !FreeQ[boxStructure, SuperscriptBox[_, "2"]];

  If[hasFraction && hasSquaredSym,
    pass["Found FractionBox + ^2", True],
    fail["formatting rule NOT detected", True, False]]];



(* ::Chapter::Closed:: *)
(*Calculus Engine*)


(* ========================================================================= *)
Print["\n--- SECTION 6: CALCULUS ENGINE ---"];
(* ========================================================================= *)

Module[{testProduct, a, testCD, testLinearity},

	(* Leibniz Product Rule *)
	(* Input: d(f*g)/dx *)
	(* Expected: (df/dx)g + f(dg/dx) *)
	testProduct = ExpandDerivatives[Partials[f * g, x[\[ScriptCapitalU][\[Mu]]]]];
	expectedProduct = Partials[f, x[\[ScriptCapitalU][\[Mu]]]] * g + f * Partials[g, x[\[ScriptCapitalU][\[Mu]]]];
	AssertEqual[testProduct, expectedProduct, "Leibniz Product Rule"];

	(* Covariant Derivative Expansion *)
	(* Input: CD[ A^u, x^v ] *)
	(* Expected: Partial[A^u, x^v] + (Something containing Gamma) *)
	testCD = CD[A[\[ScriptCapitalU][\[Mu]]], x[\[ScriptCapitalU][\[Nu]]]];
	(* LOGIC: It must be a Sum. One part is Partials. The other part MUST contain Gamma. *)
	AssertTrue[MatchQ[testCD, Plus[Partials[_, _], _]] && !FreeQ[testCD, \[CapitalGamma]],
	  "Covariant Derivative Expansion"];
	  
	(* Power Rule *)
	AssertEqual[ExpandDerivatives[Partials[a[\[ScriptCapitalU][\[Mu]]]^2, x[\[ScriptCapitalU][\[Nu]]]]], 
	            2 a[\[ScriptCapitalU][\[Mu]]] * Partials[a[\[ScriptCapitalU][\[Mu]]], x[\[ScriptCapitalU][\[Nu]]]],
	            "Power Rule" ];
	
	(* Chain Rule (Linearity) *)
	(* Input: d(A+B)/dx *)
	testLinearity = ExpandDerivatives[Partials[A + B, x[\[ScriptCapitalU][\[Mu]]]]];
	AssertTrue[MatchQ[testLinearity, Plus[Partials[A, _], Partials[B, _]]],
	  "Differentiation Linearity"];
  ];

(* Covariant Derivative of a Scalar *)
Module[{phi, scalarCD},
  valence[phi] = noValence; (* Define as scalar *)
  scalarCD = CD[phi, x[\[ScriptCapitalU][\[Mu]]]];
  
  AssertEqual[scalarCD, Partials[phi, x[\[ScriptCapitalU][\[Mu]]]], 
    "CD of Scalar reduces to Partial"];
    
  AssertTrue[FreeQ[scalarCD, \[CapitalGamma]], 
    "Scalar CD contains no Connection Coefficients"];
];

(* Nested Covariant Derivatives (Stress Test for Alpha-Conversion) *)
Module[{nestedCD, allIndices, generatedDummies},
  
  nestedCD = CD[CD[A[\[ScriptCapitalU][\[Mu]]], x[\[ScriptCapitalU][\[Nu]]]], x[\[ScriptCapitalU][\[Rho]]]];
  
  (* Assertion: Check for generated dummy indices to ensure uniqueness *)
  allIndices = Cases[nestedCD, (\[ScriptCapitalU] | \[ScriptCapitalD])[s_Symbol] :> s, Infinity];
  
  (* Filter for symbols that look like generated Lambdas *)
  generatedDummies = Select[DeleteDuplicates[allIndices],
    StringMatchQ[SymbolName[#], "\[FormalLambda]" ~~ (DigitCharacter ..) ~~ EndOfString] ||
    StringMatchQ[SymbolName[#], "\[FormalLambda]$" ~~ __] &];
    
  (* We expect at least 2 distinct generated dummies (one for each CD layer) *)
  AssertTrue[Length[generatedDummies] >= 2, 
    "Nested CD Alpha-Conversion (Unique Indices Generated)"];
];
  


(* ::Chapter::Closed:: *)
(*Quotient Theorem*)


(* ========================================================================= *)
Print["\n--- SECTION 7: QUOTIENT THEOREM (Extraction) ---"];
(* ========================================================================= *)

Module[{expr1, expected1, expr2, expected2, formalS},

	(* Define the reserved symbol the engine now uses *)
	formalS = Symbol["\[FormalS]"];
	  
	(* Test 1: Simple Extraction *)
	(* T_u * A^u -> T_u *)
	expr1 = T[\[ScriptCapitalD][\[Mu]]] * A[\[ScriptCapitalU][\[Mu]]];
	
	(* Expect the formal index (i1) generated by the engine *)
	expected1 = T[\[ScriptCapitalD][formalS]]; 
	
	AssertEqual[ExtractCoefficient[expr1, A], expected1, 
	  "Quotient Extraction (Simple)"];
	
	(* Test 2: Distributed Terms (The Riemann Case) *)
	expr2 = R[\[ScriptCapitalD][\[Mu]]] * A[\[ScriptCapitalU][\[Mu]]] + 
	        S[\[ScriptCapitalD][\[Nu]]] * A[\[ScriptCapitalU][\[Nu]]];
	
	(* Expect Formal Indices (i1) because ExtractCoefficient resets counters per term *)
	(* Explicitly construct the expected result using the formal symbol i1 *)
	formalIndex = Symbol["\[FormalI]1"];
	expected2 = R[\[ScriptCapitalD][formalS]] + S[\[ScriptCapitalD][formalS]];
	
	AssertEqual[ExtractCoefficient[expr2, A], expected2, 
	  "Quotient Extraction (Distributed Dummies)"];
	  
	(* Test 3: Zero Case *)
	(* If A is not present, return 0 *)
	AssertEqual[ExtractCoefficient[B[\[ScriptCapitalU][\[Mu]]], A], 0, 
	  "Quotient Extraction (Zero Case)"];
  
];



(* ::Chapter::Closed:: *)
(*Tensor Form*)


(* ========================================================================= *)
Print["\n--- SECTION 8: TENSOR FORM ROBUSTNESS ---"];
(* ========================================================================= *)

Module[{weirdTerm, formatted, formalPattern, isClean},

  (* Stress Test: Create a term with arbitrary formal indices *)
  weirdTerm = CreateExtendedFormal["\[FormalQ]"] * CreateExtendedFormal["\[FormalZ]"] * CreateExtendedFormal["\[FormalI]99"];
  
  (* Apply TensorForm *)
  formatted = TensorForm[weirdTerm];

  (* THE FIX: Use CharacterRange to inspect the result *)
  formalPattern = CharacterRange["\[FormalA]", "\[FormalZ]"];

  (* Assertion: The output should NOT contain any Formal symbols anymore *)
  isClean = FreeQ[formatted, s_Symbol /; StringStartsQ[SymbolName[s], formalPattern]];

  AssertTrue[isClean, "TensorForm maps arbitrary formals (Q, Z, I99) to Greek"];

];



(* ::Chapter::Closed:: *)
(*Comma Notation Display Logic*)


(* ========================================================================= *)
Print["\n--- SECTION 9: COMMA NOTATION DISPLAY LOGIC ---"];
(* ========================================================================= *)

Module[{checkDisplay, box, hasParens, isSubscript},

    (* Helper to inspect the raw boxes generated by the FrontEnd *)
    checkDisplay[label_, expr_, shouldHaveParens_] := (
	    box = MakeBoxes[expr, StandardForm];
	    
	    (* 1. It must be a SubscriptBox (Comma notation) *)
	    isSubscript = MatchQ[box, SubscriptBox[_, _]];
	    
	    (* 2. Check for explicit parentheses in the first argument *)
	    (* look for RowBox[{"(", ... }] inside the box structure *)
	    hasParens = !FreeQ[box, "("];
	    
	    If[isSubscript && (hasParens === shouldHaveParens),
	      pass[label, DisplayForm@box],
	      fail[label, 
	        If[shouldHaveParens, 
	          "Subscript w/ Parens", 
	          "Subscript w/o Parens"], 
	        DisplayForm@box]
	    ];
    );

  (* TEST 1: The "Happy Path" (Simple Tensor) *)
  (* A^u_,v -> No Parens *)
  checkDisplay["Atomic Tensor", 
    Partials[A[\[ScriptCapitalU][\[Mu]]], x[\[ScriptCapitalU][\[Nu]]]], 
    False];

  (* TEST 2: The "Gamma Glue" Case (Product) *)
  (* (A^u Gamma)_,v -> MUST HAVE PARENS *)
  checkDisplay["Sealed Product (Gamma Glue)", 
    Partials[A[\[ScriptCapitalU][\[Mu]]] * \[CapitalGamma][\[ScriptCapitalU][a], \[ScriptCapitalD][b], \[ScriptCapitalD][c]], x[\[ScriptCapitalU][\[Nu]]]], 
    True];

  (* TEST 3: The "Linearity" Case (Sum) *)
  (* (A + B)_,v -> MUST HAVE PARENS *)
  checkDisplay["Sealed Sum", 
    Partials[A + B, x[\[ScriptCapitalU][\[Nu]]]], 
    True];

  (* TEST 4: The "Non-Linear" Case (Power) *)
  (* (A^2)_,v -> MUST HAVE PARENS *)
  checkDisplay["Sealed Power", 
    Partials[A^2, x[\[ScriptCapitalU][\[Nu]]]], 
    True];

  (* TEST 5: The "Funky" Case (Arbitrary Function) *)
  (* Sin[x]_,v -> No Parens (Head is Sin, not Times/Plus/Power) *)
  checkDisplay["Arbitrary Function (Sin)", 
    Partials[Sin[theta], x[\[ScriptCapitalU][\[Nu]]]], 
    False];

  (* TEST 6: Complex Nested Structure *)
  (* (A * (B+C))_,v -> MUST HAVE PARENS (Head is Times) *)
  checkDisplay["Nested Math (A*(B+C))", 
    Partials[A * (B + C), x[\[ScriptCapitalU][\[Nu]]]], 
    True];
    
];



(* ::Chapter::Closed:: *)
(*Einstein Summation (Contract & ContractAll)*)


(* ========================================================================= *)
Print["\n--- SECTION 10: EINSTEIN SUMMATION (Contract & ContractAll) ---"];
(* ========================================================================= *)

Module[{coords, exprSingle, exprDouble, exprMixed, exprPartial, 
     resSingle, resDouble, resPartial},
  
  (* Define dummy coordinates for deterministic summation *)
  coords = {1, 2};

  (* TEST 1: Single Contraction (Contract) *)
  (* A^u B_u -> A^1 B_1 + A^2 B_2 *)
  exprSingle = A[\[ScriptCapitalU][\[Mu]]] * B[\[ScriptCapitalD][\[Mu]]];
  resSingle = Contract[exprSingle, coords];
  
  AssertEqual[resSingle, 
    A[\[ScriptCapitalU][1]]*B[\[ScriptCapitalD][1]] + A[\[ScriptCapitalU][2]]*B[\[ScriptCapitalD][2]], 
    "Single Contraction (A^u B_u)"];

  (* TEST 2: Recursive vs One-Shot (The Double Contraction) *)
  (* A^u B_u C^v D_v *)
  (* don't use C and D, they're reserved. use double-struck characters *)
  exprDouble = A[\[ScriptCapitalU][\[Mu]]] * B[\[ScriptCapitalD][\[Mu]]] * \[DoubleStruckCapitalC][\[ScriptCapitalU][\[Nu]]] * \[DoubleStruckCapitalD][\[ScriptCapitalD][\[Nu]]];
  
  (* Contract (One-Shot): Should only expand ONE pair (e.g., Mu), leaving Nu symbolic *)
  Module[{resOneShot, hasMu, hasNu},
    resOneShot = Contract[exprDouble, coords];
    hasMu = !FreeQ[resOneShot, \[Mu]];
    hasNu = !FreeQ[resOneShot, \[Nu]];
    
    (* One should be gone, one should remain *)
    If[Xor[hasMu, hasNu],
      pass["Contract (One-Shot) expanded exactly one pair", {hasMu, hasNu}],
      fail["Contract (One-Shot) behavior incorrect", "One pair remaining", {hasMu, hasNu}]
    ];
  ];

  (* ContractAll (Recursive): Should expand BOTH pairs *)
  resDouble = ContractAll[exprDouble, coords];
  If[FreeQ[resDouble, \[Mu]] && FreeQ[resDouble, \[Nu]],
     pass["ContractAll expanded all pairs recursively", True],
     fail["ContractAll left symbolic indices", "None", resDouble]
  ];

  (* TEST 3: Variance Handling in Partials *)
  (* d(A^u)/dx^u -> Treated as A^u and D_u *)
  (* This validates the canonicalization pipeline: Partials[..., x[U]] -> PD[..., D] *)
  exprPartial = Partials[A[\[ScriptCapitalU][\[Mu]]], x[\[ScriptCapitalU][\[Mu]]]];
  resPartial = ContractAll[exprPartial, coords];
  
  AssertEqual[resPartial, 
    Partials[A[\[ScriptCapitalU][1]], x[\[ScriptCapitalU][1]]] + Partials[A[\[ScriptCapitalU][2]], x[\[ScriptCapitalU][2]]], 
    "Variance Handling (Divergence: d(A^u)/dx^u)"];

  (* TEST 4: Mixed Sum/Product *)
  (* A^u B_u + C^v D_v *)
  (* ContractAll should handle distribution over Plus *)
  exprMixed = \[DoubleStruckCapitalA][\[ScriptCapitalU][\[Mu]]] * \[DoubleStruckCapitalB][\[ScriptCapitalD][\[Mu]]]  +  \[DoubleStruckCapitalC][\[ScriptCapitalU][\[Nu]]] * \[DoubleStruckCapitalD][\[ScriptCapitalD][\[Nu]]];
  (* _Symbol pattern excludes the numerical indices used in this test *) 
  AssertTrue[FreeQ[ContractAll[exprMixed, coords], \[ScriptCapitalU][_Symbol]], 
    "Distribution over Sum (A^u B_u + C^v D_v)"];

  (* TEST 5: Protection of Free Indices *)
  (* A^u B_v (No match) -> Should remain untouched *)
  AssertEqual[ContractAll[A[\[ScriptCapitalU][\[Mu]]] * B[\[ScriptCapitalD][\[Nu]]], coords], 
    A[\[ScriptCapitalU][\[Mu]]] * B[\[ScriptCapitalD][\[Nu]]], 
    "Free Indices Protected (No Contraction)"];

  (* TEST 6: Coordinate Exclusion *)
  (* Ensure the 'coords' themselves are not treated as summation indices if they appear in the expression *)
  (* e.g. if coords={t,r}, don't try to contract 't' in A^t *)
  Module[{badExpr},
    badExpr = A[\[ScriptCapitalU][1]]; (* 1 is a coord *)
    AssertEqual[
      ContractAll[badExpr, coords], 
      badExpr, 
      "Coordinates Excluded from Scanner"];
  ];
];



(* ::Chapter::Closed:: *)
(*Electrodynamics and Gauge Invariance*)


(* ========================================================================= *)
Print["\n--- SECTION 11: ELECTRODYNAMICS & GAUGE INVARIANCE ---"];
(* ========================================================================= *)

Module[{gaugeTransforms, \[Psi], A, \[CapitalLambda], e, 
  cde, transformedCde, expectedCde, 
  F, transformedF},

  (* --- TEST 1: Covariant Derivative is Covariant --- *)
  (* prove: Subscript[D, \[Mu]]\[Psi]' == \[ExponentialE]^(\[ImaginaryI]\[VeryThinSpace]e\[VeryThinSpace]\[CapitalLambda]) Subscript[D, \[Mu]]\[Psi] *)
  
  gaugeTransforms = {
    \[Psi] -> E^(I e \[CapitalLambda]) * \[Psi],
    A[\[ScriptCapitalD][idx_]] :> A[\[ScriptCapitalD][idx]] + Partials[\[CapitalLambda], x[\[ScriptCapitalU][idx]]]};
  
  (* invoke gauge-theory CD with const charge 'e' and gauge connection 'A' *) 
  cde = CD[\[Psi], x[\[ScriptCapitalU][\[Mu]]], e, A];
  
  transformedCde = (cde /. 
        gaugeTransforms //. 
      differentiationRules /. 
    {Partials[e, _] :> 0}) // Expand;
     
  expectedCde = E^(I e \[CapitalLambda]) * cde // Expand;
  
  AssertEqual[transformedCde, expectedCde, 
    "U(1) Gauge Covariance ((\[Psi] \!\(\*SubscriptBox[\(D\), \(\[Mu]\)]\)\!\(\*SuperscriptBox[\()\), \(\[Prime]\),\nMultilineFunction->None]\)\[Equal]\!\(\*SuperscriptBox[\(\[ExponentialE]\), \(\[ImaginaryI]e\[CapitalLambda]\)]\) \[Psi] \!\(\*SubscriptBox[\(D\), \(\[Mu]\)]\))"];

  (* --- TEST 2: Gauge Invariance of Field Strength Subscript[F, \[Mu]\[Nu]] --- *)
  (* prove: Subscript[F', \[Mu]\[Nu]] == Subscript[F, \[Mu]\[Nu]] *)

  (* The Field Strength Tensor *)
  F = Partials[A[\[ScriptCapitalD][n]], x[\[ScriptCapitalU][m]]] - Partials[A[\[ScriptCapitalD][m]], x[\[ScriptCapitalU][n]]];
  
  (* Transform and Expand *)
  transformedF = (F /. gaugeTransforms // ExpandDerivatives) // Expand;
  
  AssertEqual[transformedF, F, 
    "Field Strength Invariance (F' == F)"];
];

Module[{geometricW, mixedGaugeRules, gW, \[CapitalLambda], W, Z, transformedW, expectedW},
  (* --- TEST 3: Weinberg-Salam Mixed Covariance (Vector Field W^\[Alpha]) --- *)
  (* Prove that CD handles Spacetime AND Gauge indices simultaneously. *)
  (* prove: Subscript[D, \[Mu]] W'^\[Alpha] == \[ExponentialE]^(\[ImaginaryI]\[ThinSpace]gW\[VeryThinSpace]\[CapitalLambda]) * Subscript[D, \[Mu]] W^\[Alpha] *)
    
  (* Charged Vector Boson W^\[Alpha] *)
  (* With spacetime index \[Alpha]; CD generates \[CapitalGamma] terms *)
  geometricW = CD[W[\[ScriptCapitalU][\[Alpha]]], x[\[ScriptCapitalU][\[Mu]]], gW, Z];
  
  (* Transformation Rules *)
  (* W rotates by phase \[CapitalLambda]. Z shifts by \[PartialD]\[CapitalLambda]. \[CapitalGamma] is inert to gauge choice. *)
  mixedGaugeRules = {
    W[\[Mu]_] :> E^(I\[ThinSpace]gW \[CapitalLambda]) * W[\[Mu]],
    Z[\[ScriptCapitalD][\[Mu]_]] :> Z[\[ScriptCapitalD][\[Mu]]] + Partials[\[CapitalLambda], x[\[ScriptCapitalU][\[Mu]]]]};

  transformedW = (geometricW /. 
        mixedGaugeRules //. 
      differentiationRules /. 
    {Partials[gW, _] :> 0}) // Expand;
  
  (* Expectation *)
  (* The original mixed object, just phase-rotated *)
  expectedW = E^(I\[ThinSpace]gW \[CapitalLambda]) * geometricW // Expand;
  
  AssertEqual[transformedW, expectedW, 
    "Weinberg-Salam Mixed Covariance (Geometric + Gauge)"];
];



(* ::Chapter::Closed:: *)
(*Riemann Curvature Derivation*)


(* ========================================================================= *)
Print["\n--- SECTION 12: RIEMANN CURVATURE DERIVATION ---"];
(* ========================================================================= *)

Module[{comm, term1, term2, A, calculatedRiemann, expectedRiemann, formalS, \[Lambda]},

  term1 = CD[CD[A[\[ScriptCapitalU][\[Mu]]], x[\[ScriptCapitalU][\[Nu]]]], x[\[ScriptCapitalU][\[Rho]]]];
  term2 = CD[CD[A[\[ScriptCapitalU][\[Mu]]], x[\[ScriptCapitalU][\[Rho]]]], x[\[ScriptCapitalU][\[Nu]]]];
  comm = term1 - term2;

  calculatedRiemann = (comm // ExpandDerivatives // Expand // CanonicalizeIndices) //. torsionRules;
   
  calculatedRiemann   = ExtractCoefficient[calculatedRiemann, A];

  expectedRiemann = 
   ( ( Partials[\[CapitalGamma][\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Nu]], \[ScriptCapitalD][\[FormalS]]], x[\[ScriptCapitalU][\[Rho]]]]     - 
       Partials[\[CapitalGamma][\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Rho]], \[ScriptCapitalD][\[FormalS]]], x[\[ScriptCapitalU][\[Nu]]]]     + 
       \[CapitalGamma][\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Rho]], \[ScriptCapitalD][\[Lambda]]] * \[CapitalGamma][\[ScriptCapitalU][\[Lambda]], \[ScriptCapitalD][\[Nu]], \[ScriptCapitalD][\[FormalS]]]     - 
       \[CapitalGamma][\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Nu]], \[ScriptCapitalD][\[Lambda]]] * \[CapitalGamma][\[ScriptCapitalU][\[Lambda]], \[ScriptCapitalD][\[Rho]], \[ScriptCapitalD][\[FormalS]]] ) //. torsionRules );

  AssertEqual[calculatedRiemann, expectedRiemann, 
    "Riemann Derivation matches Textbook"];
];



(* ::Chapter::Closed:: *)
(*Geometry Operators: Gradient, Laplacian*)


(* ========================================================================= *)
Print["\n--- SECTION 13: GEOMETRY OPERATORS (Gradient, Laplacian) ---"];
(* ========================================================================= *)

Block[{coords, flatMetric, flatRules, flatInvRules, sqrtDetFlat, 
       t, r, \[Theta], \[Phi], gDD, gUU, \[ScriptCapitalD], \[ScriptCapitalU]},

  (* 1. DEFINE THE TEST ENVIRONMENT: Flat Space in Spherical Coords *)
  (* ds^2 = -dt^2 + dr^2 + r^2 dtheta^2 + r^2 sin^2(theta) dphi^2 *)
  coords = {t, r, \[Theta], \[Phi]};
  
  (* Diagonal Metric *)
  flatMetric = DiagonalMatrix[{-1, 1, r^2, r^2 Sin[\[Theta]]^2}];
  
  (* Generate Rules using the toolkit's own MatrixToUDRules *)
  flatRules = MatrixToUDRules[flatMetric, gDD, \[ScriptCapitalD], coords];
  flatRules = Join[flatRules, {gDD[__] -> 0}];
  
  flatInvRules = MatrixToUDRules[Inverse[flatMetric], gUU, \[ScriptCapitalU], coords];
  flatInvRules = Join[flatInvRules, {gUU[__] -> 0}];
  
  (* Determinant Factor (sqrt[-g] = r^2 sin(theta)) *)
  sqrtDetFlat = r^2 Sin[\[Theta]];

  (* TEST 1: CalculateGammaComponent (The Factory Wrapper) *)
  (* Expected: Gamma^r_thetatheta = -r *)
  AssertEqual[
    CalculateGammaComponent[r, \[Theta], \[Theta], gDD, flatRules, gUU, flatInvRules, coords],
    -r,
    "Gamma Factory (Spherical \!\(\*SubscriptBox[\(\[CapitalGamma]\), \(r\ \[Theta]\ \[Theta]\)]\))"
  ];

  (* TEST 2: GradRaised (Contravariant Derivative) *)
  (* Function f(r) = r^2. Gradient_mu = {0, 2r, 0, 0}. *)
  (* g^rr = 1, so Gradient^r = 1 * 2r = 2r. *)
  AssertEqual[
    GradRaised[r^2, gUU, flatInvRules, coords],
    {0, 2*r, 0, 0},
    "GradRaised (Radial Function \!\(\*SuperscriptBox[\(r\), \(2\)]\))"
  ];

  (* TEST 3: GradSquared (Norm of Gradient) *)
  (* Function f(t, r) = t + r. *)
  (* Norm = g^tt(1)^2 + g^rr(1)^2 = -1 + 1 = 0 (Null Vector). *)
  AssertEqual[
    GradSquared[t + r, gUU, flatInvRules, coords],
    0,
    "GradSquared (Null Vector Norm)"
  ];

  (* TEST 4: ScalarLaplacian (Harmonic Function Check) *)
  (* In 3D Flat Space, 1/r is harmonic everywhere except origin. *)
  AssertEqual[
    ScalarLaplacian[1/r, gUU, flatInvRules, sqrtDetFlat, coords],
    0,
    "ScalarLaplacian (Harmonic 1/r)"
  ];

  (* TEST 5: ScalarLaplacian (Polynomial Check) *)
  (* f(r) = r^2. Laplacian = 1/r^2 d_r(r^2 d_r r^2) = 1/r^2 d_r(2r^3) = 6. *)
  AssertEqual[
    ScalarLaplacian[r^2, gUU, flatInvRules, sqrtDetFlat, coords],
    6,
    "ScalarLaplacian (Polynomial \!\(\*SuperscriptBox[\(r\), \(2\)]\))"
  ];
];



(* ::Chapter::Closed:: *)
(*Formal Symbols*)


(* ========================================================================= *)
Print["\n--- SECTION 14: FORMAL SYMBOLS ---"];
(* ========================================================================= *)

(*SETUP*)
Protect[\[FormalI]123];
protectedDummy=CreateExtendedFormal["\[FormalJ]888"];
CreateExtendedFormal[\[FormalI]999];



(* ::Section:: *)
(*Identity and Built-In Protection*)


Print["--- (1) Identity & Built-in Protection ---"];

AssertEqual[checkFormalIdentity[\[FormalI]],"Pure","input: formal symbol literal"];
AssertEqual[checkFormalIdentity["\[FormalI]"],"Pure","input: formal symbol string"];
AssertEqual[checkFormalIdentity[\[FormalJ]1234],"Extended","input: extended symbol literal"];
AssertEqual[checkFormalIdentity["\[FormalJ]2345"],"Extended","input: extended symbol string"];
AssertEqual[checkFormalIdentity[foo],"Neither","non-formal symbol literal"];
AssertEqual[checkFormalIdentity["foo"],"Neither","non-formal string"];
AssertEqual[checkFormalIdentity[""],"Neither","non-formal empty string"];
AssertEqual[checkFormalIdentity[],$Failed,"empty string"];
AssertEqual[checkFormalIdentity[42],$Failed,"integer"];

AssertProtected[\[FormalI],"Identity: Built-in \[FormalI] is Protected"];

AssertTrue[FormalSymbolQ[\[FormalI]],"Identity: Latin Pure Formal recognized"];

AssertTrue[FormalSymbolQ[\[FormalAlpha]],"Identity: Greek Pure Formal recognized"];



(* ::Section:: *)
(*Pointer and Construction Logic*)


Print["--- (2) Pointer & Construction Logic ---"];

AssertTrue[FormalSymbolExtendedQ[Evaluate@protectedDummy],
"Pointer: Variable holding symbol recognized"];

AssertProtected[protectedDummy,
"Pointer: Target Extended symbol \[FormalJ]888 is Protected"];

AssertMatchQ[Quiet[CreateExtendedFormal[""]],$Failed,
"Constructor: Rejects empty string"];



(* ::Section:: *)
(*Structural Variety & Boundaries*)


Print["--- (3) Structural Variety & Boundaries ---"];

AssertTrue[FormalSymbolExtendedQ[\[FormalI]123],"Structure: Numbered dummy recognized"];

AssertFalse[FormalSymbolExtendedQ[Partial\[FormalX]],"Structure: Prefixed symbol rejected"];

AssertFalse[FormalSymbolExtendedQ[\[FormalI]],"Boundary: Pure is NOT Extended"];

AssertFalse[FormalSymbolQ[\[FormalI]123],"Boundary: Extended is NOT Pure"];

AssertFalse[FormalSymbolQ[foo], "Boundary: Ordinary Symbol is not Pure System formal"];
AssertFalse[FormalSymbolQ["foo"], "Boundary: String is not Pure System formal symbol"];
AssertFalse[FormalSymbolQ[42], "Boundary: Number is not Pure System formal symbol"];

AssertFalse[FormalSymbolExtendedQ[foo], "Boundary: Ordinary Symbol is not Extended formal"];
AssertFalse[FormalSymbolExtendedQ["foo"], "Boundary: String is not Extended formal symbol"];
AssertFalse[FormalSymbolExtendedQ[42], "Boundary: Number is not Extended formal symbol"];



(* ::Section:: *)
(*Block-Scoping Integrity (Hold Check)*)


Print["--- (4) Scoping Integrity (The 'Hold' Check) ---"];

Block[{x=10,\[FormalI]=5,\[FormalI]123=7},

	AssertTrue[FormalSymbolQ[\[FormalI]],
	"Scoping: Built-in identity persists despite value 5"];
	
	AssertFalse[FormalSymbolExtendedQ[\[FormalI]123],
	"Non-system symbol not protected, thus not extended."];
	
	Protect[\[FormalI]123];
	AssertTrue[FormalSymbolExtendedQ[\[FormalI]123],
	"Scoping: Extended identity after explicit protection"]
];



(* ::Section:: *)
(*Constructor Validation*)


Print["--- (5) Constructor Validation ---"];

AssertMatchQ[Quiet[CreateExtendedFormal["\[FormalK]\[FormalJ]888"]],$Failed,
"Safety: Reject double formal prefix"];

AssertMatchQ[Quiet[CreateExtendedFormal["plainVar"]],$Failed,
"Safety: Reject plain string"];

AssertTrue[Head[CreateExtendedFormal["\[FormalK]888"]]===Symbol,
"Structure: Accept single formal prefix"];



(* ::Section:: *)
(*Start-Character Enforcement*)


Print["--- (6) Start-Character Enforcement ---"];

AssertFalse[FormalSymbolExtendedQ["var\[FormalI]123"],
"Strictness: Reject buried formal (ExtendedQ -> False)"];

AssertFalse[FormalSymbolQ["var\[FormalI]123"],
"Strictness: Reject buried formal (PureQ -> False)"];

AssertFalse[FormalSymbolExtendedQ["x\[FormalI]"],
"Strictness: Reject suffix formal (ExtendedQ -> False)"];

AssertFalse[FormalSymbolQ["x\[FormalI]"],
"Strictness: Reject suffix formal (PureQ -> False)"];

Print["--- (6) Polymorphism ---"]

AssertTrue[FormalSymbolQ["\[FormalI]"],
"Polymorphism: String \"\[FormalI]\" is a Pure symbol"];

AssertTrue[FormalSymbolExtendedQ["\[FormalI]999"],
"Polymorphism: String \"\[FormalI]999\" is an Extended symbol"];



(* ::Chapter::Closed:: *)
(*Indexed Objects and Contractions*)


(* ========================================================================= *)
Print["\n--- SECTION 15: GENERALIZED CONTRACTION & ARITY DETECTION ---"];
(* ========================================================================= *)

Module[{mockGamma, testCoords, resFunction, resSlots, resSymbol, resMismatch},

  (* 1. Arity Detection Tests *)
  AssertEqual[determineArity[x |-> x^2 + x], 1, 
    "Arity: Explicit Function (x |-> ...)"];
    
  AssertEqual[determineArity[{x, y} |-> x*y], 2, 
    "Arity: Explicit List Function ({x,y} |-> ...)"];
    
  AssertEqual[determineArity[#1 + #2^2 &], 2, 
    "Arity: Slot Function (#1, #2)"];
    
  AssertEqual[determineArity[#^2 &], 1, 
    "Arity: Single Slot Function (#)"];

  (* Mock Symbol with DownValues *)
  ClearAll[mockGamma];
  mockGamma[a_, b_, c_] := a + b + c;
  AssertEqual[determineArity[mockGamma], 3, 
    "Arity: DownValues Inspection (mockGamma[a,b,c])"];


  (* 2. ContractIndexedAll Tests *)
  testCoords = {1, 2};
  
  (* Case A: Explicit Function *)
  resFunction = ContractIndexedAll[{j, k} |-> (j + k), testCoords];
  (* Sum[j+k, {j,1,2}, {k,1,2}] = (1+1)+(1+2)+(2+1)+(2+2) = 2+3+3+4 = 12 *)
  AssertEqual[resFunction, 12, "ContractIndexedAll: Function Syntax"];

  (* Case B: Slot Syntax *)
  resSlots = ContractIndexedAll[#1 * #2 &, testCoords];
  (* Sum[j*k] = 1*1 + 1*2 + 2*1 + 2*2 = 1+2+2+4 = 9 *)
  AssertEqual[resSlots, 9, "ContractIndexedAll: Slot Syntax"];

  (* Case C: Symbol Syntax *)
  (* Sum[a+b+c, {a,1,2}, {b,1,2}, {c,1,2}] *)
  (* Sum of elements is 12. 3 indices -> 2*12 + 2*12 + 2*12? No, let's trust Mathematica math. *)
  (* Sum[a,{a,1,2}]*4 + ... = 3*4 + 3*4 + 3*4 = 36 *)
  resSymbol = ContractIndexedAll[mockGamma, testCoords];
  AssertEqual[resSymbol, 36, "ContractIndexedAll: Symbol/DownValue Syntax"];

  (* Case D: Manual Override *)
  (* Force a 1-argument function to be treated as 2-argument (iterating twice) *)
  (* The function ignores the second arg, but Sum iterates it. *)
  resMismatch = ContractIndexedAll[#1 &, testCoords, 2];
  (* Sum[j, {j,1,2}, {k,1,2}] = (1+2)*2 = 6 *)
  AssertEqual[resMismatch, 6, "ContractIndexedAll: Manual Arity Override"];

];


(* ::Chapter:: *)
(*Gauge Theory Equations of Motion*)


(* ========================================================================= *)
Print["\n--- SECTION 16: GAUGE THEORY EOM (Indexer & UD) ---"];
(* ========================================================================= *)

Module[{
    coords, t, r, \[Theta], vIdx, V, k,
    gMat, gInvMat,
    gIndexer, gInvIndexer, \[CapitalGamma]Indexer,
    gDD, gUU, gDDRules, gUURules,
    expectedPot, expectedCurv, expectedAbs,
    resPotIdx, resCurvIdx, resAbsIdx,
    resPotUD, resCurvUD, resAbsUD
  },

  (* 1. Setup 2D Polar Space as the Testbed *)
  (* This tests the identical mathematical structures as the baton's SO(3) metric *)
  coords = {r, \[Theta]};
  gMat = DiagonalMatrix[{1, r^2}];
  gInvMat = DiagonalMatrix[{1, 1/r^2}];

  (* Indexer Setup *)
  gIndexer = MakeIndexer[gMat, coords];
  gInvIndexer = MakeIndexer[gInvMat, coords];
  \[CapitalGamma]Indexer[\[Sigma]_, \[Mu]_, \[Nu]_] := ChristoffelsFromMetricIndexer[gIndexer, gInvIndexer, \[Sigma], \[Mu], \[Nu], coords];
  
  (* Note: Indexer requires pre-injected velocities to mimic the notebook logic *)
  vIdx = MakeIndexer[{r'[t], \[Theta]'[t]}, coords];

  (* UD Setup *)
  gDDRules = MatrixToUDRules[gMat, gDD, \[ScriptCapitalD], coords];
  gDDRules = Join[gDDRules, {gDD[__] -> 0}];
  gUURules = MatrixToUDRules[gInvMat, gUU, \[ScriptCapitalU], coords];
  gUURules = Join[gUURules, {gUU[__] -> 0}];

  (* 2. Define Physics & Expected Results *)
  (* Potential with explicit time dependence to stress-test the `injectTime` logic *)
  V = (1/2) * k * r^2 * t; 

  (* Expected Force from Potential: F^\[Mu] = -g^\[Mu]\[Nu] \!\(
\*SubscriptBox[\(\[PartialD]\), \(\[Nu]\)]V\) *)
  (* The \[Theta] component is perfectly 0, testing zero-handling *)
  expectedPot = {-k * r[t] * t, 0};
  
  (* Expected Inertial Force: \[CapitalGamma]^\[Mu]_\[Nu]\[Lambda] v^\[Nu] v^\[Lambda] *)
  (* Yields classical centrifugal (-r \[Theta]'^2) and Coriolis (2/r r' \[Theta]') forces *)
  expectedCurv = {-r[t] * \[Theta]'[t]^2, (2/r[t]) * r'[t] * \[Theta]'[t]};
  
  (* Expected Absolute Derivative: Coordinate Accel + Inertial Force *)
  expectedAbs = {r''[t] - r[t] * \[Theta]'[t]^2, \[Theta]''[t] + (2/r[t]) * r'[t] * \[Theta]'[t]};

  (* 3. Test Indexer Sector *)
  resPotIdx = PotentialScalarFieldFromMetricIndexer[V, gInvIndexer, t, coords] // Simplify;
  AssertEqual[resPotIdx, expectedPot, "PotentialScalarFieldFromMetricIndexer (2D Polar)"];

  resCurvIdx = CurvatureForcesFromConnectionIndexer[vIdx, \[CapitalGamma]Indexer, t, coords] // Simplify;
  AssertEqual[resCurvIdx, expectedCurv, "CurvatureForcesFromConnectionIndexer (2D Polar)"];

  resAbsIdx = AbsoluteDerivativeFromConnectionIndexer[vIdx, \[CapitalGamma]Indexer, t, coords] // Simplify;
  AssertEqual[resAbsIdx, expectedAbs, "AbsoluteDerivativeFromConnectionIndexer (2D Polar)"];

  (* 4. Test UD Sector *)
  resPotUD = PotentialScalarFieldUD[V, gUU, gUURules, t, coords] // Simplify;
  AssertEqual[resPotUD, expectedPot, "PotentialScalarFieldUD (2D Polar)"];

  resCurvUD = CurvatureForcesUD[gDD, gDDRules, gUU, gUURules, t, coords] // Simplify;
  AssertEqual[resCurvUD, expectedCurv, "CurvatureForcesUD (2D Polar)"];

  resAbsUD = AbsoluteDerivativeUD[gDD, gDDRules, gUU, gUURules, t, coords] // Simplify;
  AssertEqual[resAbsUD, expectedAbs, "AbsoluteDerivativeUD (2D Polar)"];
  
  (* 5. Corner Case: 1D Flat Space (Stress Test Arity and Degenerate Collapse) *)
  Module[{coords1D, V1D, gMat1D, gInvMat1D, gIdx1D, gInvIdx1D, \[CapitalGamma]Idx1D, v1D, expectedAbs1D, x},
    coords1D = {x};
    V1D = 0;
    gMat1D = {{1}};
    gInvMat1D = {{1}};
    
    gIdx1D = MakeIndexer[gMat1D, coords1D];
    gInvIdx1D = MakeIndexer[gInvMat1D, coords1D];
    \[CapitalGamma]Idx1D[\[Sigma]_, \[Mu]_, \[Nu]_] := ChristoffelsFromMetricIndexer[gIdx1D, gInvIdx1D, \[Sigma], \[Mu], \[Nu], coords1D];
    v1D = MakeIndexer[{x'[t]}, coords1D];
    
    expectedAbs1D = {x''[t]};
    
    AssertEqual[
      AbsoluteDerivativeFromConnectionIndexer[v1D, \[CapitalGamma]Idx1D, t, coords1D] // Simplify, 
      expectedAbs1D, 
      "AbsoluteDerivativeFromConnectionIndexer (1D Corner Case)"
    ];
  ];
];


(* ::Chapter::Closed:: *)
(*Summary*)


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


