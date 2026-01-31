(* ::Package:: *)

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
   
fail[label_, expected_, actual_]:=
  (FailCount++;
   Print["[", Style["FAIL", Red], "] ", label, 
     "\n\tExpected ", expected,
     "\n\tGot ", actual]);

AssertValence[expr_, expected_, label_] :=
  Module[{v = valence[expr]},
   If[v === expected,
    pass[label, v],
    fail[label, expected, v]]];

AssertEqual[actual_, expected_, label_] :=
  Module[{canAct = CanonicalizeIndices[actual], canExp = CanonicalizeIndices[expected]},
   If[canAct === canExp,
    pass[label, canAct],
    fail[label, canExp, canAct]]];
    
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
Print["\n--- SECTION 2: ALGEBRAIC CANONICALIZATION ---"];
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
Print["\n--- SECTION 3: PHYSICS, METRIC & TRANSFORMATIONS ---"];
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
    
(* TEST: Associativity of Contraction (Triple Metric Sandwich) *)
(* Subscript[g, \[Mu]\[Alpha]] g^\[Alpha]\[Beta] Subscript[g, \[Beta]\[Nu]] \[LongRightArrow] Subscript[g, \[Mu]\[Nu]] *)
Module[{termSandwich},
  termSandwich = g[\[ScriptCapitalD][\[Mu]], \[ScriptCapitalD][\[Alpha]]] * g[\[ScriptCapitalU][\[Alpha]], \[ScriptCapitalU][\[Beta]]] * g[\[ScriptCapitalD][\[Beta]], \[ScriptCapitalD][\[Nu]]];
                 
  AssertEqual[termSandwich //. metricRules, g[\[ScriptCapitalD][\[Mu]], \[ScriptCapitalD][\[Nu]]], 
    "Triple Metric Sandwich (g.g.g -> g)"]];

(* TEST: Delta Contracting on Gamma *)
(* \[CapitalGamma]^r * Subscript[\[Delta]^s, r] \[LongRightArrow] \[CapitalGamma]^s *)
Module[{gammaTerm},
  gammaTerm = \[CapitalGamma][\[ScriptCapitalU][r], \[ScriptCapitalD][a], \[ScriptCapitalD][b]] * \[Delta][\[ScriptCapitalU][s], \[ScriptCapitalD][r]];
  (* NOTE: Use //. metricRules for arbitrary tensors *)
  AssertEqual[gammaTerm //. metricRules, \[CapitalGamma][\[ScriptCapitalU][s], \[ScriptCapitalD][a], \[ScriptCapitalD][b]], 
    "Delta-Gamma Contraction"]];
    
Print["\n=== Metric Differentiation Rules ==="];

(* ========================================================================= *)
(* Metric Differentiation & Robust Contraction                               *)
(* Verify:                                                                   *)
(* 1. Permissive Contraction (Index alignment)                               *)
(* 2. Metric Symmetry (g_ab = g_ba)                                          *)
(* 3. Delta-on-Derivative Chain Rule                                         *)
(* ========================================================================= *)

(* Assuming 'metricDifferentiationRules' and 'metricRules' are defined in v1.9.0 *)
allMetricRules = (metricRules ~Join~ metricDifferentiationRules);

(* ------------------------------------------------------------------------- *)
(* TEST 1: Permissive Contraction ("Index Flip")                             *)
(* ------------------------------------------------------------------------- *)
test1Expr = g[\[ScriptCapitalD][mu], \[ScriptCapitalD][lam]] * g[\[ScriptCapitalU][mu], \[ScriptCapitalU][nu]];
test1Result = test1Expr //. allMetricRules // Simplify;
test1Expected = \[Delta][\[ScriptCapitalU][nu], \[ScriptCapitalD][lam]];

AssertEqual[test1Result, test1Expected, "Permissive Contraction"]

(* ------------------------------------------------------------------------- *)
(* TEST 2: Metric Symmetry inside Derivatives                                *)
(* Problem: d(g_uv) - d(g_vu) should be 0.                                   *)
(* ------------------------------------------------------------------------- *)
test2Expr = Partials[g[\[ScriptCapitalD][mu], \[ScriptCapitalD][nu]], x[\[ScriptCapitalU][rho]]] - 
            Partials[g[\[ScriptCapitalD][nu], \[ScriptCapitalD][mu]], x[\[ScriptCapitalU][rho]]];
test2Result = test2Expr //. allMetricRules // Simplify;

AssertEqual[test2Result, 0, "Metric Symmetry in Derivatives"]

(* ------------------------------------------------------------------------- *)
(* TEST 3: The Delta Chain Rule                                              *)
(* Problem: delta^s_m * d/dx^s must become d/dx^m                            *)
(* ------------------------------------------------------------------------- *)
test3Expr = \[Delta][\[ScriptCapitalU][sig], \[ScriptCapitalD][mu]] * Partials[f[x], x[\[ScriptCapitalU][sig]]];
test3Result = test3Expr //. allMetricRules // Simplify;
test3Expected = Partials[f[x], x[\[ScriptCapitalU][mu]]];

AssertEqual[test3Result, test3Expected, "Delta Chain Rule"]

(* ------------------------------------------------------------------------- *)
(* TEST 4: Full Metric Compatibility                                         *)
(* Problem: CD[g] must vanish identically for Levi-Civita connection.        *)
(* ------------------------------------------------------------------------- *)
(* 1. Define CD of Metric *)
rawCD = CD[g[\[ScriptCapitalD][mu], \[ScriptCapitalD][nu]], x[\[ScriptCapitalU][rho]]];

compatibilityResult = ((rawCD /. ruleLeviCivita) // Expand) //. allMetricRules;

AssertEqual[compatibilityResult, 0, "Metric Compatibility"]

(* ------------------------------------------------------------------------- *)
(* TEST 5: Levi-Civita Dummy Index Safety (The "Gamma Squared" Test)         *)
(* Problem: Gamma * Gamma must generate two DISTINCT summation indices.      *)
(* ------------------------------------------------------------------------- *)
Module[{gammaProd, expandedProd, dummies},
  gammaProd = \[CapitalGamma][\[ScriptCapitalU][mu], \[ScriptCapitalD][a], \[ScriptCapitalD][b]] * \[CapitalGamma][\[ScriptCapitalU][nu], \[ScriptCapitalD][c], \[ScriptCapitalD][d]];
  
  expandedProd = gammaProd /. ruleLeviCivita;
  
  (* Extract all Upper indices generated in the expansion (the summation indices) *)
  dummies = Cases[expandedProd, g[\[ScriptCapitalU][_], \[ScriptCapitalU][s_]] :> s, Infinity];
  
  (* We expect 2 dummies. If Unique works, they must NOT be equal. *)
  If[Length[dummies] == 2 && dummies[[1]] =!= dummies[[2]],
    pass["Levi-Civita Index Safety (Unique indices generated)", dummies],
    fail["Levi-Civita Index Safety", "Distinct Indices", dummies]
  ];
];

(* ========================================================================= *)
Print["\n--- SECTION 4: METRIC EQUIVALENCE (\!\(\*SuperscriptBox[\(A\), \(\[Mu]\)]\) \!\(\*SubscriptBox[\(B\), \(\[Mu]\)]\) == \!\(\*SubscriptBox[\(A\), \(\[Nu]\)]\) \!\(\*SuperscriptBox[\(B\), \(\[Nu]\)]\)) ---"];
(* ========================================================================= *)

Module[{termUp, termDown},
  termUp = A[\[ScriptCapitalU][\[Mu]]] * B[\[ScriptCapitalD][\[Mu]]]; 
  termDown = A[\[ScriptCapitalD][\[Nu]]] * B[\[ScriptCapitalU][\[Nu]]];

  Module[{
     is1 = CanonicalizeIndices[termUp], 
     is2 = CanonicalizeIndices[termDown]},
    If[is1 =!= is2,
      pass["Structural Distinction \!\(\*SuperscriptBox[\(A\), \(\[Mu]\)]\) \!\(\*SubscriptBox[\(B\), \(\[Mu]\)]\) != \!\(\*SubscriptBox[\(A\), \(\[Nu]\)]\) \!\(\*SuperscriptBox[\(B\), \(\[Nu]\)]\) initially", {is1, is2}],
      fail["Structural Distinction Failed", is1, is2]]];

  Module[{expandRule, termDownExpanded, termDownReduced, termFinal},
    expandRule = A[\[ScriptCapitalD][idx_]] :>
       Module[{fresh = Unique["\[Alpha]"]},
        g[\[ScriptCapitalD][idx], \[ScriptCapitalD][fresh]] * Hold[A[\[ScriptCapitalU][fresh]]]];

    Print[Style["\[ScriptCapitalU]\[ScriptCapitalD] will rearrange terms", Gray]];
    termDownExpanded = termDown /. expandRule;
    termDownReduced = termDownExpanded /. metricRules;
    termFinal = ReleaseHold[termDownReduced];

    AssertEqual[termFinal, termUp, 
      "Metric Equivalence (\!\(\*SubscriptBox[\(A\), \(\[Nu]\)]\) \!\(\*SuperscriptBox[\(B\), \(\[Nu]\)]\) reduces to \!\(\*SuperscriptBox[\(A\), \(\[Mu]\)]\) \!\(\*SubscriptBox[\(B\), \(\[Mu]\)]\))"]]];
  
(* ========================================================================= *)
Print["\n--- SECTION 5: SECOND DERIVATIVE Box Structure ---"];
(* ========================================================================= *)

Module[{boxStructure, hasFraction, hasSquaredSym},

  (* 1. EXECUTE (Direct Injection) *)
  (* Pass the structure DIRECTLY to MakeBoxes to bypass variable evaluation issues *)
  boxStructure = MakeBoxes[Partials[Partials[f, x], y], StandardForm];

  (* 2. VALIDATION *)
  (* Check for FractionBox *)
  hasFraction = MatchQ[boxStructure, FractionBox[_, _]];

  (* Check for Superscript "2" (Robust against encoding differences) *)
  hasSquaredSym = !FreeQ[boxStructure, SuperscriptBox[_, "2"]];

  If[hasFraction && hasSquaredSym,
    pass["Found FractionBox + ^2", True],
    fail["formatting rule NOT detected", True, False]]];

(* ========================================================================= *)
Print["\n--- SECTION 6: CALCULUS ENGINE ---"];
(* ========================================================================= *)

Module[{testProduct, testCD, testLinearity},

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

(* Chain Rule (Linearity) *)
(* Input: d(A+B)/dx *)
testLinearity = ExpandDerivatives[Partials[A + B, x[\[ScriptCapitalU][\[Mu]]]]];
AssertTrue[MatchQ[testLinearity, Plus[Partials[A, _], Partials[B, _]]],
  "Differentiation Linearity"];
  
];

(* TEST: Covariant Derivative of a Scalar *)
Module[{phi, scalarCD},
  valence[phi] = {{}, {}}; (* Define as scalar *)
  scalarCD = CD[phi, x[\[ScriptCapitalU][\[Mu]]]];
  
  AssertEqual[scalarCD, Partials[phi, x[\[ScriptCapitalU][\[Mu]]]], 
    "CD of Scalar reduces to Partial"];
    
  AssertTrue[FreeQ[scalarCD, \[CapitalGamma]], 
    "Scalar CD contains no Connection Coefficients"];
];

(* TEST: Nested Covariant Derivatives (Stress Test for Alpha-Conversion) *)
Module[{nestedCD, allIndices, generatedDummies},
  
  nestedCD = CD[CD[A[\[ScriptCapitalU][\[Mu]]], x[\[ScriptCapitalU][\[Nu]]]], x[\[ScriptCapitalU][\[Rho]]]];
  
  (* Assertion: Check for generated dummy indices to ensure uniqueness *)
  allIndices = Cases[nestedCD, (\[ScriptCapitalU] | \[ScriptCapitalD])[s_Symbol] :> s, Infinity];
  
  (* Filter for symbols that look like generated Lambdas *)
  generatedDummies = Select[DeleteDuplicates[allIndices],
    StringMatchQ[SymbolName[#], "\[Lambda]" ~~ (DigitCharacter ..) ~~ EndOfString] ||
    StringMatchQ[SymbolName[#], "\[Lambda]$" ~~ __] &];
    
  (* We expect at least 2 distinct generated dummies (one for each CD layer) *)
  AssertTrue[Length[generatedDummies] >= 2, 
    "Nested CD Alpha-Conversion (Unique Indices Generated)"];
];
  
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

(* FIX: Expect Formal Indices (i1) because ExtractCoefficient resets counters per term *)
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

(* ========================================================================= *)
Print["\n--- SECTION 8: TENSOR FORM ROBUSTNESS ---"];
(* ========================================================================= *)

Module[{weirdTerm, formatted, formalPattern, isClean},

  (* Stress Test: Create a term with arbitrary formal indices *)
  weirdTerm = Symbol["\[FormalQ]"] * Symbol["\[FormalZ]"] * Symbol["\[FormalI]99"];
  
  (* Apply TensorForm *)
  formatted = TensorForm[weirdTerm];

  (* THE FIX: Use CharacterRange to inspect the result *)
  formalPattern = CharacterRange["\[FormalA]", "\[FormalZ]"];

  (* Assertion: The output should NOT contain any Formal symbols anymore *)
  isClean = FreeQ[formatted, s_Symbol /; StringStartsQ[SymbolName[s], formalPattern]];

  AssertTrue[isClean, "TensorForm maps arbitrary formals (Q, Z, I99) to Greek"];

];

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
    (* We look for RowBox[{"(", ... }] inside the box structure *)
    hasParens = !FreeQ[box, "("];
    
    If[isSubscript && (hasParens === shouldHaveParens),
      pass[label, DisplayForm@box],
      fail[label, 
        If[shouldHaveParens, "Subscript w/ Parens", "Subscript w/o Parens"], 
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

(* ========================================================================= *)
Print["\n--- SECTION 10: EINSTEIN SUMMATION (Contract & ContractAll) ---"];
(* ========================================================================= *)

Module[{coords, exprSingle, exprDouble, exprMixed, exprPartial, resSingle, resDouble, resPartial},
  
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
  exprDouble = A[\[ScriptCapitalU][\[Mu]]] * B[\[ScriptCapitalD][\[Mu]]] * C[\[ScriptCapitalU][\[Nu]]] * D[\[ScriptCapitalD][\[Nu]]];
  
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
  exprMixed = A[\[ScriptCapitalU][\[Mu]]]*B[\[ScriptCapitalD][\[Mu]]] + C[\[ScriptCapitalU][\[Nu]]]*D[\[ScriptCapitalD][\[Nu]]];
  AssertTrue[FreeQ[ContractAll[exprMixed, coords], \[ScriptCapitalU][_Symbol]], 
    "Distribution over Sum (A^u B_u + C^v D_v)"];

  (* TEST 5: Protection of Free Indices *)
  (* A^u B_v (No match) -> Should remain untouched *)
  AssertEqual[ContractAll[A[\[ScriptCapitalU][\[Mu]]] * B[\[ScriptCapitalD][\[Nu]]], coords], 
    A[\[ScriptCapitalU][\[Mu]]] * B[\[ScriptCapitalD][\[Nu]]], 
    "Free Indices Protected (No Contraction)"];

  (* TEST 6: Coordinate Exclusion *)
  (* Ensure the symbols in 'coords' themselves are not treated as indices if they appear in the expression *)
  (* e.g. if coords={t,r}, don't try to contract 't' in A^t *)
  Module[{badExpr},
    badExpr = A[\[ScriptCapitalU][1]]; (* 1 is a coord *)
    AssertEqual[
      ContractAll[badExpr, coords], 
      badExpr, 
      "Coordinate Symbols Excluded from Scanner"];
  ];
];


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
  
  cde = CD[\[Psi], x[\[ScriptCapitalU][\[Mu]]], e, A];
  
  transformedCde = cde /. gaugeTransforms //. differentiationRules /. {Partials[e, _] :> 0} // Expand;
     
  expectedCde = E^(I e \[CapitalLambda]) * cde // Expand;
  
  AssertEqual[transformedCde, expectedCde, 
    "U(1) Gauge Covariance (\!\(\*SubscriptBox[\(D\), \(\[Mu]\)]\)\[Psi]' == \!\(\*SuperscriptBox[\(\[ExponentialE]\), \(\[ImaginaryI]\[VeryThinSpace]e\[VeryThinSpace]\[CapitalLambda]\)]\) \!\(\*SubscriptBox[\(D\), \(\[Mu]\)]\)\[Psi])"];

  (* --- TEST 2: Gauge Invariance of Field Strength Subscript[F, \[Mu]\[Nu]] --- *)
  (* prove: Subscript[F', \[Mu]\[Nu]] == Subscript[F, \[Mu]\[Nu]] *)

  (* The Field Strength Tensor Subscript[F, \[Mu]\[Nu]] = \!\(
\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]
\*SubscriptBox[\(A\), \(\[Nu]\)]\) - \!\(
\*SubscriptBox[\(\[PartialD]\), \(\[Nu]\)]
\*SubscriptBox[\(A\), \(\[Mu]\)]\) *)
  F = Partials[A[\[ScriptCapitalD][n]], x[\[ScriptCapitalU][m]]] - Partials[A[\[ScriptCapitalD][m]], x[\[ScriptCapitalU][n]]];
  
  (* Transform and Expand *)
  transformedF = F /. gaugeTransforms // ExpandDerivatives // Expand;
  
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

  transformedW = geometricW /. mixedGaugeRules //. differentiationRules /. {Partials[gW, _] :> 0} // Expand;
  
  (* Expectation *)
  (* The original mixed object, just phase-rotated *)
  expectedW = E^(I\[ThinSpace]gW \[CapitalLambda]) * geometricW // Expand;
  
  AssertEqual[transformedW, expectedW, 
    "Weinberg-Salam Mixed Covariance (Geometric + Gauge)"];
];

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

