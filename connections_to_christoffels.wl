Print["========================================================================="];
Print["Christoffel Symbols for Gravitation from Metric via the \
\[ScriptCapitalU]\[ScriptCapitalD] Calculus"];
Print["========================================================================="];

ClearAll[g, GammaSymmetryRule, MetricSymmetryRule, term1, term2, 
  term3, combination, targetIndex];

Print["1. PHYSICAL CONSTRAINTS -------------------------------------------------"];
Print["1a. Torsion-free geometry: \
\!\(\*SubscriptBox[SuperscriptBox[\(\[CapitalGamma]\), \(\[Alpha]\)], \
\(\[Beta]\[Gamma]\)]\) == \!\(\*SubscriptBox[SuperscriptBox[\(\
\[CapitalGamma]\), \(\[Alpha]\)], \(\[Gamma]\[Beta]\)]\), \
[MTW] Box 10.2, page 250 "];
GammaSymmetryRule = Echo[
   \[CapitalGamma][u_, \[ScriptCapitalD][b_], \[ScriptCapitalD][c_]] :>
    \[CapitalGamma][
     u, \[ScriptCapitalD][Sort[{b, c}][[1]]], \[ScriptCapitalD][
      Sort[{b, c}][[2]]]],
   "\[CapitalGamma] Symmetry: "];

Print["1b. Metric Symmetry: \!\(\*SubscriptBox[\(g\), \(\[Alpha]\
\[Beta]\)]\) == \!\(\*SubscriptBox[\(g\), \(\[Beta]\[Alpha]\)]\), \
[MTW] Equation 8.24, page 210"];
Print[Style[
   "(output manipulated as expected by \[ScriptCapitalU]\
\[ScriptCapitalD] display rules)", Gray]];
MetricSymmetryRule = Echo[
   g[\[ScriptCapitalD][a_], \[ScriptCapitalD][b_]] :>
    g[\[ScriptCapitalD][Sort[{a, b}][[1]]], \[ScriptCapitalD][
      Sort[{a, b}][[2]]]],
   "g Symmetry"];

Print["1c. Metric Compatibility Postulates \!\(\*SubscriptBox[\(\
\[Del]\), \([\[Lambda]\)]\)\!\(\*SubscriptBox[\(g\), \
\(\[Mu]\[Nu]\(]\)\)]\) = 0 [MTW] Eqn 8.23 & Chapter 14."];

(*Term 1: \!\(
\*SubscriptBox[\(\[Del]\), \(\[Lambda]\)]
\*SubscriptBox[\(g\), \(\[Mu]\[Nu]\)]\)*)
term1 = CD[g[\[ScriptCapitalD][\[Mu]], \[ScriptCapitalD][\[Nu]]], 
     x[\[ScriptCapitalU][\[Lambda]]]] /. GammaSymmetryRule /. 
   MetricSymmetryRule;
Echo[TensorForm[term1], 
  "0 = \!\(\*SubscriptBox[\(\[Del]\), \
\(\[Lambda]\)]\)\!\(\*SubscriptBox[\(g\), \(\[Mu]\[Nu]\)]\) ="];

(*Term 2: \!\(
\*SubscriptBox[\(\[Del]\), \(\[Mu]\)]
\*SubscriptBox[\(g\), \(\[Nu]\[Lambda]\)]\) (Left-Cycle Indices)*)
term2 = CD[g[\[ScriptCapitalD][\[Nu]], \[ScriptCapitalD][\[Lambda]]], 
     x[\[ScriptCapitalU][\[Mu]]]] /. GammaSymmetryRule /. 
   MetricSymmetryRule;
Echo[TensorForm[term2], 
  "0 = \!\(\*SubscriptBox[\(\[Del]\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\
\(g\), \(\[Mu]\[Lambda]\)]\) ="];

(*Term 3: \!\(
\*SubscriptBox[\(\[Del]\), \(\[Nu]\)]
\*SubscriptBox[\(g\), \(\[Lambda]\[Mu]\)]\) (Left-Cycle Indices)*)
term3 = CD[g[\[ScriptCapitalD][\[Lambda]], \[ScriptCapitalD][\[Mu]]], 
     x[\[ScriptCapitalU][\[Nu]]]] /. GammaSymmetryRule /. 
   MetricSymmetryRule;
Echo[TensorForm[term3], 
  "0 = \!\(\*SubscriptBox[\(\[Del]\), \(\[Nu]\)]\)\!\(\*SubscriptBox[\
\(g\), \(\[Lambda]\[Mu]\)]\) ="];


Print["2. CYCLIC PERMUTATION TRICK ---------------------------------------------"];
Print["Write 'Koszul' sum of \[Del]g permutations that isolates a \
\[CapitalGamma] term."];
Print["[MTW] Equation 8.24b, page 210, in coordinate basis \
(\!\(\*SubscriptBox[\(c\), \(abc\)]\)===0)"];
Print["The Linear Combination 0 = (\!\(\*SubscriptBox[\(\[Del]\), \(\
\[Mu]\)]\)\!\(\*SubscriptBox[\(g\), \(\[Nu]\[Lambda]\)]\) + \
\!\(\*SubscriptBox[\(\[Del]\), \(\[Nu]\)]\)\!\(\*SubscriptBox[\(g\), \
\(\[Lambda]\[Mu]\)]\) - \!\(\*SubscriptBox[\(\[Del]\), \(\[Lambda]\)]\
\)\!\(\*SubscriptBox[\(g\), \(\[Mu]\[Nu]\)]\)) ==="];

Echo[combination = (term2 + term3 - term1), "0 ="];

Echo[gammaPart = 
   Select[List @@ combination, ! FreeQ[#, \[CapitalGamma]] &] // Total,
  "\[CapitalGamma] part ="];
Echo[gammaPartCanonical = gammaPart // CanonicalizeIndices,
  "\[CapitalGamma] part (canonical) ="];
Echo[metricPartials = 
   Select[List @@ combination, FreeQ[#, \[CapitalGamma]] &] // Total,
  "g partials"];


Print["3. SOLVE FOR \[CapitalGamma] ----------------------------------------------------------"];

(*Equation: gammaPart + metricPartials = 0 => -gammaPart = \
metricPartials*)
Print["3a. Equation to solve:"];
lhsRaw = -gammaPartCanonical;
rhsRaw = metricPartials;

Echo[TensorForm[lhsRaw == rhsRaw], "Isolated Equation:"];

Print["3b. Find correct \!\(\*SuperscriptBox[\(g\), \(-1\)]\) to \
multiply through."];

(*1. Find the metric inside the LHS term*)
Echo[metricFound = First[Cases[lhsRaw, g[__], Infinity]], "g found"];

(*2. Find the Gamma inside the LHS term*)
Echo[gammaFound = First[Cases[lhsRaw, \[CapitalGamma][__], Infinity]],
   "\[CapitalGamma] found"];

(*3. Extract raw symbols from the indices*)
Echo[metricIndices = First /@ (List @@ metricFound), "g indices"];
Echo[gammaUpIndex = First[First[gammaFound]], 
  "\[CapitalGamma] up index"];

(*4. Identify the Target Index*)
(*The g index that matches \[CapitalGamma] up-index is the Dummy. \
The other is the Target.*)
Echo[targetIndex = If[metricIndices[[1]] === gammaUpIndex,
    metricIndices[[2]], metricIndices[[1]]], "target index"];

(*5. Construct the specific Inverse needed*)
Echo[invMetricFactor = 
   1/2*g[\[ScriptCapitalU][\[Sigma]], \[ScriptCapitalU][targetIndex]],
   "\!\(\*FractionBox[\(1\), \(2\)]\)\!\(\*SuperscriptBox[\(g\), \(-1\
\)]\) ="];
Print[Style[
   "(1/2 cancels factor 2 from \[CapitalGamma] part (canonical) above)",
    Gray]];

Print["3c. Multiply and Contract"];

(*\[Delta]-\[CapitalGamma]\
 contraction rule (will be promoted into the engine in next installmen\
t)*)
Echo[deltaContractionRule = \[Delta][\[ScriptCapitalU][
       s_], \[ScriptCapitalD][r_]]*\[CapitalGamma][\[ScriptCapitalU][
       r_], a_, b_] :> \[CapitalGamma][\[ScriptCapitalU][s], a, b],
  "\[Delta]-\[CapitalGamma] contraction rule: "];
Print[Style[
   "(will be promoted into \[ScriptCapitalU]\[ScriptCapitalD] in the \
future)", Gray]];

(*Execute the algebra*)
Echo[lhsSolved = 
   lhsRaw*invMetricFactor //. metricRules //. deltaContractionRule,
  "LHS solved: "];

(*Format RHS:Factor out 1/2 Subscript[g^\[Sigma], \[Lambda]]*)
Echo[rhsSolved = 
   Collect[rhsRaw*invMetricFactor, 
    g[\[ScriptCapitalU][_], \[ScriptCapitalU][_]]],
  "RHS solved:"];

(*Step D: Final Result*)
Print["\n=== CHRISTOFFEL SYMBOLS FOR GRAVITATION ==="];
Print["aka The Fundamental Theorem of Riemannian Geometry"];
finalEquation = lhsSolved == rhsSolved;
TensorForm[finalEquation]
