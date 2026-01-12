(* =========================================================================*)
(*DERIVATION:The Transformation Law of Gamma*)
(* =========================================================================*)
ClearAll[ap, bp, mu, nu];

(*0. SPECIAL RULES, not in \[ScriptCapitalU]\[ScriptCapitalD] proper*)
(*Subscript[A, \
,\[Beta]']->\[PartialD]x^\[Mu]/\[PartialD]x^\[Beta]'Subscript[A, ,\
\[Mu]]*)
(*Condition/;!MatchQ[expr,x[_\
]] prevents expanding a Jacobian itself (dx/dx')*)
manualChainRule =
  Partials[expr_, x[\[ScriptCapitalU][bp]]] /; ! MatchQ[expr, x[_]] :>
   Module[{fresh = Unique["\[Mu]"]},
    Partials[x[\[ScriptCapitalU][fresh]], x[\[ScriptCapitalU][bp]]]*
     Partials[expr, x[\[ScriptCapitalU][fresh]]]];

(*Combine differentiationRules from \[ScriptCapitalU]\[ScriptCapitalD]\
 and the special manualChainRule here into a single flat list*)
allRules = Flatten[{differentiationRules, manualChainRule}];

(*1. transformed covariant derivative*)
targetTensor = Echo[
   (*Jacobians*)
   Partials[x[\[ScriptCapitalU][ap]], x[\[ScriptCapitalU][mu]]]*
    Partials[x[\[ScriptCapitalU][nu]], x[\[ScriptCapitalU][bp]]]*
    CD[A[\[ScriptCapitalU][mu]], x[\[ScriptCapitalU][nu]]],
   "CD says \!\(\*SubscriptBox[SuperscriptBox[\(A\), \(\[Alpha]'\)], \
\(; \(\\\ \)\(\[Beta]'\)\)]\) ="];

(*2. just calculate*)
(*Define A' and x'*)
Echo[Aprimed = A[\[ScriptCapitalU][ap]] /. robustTransformRules,
  "2a. \!\(\*SuperscriptBox[\(A\), \(\[Mu]'\)]\) ="];
xprimed = x[\[ScriptCapitalU][bp]];
(* A^\[Mu]' Subscript[\[CapitalGamma]^\[Alpha]', \[Beta]' \[Mu]'] = \
Subscript[A^\[Alpha]', ; \[Beta]'] - Subscript[A^\[Alpha]', , \
\[Beta]']*)
brokenPartialOnly = Echo[
   Partials[Aprimed, xprimed] //. allRules,
   "2b. Broken partial \!\(\*SubscriptBox[SuperscriptBox[\(A\), \(\
\[Alpha]'\)], \(, \(\\\ \)\(\[Beta]'\)\)]\) ="];
gammaPrimeTimesVector =
  targetTensor - brokenPartialOnly;

(*3. FORMATTING (Lovelock Style)*)
(*Collect terms by A to isolate the transformation law*)
prettyResult = Echo[
   gammaPrimeTimesVector /. {ap -> \[Alpha]', bp -> \[Beta]', 
     mu -> \[Mu], nu -> \[Nu]},
   "3a. Residual (\!\(\*SubscriptBox[SuperscriptBox[\(A\), \
\(\[Alpha]'\)], \(; \(\\\ \)\(\[Beta]'\)\)]\) - \
\!\(\*SubscriptBox[SuperscriptBox[\(A\), \(\[Alpha]'\)], \(, \(\\\ \)\
\(\[Beta]'\)\)]\)) ="];

Print["\n=== \[CapitalGamma] TRANSFORMATION LAW (without quotient) ==="];
Print["The term involving \[CapitalGamma]' must satisfy:"];
(*HERE IS THE MAGIC*)
(*Tensor Form does index coalescing and simplification!*)
Echo[
  TensorForm[Collect[prettyResult, A[\[ScriptCapitalU][_]]]],
  "3b. \!\(\*SuperscriptBox[\(A\), \(\[Gamma]'\(\\\ \
\)\)]\)\!\(\*SubscriptBox[SuperscriptBox[\(\[CapitalGamma]\), \(\\\ \
\(\[Alpha]'\)\)], \(\[Beta]' \[Gamma]'\)]\) = "];

(*4.APPLY QUOTIENT THEOREM*)
(*Extract the coefficient of A^cp=A[\[ScriptCapitalU][cp]].*)

(*temporarily force index cp*)
inverseVectorRule = A[\[ScriptCapitalU][idx_]] :>
   Partials[x[\[ScriptCapitalU][idx]], x[\[ScriptCapitalU][cp]]]*
    A[\[ScriptCapitalU][cp]];

(*Apply to residual*)
(*ensuring every A term is linear in A^cp*)
rhsWithTargetA = gammaPrimeTimesVector /. inverseVectorRule;

(*Extract (strip) the coefficient*)
finalGammaLaw = Coefficient[rhsWithTargetA, A[\[ScriptCapitalU][cp]]];

(*5. FINAL FORMATTING*)
prettyResult =
  finalGammaLaw /. {ap -> \[Alpha]', bp -> \[Beta]', cp -> \[Gamma]', 
    mu -> \[Mu], nu -> \[Nu]};

Print["\n=== \[CapitalGamma] TRANSFORMATION LAW ==="];
Print["By a Quotient Theorem, stripping A to find \[CapitalGamma]':"];
(*MORE MAGIC FROM TensorForm*)
Echo[TensorForm[prettyResult], 
  "5. \!\(\*SubscriptBox[SuperscriptBox[\(\[CapitalGamma]\), \(\
\[Alpha]'\)], \(\[Beta]' \[Gamma]'\)]\) ="];
Print["Not Tensorial!"]
