(* =========================================================================*)
(*DERIVATION:The Transformation Law of Gamma*)
(* =========================================================================*)
ClearAll[ap, bp, mu, nu];

(*0. SPECIAL RULES*)
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

(*Combine rules from the engine and here into a single flat list*)
allRules = Flatten[{differentiationRules, manualChainRule}];

(*1. OBJECTIVE*)
(*Target=Jac*Jac*CD[A_unprimed]*)
targetTensor = Echo[
   Partials[x[\[ScriptCapitalU][ap]], x[\[ScriptCapitalU][mu]]]*
    Partials[x[\[ScriptCapitalU][nu]], x[\[ScriptCapitalU][bp]]]*
    CD[A[\[ScriptCapitalU][mu]], x[\[ScriptCapitalU][nu]]],
   "Target Tensorial Transformation \
\!\(\*FractionBox[\(\[PartialD]\*SuperscriptBox[\(x\), \(\[Alpha]'\)]\
\), \(\[PartialD]\*SuperscriptBox[\(x\), \
\(\[Mu]\)]\)]\)\!\(\*FractionBox[\(\[PartialD]\*SuperscriptBox[\(x\), \
\(\[Nu]\)]\), \(\[PartialD]\*SuperscriptBox[\(x\), \(\[Beta]'\)]\)]\)\
(\!\(\*SubscriptBox[SuperscriptBox[\(A\), \(\[Mu]\)], \(, \(v\)\)]\) + \
\!\(\*SuperscriptBox[\(A\), \(\[Lambda]\)]\) \
\!\(\*SubscriptBox[SuperscriptBox[\(\[CapitalGamma]\), \(\[Mu]\)], \(\
\[Nu]\[Lambda]\)]\))"];
(*2. REALITY*)
(*Define A' and x'*)
Aprimed = A[\[ScriptCapitalU][ap]] /. robustTransformRules;
xprimed = x[\[ScriptCapitalU][bp]];

(*2a.The Definition (Visual Only)*)
Echo[
  HoldForm[
    CD[A[\[ScriptCapitalU][ap]], 
     x[\[ScriptCapitalU][bp]]]] /. {ap -> \[Alpha]', bp -> \[Beta]'},
  "2a. Definition (The Object we are expanding):"];

(*2b.The Expansion*)
(*Calculate the actual value using the flattened rules list*)
realityExpanded = Echo[
   CD[Aprimed, xprimed] //. allRules,
   "2b. Reality (Expanded via Chain Rule):"];

(*3. THE SOLUTION*)
(*GammaTerm_Primed=Target-(Reality_without_Gamma_Prime)*)

brokenPartialOnly = Echo[
   Partials[Aprimed, xprimed] //. allRules,
   "3a. The broken partial (no semicolon) \
\!\(\*SubscriptBox[SuperscriptBox[\(A\), \(\[Alpha]'\)], \(, \
\(\[Beta]'\)\)]\),"];
gammaPrimeTimesVector = Echo[
   targetTensor - brokenPartialOnly,
   "3b. Residual (\!\(\*FractionBox[\(\[PartialD]\*SuperscriptBox[\(x\
\), \(\[Alpha]'\)]\), \(\[PartialD]\*SuperscriptBox[\(x\), \(\[Mu]\)]\
\)]\)\!\(\*FractionBox[\(\[PartialD]\*SuperscriptBox[\(x\), \
\(\[Nu]\)]\), \(\[PartialD]\*SuperscriptBox[\(x\), \(\[Beta]'\)]\)]\)\
(\!\(\*SubscriptBox[SuperscriptBox[\(A\), \(\[Mu]\)], \(, \(v\)\)]\) + \
\!\(\*SuperscriptBox[\(A\), \(\[Lambda]\)]\) \
\!\(\*SubscriptBox[SuperscriptBox[\(\[CapitalGamma]\), \(\[Mu]\)], \(\
\[Nu]\[Lambda]\)]\))) - \!\(\*SubscriptBox[SuperscriptBox[\(A\), \(\
\[Alpha]'\)], \(, \(\[Beta]'\)\)]\)"];

(*4. FORMATTING (Lovelock Style)*)
(*We Collect terms by A to isolate the transformation law*)

prettyResult = 
  gammaPrimeTimesVector /. {ap -> \[Alpha]', bp -> \[Beta]', 
    mu -> \[Mu], nu -> \[Nu]};

Print["\n=== THE TRANSFORMATION LAW (without quotient) ==="];
Print["The term involving Gamma' must satisfy:"];
Print["(A will be quotiented out in the next step)"];
Print[Row[{Style[
     "(\!\(\*SuperscriptBox[\(A\), \
\(\[Gamma]'\)]\))\!\(\*SubscriptBox[SuperscriptBox[\(\[CapitalGamma]\)\
, \(a'\)], \(\[Beta]' \[Gamma]'\)]\)  =  ", Bold, 14],
    TensorForm[Collect[prettyResult, A[\[ScriptCapitalU][_]]]]}]];

(*5. VIA THE QUOTIENT THEOREM*)

(*Extract the coefficient of A[\[ScriptCapitalU][cp]].*)
(*When we convert unprimed A to primed A, force 'cp'.*)

(*Define Inverse Rule:A^mu->(dx^mu/dx^cp)*A^cp*)
inverseVectorRuleFixed = A[\[ScriptCapitalU][idx_]] :>
   Partials[x[\[ScriptCapitalU][idx]], x[\[ScriptCapitalU][cp]]]*
    A[\[ScriptCapitalU][cp]];

(*Apply to our result*)
(*ensuring every A term is linear in A^cp*)
rhsWithTargetA = gammaPrimeTimesVector /. inverseVectorRuleFixed;

(*Now extract the coefficient*)
finalGammaLaw = Coefficient[rhsWithTargetA, A[\[ScriptCapitalU][cp]]];

(*6. FINAL FORMATTING*)

prettyResult = Echo[
   finalGammaLaw /. {ap -> \[Alpha]', bp -> \[Beta]', cp -> \[Gamma]',
      mu -> \[Mu], nu -> \[Nu]},
   "Final \[CapitalGamma] Law (before TensorForm)"];

Print["\n=== THE TRANSFORMATION LAW ==="];
Print["By the Quotient Theorem, strip A to find Gamma':"];
Print[Row[{Style[
     "\!\(\*SubscriptBox[SuperscriptBox[\(\[CapitalGamma]\), \(\
\[Alpha]'\)], \(\[Beta]' \[Gamma]'\)]\)  =  ", Bold, 16],
    TensorForm[prettyResult]}]];
