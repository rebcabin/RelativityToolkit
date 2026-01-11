ClearAll[ap, bp, mu];

(*Step 1:Define the Input*)
(*Partial derivative of the Transformed Vector A' w.r.t x'*)
(*Input:d(A^a')/d(x^b')*)
inputTerm = Echo[
   Partials[
    (*Expand A^a' into J*A^mu immediately via existing rule*)
    A[\[ScriptCapitalU][ap]] /. robustTransformRules, 
    x[\[ScriptCapitalU][bp]]],
   "1. Input \!\(\*SubscriptBox[\(\[PartialD]\), \(\[Beta]'\)]\)(\!\(\
\*SuperscriptBox[\(A\), \(\[Alpha]'\)]\)):"];

(*Step 2:Mechanical Expansion*)
(*Apply Leibniz and Chain Rule repeatedly until stable*)
expandedTerm = Echo[ExpandDerivatives[inputTerm],
   "2. Mechanically Expanded Derivative:"];

(*Step 3:Formatting*)
(*Map internal unique dummies to readable Greek*)
prettyResult = 
  Echo[expandedTerm /. {ap -> \[Alpha]', bp -> \[Beta]', mu -> \[Mu]},
   "3. Peek at unique dummy indices"];

brokenPartial = Echo[TensorForm[prettyResult],
   "4. Expanded \!\(\*SubscriptBox[\(\[PartialD]\), \
\(\[Beta]'\)]\)(\!\(\*SuperscriptBox[\(A\), \(\[Alpha]'\)]\)) in \
Readable Greek"];eek"];
