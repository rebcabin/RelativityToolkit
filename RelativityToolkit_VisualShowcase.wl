(* =========================================================================*)
(* VISUAL SHOWCASE v.1.4.3 :THE RELATIVITY TOOLKIT IN ACTION*)
(* =========================================================================*)
Print["\n================================================================"];
Print[Style["VISUAL SHOWCASE: FORMAL DIFFERENTIAL GEOMETRY", Bold, 16]];
Print["version " <> RelativityToolkitVersion];
Print["\n================================================================\n"];

(*---1. THE ZOO OF TENSORS-----------------------------------------------*)
Print[Style["1. The Tensor Zoo (Standard Objects)", Bold, 14]];
Print["These objects carry valence metadata hidden inside their \
structure."];

tensorZoo = {
   {"Object Name", "Wolfram Input", "Textbook Output"},
   {"Contravariant Vector", "x[\[ScriptCapitalU][\[Mu]]]", x[\[ScriptCapitalU][\[Mu]]]},
   {"Covariant Covector", "p[\[ScriptCapitalD][\[Nu]]]", p[\[ScriptCapitalD][\[Nu]]]},
   {"Mixed Tensor", "T[\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Nu]]]", T[\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Nu]]]},
   {"The Metric", "g[\[ScriptCapitalD][\[Mu]], \[ScriptCapitalD][\[Nu]]]", g[\[ScriptCapitalD][\[Mu]], \[ScriptCapitalD][\[Nu]]]},
   {"Inverse Metric", "g[\[ScriptCapitalU][\[Mu]], \[ScriptCapitalU][\[Nu]]]", g[\[ScriptCapitalU][\[Mu]], \[ScriptCapitalU][\[Nu]]]},
   {"Kronecker Delta", "\[Delta][\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Nu]]]", \[Delta][\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Nu]]]}, 
   {"Jacobian (Partial)", "Partials[x[\[ScriptCapitalU][\[Alpha]]], x[\[ScriptCapitalU][\[Beta]]]]", 
    Partials[x[\[ScriptCapitalU][\[Alpha]]], x[\[ScriptCapitalU][\[Beta]]]]}};

Print[Grid[tensorZoo,
   Frame -> All,
   FrameStyle -> Directive[Thin, GrayLevel[0.8]],
   Background -> {None, {LightGray, None}},
   Spacings -> {2, 1.5},
   Alignment -> {{Left, Left, Center}, Center}]];

(*---2. METRIC GYMNASTICS------------------------------------------------*)
Print["\n", 
  Style["2. Metric Gymnastics (Raising & Lowering)", Bold, 14]];
Print["Watch the metric 'swallow' indices to change their variance."];

(*Define a helper to pretty-print operations*)
VisualizeOp[name_, input_, rule_] :=
  Module[{res}, res = input /. rule;
   Print[Grid[{
      {Style[name, Bold, 12], SpanFromLeft},
      {"Input:", input},
      {"Result:", res}},
     Alignment -> Left,
     Spacings -> {2, 1},
     Frame -> {{True, False}, {False, True, False}},
     (*Left bar*)FrameStyle -> Thick]];];

VisualizeOp["Index Lowering", 
  g[\[ScriptCapitalD][\[Mu]], \[ScriptCapitalD][\[Nu]]]*A[\[ScriptCapitalU][\[Nu]]], 
  metricRules];

VisualizeOp["Index Raising", 
  g[\[ScriptCapitalU][\[Mu]], \[ScriptCapitalU][\[Nu]]]*p[\[ScriptCapitalD][\[Nu]]], 
  metricRules];

VisualizeOp["The Inverse Identity", 
  g[\[ScriptCapitalU][\[Mu]], \[ScriptCapitalU][\[Alpha]]]*g[\[ScriptCapitalD][\[Alpha]], \[ScriptCapitalD][\[Nu]]], 
  metricRules];
(*---3. COORDINATE TRANSFORMATIONS---------------------------------------*)
Print["\n", 
  Style["3. Coordinate Transformations (Alpha-Conversion)", Bold, 14]];
Print["Note the automatically generated unique dummy index (e.g., \
\[Mu]$...)."];

VisualizeOp["Transforming a Vector",
  A[\[ScriptCapitalU][Superscript["a", "\[Prime]"]]],
  robustTransformRules];
(*---4. PRETTY PRINTING--------------------------------------------------*)
Print["\n", Style["4. Canonicalization & Pretty Printing", Bold, 14]];
Print["Simplifying dummy indices into standard Greek letters."];

rawSum =   A[\[ScriptCapitalU][\[Mu]]]*B[\[ScriptCapitalD][\[Mu]]] + A[\[ScriptCapitalU][\[Nu]]]*B[\[ScriptCapitalD][\[Nu]]];

Print[Grid[{
    {"Raw Sum (Different Indices):", rawSum},
    {"Canonicalized (TensorForm):", TensorForm[rawSum]}},
   Alignment -> Left, Spacings -> {2, 1.5}]];
(*---5. METRIC EQUIVALENCE DEMO -------------------------------*)
Print["\n",Style["5. Physical Equivalence Proof",Bold,14]];
Print["Demonstrating that ",
Style["\!\(\*SuperscriptBox[\(A\), \(\[Mu]\)]\) \!\(\*SubscriptBox[\(B\), \(\[Mu]\)]\)",Italic],
" is equivalent to ",
Style["\!\(\*SubscriptBox[\(A\), \(\[Nu]\)]\) \!\(\*SuperscriptBox[\(B\), \(\[Nu]\)]\)",Italic],
"."];
Print["(\[ScriptCapitalU]\[ScriptCapitalD] will reorganize and canonicalize displays.)"]; 

(*Define the starting point:A_v B^v*)
startTerm=A[\[ScriptCapitalD][\[Nu]]]B[\[ScriptCapitalU][\[Nu]]];

(*Step 1: Definition*)
step1Term=(g[\[ScriptCapitalD][\[Nu]],\[ScriptCapitalD][\[Alpha]]]A[\[ScriptCapitalU][\[Alpha]]])B[\[ScriptCapitalU][\[Nu]]];

(*Step 2: Commutation*)
step2Term=A[\[ScriptCapitalU][\[Alpha]]](g[\[ScriptCapitalD][\[Nu]],\[ScriptCapitalD][\[Alpha]]]B[\[ScriptCapitalU][\[Nu]]]);

(*Step 3: Contraction*)
step3Term=step2Term/.metricRules;

(*Helper for styling*)
ClearAll[TableHead];
TableHead[txt_]:=Style[txt,Bold,12,FontFamily->"Helvetica"];

Print[Grid[{
{TableHead["Step"],TableHead["Expression"],TableHead["Input"]},
{"1. Start",startTerm,"Input: A[\[ScriptCapitalD][\[Nu]]]B[\[ScriptCapitalU][\[Nu]]]"},
{"2. Definition",step1Term,"(g[\[ScriptCapitalD][\[Nu]],\[ScriptCapitalD][\[Alpha]]] A[\[ScriptCapitalU][\[Alpha]]]) B[\[ScriptCapitalU][\[Nu]]]"},
{"3. Commute?",step2Term,"A[\[ScriptCapitalU][\[Alpha]]] (g[\[ScriptCapitalD][\[Nu]],\[ScriptCapitalD][\[Alpha]]] B[\[ScriptCapitalU][\[Nu]]])"},
{"4. Contract",step3Term,"step2Term/. metricRules"},
{"5. Equality?",step3Term===startTerm,"lhs:4 === rhs:1"}},
Frame->All,
FrameStyle->Directive[Thin,GrayLevel[0.7]],
Background->{None,{LightGray,None}},
Spacings->{2,1.5},
Alignment->{{Left,Center,Left},Center}]];

Print["\n================================================================"];
(* =========================================================================*)
(*VISUAL SHOWCASE:The "Second Derivative Gallery"*)
(* =========================================================================*)
Print["\n--- Gallery of Second Derivatives ---"];

(*Define a list of scenarios to render*)
gallery = {
   {"Generic Mixed Partial", Partials[Partials[f, y], x]},
   {"Coordinate Hessian (The 'Garbage Term')", 
    Partials[
     Partials[x[\[ScriptCapitalU][\[Alpha]']], 
      x[\[ScriptCapitalU][\[Beta]]]], x[\[ScriptCapitalU][\[Alpha]]]]},
   {"Nested in an Equation", 
    lhs == Partials[
      Partials[A[\[ScriptCapitalU][\[Mu]]], x[\[ScriptCapitalU][\[Nu]]]],
       x[\[ScriptCapitalU][\[Lambda]]]]},
   {"Multiplied by Jacobian", 
    Partials[x[\[ScriptCapitalU][\[Beta]]], 
      x[\[ScriptCapitalU][\[Beta]']]]*
     Partials[
      Partials[x[\[ScriptCapitalU][\[Alpha]']], 
       x[\[ScriptCapitalU][\[Beta]]]], x[\[ScriptCapitalU][\[Alpha]]]]},
   {"Comma Notation", Partials[A[\[ScriptCapitalU][\[Mu]]], x[\[ScriptCapitalU][\[Nu]]]]},
   (* NEW: Covariant Derivative Syntax *)
{"Covariant Derivative (Abstract)", HoldForm[CD[A[\[ScriptCapitalU][\[Mu]]], x[\[ScriptCapitalU][\[Nu]]]]]},
{"Covariant Derivative (Expanded)", TensorForm@CD[A[\[ScriptCapitalU][\[Mu]]], x[\[ScriptCapitalU][\[Nu]]]]}
  };

(*Display as a formatted grid*)
Grid[Prepend[gallery /. {desc_, expr_} :>
    {Style[desc, Bold, 14], DisplayForm[ToBoxes[expr, StandardForm]]},
  {Style["Description", Bold, 16],
   Style["Rendered Output", Bold, 16]}],
 Frame -> All, Spacings -> {2, 2}, Alignment -> Left]
