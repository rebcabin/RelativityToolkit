(* =========================================================================*)
(*DERIVATION:The Fundamental Theorem of Riemannian Geometry*)
(*(Deriving the Christoffel Symbols from the Metric)*)
(* =========================================================================*)
ClearAll[g, GammaSym, term1, term2, term3, sum, solution];

(*1. DEFINE PHYSICAL CONSTRAINTS------------------------------------------*)

(*Constraint A:Torsion-Free Geometry (Gamma is Symmetric)*)
(*Gamma^a_bc==Gamma^a_cb*)
GammaSymmetryRule = \[CapitalGamma][
    u_, \[ScriptCapitalD][b_], \[ScriptCapitalD][
     c_]] :> \[CapitalGamma][
    u, \[ScriptCapitalD][Sort[{b, c}][[1]]], \[ScriptCapitalD][
     Sort[{b, c}][[2]]]];

(*Constraint B:Metric Symmetry*)
(*g_ab==g_ba*)
MetricSymmetryRule = 
  g[\[ScriptCapitalD][a_], \[ScriptCapitalD][b_]] :> 
   g[\[ScriptCapitalD][Sort[{a, b}][[1]]], \[ScriptCapitalD][
     Sort[{a, b}][[2]]]];


(*2. THE CYCLIC PERMUTATION TRICK-----------------------------------------*)
(*We write the equation CD[g]=0 for three permutations of indices*)
(*The engine now handles CD[g] natively!*)

Print["\n=== 1. The Metric Compatibility Condition (Expanded) ==="];
(*Term 1:nabla_lambda g_mu_nu*)
term1 = CD[g[\[ScriptCapitalD][\[Mu]], \[ScriptCapitalD][\[Nu]]], 
     x[\[ScriptCapitalU][\[Lambda]]]] /. GammaSymmetryRule /. 
   MetricSymmetryRule;
Echo[TensorForm[term1], "Nabla_\[Lambda] g_\[Mu]\[Nu] = "];

(*Term 2:nabla_mu g_nu_lambda (Cyclic)*)
term2 = CD[g[\[ScriptCapitalD][\[Nu]], \[ScriptCapitalD][\[Lambda]]], 
     x[\[ScriptCapitalU][\[Mu]]]] /. GammaSymmetryRule /. 
   MetricSymmetryRule;

(*Term 3:nabla_nu g_lambda_mu (Cyclic)*)
term3 = CD[g[\[ScriptCapitalD][\[Lambda]], \[ScriptCapitalD][\[Mu]]], 
     x[\[ScriptCapitalU][\[Nu]]]] /. GammaSymmetryRule /. 
   MetricSymmetryRule;


Print["\n=== 2. The Linear Combination (Term1 + Term2 - Term3) ==="];
(*We form the specific sum that isolates one Gamma term.*)
(*Standard textbook trick:d_mu g_nu_lam+d_nu g_lam_mu-d_lam g_mu_nu*)

combination = (term2 + term3 - term1);

(*Simplify by collecting terms*)
simplifiedSum = combination // Simplify;

Echo[TensorForm[simplifiedSum], "Sum (must be zero) = "];


(*3. SOLVE FOR GAMMA------------------------------------------------------*)
Print["\n=== 3. The Solution ==="];

(*The result is:(Partials...)-2*Gamma*g*)
(*So:2*Gamma*g=(Partials...)*)

(*Isolate the Gamma term manually to show the structure*)
gammaTerm = 
  Select[List @@ simplifiedSum, ! FreeQ[#, \[CapitalGamma]] &][[1]];
metricPartials = 
  Select[List @@ simplifiedSum, FreeQ[#, \[CapitalGamma]] &];

(*Construct the equation*)
equation = (-gammaTerm) == Total[metricPartials];

Print["Isolating the Connection term:"];
Print[TensorForm[equation]];

Print["\nMultiply by inverse metric (1/2 g^\[Sigma]\[Rho]) to get:"];

(*Visual representation of the final formula*)
finalFormula = 
  Row[{SubsuperscriptBox["\[CapitalGamma]", RowBox[{"\[Mu]", "\[Nu]"}],
      "\[Sigma]"], " = ", FractionBox["1", "2"], 
    SuperscriptBox["g", RowBox[{"\[Sigma]", "\[Lambda]"}]], "(", 
    RowBox[{SubscriptBox["g", 
       RowBox[{"\[Nu]", "\[Lambda]", ",", "\[Mu]"}]], "+", 
      SubscriptBox["g", RowBox[{"\[Lambda]", "\[Mu]", ",", "\[Nu]"}]],
       "-", SubscriptBox["g", 
       RowBox[{"\[Mu]", "\[Nu]", ",", "\[Lambda]"}]]}], ")"}];

Print[DisplayForm[finalFormula]];
