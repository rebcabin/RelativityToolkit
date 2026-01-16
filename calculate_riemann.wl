(* DEFINITION:The Commutator*)
(* \!\(
\*SubscriptBox[\(\[Del]\), \(\[Rho]\)]\(
\*SubscriptBox[\(\[Del]\), \(\[Nu]\)]
\*SuperscriptBox[\(A\), \(\[Mu]\)]\)\) - \!\(
\*SubscriptBox[\(\[Del]\), \(\[Nu]\)]\(
\*SubscriptBox[\(\[Del]\), \(\[Rho]\)]
\*SuperscriptBox[\(A\), \(\[Mu]\)]\)\) *)
Echo[term1 = 
   CD[CD[A[\[ScriptCapitalU][\[Mu]]], x[\[ScriptCapitalU][\[Nu]]]], 
    x[\[ScriptCapitalU][\[Rho]]]], 
  "\!\(\*SubscriptBox[\(\[Del]\), \(\[Rho]\)]\)\!\(\*SubscriptBox[\(\
\[Del]\), \(\[Nu]\)]\)\!\(\*SuperscriptBox[\(A\), \(\[Mu]\)]\):"];
Echo[term2 = 
    CD[CD[A[\[ScriptCapitalU][\[Mu]]], x[\[ScriptCapitalU][\[Rho]]]], 
     x[\[ScriptCapitalU][\[Nu]]]], 
   "\!\(\*SubscriptBox[\(\[Del]\), \(\[Nu]\)]\)\!\(\*SubscriptBox[\(\
\[Del]\), \(\[Rho]\)]\)\!\(\*SuperscriptBox[\(A\), \(\[Mu]\)]\):"] ;;
  
  Echo[commutator = term1 - term2,
   "raw commutator"];

(* STEP 1: Simplify *)
(* 1.1 Expand derivatives *)
 Echo[expanded = ExpandDerivatives[commutator],
  "xpd commutator:"]; 

Echo[tmp$ = expanded - commutator, "Expanded - SealedBox diagnostic:"];

(* 1.2 Expand algebraically and apply torsion rules *)
(* crucial to separate \[CapitalGamma]*Partial from \[CapitalGamma]*\
\[CapitalGamma] *)
Echo[simplified = 
   CanonicalizeIndices[expanded // Expand] //. torsionRules,
  "alg. xp + torsion elmin:"];

(* STEP 2: Verify Algebra *)
(* Does the second derivative of A vanish? *)
Echo[hasSecondDerivative = ! FreeQ[simplified, Partials[Partials[__]]],
  "test: has 2d derivs?"];

(* Apply Quotient Theorem *)
Echo[rawRiemann = ExtractCoefficient[simplified, A],
  "raw Riemann"];

Print["\n=== THE RIEMANN CURVATURE TENSOR ==="];

(* 5. Readable Greek *)
Echo[TensorForm[rawRiemann], 
  "\!\(\*SubscriptBox[SuperscriptBox[\(R\), \(\[Mu]\)], \(\[Lambda]\
\[Nu]\[Rho]\)]\) ="];
