(* ::Package:: *)

(* ========================================================================= *)
(* RELATIVITY TOOLKIT ENGINE (Script Mode)                                   *)
(* Version: 1.3.1 (Reset to Script + CD Logic)                               *)
(* ========================================================================= *)

RelativityToolkitVersion = "1.3.1";

(* 1. CLEAN SLATE ---------------------------------------------------------- *)
(* Unprotect and clear to ensure reloading works cleanly *)
Unprotect[MakeBoxes];
Quiet[MakeBoxes[Partials[_, _], StandardForm] =.];
Quiet[MakeBoxes[\[Delta][__], StandardForm] =.];
Quiet[MakeBoxes[CD[__], StandardForm] =.];
Quiet[MakeBoxes[Gamma[__], StandardForm] =.];
Quiet[DownValues[MakeBoxes] = Select[
    DownValues[MakeBoxes], 
    FreeQ[#, \[ScriptCapitalU]] && FreeQ[#, \[ScriptCapitalD]] &]];
Protect[MakeBoxes];

ClearAll[valence, CanonicalizeIndices, TensorForm, Partials, CD, Gamma, 
         upQ, downQ, x, p, g, u, A, B, T, P, Q, S, 
         metricRules, robustTransformRules, covariantDerivativeRules];

(* ========================================================================= *)
(* 2. VALENCE LOGIC (The Type Checker)                                       *)
(* ========================================================================= *)

(* --- Predicates --- *)
upQ[x_] := upQ[valence[x]];
downQ[x_] := downQ[valence[x]];
upQ[{{__}, {}}] := True; upQ[___] := False;
downQ[{{}, {__}}] := True; downQ[___] := False;

(* --- Helpers --- *)
contractValence[{u_List, d_List}] := Module[{common},
  common = Intersection[u, d];
  {DeleteElements[u, common], DeleteElements[d, common]}
];

(* --- Base Cases --- *)
valence[x_Symbol] := {{}, {}};
valence[_?NumericQ] := {{}, {}};
valence[] := Null;

(* --- Atoms --- *)
valence[_[\[ScriptCapitalU][alpha_]]] := {{alpha}, {}};
valence[_[\[ScriptCapitalD][alpha_]]] := {{}, {alpha}};

(* --- Metric & Delta --- *)
valence[g[\[ScriptCapitalD][mu_], \[ScriptCapitalD][nu_]]] := {{}, {mu, nu}};
valence[g[\[ScriptCapitalU][mu_], \[ScriptCapitalU][nu_]]] := {{mu, nu}, {}};
valence[\[Delta][\[ScriptCapitalU][mu_], \[ScriptCapitalD][nu_]]] := {{mu}, {nu}};

(* --- Partials --- *)
valence[Partials[num_, den_]] := Module[{vn, vd, vdFlipped},
  vn = valence[num]; vd = valence[den];
  vdFlipped = {vd[[2]], vd[[1]]};
  {Join[vn[[1]], vdFlipped[[1]]], Join[vn[[2]], vdFlipped[[2]]]}
];

(* --- Covariant Derivative (NEW) --- *)
valence[CD[\[ScriptCapitalD][mu_]][tensor_]] := Module[{u, d},
  {u, d} = valence[tensor];
  {u, Append[d, mu]} (* Adds a covariant index *)
];

(* --- Connection (NEW) --- *)
valence[Gamma[\[ScriptCapitalU][a_], \[ScriptCapitalD][b_], \[ScriptCapitalD][c_]]] := {{a}, {b, c}};

(* --- Differentiation --- *)
valence[Derivative[1][f_][arg_]] := Module[{u, d}, {u, d} = valence[arg]; {d, u}];
valence[Derivative[_][_][f_] /; (valence[f] =!= {{}, {}})] := valence[f];

(* --- Recursion --- *)
valence[(h : _[\[ScriptCapitalU][_]] | _[\[ScriptCapitalD][_]])[___]] := valence[h];
valence[prod_Times] := contractValence[Fold[Join[#1, valence[#2], 2] &, {{}, {}}, List @@ prod]];
valence[sum_Plus] := Module[{vs},
  vs = valence /@ (List @@ sum);
  If[Apply[SameQ, vs], First[vs], Print["Valence Mismatch"]; {{}, {}}]
];
valence[Power[b_, _]] := valence[b];
valence[h_[a_, b_, rest___]] := Join[valence[h[a]], valence[h[b, rest]], 2];
valence[___] := {{}, {}};


(* ========================================================================= *)
(* 3. DISPLAY RULES                                                          *)
(* ========================================================================= *)

Unprotect[MakeBoxes];

(* Partials *)
MakeBoxes[Partials[num_, den_], StandardForm] := 
  FractionBox[RowBox[{"\[PartialD]", MakeBoxes[num, StandardForm]}], 
              RowBox[{"\[PartialD]", MakeBoxes[den, StandardForm]}]];

(* CD Display: Del_mu T (NEW) *)
MakeBoxes[CD[\[ScriptCapitalD][idx_]][expr_], StandardForm] := 
  RowBox[{SubscriptBox["\[Nabld]", MakeBoxes[idx, StandardForm]], MakeBoxes[expr, StandardForm]}];

(* Gamma Display (NEW) *)
MakeBoxes[Gamma[\[ScriptCapitalU][u_], \[ScriptCapitalD][d1_], \[ScriptCapitalD][d2_]], StandardForm] :=
  SubsuperscriptBox["\[CapitalGamma]", 
    RowBox[{MakeBoxes[d1, StandardForm], MakeBoxes[d2, StandardForm]}], 
    MakeBoxes[u, StandardForm]];

(* Kronecker Delta *)
MakeBoxes[\[Delta][\[ScriptCapitalU][up_], \[ScriptCapitalD][down_]], StandardForm] := 
  SubsuperscriptBox["\[Delta]", MakeBoxes[down, StandardForm], MakeBoxes[up, StandardForm]];
MakeBoxes[\[Delta][\[ScriptCapitalD][down_], \[ScriptCapitalU][up_]], StandardForm] := 
  SubsuperscriptBox["\[Delta]", MakeBoxes[down, StandardForm], MakeBoxes[up, StandardForm]];

(* Generic Tensors *)
MakeBoxes[h_[indices__], StandardForm] /; 
  (h =!= \[Delta]) && (h =!= Partials) && (h =!= CD) && (h =!= Gamma) &&
  MatchQ[{indices}, { (_[\[ScriptCapitalU]] | _[\[ScriptCapitalD]] | \[ScriptCapitalU][_] | \[ScriptCapitalD][_]) .. }] := 
 Module[{formattedScripts},
  formattedScripts = {indices} /. {
     \[ScriptCapitalU][i_] :> SuperscriptBox["", MakeBoxes[i, StandardForm]],
     \[ScriptCapitalD][i_] :> SubscriptBox["", MakeBoxes[i, StandardForm]]
  };
  RowBox[Prepend[formattedScripts, MakeBoxes[h, StandardForm]]]
 ]

Protect[MakeBoxes];


(* ========================================================================= *)
(* 4. ALGEBRAIC SIMPLIFICATION                                               *)
(* ========================================================================= *)

CanonicalizeTerm[term_] := Module[{indices, counts, dummies, replacementRules},
   indices = Cases[term, (\[ScriptCapitalU] | \[ScriptCapitalD])[i_] :> i, Infinity];
   counts = Tally[indices];
   dummies = Select[counts, Last[#] == 2 &][[All, 1]];
   replacementRules = MapIndexed[#1 -> Symbol["\[FormalI]" <> ToString[First[#2]]] &, dummies];
   term /. replacementRules
  ];

CanonicalizeIndices[expr_Plus] := Total[CanonicalizeIndices /@ List @@ expr];
CanonicalizeIndices[expr_Equal] := Equal @@ (CanonicalizeIndices /@ List @@ expr);
CanonicalizeIndices[expr_] := CanonicalizeTerm[expr];


(* ========================================================================= *)
(* 5. PHYSICS RULES                                                          *)
(* ========================================================================= *)

metricRules = {
   (* Lowering *)
   g[\[ScriptCapitalD][mu_], \[ScriptCapitalD][nu_]] * vec_[\[ScriptCapitalU][nu_]] :> vec[\[ScriptCapitalD][mu]],
   vec_[\[ScriptCapitalU][nu_]] * g[\[ScriptCapitalD][mu_], \[ScriptCapitalD][nu_]] :> vec[\[ScriptCapitalD][mu]],
   g[\[ScriptCapitalD][nu_], \[ScriptCapitalD][mu_]] * vec_[\[ScriptCapitalU][nu_]] :> vec[\[ScriptCapitalD][mu]],
   vec_[\[ScriptCapitalU][nu_]] * g[\[ScriptCapitalD][nu_], \[ScriptCapitalD][mu_]] :> vec[\[ScriptCapitalD][mu]],

   (* Raising *)
   g[\[ScriptCapitalU][mu_], \[ScriptCapitalU][nu_]] * covec_[\[ScriptCapitalD][nu_]] :> covec[\[ScriptCapitalU][mu]],
   covec_[\[ScriptCapitalD][nu_]] * g[\[ScriptCapitalU][mu_], \[ScriptCapitalU][nu_]] :> covec[\[ScriptCapitalU][mu]],
   g[\[ScriptCapitalU][nu_], \[ScriptCapitalU][mu_]] * covec_[\[ScriptCapitalD][nu_]] :> covec[\[ScriptCapitalU][mu]],
   covec_[\[ScriptCapitalD][nu_]] * g[\[ScriptCapitalU][nu_], \[ScriptCapitalU][mu_]] :> covec[\[ScriptCapitalU][mu]],

   (* Inverse Identity *)
   g[\[ScriptCapitalU][mu_], \[ScriptCapitalU][alpha_]] * g[\[ScriptCapitalD][alpha_], \[ScriptCapitalD][nu_]] :> 
       \[Delta][\[ScriptCapitalU][mu], \[ScriptCapitalD][nu]],
   g[\[ScriptCapitalD][alpha_], \[ScriptCapitalD][nu_]] * g[\[ScriptCapitalU][mu_], \[ScriptCapitalU][alpha_]] :> 
       \[Delta][\[ScriptCapitalU][mu], \[ScriptCapitalD][nu]]
};

robustTransformRules = {
   A_[\[ScriptCapitalU][primed_]] :> Module[{fresh = Unique["\[Mu]"]}, 
     Partials[x[\[ScriptCapitalU][primed]], x[\[ScriptCapitalU][fresh]]] * A[\[ScriptCapitalU][fresh]]],
   p_[\[ScriptCapitalD][primed_]] :> Module[{fresh = Unique["\[Nu]"]}, 
     Partials[x[\[ScriptCapitalU][fresh]], x[\[ScriptCapitalU][primed]]] * p[\[ScriptCapitalD][fresh]]]
};

(* NEW: Covariant Derivative Rules *)
covariantDerivativeRules = {
   (* Linearity and Leibniz *)
   CD[idx_][a_ + b_] :> CD[idx][a] + CD[idx][b],
   CD[idx_][a_ * b_] :> CD[idx][a] * b + a * CD[idx][b],
   
   (* Scalar Rule *)
   CD[\[ScriptCapitalD][b_]][f_Symbol] /; (valence[f] === {{}, {}}) :> 
     Partials[f, x[\[ScriptCapitalU][b]]],
   
   (* Vector Rule (A^a) - Uses Module to ensure indices match! *)
   CD[\[ScriptCapitalD][b_]][A_[\[ScriptCapitalU][a_]]] :> 
     Module[{gam = Unique["\[Gamma]"]},
       Partials[A[\[ScriptCapitalU][a]], x[\[ScriptCapitalU][b]]] + 
       Gamma[\[ScriptCapitalU][a], \[ScriptCapitalD][b], \[ScriptCapitalD][gam]] * A[\[ScriptCapitalU][gam]]
     ],

   (* Covector Rule (A_a) - Uses Module to ensure indices match! *)
   CD[\[ScriptCapitalD][b_]][A_[\[ScriptCapitalD][a_]]] :> 
     Module[{gam = Unique["\[Gamma]"]},
       Partials[A[\[ScriptCapitalD][a]], x[\[ScriptCapitalU][b]]] - 
       Gamma[\[ScriptCapitalU][gam], \[ScriptCapitalD][b], \[ScriptCapitalD][a]] * A[\[ScriptCapitalD][gam]]
     ]
};


(* ========================================================================= *)
(* 6. PRETTY PRINTING                                                        *)
(* ========================================================================= *)

TensorForm[expr_] := Module[{canonExpr, indexMap, prettyIndices},
   canonExpr = CanonicalizeIndices[expr];
   prettyIndices = {\[Lambda], \[Kappa], \[Rho], \[Sigma], \[Mu], \[Nu], \[Tau], \[Eta], \[Chi], \[Psi]};
   indexMap = Table[Symbol["\[FormalI]" <> ToString[n]] -> prettyIndices[[n]], {n, Length[prettyIndices]}];
   HoldForm[Evaluate[canonExpr /. indexMap]]
];
