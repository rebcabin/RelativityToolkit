(* ================================= *)
(* Compiler Backend Proof of Concept *)
(* ================================= *)

(* Single-purpose compiler component that differentiates metric tensors w.r.t. coordinates in \[ScriptCapitalU]\[ScriptCapitalD]. *) 
(* demo by lowering Schwarzschild Christoffel symbols onto the Wolfram Function Repository (WFR). *)

Module[{metricObj, rawCoords, workingCoords, simpleMap, 
        g\[ScriptCapitalD]\[ScriptCapitalD], 
        g\[ScriptCapitalU]\[ScriptCapitalU], 
        symbolicGamma, myGammaArray, truthArray, diffMatrix, isMatch},

(* Pull covariant Schwarzschild metric from WFR. In \[ScriptCapitalU]\[ScriptCapitalD], this is g[\[ScriptCapitalD][\[Mu]],\[ScriptCapitalD][\[Nu]]] *)

metricObj = ResourceFunction["MetricTensor"]["Schwarzschild"];

(* Rationalize Symbols *)

(* Inexplicably, Schwarzschild from WFR has strings containing formal symbols, *)
(* which print just like formal symbols. Reveal via FullForm on rawCoords *)

rawCoords = metricObj["Coordinates"];

(* \[ScriptCapitalU]\[ScriptCapitalD] will have problems later taking derivatives with respect to strings. *)
(* Just map them to ordinary formal variables, input via Esc-.t-Esc, etc. *)

workingCoords = {\[FormalT], \[FormalR], \[FormalTheta], \[FormalPhi]};
simpleMap = Thread[rawCoords -> workingCoords];

(* Covariant Subscript[g, \[Mu] \[Nu]] *)

(g\[ScriptCapitalD]\[ScriptCapitalD] = 
 metricObj["MatrixRepresentation"] /. 
   simpleMap /. {"\[FormalCapitalM]" -> \[FormalCapitalM]}) // Echo[#, "g_mn"]&;

(* Contravariant g^( \[Mu] \[Nu]) *)

(* We'll need the inverse, i.e., the contravariant metric, later for the Christoffel symbols *)

g\[ScriptCapitalU]\[ScriptCapitalU] = 
 FullSimplify[Inverse[g\[ScriptCapitalD]\[ScriptCapitalD]]];

(* Compiling Derivatives of the Metric *)

(* Two-pass pipeline compilation of derivatives (algebrize then differentiate).  *)

(* translate from 0-based indexing to 1-based at the last possible place *)

(* Export as a symbol in Global *)

ClearAll[DifferentiateTheMetric];
DifferentiateTheMetric[expr_, gDD_, gUU_] :=
  Module[{lowerGeometry, activateCalculus},
   (* PASS 1: Geometry->Algebra*)
   (* Correct for physics g_00 to Wolfram [[1,
   1]] at the last possible moment *)
   lowerGeometry = expr //. {
      g[\[ScriptCapitalD][a_], \[ScriptCapitalD][b_]] :> gDD[[1 + a, 1 + b]],
      g[\[ScriptCapitalU][a_], \[ScriptCapitalU][b_]] :> gUU[[1 + a, 1 + b]]};
   (* PASS 2: transpile \[ScriptCapitalU]\[ScriptCapitalD] Partials \[LongRightArrow] 
   Wolfram D *)
   activateCalculus = lowerGeometry //.
     {Partials[content_, x[\[ScriptCapitalU][dir_]]] :>
       D[content, workingCoords[[1 + dir]]]};
   Simplify[activateCalculus]];

(* Manual construction of components of \[CapitalGamma] in \[ScriptCapitalU]\[ScriptCapitalD], again with conventional (non-Wolfram) zero-based indexing: *)

symbolicGamma[\[Sigma]_, \[Mu]_, \[Nu]_] :=
  Module[{\[Lambda]},(* dummy index *)
   Sum[1/2 g[\[ScriptCapitalU][\[Sigma]], \[ScriptCapitalU][\[Lambda]]]*
     (Partials[g[\[ScriptCapitalD][\[Lambda]], \[ScriptCapitalD][\[Mu]]], 
        x[\[ScriptCapitalU][\[Nu]]]] +
       Partials[g[\[ScriptCapitalD][\[Lambda]], \[ScriptCapitalD][\[Nu]]], 
        x[\[ScriptCapitalU][\[Mu]]]] -
       Partials[g[\[ScriptCapitalD][\[Mu]], \[ScriptCapitalD][\[Nu]]], 
        x[\[ScriptCapitalU][\[Lambda]]]]), {\[Lambda], 0, 3}]];

(myGammaArray = Table[
    DifferentiateTheMetric[
     symbolicGamma[l, m, n], 
     g\[ScriptCapitalD]\[ScriptCapitalD], 
     g\[ScriptCapitalU]\[ScriptCapitalU]],
    {l, 0, 3}, {m, 0, 3}, {n, 0, 3}]) // Echo[# // MatrixForm, "\[CapitalGamma] from \[ScriptCapitalU]\[ScriptCapitalD]"]&;

(* "ground truth" from WFR: *)

(truthArray = 
  ResourceFunction["ChristoffelSymbol"][
   g\[ScriptCapitalD]\[ScriptCapitalD], workingCoords]) // Echo[# // Simplify // MatrixForm, "\[CapitalGamma] from WFR"]&;
   
diffMatrix = Simplify[myGammaArray - truthArray];

(isMatch = AllTrue[Flatten[diffMatrix], (PossibleZeroQ[#] || # == 0) &])
   // Echo[#, "computed \[CapitalGamma] == ground truth from WFR"]& ;

]; (* end Module *)
