(* ::Package:: *)

(* ::Section:: *)
(*Ansatz*)


(* ::Text:: *)
(*Set up a coordinate system in flat spacetime, at a safe distance from the central black hole, but with origin at that center. Measure all events with respect to this coordinate system. Solutions of these equations with respect to proper time account for spacetime curvature.*)


(* ::Text:: *)
(*Assume as a guess, or ansatz, the following form for an element of proper distance along a geodesic:*)


(* ::DisplayFormulaNumbered:: *)
(*d s^2=-A r d t^2+B r d r^2+r^2 d \[Theta]^2+r^2 Sin[\[Theta]]^2 d \[Phi]^2\[Congruent]Subscript[g, t t] d t^2+Subscript[g, r r] d r^2+Subscript[g, \[Theta] \[Theta]] d \[Theta]^2+Subscript[g, \[Phi] \[Phi]] d \[Phi]^2*)


(* ::Text:: *)
(*with two unknown functions of radius, A[r] and B[r], and with symbolic coordinates, t, r, \[Theta], \[Phi], all assumed to be functions of proper time, \[Tau], the time experienced by a particle in free-fall on a geodesic. Notice these functions do not depend on the angle coordinates \[Theta] and \[Phi]. That is the meaning of "spherically symmetric."*)


(* ::Subsection:: *)
(*Covariant Metric Tensor*)


(* ::Text:: *)
(*Express the covariant metric tensor of this ansatz as a diagonal matrix (Wolfram array); save the coordinates for later:*)


Echo[(SchwAnsatz=DiagonalMatrix[{-A[r],B[r],r^2,r^2 Sin[\[Theta]]^2}])//MatrixForm,"Schw ansatz :"];
coords={t,r,\[Theta],\[Phi]};


(* ::Item:: *)
(*Coordinate time, t, is also a function of proper time, expressing time dilation. *)


(* ::Item:: *)
(*The t t component of Subscript[g,  \[Mu] \[Nu]] is negative because we employ the (-1,1,1,1) sign convention for Lorentz invariance.*)


(* ::Subsection:: *)
(*Metric Rules*)


(* ::Text:: *)
(*Call the new compiler function, MatrixToUDRules, to create some rules that map \[ScriptCapitalU]\[ScriptCapitalD]-indexed components of a given symbol, say gDD, to the components of the ansatz matrix. We'll need these rules later to concretize an abstract expression for \[CapitalGamma]. *)


Echo[SchwRules=MatrixToUDRules[SchwAnsatz,gDD,\[ScriptCapitalD],coords], "Schw rules :"];


(* ::Item::Italic:: *)
(*For brevity, MatrixToUDRules doesn't produce rules for zeros in the matrix. We'll put those in later, by hand, in a concise way that exploits structural knowledge of g.*)


(* ::Subsection:: *)
(*Contravariant Metric Tensor and Rules*)


(* ::Text:: *)
(*The contravariant version, g^( \[Mu] \[Nu]), of g is just the matrix inverse of the covariant version. Ending with the needed rules for gUU, we get:*)


SchwAnsatzInv=Inverse[SchwAnsatz];
Echo[SchwInvRules=MatrixToUDRules[SchwAnsatzInv,gUU,\[ScriptCapitalU],coords], "Schw Inv rules :"];


(* ::Subsection:: *)
(*Christoffel Symbols*)


(* ::Text:: *)
(*The functions developed in an earlier notebook, Riemann Curvature in \[ScriptCapitalU]\[ScriptCapitalD] + Compiler POC, have been captured in a new compiler function, ChristoffelsFromMetric, which computes a commutator of covariant derivatives. See it as an unevaluated contraction over a bound Unique index:*)


Echo[ChristoffelsFromMetric[gDD,gUU,\[Sigma],\[Mu],\[Nu]], "Unevaluated Contraction :"];


(* ::Text:: *)
(*The new compiler function ContractAll exhibits the contraction carried out over all coordinates. Note that the up-down index pairs are now the coordinate symbols rather than summation indices. Each term in the following exhibit is a product of components, not a further (recursive) contraction.*)


ContractAll[ChristoffelsFromMetric[gDD,gUU,\[Sigma],\[Mu],\[Nu]] ,coords]


(* ::Subsubsection:: *)
(*Inspect One Component*)


(* ::Text:: *)
(*Now we see clearly the reason for the rules above:*)


Echo[\[CapitalGamma]rttX=ContractAll[ChristoffelsFromMetric[gDD,gUU,r,t,t] ,coords]/.
SchwRules /. 
SchwInvRules, "\!\(\*SubscriptBox[SuperscriptBox[\(\[CapitalGamma]\), \(\[VeryThinSpace]\(r\)\)], \(\[VeryThinSpace]\(t\[VeryThinSpace]t\)\)]\) ="];


(* ::Text:: *)
(*Notice the unevaluated partial derivatives in comma notation, -Subscript[A[r],  , r] and Subscript[A[r],  , t], Subscript[gDD,  r t , t],etc. *)


(* ::Subsubsection:: *)
(*Kill Off-Diagonal Elements*)


(* ::Text:: *)
(*Write concise rules to zero-out the off-diagonal components:*)


offDiagonalRules={
gUU[\[ScriptCapitalU][a_],\[ScriptCapitalU][b_]]/;a=!=b->0,
gDD[\[ScriptCapitalD][a_],\[ScriptCapitalD][b_]]/;a=!=b->0};
Echo[(\[CapitalGamma]rttY=\[CapitalGamma]rttX)/.offDiagonalRules,"\!\(\*SubscriptBox[SuperscriptBox[\(\[CapitalGamma]\), \(\[VeryThinSpace]\(r\)\)], \(\[VeryThinSpace]\(t\[VeryThinSpace]t\)\)]\) diag only ="];


(* ::Text:: *)
(*Notice, again, the unevaluated derivatives. \[ScriptCapitalU]\[ScriptCapitalD] doesn't evaluate derivatives until you tell it to do so.*)


(* ::Item:: *)
(*Killing the off-diagonal terms is not strictly necessary in this step as the next function, EvaluateDerivatives, would have zeroed them out anyway. But it's conceptually cleaner this way.*)


(* ::Subsubsection:: *)
(*Evaluate \[ScriptCapitalU]\[ScriptCapitalD] Partial Derivatives*)


(* ::Text:: *)
(*Another new \[ScriptCapitalU]\[ScriptCapitalD] compiler function, EvaluateUDPartials, produces a factor of A'[r], which is, of course, Subscript[A[r],  , r], just in terms (the tick notation) that Wolfram understands. We'll wrestle this into differential equations for A.*)


Echo[\[CapitalGamma]rttA=EvaluateUDPartials[\[CapitalGamma]rttY],"\!\(\*SubscriptBox[SuperscriptBox[\(\[CapitalGamma]\), \(\[VeryThinSpace]\(r\)\)], \(\[VeryThinSpace]\(t\[VeryThinSpace]t\)\)]\) evaluated ="];


(* ::Subsection:: *)
(*All \[CapitalGamma] Components*)


(* ::Text:: *)
(*Now that we're happy with a reduction strategy, apply it to all components of \[CapitalGamma] in a new 3D matrix, one dimension for each index:*)


(Schw\[CapitalGamma]=Table[
ContractAll[ChristoffelsFromMetric[gDD,gUU,\[Sigma],\[Mu],\[Nu]],coords]/.
SchwRules/.SchwInvRules/.offDiagonalRules//EvaluateUDPartials,
{\[Sigma],coords},{\[Mu],coords},{\[Nu],coords}])//
Echo[#//MatrixForm,"Ansatz \[CapitalGamma] ="]&;


(* ::Item:: *)
(*Here we really needed to kill the off-diagonal terms to produce a visually tolerable result. *)


(* ::Subsubsection:: *)
(*\[CapitalGamma] Indexer*)


(* ::Text:: *)
(*To reduce opportunities for human error, we really want to index objects with the coordinates rather than with numerical position numbers. We'd rather write \[CapitalGamma]get[r,t,t] than \[CapitalGamma][[1,0,0]]. MakeIndexer is the new compiler function that makes a function that maps symbolic coordinates to Part expressions. Let's bind and test \[CapitalGamma]get on an arbitrary component:*)


Echo[(\[CapitalGamma]Get=MakeIndexer[Schw\[CapitalGamma],coords])[r,r,r],"\!\(\*SubscriptBox[SuperscriptBox[\(\[CapitalGamma]\), \(\[VeryThinSpace]\(r\)\)], \(\[VeryThinSpace]\(r\[VeryThinSpace]r\)\)]\) ="];


(* ::Subsection:: *)
(*The Riemann Ansatz in Full*)


(* ::Text:: *)
(*Call another new compiler function, CalculateRiemannComponent, passing in \[CapitalGamma]get and the coordinate symbols. Iterate symbolic indices \[Mu], \[Lambda], \[Nu], and \[Rho] over all 256 combinations of the coordinates. There is far too much to inspect, by hand, here, but it's instructing to see some of it.*)


Echo[(SchwRiemann = Table[
  CalculateRiemannComponent[\[Mu],\[Lambda],\[Nu],\[Rho],\[CapitalGamma]Get,coords],
{\[Mu],coords},{\[Lambda],coords},{\[Nu],coords},{\[Rho],coords}])//Short[#,5]&,
"\!\(\*SubscriptBox[SuperscriptBox[\(R\), \(\[Mu]\)], \(\[Lambda]\[Nu]\[Rho]\)]\) ="];


(* ::Subsubsection:: *)
(*Riemann Indexer*)


(* ::Text:: *)
(*As with \[CapitalGamma], we'll index Riemann via coordinate symbols. Look at a couple of components and check a famous antisymmetry condition:*)


Echo[(RGet=MakeIndexer[SchwRiemann,coords])[t,r,t,r],"\!\(\*SubscriptBox[SuperscriptBox[\(R\), \(\[VeryThinSpace]\(t\)\)], \(\[VeryThinSpace]\(r\[VeryThinSpace]t\[VeryThinSpace]r\)\)]\) ="];
Echo[RGet[t,r,r,t],"\!\(\*SubscriptBox[SuperscriptBox[\(R\), \(\[VeryThinSpace]\(r\)\)], \(\[VeryThinSpace]\(t\[VeryThinSpace]t\[VeryThinSpace]r\)\)]\) ="];
Echo[RGet[t,r,r,t]===-RGet[t,r,t,r]]


(* ::Subsection:: *)
(*The Ricci Ansatz*)


(* ::Text:: *)
(*Via another new compiler function, CalculateRicciComponent, which contracts Riemann over its up index and its second down index. Pass in Rget and the coords, and iterate over the remaining 16 index values. *)


Echo[(SchwRicci=Table[
CalculateRicciComponent[\[Lambda],\[Rho],RGet,coords],
{\[Lambda],coords},{\[Rho],coords}])//TraditionalForm,"\!\(\*SubscriptBox[\(R\), \(\[VeryThinSpace]\(\[Lambda]\[VeryThinSpace]\[Rho]\)\)]\) ="];


(* ::Section:: *)
(*The Einstein Vacuum Equations*)


(* ::Text:: *)
(*We can see immediately that there is a lot of structure in Ricci. In particular, the t t and r r components are very similar. We'll wrestle a tight, easy equation out of them.*)


(* ::Text:: *)
(*Of course, we'll need a getter for Ricci.*)


RicciGet=MakeIndexer[SchwRicci,coords];
Echo[Rtt=RicciGet[t,t],"\!\(\*SubscriptBox[\(R\), \(\[VeryThinSpace]\(t\[VeryThinSpace]t\)\)]\) ="];
Echo[Rrr=RicciGet[r,r],"\!\(\*SubscriptBox[\(R\), \(\[VeryThinSpace]\(r\[VeryThinSpace]r\)\)]\) ="];


(* ::Text:: *)
(*Expand Subscript[R,  t t] and Subscript[R,  r r] to spot a way to get rid of A''[r]:*)


(* ::Input:: *)
(*Rtt//Expand*)


(* ::Input:: *)
(*Rrr//Expand*)


(* ::Text:: *)
(*It's obvious that Subscript[R,  t t]/A[r]+Subscript[R,  r r]/B[r] will do it:*)


(* ::Input:: *)
(*(Rtt/A[r]+Rrr/B[r])//Expand*)


(* ::Subsubsection:: *)
(*Relation between A[r] and B[r]*)


(* ::Text:: *)
(*Because all these terms are 0 (Ricci vanishes in the vacuum), we can now solve for B[r] in terms of A[r]:*)


Echo[DSolve[Rtt/A[r]+Rrr/B[r]==0,B[r],r],"Relation between A and B :"];


(* ::Subsubsection:: *)
(*Boundary Conditions at Infinity*)


(* ::Text:: *)
(*Apply boundary conditions: flat space at infinity, meaning \!\(\*UnderscriptBox[\(lim\), \(r -> \[Infinity]\)]\)A[r]=1 and \!\(\*UnderscriptBox[\(lim\), \(r -> \[Infinity]\)]\)B[r]=1 (time and space have their undistorted coordinate meanings in flat space, far from the black hole), to deduce that Subscript[\[ConstantC], 1]=1.\[ImplicitPlus]*)


(* ::Subsubsection:: *)
(*Final Equation and Solution*)


(* ::Text:: *)
(*Now, drag out the \[Theta] \[Theta] component and solve for a final differential equation for A[r]:*)


Echo[R\[Theta]\[Theta]=RicciGet[\[Theta],\[Theta]],"\!\(\*SubscriptBox[\(R\), \(\[VeryThinSpace]\(\[Theta]\[VeryThinSpace]\[Theta]\)\)]\) ="];
Echo[(finalODE=(R\[Theta]\[Theta]==0)/. {B[r]->1/A[r],B'[r]->D[1/A[r],r]}//
FullSimplify),"Vacuum ODE for A[r] :"];
Echo[SchwA=DSolve[finalODE,A[r],r],"Schwarzschild Solution :"];


(* ::Section:: *)
(*Constant of Integration*)


(* ::Text:: *)
(*We just need to figure out the new integration constant, Subscript[\[ConstantC], 1].*)


(* ::Text:: *)
(*Demand the Newtonian limit: that the black hole behave like a normal Newtonian star when we are far away from it. *)


(* ::Text:: *)
(*In this weak-field limit, it is a standard result (not derived in this notebook) that the t t component of the metric, Subscript[g,  t t], be directly related to the Newtonian gravitational potential \[CapitalPhi]:*)


(* ::DisplayFormulaNumbered:: *)
(*Subscript[g, t t]\[TildeTilde]-(1+2 \[CapitalPhi])*)


(* ::Text:: *)
(*in geometric units where G=c=1. *)


(* ::Text:: *)
(*For a star of mass M, \[CapitalPhi]=-M/r, therefore*)


(* ::DisplayFormulaNumbered:: *)
(*Subscript[g, t t]\[TildeTilde]-(1-(2 M)/r)*)


(* ::Text:: *)
(*Substituting our solution for A[r] into the ansatz, we have*)


(* ::DisplayFormulaNumbered:: *)
(*Subscript[g, t t]=-A[r]=-(1+Subscript[\[ConstantC], 1]/r)*)


(* ::Text:: *)
(*Thus Subscript[\[ConstantC], 1]=-2M.*)


(* ::Section:: *)
(*Validation Against Wolfram Function Repository*)


(* ::Text:: *)
(*Pulling together all our results:*)


Echo[SchwAnsatz/.{B[r]->1/A[r]}/.SchwA/.{C[1]->-2M},"Schwarzschild Solution via \[ScriptCapitalU]\[ScriptCapitalD] :"];


(* ::Text:: *)
(*and comparing against the WFR resource*)


Echo[ResourceFunction["MetricTensor"]["Schwarzschild"]["MatrixRepresentation"],"Schwarzschild Solution via WFR :"];
