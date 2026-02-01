(* ::Package:: *)

(* ::Title:: *)
(*Gauge Theory in \[ScriptCapitalU]\[ScriptCapitalD]*)


(* ::Author:: *)
(*Brian Beckman*)


(* ::Author:: *)
(*Feb 2026*)


(* ::Section:: *)
(*Abstract*)


(* ::Text:: *)
(*This notebook extends the Relativity Toolkit (v1.9.0) from general relativity into the domain of Classical Gauge Theory. We demystify the abstract concept of a "gauge field" by connecting it to the familiar concept of a reference frame. Just as inertial forces (like centrifugal force) arise from the choice of a rotational reference frame, we argue that fundamental forces\[LongDash]including electrodynamics and the weak interaction\[LongDash]arise from choices of "internal" reference frames (gauges).*)


(* ::Text:: *)
(*In the (\[ScriptCapitalU]\[ScriptCapitalD] (up-down) tensorial calculus, we implement a generalized covariant-derivative operator that handles both spacetime geometry (Christoffel connections) and internal phase geometry (gauge connections) simultaneously. We validate this engine through four automated, symbolic proofs:*)


(* ::Item:: *)
(*Metric Compatibility: the Levi-Civita connection is the unique structure preserving the metric (\[Del]g=0)*)


(* ::Item:: *)
(*U(1) Gauge Covariance: the covariant derivative of a scalar field transforms via a simple phase rotation*)


(* ::Item:: *)
(*Field Strength Invariance: the electromagnetic tensor Subscript[F, \[Mu] \[Nu]]\:200b is invariant under local gauge transformations*)


(* ::Item:: *)
(*Charged Vector Boson Covariance: Simulating the U(1) sector of the Weinberg-Salam electroweak theory to prove covariance for a field W^ \[Alpha] that enjoys both spacetime and internal charge invariance*)


(* ::Text:: *)
(*Finally, we apply the updated engine to re-derive the Riemann curvature tensor from the commutator of covariant derivatives. *)


(* ::Item:: *)
(*A .wl script version of code for this notebook is available at https://github.com/rebcabin/RelativityToolkit/blob/main/non_quantum_gauge_theory.wl. *)


(* ::Section:: *)
(*Initialize the Relativity Toolkit*)


(* ::Subsection:: *)
(*Optional: Quit for a Fresh Kernel*)


(* ::Item:: *)
(*Note, if you call Quit in a notebook, evaluation may hang. If you want to "Evaluate Notebook," mark the following two cells "Evaluatable"  in the Cell->Property menu, call Quit and FrontEndTokenExecute once each, then comment out the cells, or mark them unevaluatable again. Alternatively, you can quit the kernel via the Evaluation menu (last item). At that point, you can "Evaluate Notebook" in a clean environment, or evaluate cells individually.*)


(* ::Input:: *)
(*Quit[];*)


(* ::Input:: *)
(*FrontEndTokenExecute["DeleteGeneratedCells"]*)


(* ::Subsection:: *)
(*Download the Code*)


RTbranch="main";
RTbase="https://raw.githubusercontent.com/rebcabin/RelativityToolkit/"<>RTbranch<>"/";
RTbust="?t="<>ToString[Round[AbsoluteTime[]]];(*Force fresh download*)
RTrules=RTbase<>"RelativityToolkit.wl"<>RTbust;
RTtests=RTbase<>"RelativityToolkit_RegressionTests.wl"<>RTbust;
RTviz=RTbase<>"RelativityToolkit_VisualShowcase.wl"<>RTbust;
Get[RTrules];


(* ::Subsection:: *)
(*Optional: See all Tests and Results from Prior Works*)


(* ::Text:: *)
(*Make any of the following cells evaluatable if you wish. They're unevaluatable by default to keep this notebook trim.*)


(* ::Input:: *)
(*Get[RTtests];*)


(* ::Input:: *)
(*Get[RTviz](* no semicolon, on purpose *)*)


(* ::Input:: *)
(*Get[RTbase<>"covariant_derivative.wl"]*)


(* ::Input:: *)
(*Get[RTbase<>"law_of_gamma.wl"]*)


(* ::Input:: *)
(*Get[RTbase<>"connections_to_christoffels.wl"]*)


(* ::Input:: *)
(*Get[RTbase<>"schwarzschild_from_scratch.wl"]*)


(* ::Section:: *)
(*All Physics is Gauge Theory*)


(* ::Text:: *)
(*Look at the dictionary definition of "gauge:"*)


(* ::ItemNumbered:: *)
(*n. A device or instrument for measuring the magnitude, amount, or contents of something, typically with a visual display. Ex.: a fuel gauge*)


(* ::ItemNumbered:: *)
(*n. The thickness, size, or capacity of something, especially as a standard measure. Ex.: the gauge of a rail line.*)


(* ::ItemNumbered:: *)
(*v. To estimate or determine the magnitude, amount, or volume of. Ex. Gauge the intrinsic luminosity of a star.*)


(* ::ItemNumbered:: *)
(*v. To measure the dimensions of an object with a gauge instrument. Ex. Gauge the assemblies and plane them to width.*)


(* ::Text:: *)
(*All this is refreshingly concrete, referring to ordinary objects and activities one might see in the trades or in the physics lab. None of this hints at the abstract wizardry of gauge theory for electrodynamics, the nuclear forces, and even gravitation. Where are covariance, invariance, symmetry, conservation, unitarity, group theory, fibre bundles, and so on, in these definitions? *)


(* ::Text:: *)
(*The answer is the other way around: all these abstractions are just mathematical machinery for various kinds of gauging, in the sense of measuring, various quantities in electrodynamics, the nuclear forces, and gravitation. In fact, we can't measure anything without setting up a gauge. Most familiarly, we must choose coordinates for measuring positions and momenta in spacetime. Setting a gauge is setting a coordinate basis for these quantities. Likewise, we must choose a reference for measuring the phase of an electromagnetic field. Setting a gauge for electrodynamics is setting that phase reference. By analogy to position and momentum, we can say that setting the phase reference is setting a coordinate system for phase. *)


(* ::Text:: *)
(*It seems that coordinates, reference frames, zero points, and bases are all so deeply related as to purpose and necessity that we can call them "gauges" and we can call the activities of setting them up as "setting gauge" or "choosing gauge."*)


(* ::Text:: *)
(*Now, to the surprising statement: All Forces Arise from Choice of Gauge. *)


(* ::Text:: *)
(*We're not accustomed to this idea. We imagine forces as intrinsic physics, independent of gauges used to measure them. Planets are attracted to the Sun by a gravitational force. Electrons float around atomic nuclei by an attractive Coulomb force. Springs and pulleys apply forces to masses. All without reference to a coordinate frame or gauge. *)


(* ::Text:: *)
(*But forces change motions, and motions must be measured in coordinate systems, gauges. Different choices of gauge yield different values for motion measurements, so different gauges imply different forces to produce the observed motions. Some of these forces are only apparent, centrifugal and Coriolis forces for example. We can make them go away by choosing different a different gauge, say an inertial frame. Other forces are such that no choice of gauge can make them go away. The electric force is like that, and so is geodesic deviation, which manifests as an unavoidable force in curved spacetime. *)


(* ::Text:: *)
(*Choice of gauge can change quantities, like phase, other than force. By finding the symmetries\[LongDash]the choices of gauge that don't change the physics\[LongDash]we find conservation laws via Noether's theorem. When choice of the zero of stopwatches doesn't affect the physics, when the physics is invariant with gauge choice for time, when time symmetry obtains, then energy is conserved. When choice of the zero of position measurements doesn't affect the physics, when the physics is invariant with gauge choice for position, when position symmetry obtains, momentum is conserved. When choice of a zero for phase doesn't affect the physics, when the physics is invariant with gauge choice for phase, when phase symmetry obtains, electric charge is conserved. And so on with respect to invariance to unitary transformations in nuclear physics: hypercharge and color charge are conserved. *)


(* ::Text:: *)
(*In elementary physics, we don't call choice of reference frame a "gauge choice." Nor do we hear the exotic idea that gauge choice generates force. But such is the case. For an elementary example, consider measuring the altitude of an Earth satellite flying in a stable, circular, free-fall orbit. If we measure altitude with respect to mean sea level, the satellite seems to have a constant altitude, Earth's oblateness aside, and no forces on it can be measured. However, if we measure altitude with respect to the irregular surface of the Earth, the satellite's altitude varies madly up and down. To naive observers, unaware of this poor choice of gauge, the satellite appears to suffer inexplicable forces pushing it up and down. However, these apparent forces arise merely from the choice of gauge. Here, the term "gauge" fits entirely with its ordinary, dictionary meaning, as an altimeter with its arbitrary origin setting. An even more elementary example is a rotating reference frame. If we measure positions and momenta of free particles in an inertial lab frame, we see no forces. If, however, we measure from a rotating frame, we see centrifugal and Coriolis forces, arising merely from the choice of gauge, of reference frame.*)


(* ::Text:: *)
(*Gauge theory is totally pertinent to classical mechanics, but physics students don't study "gauge" until classical electrodynamics, where it's an abstract, arbitrary field whose gradient is added to the vector potential. There is no conceivable relationship of this idea to the ordinary meaning of "gauge." By that time, it's too late to go back and explain the forces of elementary mechanics as arising from gauge choice. Our poor physics students will carry multiple bags, all the way through general relativity (GR) without an interpretation of "gauge," until the word appears again as more abstract fields in particle physics. Our students could have packed all their ideas into one piece of baggage with a concrete view of gauge as just a choice of reference frame.*)


(* ::Text:: *)
(*All forces, apparent and real, do, in fact, arise from choice of gauge. For the "real" forces, we can't choose a gauge that gets rid of them, as we could do with inertial frames. But the symmetries of gauge choice drive conservation laws, via Noether's theorem, which really should be the travel sticker on the baggage and the visa stamps in our passports as we journey through all of physics. *)


(* ::Section:: *)
(*Gauge Forces as Connections*)


(* ::Text:: *)
(*Now, with some precision, let's see how connections are the ultimate source of force from gauge choice. Recall from earlier notebooks that in GR, the Christoffel symbols are the connections that separate intrinsic curvature of spacetime, that no choice of gauge can eliminate, from extrinsic curvature caused by choice of gauge. At a single point, we can always choose a local inertial frame, a gauge that makes Christoffel symbols vanish, and acceleration, thus force, go away. This is Einstein's famous "elevator" demonstration of the Principle of Equivalence. The paths of two particles, initially parallel, will converge or diverge in a curved spacetime, however, and no choice of gauge can make such "tidal forces" go away. *)


(* ::Text:: *)
(*We've studied covariant derivative, connections, and Christoffel symbols in this series of notebooks, specifically in relativity. We've repeatedly read that we can and should recast these ideas in terms of gauge theory. Now is the time, with the clarity that gauge choice is just choice of reference frame, though in a generalized sense. We've seen that geometrical frame invariance, now geometrical gauge invariance, forces the "discovery" of covariant derivative and connection. We've seen that connection drives equations of motion, thus forces, for Schwarzschild orbits. We're now positioned to explore the other forces of Nature on the same terms.*)


(* ::Text:: *)
(*Back to our physics students, with lighter baggage, encountering gauge again, not for the first time, in classical electrodynamics. One realizes that the gradient of any differentiable scalar field \[CapitalLambda], rightfully called a gauge, can be added to the vector potential without altering the physics:*)


(* ::DisplayFormulaNumbered:: *)
(*Subscript[\!\(\*OverscriptBox[\(A\), \(_\)]\), \[Mu]][x^\[Nu]]=Subscript[A, \[Mu]][x^\[Nu]]+\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\(\[CapitalLambda][*)
(*\*SuperscriptBox[\(x\), \(\[Nu]\)]]\)\)*)


(* ::Text:: *)
(*In what sense is \[CapitalLambda] the choice of a reference frame? Electrodynamical gauge invariance is presented as phase invariance, a factor of E^( i \[CapitalLambda]) that can be multiplied into a charged matter field without altering the physics. Spoiler: choose phase in an abstract reference frame. This is the primary principle of electrodynamical gauge as a coordinate frame. Gradient added to vector potential is the mechanism, the "induced transformation," that upholds that principle. Here's how that works:*)


(* ::Text:: *)
(*Change a matter field \[Psi] by rotating its phase (gauge) by an arbitrary (but smoothly varying) amount, \[CapitalLambda](x^\[Nu]), at every point x^\[Nu], without changing the geometrical coordinates x^ \[Mu]:*)


(* ::DisplayFormulaNumbered:: *)
(*\!\(\*OverscriptBox[\(\[Psi]\), \(_\)]\)[x^\[Nu]]=E^(i \[CapitalLambda][x^\[Nu]]) \[Psi] x^\[Nu]*)


(* ::Text:: *)
(*This is just like choosing a coordinate system, with an arbitrary origin, locally, at each point, for the phase of the matter fields, without changing the spacetime geometry. *)


(* ::Text:: *)
(*Take the ordinary partial derivative, Subscript[\[PartialD], \[Mu]], of this new field \!\(\*OverscriptBox[\(\[Psi]\), \(_\)]\). The ordinary product rule spits out a "garbage term" because \[CapitalLambda] varies with position:*)


(* ::DisplayFormulaNumbered:: *)
(*\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\( *)
(*\*OverscriptBox[\(\[Psi]\), \(_\)]( *)
(*\*SuperscriptBox[\(x\), \(\[Nu]\)])\)\)=E^(i \[CapitalLambda](x^\[Nu])) (\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\(\[Psi]( *)
(*\*SuperscriptBox[\(x\), \(\[Nu]\)])\)\)+i(\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\[CapitalLambda]\)(x^\[Nu]))\[Psi](x^\[Nu])),or,abbreviating*)
(*\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]*)
(*\*OverscriptBox[\(\[Psi]\), \(_\)]\)=E^(i \[CapitalLambda]) (\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\[Psi]\)+i(\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\[CapitalLambda]\))\[Psi])*)


(* ::Text:: *)
(*Sound familiar? This is exactly what happened in relativity. The "garbage terms" drove our "discovery" of covariant derivative and connection coefficients. Here, the extra term i(\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\[CapitalLambda]\))\[Psi] ruins the rest of the physics. *)


(* ::Text:: *)
(*As before, we find that the ordinary derivative is not covariant: it doesn't survive changes in phase coordinates, changes in choice of \[CapitalLambda](x^ \[Mu]).*)


(* ::Text:: *)
(*In GR, the gauge was spacetime geometry itself, so compensating for coordinate change required 4D index manipulation. With electrodynamics, compensating for the phase coordinate changes is easier. The electrodynamical connection is a one-index object because the electrodynamical gauge field is one-dimensional.*)


(* ::Text:: *)
(*To clean up the electrodynamical gauge garbage, we need a correction term in our covariant derivative of the matter field, just as we did with geometry and GR:*)


(* ::DisplayFormulaNumbered:: *)
(*Subscript[D, \[Mu]]\[Congruent]\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\((\(-i\))\)\) Subscript[A, \[Mu]]*)


(* ::Text::Bold:: *)
(*Yes, the connection is the vector potential, hence it drives forces! *)


(* ::Text:: *)
(*For the garbage terms to cancel out exactly, Subscript[A, \[Mu]] must transform as we learned in school, as in Equation 1 (abbreviated):*)


(* ::DisplayFormulaNumbered:: *)
(*Subscript[\!\(\*OverscriptBox[\(A\), \(_\)]\), \[Mu]]=Subscript[A, \[Mu]]+\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\[CapitalLambda]\)*)


(* ::Text:: *)
(*We have *)


(* ::DisplayFormulaNumbered:: *)
(*Subscript[D, \[Mu]] \[Psi]\[Congruent]\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\[Psi]\)-i Subscript[A, \[Mu]] \[Psi]*)


(* ::Text:: *)
(*and*)


(* ::DisplayFormulaNumbered:: *)
(*Subscript[D, \[Mu]]\!\(\*OverscriptBox[\(\[Psi]\), \(_\)]\)=\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]*)
(*\*OverscriptBox[\(\[Psi]\), \(_\)]\)-i Subscript[\!\(\*OverscriptBox[\(A\), \(_\)]\), \[Mu]]\!\(\*OverscriptBox[\(\[Psi]\), \(_\)]\)=E^(i \[CapitalLambda]) (\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\[Psi]\)+i(\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\[CapitalLambda]\))\[Psi])-i Subscript[\!\(\*OverscriptBox[\(A\), \(_\)]\), \[Mu]]\!\(\*OverscriptBox[\(\[Psi]\), \(_\)]\)*)
(*=E^(i \[CapitalLambda]) (\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\[Psi]\)+i(\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\[CapitalLambda]\))\[Psi])-i(Subscript[A, \[Mu]]+\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\[CapitalLambda]\))(E^(i \[CapitalLambda]) \[Psi])*)
(*=E^(i \[CapitalLambda]) \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\[Psi]\)+E^(i \[CapitalLambda]) i(\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\[CapitalLambda]\))\[Psi]-E^(i \[CapitalLambda]) i Subscript[A, \[Mu]]\[Psi]-E^(i \[CapitalLambda]) i(\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\[CapitalLambda]\))\[Psi]*)
(*=E^(i \[CapitalLambda]) \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\[Psi]\)-E^(i \[CapitalLambda]) i Subscript[A, \[Mu]]\[Psi]*)
(*=E^(i \[CapitalLambda])(\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\[Psi]\)-i Subscript[A, \[Mu]]\[Psi])*)


(* ::Text:: *)
(*transforming covariantly in gauge space with respect to the gauge choice E^( i \[CapitalLambda]). It so happens that Subscript[A,  \[Mu]] is a covariant covector, so it also transforms covariantly with change in coordinates x^ \[Nu]. Two covariant derivatives, one against phase, the other against geometrical coordinates. One idea of covariance.*)


(* ::Item:: *)
(*We now see why it's a good idea, after all, to call the covariant derivative covariant. Such honors the metaphor everywhere without being exactly correct in GR, where the covariant derivative also pertains to contravariant indices, a fact we dismissed as "historical" in an earlier notebook.*)


(* ::Text:: *)
(*We'll write some new functions for the Relativity Toolkit to automate the calculation above, after just one last philosophical section, which bridges our old material for GR more deeply to the new material on gauges in general.*)


(* ::Section:: *)
(*Covariant Derivatives, Revisited*)


(* ::Text:: *)
(*In GR, we learned to take covariant derivatives of tensors that inhabit the tangent and cotangent tensor spaces of curved manifolds. The canonical example is the covariant derivative of a contravariant vector with respect to a basis of coordinate functions:*)


With[{example=HoldForm[CD[A[\[ScriptCapitalU][\[Mu]]],x[\[ScriptCapitalU][\[Nu]]]]]},
Echo[TensorForm@@example,example ""]];


(* ::Text:: *)
(*The connection coefficients \[CapitalGamma] account for the twisting and turning of basis vectors in the tangent spaces as the vector A^ \[Mu] is parallel-transported. In GR, this twisting and turning causes geodesics to converge or diverge*)


(* ::Item:: *)
(*Geodesic deviation is indistinguishable from a "tidal" force. It's not an apparent force: we can't get rid of it by a gauge choice.*)


(* ::Text:: *)
(*The derivative above transforms like a tensor, ensuring physics is locally Lorentz invariant. This invariance invites application of Noether's theorem, implying a conserved quantity. In GR, the conserved quantity due to local Lorentz invariance is spin current, from symmetries of non-coordinate bases (see Appendix 1).*)


(* ::Text:: *)
(*In this notebook, we consider new spaces "attached" to points of the manifold like wallpaper\[LongDash]gauge fields representing electromagnetism and nuclear matter. These wallpaper spaces have their own differential geometries above and beyond the geometry of spacetime itself. *)


(* ::Text:: *)
(*In gravitation, gauge forces arise FROM the geometry. In other theories, gauge forces arise ON the geometry.*)


(* ::Item:: *)
(*That's one reason gravitation resists unification with the other forces of Nature, as explained more fully in the appendices.*)


(* ::Text:: *)
(*While it is fine to envision gauge fields as "wallpaper," the rigorous mathematical machinery for this is that of fibre bundles (Appendix 2).*)


(* ::Text:: *)
(*The goal of this notebook is to apply \[ScriptCapitalU]\[ScriptCapitalD] to these wallpaper gauge fields, encoding transformation laws, invariants, and conserved quantities with the same rigor we applied to GR.*)


(* ::Section:: *)
(*A New CD*)


(* ::Text:: *)
(*This new overload of CD can compute Abelian gauge connections like scalar phase. These connections arise on sections of the principal bundle of Appendix 2. We'll get to non-Abelian gauge connections like U(1)\[CircleTimes]SU(2) in future notebooks.*)


(* ::Subsection:: *)
(*Gauge Covariant Derivative*)


(* ::Subsubsection:: *)
(*Inputs*)


(* ::ItemNumbered:: *)
(*expr: The field being differentiated, scalar or tensor*)


(* ::ItemNumbered:: *)
(*var: the coordinate function, e.g., x[\[ScriptCapitalU][\[Mu]]], the differentiation variable*)


(* ::ItemNumbered:: *)
(*q: the coupling constant or "charge"*)


(* ::ItemNumbered:: *)
(*A: the gauge-field connection symbol. \[CapitalGamma] is implicitly kept as the spacetime connection for all theories*)


(* ::Input:: *)
(*CD[expr_,var_,q_,A_]:=*)
(*Module[{gravitationalPart,gaugeCorrection,idx},*)
(*(*Calculate the coordinate Covariant Derivative via existing CD overloads.*)*)
(*(*Handle \[PartialD] + Christoffels (if expr is a tensor)*)*)
(*(*If expr is a scalar, just return \[PartialD].*)*)
(*gravitationalPart=CD[expr,var];*)
(*(*Extract the index from the differentiation variable.*)*)
(*(*var is some x[\[ScriptCapitalU][\[Mu]]]; we need \[Mu]*)*)
(*idx=var[[1,1]];*)
(*(*Construct the Gauge Correction, e.g., -i q Subscript[A, \[Mu]] * expr*)*)
(*(*Note: This A has a DOWN (Covariant) index matching the derivative*)*)
(*gaugeCorrection=-I*q*A[\[ScriptCapitalD][idx]]*expr;*)
(*(*Combine*)*)
(*gravitationalPart+gaugeCorrection];*)


(* ::Section:: *)
(*Electrodynamics Demos*)


(* ::Subsection:: *)
(*Covariance of the Covariant Derivative*)


(* ::Text:: *)
(*In "Forces as Gauge Connections," we subjected the following expression to a 1D (Abelian) gauge transformation in a manual calculation. Include a specific, constant charge, q:*)


(cde=CD[\[Psi],x[\[ScriptCapitalU][\[Mu]]],q,A])


(* ::Text:: *)
(*Let's automate that calculation. Starting with the gauge transformations*)


(* ::DisplayFormula:: *)
(*\!\(\*OverscriptBox[\(\[Psi]\), \(_\)]\)=E^(i q \[CapitalLambda]) \[Psi] and Subscript[\!\(\*OverscriptBox[\(A\), \(_\)]\), \[Mu]]=Subscript[A, \[Mu]]+\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\(\[CapitalLambda][*)
(*\*SuperscriptBox[\(x\), \(\[Nu]\)]]\)\)*)


gaugeTransforms={
\[Psi]->E^(I q \[CapitalLambda]) \[Psi],
A[\[ScriptCapitalD][\[Mu]_]]:>A[\[ScriptCapitalD][\[Mu]]]+Partials[ \[CapitalLambda],x[\[ScriptCapitalU][\[Mu]]]]}


(* ::Text:: *)
(*prove that the covariant derivative*)


(* ::DisplayFormula:: *)
(*Subscript[D, \[Mu]] \[Psi]\[Congruent]\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\[Psi]\)-i q Subscript[A, \[Mu]] \[Psi]*)


(* ::Text:: *)
(*transforms exactly the same way as \[Psi]*)


(* ::DisplayFormula:: *)
(*Subscript[D, \[Mu]] \!\(\*OverscriptBox[\(\[Psi]\), \(_\)]\)=E^(i q \[CapitalLambda]) Subscript[D, \[Mu]] \[Psi]*)


(* ::Text:: *)
(*We just need one rule for the charge constant. Everything else is already in the toolkit's differentiationRules:*)


constantRules = {Partials[q,x[\[ScriptCapitalU][\[Mu]_]]] :> 0}


transformedCde=cde /. gaugeTransforms //. differentiationRules/.constantRules//Expand//Simplify


transformedCde===E^(I q \[CapitalLambda]) cde


(* ::Text:: *)
(*QED\[NonBreakingSpace]\[FilledSquare]*)


(* ::Subsection:: *)
(*Invariance of the Field-Strength Tensor*)


(* ::Text:: *)
(*The field-strength tensor, Subscript[F, \[Mu] \[Nu]], aka Faraday, is defined as follows*)


F[\[Mu],\[Nu]]=Partials[A[\[ScriptCapitalD][\[Nu]]], x[\[ScriptCapitalU][\[Mu]]]] - Partials[A[\[ScriptCapitalD][\[Mu]]], x[\[ScriptCapitalU][\[Nu]]]]


(* ::Text:: *)
(*usually presented as \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]*)
(*\*SubscriptBox[\(A\), \( \(\[Nu]\)\)]\)-\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Nu]\)]*)
(*\*SubscriptBox[\(A\), \( \(\[Mu]\)\)]\) to make the indices line up, but our notation checks out.*)


(* ::Text:: *)
(*Now, subject it to the gauge transform:*)


F[\[Mu],\[Nu]]/. gaugeTransforms


(* ::Text:: *)
(*See that it doesn't change by invoking the toolkit's ExpandDerivatives:*)


F[\[Mu],\[Nu]]/. gaugeTransforms // ExpandDerivatives 


(* ::Text:: *)
(*QED\[NonBreakingSpace]\[FilledSquare]*)


(* ::Subsection:: *)
(*Note: Charge Conservation as a Consequence*)


(* ::Text:: *)
(*By proving that the covariant derivative transforms just by a phase factor, we proved that physical laws built from these derivatives remain invariant (unchanged) under gauge transformations.*)


(* ::Text:: *)
(*In the Lagrangian, phase factors in terms like \!\(\*OverscriptBox[\(\[Psi]\), \(_\)]\) Subscript[D, \[Mu]]\[Psi] cancel (E^(i q \[CapitalLambda])*E^(-i q \[CapitalLambda])), showing our theory has a continuous-in-\[CapitalLambda] U(1) symmetry (invariance).*)


(* ::Text:: *)
(*According to Noether's Theorem, every continuous symmetry implies a conservation law.*)


(* ::Item:: *)
(*time-translation symmetry \[DoubleLongRightArrow] conservation of energy*)


(* ::Item:: *)
(*spatial translation symmetry \[DoubleLongRightArrow] conservation of momentum*)


(* ::Item:: *)
(*U(1) phase symmetry \[DoubleLongRightArrow] conservation of electric charge*)


(* ::Text:: *)
(*Thus, the "gauge correction" symmetry is the direct mathematical reason that electric charge is conserved.*)


(* ::Section:: *)
(*Charged Vector Boson Covariance Demo*)


(* ::Text:: *)
(*In this demo, we differentiate a charged contravariant vector field W^ \[Alpha], coupled to a neutral field Subscript[Z,  \[Mu]] by a scalar hypercharge Subscript[g, W]. Subscript[Z,  \[Mu]] acts like a vector phase correction in the non-geometric part of the covariant derivative. In addition to this non-geometric phase correction, W^ \[Alpha] generates a geometrical correction proportional to \[CapitalGamma], as with any tensor. *)


(* ::Text:: *)
(*We did not need a geometrical correction in electrodynamics because we were differentiating a scalar field \[Psi]. Its geometrical covariant derivative is exactly equal to its ordinary partial derivative\[LongDash]the twisting and turning of spacetime does not change a scalar value. We do not need a geometrical correction for Subscript[Z,  \[Mu]] because it is not the subject of differentiation in this case.*)


(* ::Text:: *)
(*This expression occurs in the electroweak, U(1) sector, an Abelian sector (multiple phase operations commute), of the Standard Model of Particle physics. In future notebooks, we will cover non-Abelian symmetries like U(1)\[CircleTimes]SU(2).*)


(* ::Text:: *)
(*We proceed just as above*)


(wsd=CD[W[\[ScriptCapitalU][\[Alpha]]],x[\[ScriptCapitalU][\[Mu]]],gW,Z])//TensorForm


mixedGaugeTransforms = {
    W[\[ScriptCapitalU][\[Alpha]_]] :> E^(I gW \[CapitalLambda])*W[\[ScriptCapitalU][\[Alpha]]],
    Z[\[ScriptCapitalD][\[Mu]_]] :> Z[\[ScriptCapitalD][\[Mu]]] + Partials[\[CapitalLambda], x[\[ScriptCapitalU][\[Mu]]]]}


transformedWsd=wsd/.mixedGaugeTransforms//.differentiationRules/.{Partials[gW, _] :> 0}//Expand//TensorForm


expectedWsd=wsd*E^(I gW \[CapitalLambda])//Expand//TensorForm


transformedWsd===expectedWsd


(* ::Text:: *)
(*QED\[NonBreakingSpace]\[FilledSquare]*)


(* ::Section:: *)
(*Geometrical GR Demos*)


(* ::Subsection:: *)
(*Covariant Derivative of a Scalar Function \[CapitalLambda]*)


(* ::Text:: *)
(*Equal to its ordinary partial derivative, rendered in comma notation*)


CD[\[CapitalLambda],x[\[ScriptCapitalU][\[Mu]]]]


(* ::Subsection:: *)
(*Covariant Derivative of a Contravariant Vector V^ \[Mu]*)


CD[V[\[ScriptCapitalU][\[Mu]]],x[\[ScriptCapitalU][\[Nu]]]]//TensorForm


(* ::Subsection:: *)
(*Covariant Derivative of a (2,0) Tensor*)


CD[T[\[ScriptCapitalU][\[Mu]],\[ScriptCapitalU][\[Nu]]],x[\[ScriptCapitalU][\[Rho]]]]//TensorForm


(* ::Subsection:: *)
(*Covariant Derivative of a (1,1) Tensor*)


CD[T[\[ScriptCapitalU][\[Mu]],\[ScriptCapitalD][\[Nu]]],x[\[ScriptCapitalU][\[Rho]]]]//TensorForm


(* ::Subsection:: *)
(*Covariant Derivative of the Covariant Metric*)


(cdg=CD[g[\[ScriptCapitalD][\[Mu]],\[ScriptCapitalD][\[Nu]]],x[\[ScriptCapitalU][\[Rho]]]])//TensorForm


(* ::Subsection:: *)
(*Metric Compatibility, \[Del]g=0*)


(* ::Text:: *)
(*\[Del]g=0 was an axiom in earlier notebooks, and it yielded the Christoffel symbols for Riemannian geometry. Here, we do the opposite, showing that the Christoffel symbols imply metric compatibility. Thus, metric compatibility is both necessary and sufficient for the Christoffel symbols.*)


(* ::Item:: *)
(*In abstract geometric contexts, as opposed to coordinate-basis contexts, the Christoffel symbols are called the Levi-Civita Connection., often written {{*)
(* {\[Lambda]},*)
(* {\[Mu] \[Nu]}*)
(*}}.*)


(* ::Item:: *)
(*Christoffel symbols were derived in Covariant Derivatives and Connections in the \[ScriptCapitalU]\[ScriptCapitalD] Calculus, assuming both torsion freedom (symmetry in lower two indices of \[CapitalGamma]), and metric compatibility*)


(* ::Item:: *)
(*If we allow torsion, as in Einstein-Cartan theory or as in Appendix 1, the present tautology is not valid. In such cases, we must introduce the contortion tensor K*)


(* ::DisplayFormulaNumbered:: *)
(*Subscript[\[CapitalGamma]^\[Lambda], \[Mu] \[Nu]]={{{\[Lambda]},{\[Mu] \[Nu]}}}+Subscript[K^\[Lambda], \[Mu] \[Nu]]*)


(* ::ItemParagraph:: *)
(*We do not develop this case further in this notebook.*)


(* ::Text:: *)
(*For pedagogy, clear the definitions loaded from the toolkit:*)


(* ::Input:: *)
(*ClearAll[ruleLeviCivita,cdgWithLeviCivitaConnection,metricDifferentiationRules];*)


(* ::Text:: *)
(*Start with the covariant derivative of the metric, cdg, from above*)


(* ::Text:: *)
(*Fetch the Levi-Civita connection via a new rule, added to the Relativity Toolkit v1.9.0:*)


(* ::DisplayFormulaNumbered:: *)
(*Subscript[\[CapitalGamma]^\[Lambda], \[Mu] \[Rho]]=1/2 g^(\[Lambda] \[Sigma]) (Subscript[g, \[Sigma] \[Rho],\[Mu]]+Subscript[g, \[Sigma] \[Mu]\[InvisibleComma],\[Rho]]-Subscript[g, \[Mu] \[Rho]\[InvisibleComma],\[Sigma]])*)


(* ::Input:: *)
(*ruleLeviCivita=\[CapitalGamma][\[ScriptCapitalU][\[Lambda]_],\[ScriptCapitalD][a_],\[ScriptCapitalD][b_]]:>*)
(*Module[{\[Sigma]=Unique["\[Sigma]"]},*)
(*1/2 g[\[ScriptCapitalU][\[Lambda]],\[ScriptCapitalU][\[Sigma]]]**)
(*(Partials[g[\[ScriptCapitalD][\[Sigma]],\[ScriptCapitalD][b]],x[\[ScriptCapitalU][a]]]+*)
(*Partials[g[\[ScriptCapitalD][\[Sigma]],\[ScriptCapitalD][a]],x[\[ScriptCapitalU][b]]]-*)
(*Partials[g[\[ScriptCapitalD][a],\[ScriptCapitalD][b]],x[\[ScriptCapitalU][\[Sigma]]]])]*)


(* ::Text:: *)
(*In cdg, use this rule to replace \[CapitalGamma] with its definition, then Expand to expose terms to further processing *)


(cdgWithLeviCivitaConnection=cdg/. ruleLeviCivita//Expand)//TensorForm


(* ::Text:: *)
(*Now consider the following new Metric Differentiation Rules in the toolkit:*)


(* ::Text:: *)
(*They include*)


(* ::Item:: *)
(*metric symmetry: Subscript[g,  \[Mu] \[Nu]]=Subscript[g,  \[Nu] \[Mu]], require by standard general relativity*)


(* ::Item:: *)
(*metric contraction to the Kronecker delta: Subscript[g,  \[Lambda] \[Nu]] g^( \[Lambda] \[Mu])=\!\(\*SubsuperscriptBox[\(\[Delta]\), \(\[VeryThinSpace]\(\[Nu]\)\), \(\[VeryThinSpace]\(\[Mu]\)\)]\)*)


(* ::Item:: *)
(*contraction of Kronecker delta on derivatives, for the chain rule: \!\(\( *)
(*\*SubsuperscriptBox[\(\[Delta]\), \( \(\[Nu]\)\), \( \(\[Mu]\)\)] *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\) = *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Nu]\)]\)*)


(* ::Item:: *)
(*delta contraction inside metric derivatives: \!\( *)
(*\*SubsuperscriptBox[\(\[Delta]\), \(\[Nu]\), \( \(\[Mu]\)\)] *)
(*\*SubscriptBox[\(g\), \( \(\[Mu] \[Lambda]\)\)]\)=Subscript[g,  \[Nu] \[Lambda]], \!\( *)
(*\*SubsuperscriptBox[\(\[Delta]\), \(\[Nu]\), \( \(\[Mu]\)\)] *)
(*\*SubscriptBox[\(g\), \( \(\[Lambda] \[Mu]\)\)]\)=Subscript[g,   \[Lambda] \[Nu]]*)


(* ::Input:: *)
(*metricDifferentiationRules={*)
(*(*Metric Symmetry*)*)
(*g[\[ScriptCapitalD][a_],\[ScriptCapitalD][b_]]/;!OrderedQ[{a,b}]:>g[\[ScriptCapitalD][b],\[ScriptCapitalD][a]],*)
(*g[\[ScriptCapitalU][a_],\[ScriptCapitalU][b_]]/;!OrderedQ[{a,b}]:>g[\[ScriptCapitalU][b],\[ScriptCapitalU][a]],*)
(*(*Permissive Contraction (All 4 Alignments)*)*)
(*(*First-First*)*)
(*g[\[ScriptCapitalD][s_],\[ScriptCapitalD][a_]]*g[\[ScriptCapitalU][s_],\[ScriptCapitalU][b_]]:>\[Delta][\[ScriptCapitalU][b],\[ScriptCapitalD][a]],*)
(*(*Second-Second*)*)
(*g[\[ScriptCapitalD][a_],\[ScriptCapitalD][s_]]*g[\[ScriptCapitalU][b_],\[ScriptCapitalU][s_]]:>\[Delta][\[ScriptCapitalU][b],\[ScriptCapitalD][a]],*)
(*(*First-Second (Redundant but Safe)*)*)
(*g[\[ScriptCapitalD][s_],\[ScriptCapitalD][a_]]*g[\[ScriptCapitalU][b_],\[ScriptCapitalU][s_]]:>\[Delta][\[ScriptCapitalU][b],\[ScriptCapitalD][a]],*)
(*(*Second-First*)*)
(*g[\[ScriptCapitalD][a_],\[ScriptCapitalD][s_]]*g[\[ScriptCapitalU][s_],\[ScriptCapitalU][b_]]:>\[Delta][\[ScriptCapitalU][b],\[ScriptCapitalD][a]],*)
(*(*Delta Contractions*)*)
(*\[Delta][\[ScriptCapitalU][bound_],\[ScriptCapitalD][free_]]*Partials[expr_,x[\[ScriptCapitalU][bound_]]]:>Partials[expr,x[\[ScriptCapitalU][free]]],*)
(*\[Delta][\[ScriptCapitalU][bound_],\[ScriptCapitalD][free_]]*Partials[g[\[ScriptCapitalD][bound_],\[ScriptCapitalD][other_]],v_]:>Partials[g[\[ScriptCapitalD][free],\[ScriptCapitalD][other]],v],*)
(*\[Delta][\[ScriptCapitalU][bound_],\[ScriptCapitalD][free_]]*Partials[g[\[ScriptCapitalD][other_],\[ScriptCapitalD][bound_]],v_]:>Partials[g[\[ScriptCapitalD][other],\[ScriptCapitalD][free]],v]};*)


(* ::Text:: *)
(*The result is immediate upon application of both the old metric rules and the new metric rules:*)


0===(cdgWithLeviCivitaConnection//.(metricRules~Join~metricDifferentiationRules))


(* ::Text:: *)
(*QED\[NonBreakingSpace]\[FilledSquare]*)


(* ::Subsection:: *)
(*Riemann Revisited*)


(* ::Text:: *)
(*Calculate the commutator: [\!\( *)
(*\*SubscriptBox[\(\[Del]\), \(\[Rho]\)], *)
(*\*SubscriptBox[\(\[Del]\), \(\[Nu]\)]\)]A^ \[Mu]:*)


term1 = CD[CD[A[\[ScriptCapitalU][\[Mu]]], x[\[ScriptCapitalU][\[Nu]]]], x[\[ScriptCapitalU][\[Rho]]]];
term2 = CD[CD[A[\[ScriptCapitalU][\[Mu]]], x[\[ScriptCapitalU][\[Rho]]]], x[\[ScriptCapitalU][\[Nu]]]];
(comm = term1 - term2)//TensorForm


(* ::Text:: *)
(*Simplification Pipeline*)


calculatedRiemann=(
(comm//
ExpandDerivatives//
Expand//
CanonicalizeIndices)//.
 torsionRules )//
ExtractCoefficient[#,A]&//
TensorForm


(* ::Text:: *)
(*Textbook definition: Subscript[\[CapitalGamma]^ \[Mu],  \[Nu] \[FormalS] , \[Rho]]-Subscript[\[CapitalGamma]^ \[Mu],  \[Rho] \[FormalS] , \[Nu]]+Subscript[\[CapitalGamma]^ \[Mu],  \[Rho] \[Lambda]] Subscript[\[CapitalGamma]^ \[Lambda],  \[Nu] \[FormalS]]-Subscript[\[CapitalGamma]^ \[Mu],  \[Nu] \[Lambda]] Subscript[\[CapitalGamma]^ \[Lambda],  \[Rho] \[FormalS]]*)


rawTextbook=Partials[\[CapitalGamma][\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Nu]], \[ScriptCapitalD][\[FormalS]]], x[\[ScriptCapitalU][\[Rho]]]] - 
Partials[\[CapitalGamma][\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Rho]], \[ScriptCapitalD][\[FormalS]]], x[\[ScriptCapitalU][\[Nu]]]] + 
\[CapitalGamma][\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Rho]], \[ScriptCapitalD][\[Lambda]]] * \[CapitalGamma][\[ScriptCapitalU][\[Lambda]], \[ScriptCapitalD][\[Nu]], \[ScriptCapitalD][\[FormalS]]] -
\[CapitalGamma][\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Nu]], \[ScriptCapitalD][\[Lambda]]] * \[CapitalGamma][\[ScriptCapitalU][\[Lambda]], \[ScriptCapitalD][\[Rho]], \[ScriptCapitalD][\[FormalS]]]


expectedRiemann=(rawTextbook//.torsionRules)//TensorForm


calculatedRiemann===expectedRiemann


(* ::Text:: *)
(*QED\[NonBreakingSpace]\[FilledSquare]*)


(* ::Section:: *)
(*Appendix 1: Spin Connection as Gravitational Gauge*)


(* ::Text:: *)
(*Gravitation acts almost like any other gauge theory, but not quite. Gravitational forces arise from geometrical connections in the geometry, while forces in other gauge theories arise from non-gravitational connections on the geometry, connections in "wallpaper" spaces like U(1) and SU(3). This partial fit between gravitation and other gauge theories only emphasizes the frustration of those seeking to unify all the force laws of Nature.*)


(* ::Text:: *)
(*To make gravitation a gauge theory, we must find conserved quantities associated from local gauge invariance. The definitive review paper is the following:*)


(* ::Item:: *)
(*Friedrich W. Hehl et al., General Relativity with Spin and Torsion: Foundations and Prospects (Rev. Mod. Phys. 48, 393, 1976)*)


(* ::ItemParagraph:: *)
(*which shows that:*)


(* ::Subitem:: *)
(*Invariance under general transformations (diffeomorphisms), implies conservation of the energy-momentum tensor.*)


(* ::Subitem:: *)
(*Invariance under local Lorentz transformations implies conservation of the spin angular momentum tensor (the spin current).*)


(* ::Subitem:: *)
(*Standard Einstein GR with vanishing torsion, which we assumed in this series of notebooks, collapses the two symmetries and hides the independent spin current.*)


(* ::Text:: *)
(*Some other references are*)


(* ::Item:: *)
(* Sean Carroll, Spacetime and Geometry: An Introduction to General Relativity*)


(* ::Subitem:: *)
(*Appendix J, Page 486 ff, under local Lorentz invariance, tetrads, and the spin connection:*)


(* ::ItemParagraph:: *)
(*detailing how spin connection \!\(\*SubsuperscriptBox[\(\[Omega]\), \(\[Mu]\), \(\[VeryThinSpace]\(a\[VeryThinSpace]b\)\)]\), arising from non-coordinate basis sets, acts as the gauge field for local Lorentz transformations. Carroll also explains that in standard GR the torsion tensor Subscript[T^ \[Lambda],  \[Mu] \[Nu]] is zero, locking the Levi-Civita connection (Christoffel symbols) to the metric and forcing the energy-momentum tensor to be symmetric, absorbing spin effects via symmetrization.*)


(* ::Subitem:: *)
(*Also see Hehl, Equation 3.8 and Sections III.B and III.C. In other references, this may be called Belinfante-Rosenfeld symmetrization.*)


(* ::Item:: *)
(*T. W. B. Kibble, Lorentz Invariance and the Gravitational Field (J. Math. Phys. 2, 212, 1961)*)


(* ::Subitem:: *)
(*Foundational paper deriving gravitation as the gauge theory of the Poincar\[EAcute] group, i.e., Lorentz + translations*)


(* ::ItemParagraph:: *)
(*Kibble shows that (1) gauging just translations in Poincar\[EAcute] yields the tetrad field, defining the metric and conserving to energy-momentum, and that (2) gauging the Lorentz subgroup, boost + rotation, yields the spin connection, permitting transformation of spinors, otherwise impossible.*)


(* ::Item:: *)
(*Video: Carmeci Academy. General Relativity | Review and Intro to The Spin Connection, https://www.youtube.com/watch?v=LjydxyNY7Yg*)


(* ::Section:: *)
(*Appendix 2: Fibre Bundles*)


(* ::Text:: *)
(*In this appendix, we introduce the standard mathematical language. We present bundles, projections, fibres, sections, connections, and invariance, with each concept applied to both geometry/gravitation and to more general gauge fields.*)


(* ::Subsection:: *)
(*TL;DR: Dictionary*)


(* ::Text:: *)
(*\!\( *)
(*TagBox[GridBox[{*)
(*{*)
(*StyleBox["Concept", "Text",*)
(*FontWeight->"Bold",*)
(*FontSlant->"Italic"], *)
(*StyleBox[*)
(*RowBox[{*)
(*RowBox[{"Gauge", " ", "Theory"}], "*)
(*", *)
(*RowBox[{"(", "electrodynamics", ")"}]}], "Text",*)
(*FontWeight->"Bold",*)
(*FontSlant->"Italic"], *)
(*StyleBox[*)
(*RowBox[{*)
(*RowBox[{"Geometry", "/", "Gravitation"}], "\[IndentingNewLine]", *)
(*RowBox[{"(", *)
(*RowBox[{"general", " ", "relativity"}], ")"}]}], "Text",*)
(*FontWeight->"Bold",*)
(*FontSlant->"Italic"], *)
(*StyleBox[*)
(*RowBox[{"Interpretation", "*)
(*", *)
(*RowBox[{"and", " ", "Analogy"}]}], "Text",*)
(*FontWeight->"Bold",*)
(*FontSlant->"Italic"]},*)
(*{" ", " ", " ", *)
(*StyleBox[" ", "Text"]},*)
(*{*)
(*StyleBox[*)
(*RowBox[{"Principal", " ", "Bundle"}], "Text"], *)
(*StyleBox[*)
(*RowBox[{"Phase", " ", "Bundle", " ", *)
(*RowBox[{"(", *)
(*StyleBox["P",*)
(*FontSlant->"Italic"], ")"}]}], "Text"], *)
(*StyleBox[*)
(*RowBox[{"Frame", " ", "Bundle", " ", *)
(*RowBox[{"(", *)
(*StyleBox["LM",*)
(*FontSlant->"Italic"], ")"}]}], "Text"], *)
(*StyleBox[*)
(*RowBox[{"All", " ", "Possible", " ", "Wallpapers"}], "Text"]},*)
(*{*)
(*StyleBox[*)
(*RowBox[{"Principal", " ", "Section"}], "Text"], *)
(*StyleBox[*)
(*RowBox[{"Local", " ", "Phase", " ", "Choice"}], "Text"], *)
(*StyleBox[*)
(*RowBox[{"Local", " ", "Frame", " ", "Choice", " ", *)
(*RowBox[{"(", "Tetrad", ")"}]}], "Text"], *)
(*StyleBox[*)
(*RowBox[{"Smooth", " ", "Wallpaper", " ", "Choices"}], "Text"]},*)
(*{*)
(*StyleBox[*)
(*RowBox[{"Structure", " ", "Group"}], "Text"], *)
(*StyleBox[*)
(*RowBox[{"U", *)
(*RowBox[{"(", "1", ")"}]}], "Text"], *)
(*StyleBox[*)
(*RowBox[{"GL", *)
(*RowBox[{"(", *)
(*RowBox[{"4", ",", "\[DoubleStruckCapitalR]"}], ")"}], " ", "or", " ", "SO", *)
(*RowBox[{"(", *)
(*RowBox[{"3", ",", "1"}], ")"}]}], "Text"], *)
(*StyleBox[*)
(*RowBox[{"Design", " ", "Rules"}], "Text"]},*)
(*{*)
(*StyleBox[*)
(*RowBox[{"Associated", " ", "Bundle"}], "Text"], *)
(*StyleBox[*)
(*RowBox[{"Complex", " ", "Line", " ", "Bundle"}], "Text"], *)
(*StyleBox[*)
(*RowBox[{"Tangent", " ", "Bundle", " ", *)
(*RowBox[{"(", *)
(*StyleBox["TM",*)
(*FontSlant->"Italic"], ")"}]}], "Text"], *)
(*StyleBox[*)
(*RowBox[{"All", " ", "Possible", " ", "Walls"}], "Text"]},*)
(*{*)
(*StyleBox[*)
(*RowBox[{"Associated", " ", "Section"}], "Text"], *)
(*StyleBox[*)
(*RowBox[{"Charged", " ", "Scalar", " ", "Field"}], "Text"], *)
(*StyleBox[*)
(*RowBox[{"Vector", " ", "Field", " ", *)
(*SuperscriptBox["V", "\[Mu]"]}], "Text"], *)
(*StyleBox[*)
(*RowBox[{"Particular", " ", "Wall", " ", "Choices"}], "Text"]},*)
(*{*)
(*StyleBox["Connection", "Text"], *)
(*StyleBox[*)
(*RowBox[{"Vector", " ", "Potential", " ", *)
(*SubscriptBox["A", "\[Mu]"]}], "Text"], *)
(*StyleBox[*)
(*RowBox[{*)
(*RowBox[{"Christoffel", " ", "/", "Spin"}], " ", "Connection"}], "Text"], *)
(*StyleBox[*)
(*RowBox[{"Wallpaper", " ", "Glue"}], "Text"]}*)
(*}],*)
(*"Grid"]\)*)


(* ::Subsection:: *)
(*Bundles*)


(* ::Text:: *)
(*Spaces attached to each point of a manifold are collected and flattened into sets called bundles. They are fibre bundles of projection functions (detail below). *)


(* ::Text:: *)
(*In applications, there are two distinguished bundles: the principal bundle and the associated bundle, in a matched pair. Looking ahead, connections on sections of the principal bundle are "sources" of forces, and the associated bundles supply the quantities subject to forces, "victims" of forces. This is clarified below under sections and connections.*)


(* ::Subsubsection:: *)
(*Geometry / Gravitation*)


(* ::Text:: *)
(*Let Subscript[T, p]M (an indivisible notation) be the set of all tangent vectors at point p of the manifold. Likewise, the cotangent bundle T^ \[Star] M is the set of all points paired with all covectors at each point, and so on for each valence (combination of up-down indices) of tensor. *)


(* ::Text:: *)
(*An associated bundle (victims of forces) of gravitation is the tangent bundle T M, the disjoint union of all points p in the manifold paired with all vectors v ripped out of their tangent spaces at p, flattened into one big set of pairs:*)


(* ::DisplayFormulaNumbered:: *)
(*T M\[Congruent]\!\(\*UnderscriptBox[\(\[UnionPlus]\), \(p \[Element] M\)]\){v\[Element]Subscript[T, p]M}={(p,v)\[VerticalSeparator]p\[Element]M,v\[Element]Subscript[T, p]M}(associated bundle,victims of forces)*)


(* ::Item:: *)
(*The other geometrical bundles, for tensors of other valences, can also be associated bundles (victims of forces).*)


(* ::Text:: *)
(*The principal bundle (sources of forces) is the frame bundle, which supplies, at each point, all possible bases for frames of reference. *)


(* ::Item:: *)
(*All possible bases include the ordinary coordinate bases that conceal spin and the tetrad bases that reveal the spin connection, source of spin-coupled forces (Appendix 1).*)


(* ::Text:: *)
(*Again, for emphasis, the associated bundles (victims of forces) are the associated tangent, cotangent, etc. bundles, which supply all possible tensors at each point.*)


(* ::Subsubsection:: *)
(*Gauge Fields*)


(* ::Text:: *)
(*With electrodynamics, imagine a copy of the symmetry group U(1) (aka structure group) attached to each point p\[Element]M. The members of U(1) are phase factors, E^( i \[CurlyPhi]) (abbreviated as \[CurlyPhi]), each representing a choice of gauge at that point. The principal bundle (sources of forces) is the disjoint union of all manifold points p paired with all possible phase factors at p, flattened into one big set of pairs.*)


(* ::DisplayFormulaNumbered:: *)
(*U(1)M\[Congruent]\!\(\*UnderscriptBox[\(\[UnionPlus]\), \(p \[Element] M\)]\){E^(i \[CurlyPhi])\[Element]U(1)}={(p,\[CurlyPhi])\[VerticalSeparator]p\[Element]M,E^(i \[CurlyPhi])\[Element]U(1)}(principal bundle,sources of forces)*)


(* ::Item:: *)
(*We might call it a phase bundle. *)


(* ::Text:: *)
(*In classical (non-quantum) electrodynamics, the associated bundle (victims of forces) is the complex line bundle, a collection of pairs of a point and a copy of the entire complex plane at that point, accounting for all possible values of a complex-valued charged matter field. This field represents density and internal alignment (physical interpretation of phase) of a notional charged fluid. *)


(* ::Item:: *)
(*The copies of the complex plane in the associated bundle is the entire complex plane, not just the members of U(1), which are confined to the unit circle in the complex plane.*)


(* ::Text:: *)
(*In quantum electrodynamics, the associated bundle is a collection of pairs of points and all possible Dirac wave functions, each of which is a superposition of spinor states. In this notebook, we cover only the classical case. The quantum case comes later in this series of notebooks.*)


(* ::Subsection:: *)
(*Projections*)


(* ::Text:: *)
(*Envision a projection function, \[Pi], that yields p from any element of T M:*)


(* ::DisplayFormulaNumbered:: *)
(*\[Pi]:T M\[LongRightArrow]M,(p,v)|->p*)


(* ::Text:: *)
(*"forgetting" the vector v.*)


(* ::Text:: *)
(*In electromagnetism, the projection function \[Pi]:(U(1)M)\[LongRightArrow]M, (p,\[CurlyPhi])|->p forgets the phase factor E^( i \[CurlyPhi]) from any given pair in U(1)M.*)


(* ::Subsection:: *)
(*Fibres*)


(* ::Text:: *)
(*In elementary mathematics, the fibre, fib(f,p)\[Congruent]f^ -1(p), of a function f at some point p is the preimage of f at p, that is, the subset of the domain of f that yield p. *)


(* ::Item:: *)
(*In this usage, f^ -1 is not an "inverse function" that yields points, but a function that yields subsets. A general f may not be "invertible" in the usual sense of a point-to-point bijective function, but we can always define a fibre: a set containing all points of the domain that yield p. Unfortunately, we have the same notation, f^ -1, both for an inverse function when f is invertible and for a fibre generally.*)


(* ::Text:: *)
(*The fibres of the bundles\[LongDash]principal or associated\[LongDash]of interest to us are those of the projection functions \[Pi]. *)


(* ::Text:: *)
(*In geometry, \[Pi]^ -1 can't "remember" the forgotten vector, so it must return the set of all vectors attached to p (or, more generally, tensors):*)


(* ::DisplayFormulaNumbered:: *)
(*p/\[Pi]={v\[Element]Subscript[T, p] M}\[Congruent]Subscript[T, p] M (victims of forces)*)


(* ::Text:: *)
(*which is, of course, the entire tangent space at p.*)


(* ::Text:: *)
(*In electrodynamics, the \[Pi]^-1 can't remember the forgotten phase factor, so it must return a complete copy of U(1) at each point:*)


(* ::DisplayFormulaNumbered:: *)
(*(1/\[Pi])[p]={E^(i \[CurlyPhi])\[Element]U[1]}\[Congruent]U[1] (sources of forces)*)


(* ::Subsection:: *)
(*Sections*)


(* ::Subsubsection:: *)
(*Geometry / Gravitation*)


(* ::Text:: *)
(*A section of the principal bundle (sources of forces, the frame bundle) is a particular choice of reference frame at each point\[LongDash]a coordinate basis for the Christoffel connection or a non-coordinate, tetrad basis for the spin connection. *)


(* ::Text:: *)
(*A section of the associated bundle (victims of forces, the tangent bundle) is a smooth choice of one vector, V^ \[Mu](p), at each point, amounting to a smoothly varying vector field subject to forces. *)


(* ::Subsubsection:: *)
(*Gauge Fields*)


(* ::Text:: *)
(*In electrodynamics, a section of the principal bundle (sources of forces, the phase bundle) is a smooth choice of phase, \[CurlyPhi](p), at each point, which is a field of complex numbers of unit magnitude. It is the gauge-theory analog of a reference frame, a locally varying gauge choice. *)


(* ::Text:: *)
(*A section of the associated bundle (victims of forces, matter fields) is a smooth choice of complex-valued charged matter field at each point (classical), or a smooth choice of Dirac wave function at each point (quantum).*)


(* ::Subsection:: *)
(*Connections*)


(* ::Subsubsection:: *)
(*Geometry / Gravitation*)


(* ::Text:: *)
(*The connection on the gravitational principal bundle (sources of forces) is the field of Christoffel Symbols Subscript[\[CapitalGamma]^ \[Lambda],  \[Mu] \[Nu]], or the field of spin connections. These connections describes how an associated section\[LongDash]the vector field V^\[Mu]\[LongDash]twists and turns as p moves. A connection is the coefficient of the correction V . \[CapitalGamma] that keeps the covariant derivative \[Del]V tensorial. *)


(* ::Subsubsection:: *)
(*Gauge Fields*)


(* ::Text:: *)
(*The connection on the principal gauge bundle (sources of forces) is the gauge potential, Subscript[A, \[Mu]](p), the field of vector potentials at each point. This field of connections describes how the associated section\[LongDash]the charged matter field or the Dirac wavefunction field\[LongDash]twists and turns as p moves. *)


(* ::Text:: *)
(*Just as Subscript[\[CapitalGamma]^ \[Lambda],  \[Mu] \[Nu]]corrects for the twisting and turning of tangent vectors, Subscript[A, \[Mu]](p) corrects for the twisting of the internal phase of the charged matter field. *)


(* ::Text:: *)
(*Just as gravitational forces arise from the Christoffel or spin connection, electrodynamical forces arise from the gauge potential. We now revise our slogan:*)


(* ::Text::Bold:: *)
(*Forces arise from connections on sections of principal bundles.*)


(* ::Subsection:: *)
(*Invariance*)


(* ::Text:: *)
(*If the Lagrangian density, \[ScriptCapitalL], of gravitation is invariant with respect to smooth frame choice at each point, the physics is locally Lorentz invariant. *)


(* ::Text:: *)
(*If the Lagrangian density, \[ScriptCapitalL], of electrodynamical gauge theory is invariant with respect to smooth choice of external unitary phase from U(1), the physics is locally gauge invariant. *)


(* ::Text:: *)
(*Invariant here means numerically equal and of the same algebraic form, as explained in my earlier notebook, From the steam age to quantum fields.*)


(* ::Section:: *)
(*Conclusion*)


(* ::Text:: *)
(*In this work, we bridge the conceptual and computational gap between general relativity and gauge theory. By treating "gauge" not just as mathematics but as a concrete choice of measurement basis\[LongDash]whether for position in spacetime or phase in a complex field\[LongDash]we unify the origin of forces under a single slogan: Forces arise from connections on sections of principal bundles.*)


(* ::Text:: *)
(*We show that "garbage terms" generated by differentiating a phase-shifted field are mathematically analogous to "garbage terms" generated by differentiating a vector in curvilinear coordinates. In both cases, a connection field\[LongDash]the vector potential Subscript[A,  \[Mu]]\:200b or the Christoffel Symbol Subscript[\[CapitalGamma]^ \[Mu], \[Nu] \[Lambda]]\[LongDash]cancels these terms and restores phase covariance and geometrical covariance, respectively.*)


(* ::Text:: *)
(*Computationally, the Relativity Toolkit v1.9.0 is now "doubly covariant." It automatically produces identities for hybrid objects like the Charged Vector Boson W^ \[Alpha], which "ride on" spacetime geometry while twisting in an internal gauge space. This confirms that our symbolic machinery is ready for more of the Standard Model.*)


(* ::Text:: *)
(*While this notebook focuses on the Abelian (commutative) U(1) sector, the geometric machinery sets the stage for final generalization to non-Abelian gauge theories incorporating SU(2) and SU(3).*)


(* ::Section:: *)
(*References*)


(* ::ItemNumbered:: *)
(*Beckman, Brian. From the steam age to quantum fields: a unified approach via UD tensorial calculus. Wolfram Community post. *)


(* ::ItemNumbered:: *)
(*Beckman, Brian. General relativity in the UD calculus. Wolfram Community post.*)


(* ::ItemNumbered:: *)
(*Beckman, Brian. Formal Differential Geometry in the UD Calculus: Part 1. Wolfram Community post.*)


(* ::ItemNumbered:: *)
(*Beckman, Brian. Covariant Derivatives and Connections in the \[ScriptCapitalU]\[ScriptCapitalD] Calculus. Wolfram Community post.*)


(* ::ItemNumbered:: *)
(*Beckman, Brian. Riemann Curvature in \[ScriptCapitalU]\[ScriptCapitalD] + Compiler POC. Wolfram Community post.*)


(* ::ItemNumbered:: *)
(*Beckman, Brian. Schwarzschild Orbits in the \[ScriptCapitalU]\[ScriptCapitalD] Calculus. Wolfram Community post (to appear).*)


(* ::ItemNumbered:: *)
(*Beckman, Brian. Schwarzschild Metric from Scratch via \[ScriptCapitalU]\[ScriptCapitalD] Compilation. Wolfram Community post (to appear).*)


(* ::ItemNumbered:: *)
(*Wolfram Function Repository. MetricTensor. For component-based calculations.*)


(* ::ItemNumbered:: *)
(*Wolfram Function Repository. ChristoffelSymbol. For component-based calculations.*)


(* ::ItemNumbered:: *)
(*[MTW] Misner, Charles W.; Thorne, Kip S.; Wheeler, John Archibald. Gravitation. Princeton University Press, 2017.*)


(* ::ItemNumbered:: *)
(*Sean Carroll, Spacetime and Geometry: An Introduction to General Relativity. Cambridge University Press, 2019.*)


(* ::ItemNumbered:: *)
(*Friedrich W. Hehl et al., General Relativity with Spin and Torsion: Foundations and Prospects (Rev. Mod. Phys. 48, 393, 1976)*)


(* ::ItemNumbered:: *)
(*T. W. B. Kibble, Lorentz Invariance and the Gravitational Field (J. Math. Phys. 2, 212, 1961)*)


(* ::ItemNumbered:: *)
(*Sciama, D. W., On the analogy between charge and spin in general relativity. In: Recent Developments in General Relativity, p. 415. Pergamon, New York, 1962.*)



