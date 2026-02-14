(* ::Package:: *)

(* ::Title:: *)
(*Covariant Derivatives and Connections in the \[ScriptCapitalU]\[ScriptCapitalD] Calculus*)


(* ::Subtitle:: *)
(*Revised Edition*)


(* ::Author:: *)
(*Brian Beckman*)


(* ::Author:: *)
(*Jan 2026*)


(* ::Section:: *)
(*Abstract*)


(* ::Text:: *)
(*In this installment of the \[ScriptCapitalU]\[ScriptCapitalD] series, we address a central issue of differential geometry: how to take a derivative in (or on) a curved manifold without breaking coordinate invariance. Ordinary partial differentiation is not tensorial, producing garbage terms that don't transform properly. The solution is the covariant derivative and its correction term, the connection coefficient \[CapitalGamma].*)


(* ::Text:: *)
(*We begin with the non-tensorial transformation law of \[CapitalGamma]. Then, applying the constraints of metric compatibility (\[Del]g=0) and torsion freedom (symmetry on lower indices of \[CapitalGamma]), we derive the particular connection coefficients needed for general relativity, the Christoffel Symbols. This derivation specializes differential geometry to  Riemannian geometry as needed for the general relativistic theory of gravitation. Step-by-step derivation in \[ScriptCapitalU]\[ScriptCapitalD] automates tiresome and risky algebraic operations for high assurance in the results.*)


(* ::Text:: *)
(*At this early stage of development, we expose \[ScriptCapitalU]\[ScriptCapitalD] as a script rather than hiding details in a package. This glass-box approach delivers better pedagogy and  incremental validation. *)


(* ::Section:: *)
(*Preface to the Revised Edition*)


(* ::Text:: *)
(*In this revised edition, I invert the relationship between comments and code. I break up the blocks of code-with-essential-comments from the first edition into alternating prose and code cells, as is usual with notebooks. The motivation for the original style was to promote the script versions, but I have since learned that the .wl script format is more flexible than I had originally assumed: it can host notebook-formatted comments\[LongDash]Section, Item, and so on\[LongDash]not just ASCII/Unicode comments. The result is that script versions of this notebook can follow the notebook format very closely. *)


(* ::Text:: *)
(*I also take the opportunity to improve the presentation in some trivial, non-material ways. *)


(* ::Item:: *)
(*A .wl script version of code for this notebook, minus the prose, is available at https://github.com/rebcabin/RelativityToolkit/blob/main/covariant_derivative_revised.wl. *)


(* ::Section:: *)
(*Audience*)


(* ::Text:: *)
(*This series of notebooks is targeted at readers with an undergraduate's STEM education\[LongDash]engineering, computer science, mathematics. Exposure to introductory courses in Classical Mechanics (e.g., Goldstein), Electrodynamics (e.g., Jackson), and Relativity (e.g., Schutz) will help. *)


(* ::Item:: *)
(*Except for MTW, I do not put standard textbooks in the References section, but offer clickable (non-affiliate!) links instead in the prose of this notebook.*)


(* ::Text:: *)
(*I also invite readers to "think like a physicist," that is, to demand conceptual cogency, but not always full mathematical rigour.*)


(* ::Section:: *)
(*Transparent Implementation*)


(* ::Text:: *)
(*In developing \[ScriptCapitalU]\[ScriptCapitalD], we made a conscious decision, for the time being, to expose the implementation as a glass-box script rather than to hide it in a Package, prioritizing transparency over encapsulation at this stage of development. When the toolkit is finalized in the future, we'll address encapsulating it in a Package.*)


(* ::Text:: *)
(*Let's load \[ScriptCapitalU]\[ScriptCapitalD]'s implementation, the RelativityToolkit, directly into your notebook context.*)


(* ::Section:: *)
(*Need for Covariant Derivative*)


(* ::Subsection:: *)
(*We Want a Derivative!*)


(* ::Text:: *)
(*Consider a curve with invertible, continuous, differentiable coordinate functions x^\[Alpha](\[Lambda]) along the curve, and tangent (contravariant) vectors u^\[Alpha]\[Congruent]d x^\[Alpha]/d \[Lambda]. Also consider a (contravariant) vector field A^\[Alpha](x^\[Beta](\[Lambda])) defined in neighborhoods around every point x^\[Alpha](\[Lambda]) along the curve. We'd like a "derivative" of the vector field, schematically:*)


(* ::DisplayFormulaNumbered:: *)
(*(d A^\[Alpha])/(d x^\[Beta])\!\(\*OverscriptBox[\(\[Congruent]\), \(?\)]\)(A^\[Alpha](x^\[Beta]+d x^\[Beta])-A^\[Alpha](x^\[Beta]))/(d x^\[Beta])*)


(* ::Item:: *)
(*We'll have a better, "cool" notation for (d A^\[Alpha])/(d x^\[Beta]) soon enough. For now, read it as "the derivative we're trying to define."*)


(* ::Text:: *)
(*Seems a reasonable thing to ask: "How much does component \[Alpha] of A change when I change coordinate \[Beta] a little?" *)


(* ::Text:: *)
(*However, Equation 1 is not a cogent definition of derivative on a manifold. We cannot subtract vectors at different points, not even infinitesimally different points. The coordinate axes at (x^\[Beta]+d x^\[Beta]) don't point in the same directions as the coordinate axes at x^\[Beta] because the manifold bends and twists away between the two points! The components of A are not commensurable at the two points because the two vectors are in different tangent spaces. Subtracting components as prescribed by Equation 1, numerically, scrambles information from other directions. *)


(* ::Subsubsection:: *)
(*Conundrum of Parallel Transport*)


(* ::Text:: *)
(*To answer this conundrum, we need backwards parallel transport, to move the vector A^\[Alpha](x^\[Beta]+d x^\[Beta]) smoothly and with a provable minimum of bending and twisting, back from x^\[Beta]+d x^\[Beta] to the point x^\[Beta], so we can measure the components of the transported A^\[Alpha](x^\[Beta]+d x^\[Beta]) on the coordinate axes at point x^\[Beta] and then subtract the numbers. *)


(* ::DisplayFormulaNumbered:: *)
(*(d A^\[Alpha])/(d x^\[Beta])\!\(\*OverscriptBox[\(\[Congruent]\), \(?\)]\)(ParallelTransport(A^\[Alpha](x^\[Beta]+d x^\[Beta]),to x^\[Beta])-A^\[Alpha](x^\[Beta]))/(d x^\[Beta])*)


(* ::Subsubsection:: *)
(*Conundrum of Coordinate-Independence*)


(* ::Text:: *)
(*Now, with subtraction conceptually solved, how will that difference survive a coordinate transformation? How can I find out, when I change coordinates from x^\[Beta] to x^\[Beta]' at the same point, how much component \[Alpha]->\[Alpha]' of A changes when I change coordinate \[Beta]->\[Beta]'? That is, what's the value of (d A^\[Alpha] ')/(d x^\[Beta] ') when all I know is (d A^\[Alpha])/(d x^\[Beta])? *)


(* ::Text:: *)
(*To answer this conundrum, we insist, as always, that the intrinsic geometry and physics not depend on choice of coordinates. But what does intrinsic mean when all we have are numerical components whose values depend on choice of coordinates?*)


(* ::Subsection:: *)
(*Intrinsic Means Tensorial*)


(* ::Text:: *)
(*Tensorial quantities encapsulate both intrinsic and coordinate-dependent information. If our desired, derivative-like quantities (d A^\[Alpha])/(d x^\[Beta]) transform like a (1,1) tensor when we change the basis vectors at point \[Beta], we'll capture both: *)


(* ::Item:: *)
(*intrinsic geometric and physical properties of a tensor*)


(* ::Item:: *)
(*extrinsic artifacts arising from choice of coordinates *)


(* ::Subitem:: *)
(*Think of changing from Cartesian to spherical coordinates in flat Euclidean 3-space: the numbers change but the geometry and physics don't.*)


(* ::Text:: *)
(*Plus, we will be able to separate intrinsic (coordinate-dependent) from extrinsic (coordinate-independent) quantities cleanly.*)


(* ::Item:: *)
(*TODO: How?*)


(* ::Item:: *)
(*The first couple of chapters of Lovelock & Rund address this issue of intrinsic, geometry-dependent effects versus extrinsic, coordinate-dependent effects rigorously. Thinking like physicists, let's be satisfied with the conceptual description above for now.*)


(* ::Subsection:: *)
(*Tensorial and Linear Requirements*)


(* ::Text:: *)
(*Agree that the components of our desired derivative, (d A^\[Alpha])/(d x^\[Beta]), transform between coordinate frames in exactly the way components of any other (1,1) tensor\[LongDash]with one up and one down index\[LongDash]would change. These components are projections of the vector A on the \[Beta] basis vectors of the coordinate system x^\[Beta], and measure both intrinsic and extrinsic information. *)


(* ::Text:: *)
(*It turns out that simply demanding that our new derivative be tensorial and linear suffices to solve all the conundrums above, including optimality. *)


(* ::Item:: *)
(*TODO: This claim of optimality may not be good enough, even for a physicist otherwise happy with mathematical hand-waving. Misner, Thorne, and Wheeler [MTW] devote many chapters (8-15) to a more careful, physicist's level of understanding, including demonstrations that the covariant derivative performs parallel transport! Those are missing pieces in this notebook. *)


(* ::Item:: *)
(*A mathematician will demand rigorous proofs. For those, see the many mathematical texts on differential geometry, perhaps starting with do\[NonBreakingSpace]Carmo's Differential Geometry of Curves and Surfaces. Other texts such as Tristan Needham, Visual Differential Geometry and Forms, and Michael Spivak's 5-volume Comprehensive Introduction to Differential Geometry are recommended.*)


(* ::Subsection:: *)
(*The Partials are Not Tensors*)


(* ::Text:: *)
(*First, let's check that \!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\[Alpha]\)], \(, \(\ \)\(\[Beta]\)\)]\) is not tensorial. It's not enough for our desired derivative, but we'll figure out how to correct it.*)


(* ::Item:: *)
(*The comma notation, \!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\[Alpha]\)], \(, \(\ \)\(\[Beta]\)\)]\), is short for \[PartialD]A^\[Alpha]/\[PartialD]x^\[Beta], also written \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Beta]\)]*)
(*\*SuperscriptBox[\(A\), \(\[Alpha]\)]\). Though we don't yet know how to define (d A^\[Alpha])/(d x^\[Beta]), \[PartialD]A^\[Alpha]/\[PartialD]x^\[Beta] is just good-ol' partial derivative from ordinary calculus, a mechanical, symbolical calculation that Wolfram automates very well. *)


(* ::Item:: *)
(*\[ScriptCapitalU]\[ScriptCapitalD] embraces the comma notation as you can see from the Visual Showcase gallery above.*)


(* ::Text:: *)
(*Let's transform bases and calculate A^\[Alpha]', contravarian components in basis x^\[Alpha]', from A^\[Alpha], contravariant components in basis x^\[Alpha]. A transforms as follows:*)


(* ::DisplayFormulaNumbered:: *)
(*A^Derivative[1][\[Alpha]]\[Congruent]\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \({x, \[Alpha]}\)]\**)
(*SuperscriptBox["x", *)
(*SuperscriptBox["\[Alpha]", "\[Prime]",*)
(*MultilineFunction->None]]\) A^\[Alpha]*)


(* ::Item:: *)
(*A mnemonic for "contravariant:" let \[PartialD]x^\[Alpha]'/\[PartialD]x^\[Alpha] measure feet (primed) per inch (unprimed), that is (1/12). Any unprimed contravariant vector A^\[Alpha] is, mnemonically, like a velocity in inches per second, and its primed partner A^\[Alpha]' is, mnemonically, like a velocity in feet per second. Multiply A^\[Alpha] (inches per second) by \[PartialD]x^\[Alpha]'/\[PartialD]x^\[Alpha] (feet per inch, 1/12) to get A^\[Alpha]' (feet per second), i.e., A^\[Alpha]'=\[PartialD]x^\[Alpha]'/\[PartialD]x^\[Alpha] A^\[Alpha]. The same rule goes for any kind of coordinate change, curvilinear, non-orthogonal, whatever: Contravariant? Multiply by \[PartialD]x^\[Alpha]'/\[PartialD]x^\[Alpha].*)


(* ::Item:: *)
(*Another mnemonic: the unprimed up index of A^\[Alpha] matches contrariwise the unprimed down index of \[PartialD]x^\[Alpha]'/\[PartialD]x^\[Alpha], given that an up index in the denominator \[PartialD]x^\[Alpha] of \[PartialD]x^\[Alpha]'/\[PartialD]x^\[Alpha] is a down index of the whole \[PartialD]x^\[Alpha]'/\[PartialD]x^\[Alpha]. \[ScriptCapitalU]\[ScriptCapitalD] automates this valence check.*)


(* ::Item:: *)
(*The quantities \[PartialD]x^\[Alpha]'/\[PartialD]x^\[Alpha] are the Jacobian of the coordinate transformation from x^\[Alpha] to  (x^\[Alpha])', sometimes written (totally ambiguously!) as J, with \[PartialD]x^\[Alpha]/\[PartialD]x^\[Alpha]' being J^ -1. In manual calculations, one must constantly check the "direction" of Jacobian every time; it's a fertile source of error. \[ScriptCapitalU]\[ScriptCapitalD] won't let you make this mistake!*)


(* ::Item:: *)
(*Covariant tensors transform with the matching index co-wise in the numerator of the Jacobian. The mnemonical example is a gradient, say degrees of temperature per foot or inch.*)


(* ::Text:: *)
(*Now calculate partial derivatives with respect to the primed frame: *)


(* ::DisplayFormulaNumbered:: *)
(*\!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\[Alpha]'\)], \(, \(\[Beta]'\)\)]\)\[Congruent](\[PartialD]/\[PartialD]x^\[Beta]')(\[PartialD]x^\[Alpha]'/\[PartialD]x^\[Alpha] A^\[Alpha])*)


(* ::Item:: *)
(*This notation is different from Lovelock & Rund, who write \!\(\*OverscriptBox[\(x\), \(_\)]\)^\[Alpha] for the transformation functions, with Greek indices that have no primes. However, we will not be confused:  x^\[Alpha]' is short for x^\[Alpha]'(x^\[Beta]), four functions of four arguments each in relativistic physics, N functions of N arguments more generally. These functions express new coordinates in terms of old. The prime isn't really on the index, but on the pair of literal x and the index. We write it on the index only for ease of typesetting. Also be reminded that the partial derivatives with respect to independent variables x^\[Beta] are mechanical, symbolical operations.*)


(* ::Subsection:: *)
(*Calculus Via \[ScriptCapitalU]\[ScriptCapitalD]*)


(* ::Text:: *)
(*Let's have \[ScriptCapitalU]\[ScriptCapitalD] do this bit of calculus. *)


(* ::Text:: *)
(*We'll need Wolfram-friendly notation for primed indices. \[Alpha]' won't do, except for display, because Wolfram interprets the prime as a derivative. We'll just write ap and bp for the primed indices \[Alpha]' and \[Beta]' and a pretty rule for display.*)


(* ::Input:: *)
(**)


ClearAll[ap,bp,\[Mu],pretty];pretty={ap->\[Alpha]',bp->\[Beta]',mu->\[Mu]};


(* ::Subsubsection:: *)
(*Define the Input*)


(* ::Text:: *)
(*Express the partial via \[ScriptCapitalU]\[ScriptCapitalD]'s inert Partials head:*)


(* ::Input:: *)
(**)


Partials[A[\[ScriptCapitalU][ap]],x[\[ScriptCapitalU][bp]]]/.pretty


(* ::Subsubsection:: *)
(*Write A^\[Alpha]' as Transformed from A^\[Alpha]*)


(* ::Text:: *)
(*Via \[ScriptCapitalU]\[ScriptCapitalD]'s robustTransformRules, write A^\[Alpha]' as the Jacobian times A^\[Alpha]. The trailing /.pretty//TensorForm are for display only *)


(* ::Input:: *)
(**)


Echo[(inputTerm=Partials[A[\[ScriptCapitalU][ap]]/. robustTransformRules,x[\[ScriptCapitalU][bp]]])/.pretty//TensorForm,
"\!\(\*SubscriptBox[\(\[PartialD]\), \(\[Beta]'\)]\)(\!\(\*SuperscriptBox[\(A\), \(\[Alpha]'\)]\)) \[Congruent] \!\(\*SubscriptBox[SuperscriptBox[\(A\), \(\[Alpha]'\)], \(, \(\\\ \)\(\[Beta]\\\ '\)\)]\) ="];


(* ::Subsubsection:: *)
(*Expand Derivatives*)


(* ::Text:: *)
(*Apply Leibniz (product) and chain rules repeatedly until stable:*)


(* ::Input:: *)
(**)


Echo[(expandedTerm=ExpandDerivatives[inputTerm])/.pretty//TensorForm,
"\!\(\*SubscriptBox[SuperscriptBox[\(A\), \(\[Alpha]'\)], \(, \(\\\ \)\(\[Beta]\\\ '\)\)]\) ="];


(* ::Section:: *)
(*Definition of Covariant Derivative*)


(* ::Subsection:: *)
(*Partials of the Transform != Transform of the Partials*)


(* ::Text:: *)
(*The partial derivative \!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\[Alpha]\)], \(, \(\ \)\(\[Beta]\)\)]\) is not tensorial, else it would transform like \[PartialD]x^\[Alpha]'/\[PartialD]x^\[Alpha] \[PartialD]x^ \[Beta]/\[PartialD]x^ \[Beta] ' \!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\[Alpha]\)], \(, \(\ \)\(\[Beta]\)\(\ \)\)]\). When we differentiate A^\[Alpha]', we get *)


(* ::DisplayFormulaNumbered:: *)
(*(\[PartialD]/\[PartialD]x^\[Beta]')(A^\[Alpha]'\[Congruent]A^\[Mu] \[PartialD]x^\[Alpha]'/\[PartialD]x^\[Mu])\[Congruent]\!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\[Alpha]'\)], \(, \(\[Beta]'\)\)]\)\[Congruent]\!\(\*SubscriptBox[\(( *)
(*\*SuperscriptBox[\(A\), \(\[Mu]\)] *)
(*\*FractionBox[\(\[PartialD]x *)
(*\*SuperscriptBox[\(\), \(\[Alpha]'\)]\), \(\[PartialD]x *)
(*\*SuperscriptBox[\(\), \(\[Mu]\)]\)])\), \(, \(\[Beta]'\)\)]\)*)


(* ::Text:: *)
(*we don't get the transform  (\[PartialD]x^\[Alpha]/\[PartialD]x^\[Alpha] ' \[PartialD]x^ \[Beta] '/\[PartialD]x^ \[Beta] \!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\[Alpha]\ '\)], \(, \(\ \)\(\[Beta]\ '\)\)]\)) of the derivative \!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\[Alpha]'\)], \(, \(\ \)\(\[Beta]\ '\)\)]\), but two terms that are difficult to interpret: (A^\[Lambda] \[PartialD]^2x^\[Alpha]'/(\[PartialD]x^\[Beta] '\[PartialD]x^\[Lambda])) and (\!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\[Lambda]\)], \(, \(\ \)\(\[Beta]\ '\)\)]\) \[PartialD]x^\[Alpha]'/\[PartialD]x^\[Lambda]).*)


(* ::Text:: *)
(*These terms have factors, \[PartialD]^2x^\[Alpha]'/(\[PartialD]x^\[Beta] '\[PartialD]x^\[Lambda]) and \!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\[Lambda]\)], \(, \(\ \)\(\[Beta]\ '\)\)]\) respectively, that are calculated in both coordinate systems, and that's disturbing. We can extract one Jacobian factor of \[PartialD]x^ \[Beta]/\[PartialD]x^ \[Beta] ', but that's it:*)


(* ::DisplayFormulaNumbered:: *)
(*\!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\[Alpha]'\)], \(, \(\[Beta]'\)\)]\)=A^\[Lambda](\[PartialD]^2x^\[Alpha]'/(\[PartialD]x^\[Beta]'\[PartialD]x^\[Lambda]))+(\!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\[Lambda]\)], \(, \(\[Beta]'\)\)]\) \[PartialD]x^\[Alpha]'/\[PartialD]x^\[Lambda])=A^\[Lambda] \[PartialD]x^\[Beta]/\[PartialD]x^\[Beta]' (\[PartialD]^2x^\[Alpha]'/(\[PartialD]x^\[Beta]\[PartialD]x^\[Lambda]))+(\!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\[Lambda]\)], \(, \(\[Beta]\)\)]\) \[PartialD]x^\[Beta]/\[PartialD]x^\[Beta]') \[PartialD]x^\[Alpha]'/\[PartialD]x^\[Lambda]=\[PartialD]x^\[Beta]/\[PartialD]x^\[Beta]' (A^\[Lambda] \[PartialD]^2x^\[Alpha]'/(\[PartialD]x^\[Beta]\[PartialD]x^\[Lambda])+\!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\[Lambda]\)], \(, \(\[Beta]\)\)]\) \[PartialD]x^\[Alpha]'/\[PartialD]x^\[Lambda])*)


(* ::Text:: *)
(*The quantity \[PartialD]x^ \[Beta]/\[PartialD]x^ \[Beta] ' (A^ \[Lambda] \[PartialD]^2x^ \[Alpha] '/(\[PartialD]x^ \[Beta]\[PartialD]x^ \[Lambda])+\!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\ \(\[Lambda]\)\)], \(, \(\ \)\(\[Beta]\)\(\ \)\)]\) \[PartialD]x^ \[Alpha]'/\[PartialD]x^ \[Lambda]) is again not tensorial because it has a free index \[Alpha]'. Worse, it depends on both coordinate systems, the primed and the unprimed, and we can't have that! Worse still, \[PartialD]^2x^ \[Alpha] '/(\[PartialD]x^ \[Beta]\[PartialD]x^ \[Lambda]) the Hessian of the coordinate transformation, is not a geometrical object, but rather an algebraic object. It can't itself be transformed. It depends on particular choices of two coordinate systems. It's of no help.*)


(* ::Subsection:: *)
(*The Way Out: Covariant Derivative*)


(* ::Text:: *)
(*There's a way out: insist on a certain form. *)


(* ::Text:: *)
(*Write the tensorial derivative we want, (d A^\[Alpha])/(d x^\[Beta]), as the non-tensorial partial derivative \!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\[Alpha]\)], \(, \(\ \)\(\[Beta]\)\(\ \)\)]\) plus a correction. *)


(* ::Text:: *)
(*Demand that the correction be linear in A^\[Alpha] and that the sum\[LongDash]partial derivative plus correction\[LongDash]transform tensorially. Defining the semicolon notation for this "corrected derivative" \!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\[Alpha]\)], \(; \(\ \)\(\[Beta]\)\(\ \)\)]\)*)


(* ::DisplayFormulaNumbered:: *)
(*\!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\[Alpha]\)], \(; \(\[Beta]\)\)]\)\[Congruent]\!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\[Alpha]\)], \(, \(\[Beta]\)\)]\)+Subscript[\[CapitalGamma]^\[Alpha], \[Beta] \[Mu]] A^\[Mu]*)


(* ::Text:: *)
(*require that it transform as follows:*)


(* ::DisplayFormulaNumbered:: *)
(*\!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\[Alpha]'\)], \(; \(\[Beta]'\)\)]\)=\[PartialD]x^\[Alpha]'/\[PartialD]x^\[Alpha] \[PartialD]x^\[Beta]/\[PartialD]x^\[Beta]' \!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\[Alpha]\)], \(; \(\[Beta]\)\)]\)*)


(* ::Text:: *)
(*then solve for Subscript[\[CapitalGamma]^ \[Alpha], \[Beta] \[Mu]]. Notice contraction on the third index: Subscript[\[CapitalGamma]^ \[Alpha], \[Beta] \[Mu]] A^ \[Mu].*)


(* ::Text:: *)
(*\!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\[Alpha]\)], \(; \(\ \)\(\[Beta]\)\)]\) is the covariant derivative. *)


(* ::Item:: *)
(*Despite the name, the covariant derivative is contravariant in \[Alpha] and covariant in \[Beta]. The word "covariant" here does not have its usual meaning, but we're stuck with this historical nomenclature.*)


(* ::Item:: *)
(*The form of a term plus a correction is ubiquitous in linear theories such as that for the Kalman filter (see my notebook Kalman filtering as a functional fold). *)


(* ::Item:: *)
(*The contravariant vector A is tensorial, measuring intrinsic properties of A. \[CapitalGamma] is not tensorial, mixing in extrinsic information about the coordinate system. The product A . \[CapitalGamma] is tensorial again. *)


(* ::Item:: *)
(*If we do our job right (proving tensoriality and linearity), \!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\[Alpha]\)], \(; \(\ \)\(\[Beta]\)\(\ \)\)]\) becomes the promised "cool notation" for (d A^\[Alpha])/(d x^\[Beta]), the derivative we want, the covariant derivative. *)


(* ::Item:: *)
(*One may also write \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Beta]\)]*)
(*\*SuperscriptBox[\(A\), \(\[Alpha]\)]\) for \!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\[Alpha]\)], \(, \(\ \)\(\[Beta]\)\)]\) and \!\( *)
(*\*SubscriptBox[\(\[Del]\), \(\[Beta]\)]*)
(*\*SuperscriptBox[\(A\), \(\[Alpha]\)]\) for \!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\[Alpha]\)], \(; \(\ \)\(\[Beta]\)\)]\). These forms frequently appear in the literature.*)


(* ::Item:: *)
(*TODO: Why must the correction Subscript[\[CapitalGamma]^ \[Alpha], \[Beta] \[Mu]] A^ \[Mu] be linear in A?*)


(* ::Item:: *)
(*SPOILER: We want our new derivative \[Del] (aka the semicolon) to behave like any other derivative. That means it must satisfy two properties:*)


(* ::Subitem:: *)
(*Linearity: \!\( *)
(*\*SubscriptBox[\(\[Del]\), \(\[Mu]\)]\(( *)
(*\*SuperscriptBox[\(A\), \(\[Nu]\)] + *)
(*\*SuperscriptBox[\(B\), \(\[Nu]\)])\)\)=\!\( *)
(*\*SubscriptBox[\(\[Del]\), \(\[Mu]\)]*)
(*\*SuperscriptBox[\(A\), \(\[Nu]\)]\)+\!\( *)
(*\*SubscriptBox[\(\[Del]\), \(\[Mu]\)]*)
(*\*SuperscriptBox[\(B\), \(\[Nu]\)]\)*)


(* ::Subitem:: *)
(*Leibniz Rule: \!\( *)
(*\*SubscriptBox[\(\[Del]\), \(\[Mu]\)]\((f *)
(*\*SuperscriptBox[\(A\), \(\[Nu]\)])\)\)=A^\[Nu] \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]f\)+f \!\( *)
(*\*SubscriptBox[\(\[Del]\), \(\[Mu]\)]*)
(*\*SuperscriptBox[\(A\), \(\[Nu]\)]\)*)


(* ::ItemParagraph:: *)
(*If we assume the general form of derivative plus a correction, \!\( *)
(*\*SubscriptBox[\(\[Del]\), \(\[Mu]\)]*)
(*\*SuperscriptBox[\(A\), \(\[Nu]\)]\)=\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]*)
(*\*SuperscriptBox[\(A\), \(\[Nu]\)]\)+C^\[Nu](A,\[PartialD]A,\[PartialD]^2A...), then:*)


(* ::Subitem:: *)
(*Linearity forces C(A) to be linear in A.*)


(* ::Subitem:: *)
(*Leibniz forces C(A) to have no derivatives of A.*)


(* ::ItemParagraph:: *)
(*Therefore, the correction must be a multiplicative term: matrix multiplication of A by some object \[CapitalGamma].*)


(* ::Item:: *)
(*Given what we want to measure, we see why Subscript[\[CapitalGamma]^ \[Alpha], \[Beta] \[Mu]] must have three indices: it measures how much A^\[Alpha] changes when we change x^\[Beta]  a little, in terms of the components of A^\[Mu] contracted against Subscript[\[CapitalGamma]^ \[Alpha], \[Beta] \[Mu]] at the home point x^ \[Mu]! Parallel transport simply must look exactly like this, no more, no less. (a physicist's hand-wave; mathematicians want more!)*)


(* ::Item:: *)
(*TODO: Is Subscript[\[CapitalGamma]^ \[Alpha], \[Beta] \[Mu]] symmetric in \[Beta] and \[Mu]? HINT: don't rathole on this now!*)


(* ::Subsection:: *)
(*Connection Coefficients*)


(* ::Text:: *)
(*In this context, Subscript[\[CapitalGamma]^ \[Alpha], \[Beta] \[Mu]] are called connection coefficients in the linear term A . \[CapitalGamma], or just connections. Later, we'll use the same symbol Subscript[\[CapitalGamma]^ \[Alpha], \[Beta] \[Mu]] for Christoffel Symbols, which are particular connection coefficients, those that satisfy restrictive physical requirements for general relativity. *)


(* ::Item:: *)
(*We shall see, over the course of future notebooks, that connections produce forces in all of physics, classical and quantum! Existence of forces depends both on coordinate (gauge) choices and on intrinsic properties like mass and charge.*)


(* ::Item:: *)
(*In restricting connection coefficients for gravitation in general relativity, we're saying "only certain kinds of manifold geometries and topologies are useful for describing gravitation." Connection coefficients are more general than Christoffel symbols, pertaining to a broader class of manifolds.*)


(* ::Section:: *)
(*Covariant Derivative and Connections in \[ScriptCapitalU]\[ScriptCapitalD]*)


(* ::Text:: *)
(*\[ScriptCapitalU]\[ScriptCapitalD]'s CD functions shows off the evaluated covariant derivative:*)


(* ::Input:: *)
(**)


Echo[TensorForm[CD[A[\[ScriptCapitalU][\[Alpha]]],x[\[ScriptCapitalU][\[Beta]]]]],"\!\(\*SubscriptBox[SuperscriptBox[\(A\), \(\[Alpha]\\\ '\)], \(; \(\\\ \)\(\[Beta]\\\ '\)\)]\) ="];


(* ::Text:: *)
(*CD's held form exhibits semicolon notation!*)


(* ::Input:: *)
(**)


Echo[HoldForm[CD[A[\[ScriptCapitalU][\[Alpha]]],x[\[ScriptCapitalU][\[Beta]]]]],"\!\(\*SubscriptBox[SuperscriptBox[\(A\), \(\[Alpha]\\\ '\)], \(; \(\\\ \)\(\[Beta]\\\ '\)\)]\) ==="];


(* ::Subsection:: *)
(*Solving for Connection Coefficients Subscript[\[CapitalGamma]^ \[Alpha], \[Beta] \[Mu]]*)


(* ::Text:: *)
(*Let's have \[ScriptCapitalU]\[ScriptCapitalD] do all the dirty work. We're not only going to solve for Subscript[\[CapitalGamma]^ \[Alpha], \[Beta] \[Mu]] , assuming only that \!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\[Alpha]\)], \(; \(\ \)\(\[Beta]\)\(\ \)\)]\)\[Congruent](\!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\[Alpha]\)], \(, \(\ \)\(\[Beta]\)\)]\) + A^\[Mu] Subscript[\[CapitalGamma]^\[Alpha], \[Beta] \[Mu]]) is tensorial (already encoded in \[ScriptCapitalU]\[ScriptCapitalD]'s CD), but we'll derive the transformation law for Subscript[\[CapitalGamma]^ \[Alpha], \[Beta] \[Mu]] , proving that \[CapitalGamma] is not tensorial, and show how the contraction A^\[Mu] Subscript[\[CapitalGamma]^\[Alpha], \[Beta] \[Mu]] subtracts, exactly, the non-tensorial parts of the messy partial derivative \!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\[Alpha]\)], \(, \(\ \)\(\[Beta]\)\)]\).*)


(* ::Text:: *)
(*IMPORTANT: The final step invokes a Quotient Theorem, by which, schematically, (T' Subscript[\[CapitalGamma]^ \[Alpha] ', \[Beta] ' \[Mu] ']=\[LeftAngleBracket]Jacobians\[RightAngleBracket] * T Subscript[\[CapitalGamma]^ \[Alpha], \[Beta] \[Mu]])  implies (Subscript[\[CapitalGamma]^ \[Alpha] ', \[Beta] ' \[Mu] ']=\[LeftAngleBracket]Jacobians\[RightAngleBracket] * Subscript[\[CapitalGamma]^ \[Alpha], \[Beta] \[Mu]]), factoring out (quotienting) tensors T and \!\(\*SuperscriptBox[\(T\), \('\)]\) and their Jacobians, if and only if (iff) they're truly arbitrary. *)


(* ::Item:: *)
(*Lovelock & Rund write dire warnings about these theorems, that misapplication is the occasion of many published errors. Here, because we know the answer is correct, even by comparison to Lovelock & Rund, we're going to hand-wave the issue, here.*)


(* ::Text:: *)
(*As before, we'll write primed indices in a brutal way as ap and bp, and prettify them on the way out.*)


(* ::Input:: *)
(**)


ClearAll[ap,bp,mu,nu, manualChainRule, allRules,targetTensor,Aprimed,xprimed,pretty];
pretty={ap->\[Alpha]',bp->\[Beta]',cp->\[Gamma]',mu->\[Mu],nu->\[Nu]};


(* ::Subsubsection:: *)
(*The Transformed Tensor We Want*)


(* ::Text:: *)
(*Have \[ScriptCapitalU]\[ScriptCapitalD] write the form we're looking for, unsimplified as of yet. In the next steps, we'll do a roundabout calculation to isolate the A . \[CapitalGamma] term and figure out how it must transform:*)


(* ::Input:: *)
(**)


Echo[(targetTensor=
(*Jacobians*)
Partials[x[\[ScriptCapitalU][ap]],x[\[ScriptCapitalU][mu]]]*
Partials[x[\[ScriptCapitalU][nu]],x[\[ScriptCapitalU][bp]]]*
CD[A[\[ScriptCapitalU][mu]],x[\[ScriptCapitalU][nu]]])/.pretty//TensorForm,
"\[ScriptCapitalU]\[ScriptCapitalD] says the tensor we want, \!\(\*SubscriptBox[SuperscriptBox[\(A\), \(\[Alpha]'\)], \(; \(\\\ \)\(\[Beta]'\)\)]\) ="];


(* ::Subsubsection:: *)
(*Calculate How \[CapitalGamma] Must Transform*)


(* ::Text:: *)
(*Convenience Variables*)


(* ::Text:: *)
(*\[ScriptCapitalU]\[ScriptCapitalD]'s robustTransformRules works on the primed, not differentiated tensor. Define a couple of convenience variables:*)


(* ::Input:: *)
(**)


Echo[(Aprimed=A[\[ScriptCapitalU][ap]]/. robustTransformRules)/.pretty//TensorForm,"\!\(\*SuperscriptBox[\(A\), \(\[Alpha]'\)]\) ="];
xprimed=x[\[ScriptCapitalU][bp]];


(* ::Text:: *)
(*Prepare a Custom Chain Rule*)


(* ::Text:: *)
(*Insert a Jacobian for a primed Partial across coordinate system, i.e., rewrite Subscript[A^ \[Alpha],  , \[Beta] '] as \[PartialD]x^ \[Mu]/\[PartialD]x^ \[Beta] ' Subscript[A^ \[Alpha],  , \[Mu]]. We can't use \[ScriptCapitalU]\[ScriptCapitalD]'s general robust transform rules because they would chain on the Jacobian, so we stop that manually. Also, because this is a special-purpose rule just for this calculation, we don't mind hard-coding on the primed index \[Beta]':*)


(* ::Input:: *)
(* Condition !MatchQ[expr,x[_]] prevents recursing a Jacobian on tself *)


manualChainRule=
Partials[expr_,x[\[ScriptCapitalU][bp]]]/;!MatchQ[expr,x[_]]:>
Module[{fresh=Unique["\[Mu]"]},
Partials[x[\[ScriptCapitalU][fresh]],x[\[ScriptCapitalU][bp]]]*
Partials[expr,x[\[ScriptCapitalU][fresh]]]];
Echo[Partials[A[\[ScriptCapitalU][\[Alpha]]],x[\[ScriptCapitalU][bp]]]/.manualChainRule/.pretty//TensorForm,"\!\(\*SubscriptBox[SuperscriptBox[\(A\), \(\[Alpha]\)], \(, \(\[VeryThinSpace]\)\(\[Beta]'\)\)]\) ="];


(* ::Text:: *)
(*Combine \[ScriptCapitalU]\[ScriptCapitalD]'s general differentiation rules with this special rule, showing nothing broke:*)


(* ::Input:: *)
(**)


allRules=Flatten[{differentiationRules,manualChainRule}];
Echo[Partials[A[\[ScriptCapitalU][\[Alpha]]],x[\[ScriptCapitalU][bp]]]/.allRules/.pretty//TensorForm,"\!\(\*SubscriptBox[SuperscriptBox[\(A\), \(\[Alpha]\)], \(, \(\[VeryThinSpace]\)\(\[Beta]'\)\)]\) ="];


(* ::Text::Bold:: *)
(*Fetch the Broken Partial*)


(* ::Text:: *)
(*Just set a variable to the broken partial we already know about:*)


(* ::Input:: *)
(**)


Echo[(brokenPartialOnly=Partials[Aprimed,xprimed]//.allRules)/.pretty//TensorForm,"Broken partial \!\(\*SubscriptBox[SuperscriptBox[\(A\), \(\[Alpha]'\)], \(, \(\\\ \)\(\[Beta]'\)\)]\) ="];


(* ::Text:: *)
(*Get the Residual*)


(* ::Text:: *)
(*Isolate the A . \[CapitalGamma] term we want from the target tensor minus the broken partial, i.e., calculate*)


(* ::DisplayFormulaNumbered:: *)
(*A^\[Mu]' Subscript[\[CapitalGamma]^\[Alpha]', \[Beta]'\[Mu]']=\!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\[Alpha]'\)], \(; \(\[Beta]'\)\)]\)-\!\(\*SubscriptBox[*)
(*SuperscriptBox[\(A\), \(\[Alpha]'\)], \(, \(\[Beta]'\)\)]\)*)


(* ::Input:: *)
(**)


Echo[(gammaPrimeTimesVector=targetTensor-brokenPartialOnly)/.pretty//TensorForm,"A.\[CapitalGamma] term :"];


(* ::Subsubsection:: *)
(*Coalesce and Simplify (Magic)*)


(* ::Text:: *)
(*This is messy, of the form -A . Hessian - A(primed) + (\[Del]A=\[PartialD]A+A . \[CapitalGamma])(primed), but we can spot that the derivative terms Subscript[A^ \[Lambda],  , \[Kappa]] might cancel, if\[NonBreakingSpace]\[Ellipsis]*)


(* ::Text:: *)
(*Here is the magic: if we collect the A' terms, \[ScriptCapitalU]\[ScriptCapitalD]'s TensorForm expands, canonicalizes, and canceling the derivative term Subscript[A^ \[Lambda],  , \[Kappa]]:*)


(* ::Subsection:: *)
(*Transformation Law 1*)


(* ::Input:: *)
(**)


Echo[TensorForm[Collect[gammaPrimeTimesVector,A[\[ScriptCapitalU][_]]]/.pretty],"\!\(\*SuperscriptBox[\(A\), \(\[Lambda]'\(\\\ \)\)]\)\!\(\*SubscriptBox[SuperscriptBox[\(\[CapitalGamma]\), \(\\\ \(\[Alpha]'\)\)], \(\[Beta]' \[Lambda]'\)]\) ="];


(* ::Text:: *)
(*The transformation law implies that \[CapitalGamma] is not tensorial: the A . Hessian term remains with a minus sign that exactly cancels out the Hessian from the messy partials, by construction. *)


(* ::Subsection:: *)
(*Quotient out A*)


(* ::Text:: *)
(*Because A is an arbitrary contravariant vector, we can factor (or quotient) it out A. *)


(* ::Text:: *)
(*Look again at the A . \[CapitalGamma] term and its indices*)


(* ::Input:: *)
(**)


Echo[gammaPrimeTimesVector/.pretty,"A.\[CapitalGamma] term (again) :"];


(* ::Text:: *)
(*Temporarily substitute a known primed index, cp=\[Gamma]', for an unknown Unique dummy, \[Mu] X Y*)


(* ::Input:: *)
(**)


(rhsWithTargetA=gammaPrimeTimesVector/.{A[\[ScriptCapitalU][idx_]]:>
Partials[x[\[ScriptCapitalU][idx]],x[\[ScriptCapitalU][cp]]]*A[\[ScriptCapitalU][cp]]})/.pretty//TensorForm


(* ::Text:: *)
(*Notice that some harmless Kronecker deltas, in the form of \[PartialD]x^ \[Kappa]/\[PartialD]x^ \[Lambda] and \[PartialD]x^ \[Kappa]/\[PartialD]x^ c, have been inserted. *)


(* ::Text:: *)
(*Just collect the coefficients of A^\[Gamma]' and we're done*)


(* ::Subsection:: *)
(*Final Transformation Law*)


(* ::Input:: *)
(**)


Echo[Coefficient[rhsWithTargetA,A[\[ScriptCapitalU][cp]]]/.pretty//TensorForm,"\!\(\*SubscriptBox[SuperscriptBox[\(\[CapitalGamma]\), \(\[Alpha]'\)], \(\[Beta]' \[Gamma]'\)]\) ="];


(* ::Section:: *)
(*Christoffel Symbols*)


(* ::Text:: *)
(*Up to this point, we have not actually used a metric tensor! MTW devote seven whole chapters to differential geometry without a metric (9-15), stressing that many general results in differential geometry\[LongDash]the topological results\[LongDash]can be derived without the metric. In particular, the covariant derivative and the connection coefficients do not, actually, depend on any metrical ability to measure distances, but only on assumptions of continuity and differentiability. *)


(* ::Text:: *)
(*But gravitation via general relativity requires a special case: Riemannian Geometry. For that, we need a metric, and even a bit more: symmetry conditions on both the connection coefficients and on the metric. From these additions, we derive the ultra-famous gravitational Christoffel symbols:*)


(* ::DisplayFormulaNumbered:: *)
(*Subscript[\[CapitalGamma]^\[Sigma], \[Mu] \[Nu]]=1/2 g^(\[Sigma] \[Lambda]) (Subscript[g, \[Lambda] \[Mu],\[Nu]]+Subscript[g, \[Lambda] \[Nu],\[Mu]]-Subscript[g, \[Mu] \[Nu],\[Lambda]])*)


(* ::Text:: *)
(*This is equation 8.24 of MTW, after renaming some indices. It is also called "The Fundamental Theorem of Riemannian Geometry" in mathematical texts like do\[NonBreakingSpace]Carmo and Lee (see References). *)


(* ::Text:: *)
(*Let's do a step-by-step derivation via \[ScriptCapitalU]\[ScriptCapitalD].*)


(* ::Text:: *)
(*Start by reserving some symbols.*)


(* ::Input:: *)
(**)


ClearAll[g,\[CapitalGamma]Sym,gSym,term1,term2,term3,koszul,targetIndex];


(* ::Subsection:: *)
(*Physical Constraints*)


(* ::Subsubsection:: *)
(*Torsion-Free Geometry*)


(* ::Text:: *)
(*By MTW, Box 10.2, page 250, \[CapitalGamma] is symmetric in its covariant (down) indices:*)


(* ::DisplayFormulaNumbered:: *)
(*Subscript[\[CapitalGamma]^\[Alpha], \[Beta] \[Gamma]]=Subscript[\[CapitalGamma]^\[Alpha], \[Gamma] \[Beta]]*)


(* ::Input:: *)
(**)


Echo[\[CapitalGamma]Sym=
\[CapitalGamma][u_,\[ScriptCapitalD][b_],\[ScriptCapitalD][c_]]:>
\[CapitalGamma][u,\[ScriptCapitalD][Sort[{b,c}][[1]]],\[ScriptCapitalD][Sort[{b,c}][[2]]]],
"\[CapitalGamma] Symmetry: "];


(* ::Subsubsection:: *)
(*Metric Symmetry*)


(* ::Text:: *)
(*By MTW, Equation 8.24, page 210, the metric g is symmetric (the display is a bit off, having gone through \[ScriptCapitalU]\[ScriptCapitalD] formatting, which was not designed for such)*)


(* ::Input:: *)
(**)


Echo[gSym=
g[\[ScriptCapitalD][a_],\[ScriptCapitalD][b_]]:>
g[\[ScriptCapitalD][Sort[{a,b}][[1]]],\[ScriptCapitalD][Sort[{a,b}][[2]]]],
"g Symmetry: "];


(* ::Subsubsection:: *)
(*Metric Compatibility Postulates*)


(* ::Text:: *)
(*By MTW, Equation 8.23 and Chapter 14:*)


(* ::DisplayFormulaNumbered:: *)
(*Grad[Subscript[g, \[Mu] \[Nu]],\[Lambda]]=Grad[Subscript[g, \[Nu] \[Lambda]],\[Mu]]=Grad[Subscript[g, \[Lambda] \[Mu]],\[Nu]]=0*)


(* ::Input:: *)
(**)


Echo[(term1=CD[g[\[ScriptCapitalD][\[Mu]],\[ScriptCapitalD][\[Nu]]],x[\[ScriptCapitalU][\[Lambda]]]]/. \[CapitalGamma]Sym/. gSym)//TensorForm,"0 = \!\(\*SubscriptBox[\(\[Del]\), \(\[Lambda]\)]\)\!\(\*SubscriptBox[\(g\), \(\[Mu]\[Nu]\)]\) ="];


(* ::Input:: *)
(**)


Echo[(term2=CD[g[\[ScriptCapitalD][\[Nu]],\[ScriptCapitalD][\[Lambda]]],x[\[ScriptCapitalU][\[Mu]]]]/. \[CapitalGamma]Sym/. gSym)//TensorForm,"0 = \!\(\*SubscriptBox[\(\[Del]\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(g\), \(\[Nu]\[Lambda]\)]\) ="];


(* ::Input:: *)
(**)


Echo[(term3=CD[g[\[ScriptCapitalD][\[Lambda]],\[ScriptCapitalD][\[Mu]]],x[\[ScriptCapitalU][\[Nu]]]]/. \[CapitalGamma]Sym/. gSym)//TensorForm,"0 = \!\(\*SubscriptBox[\(\[Del]\), \(\[Nu]\)]\)\!\(\*SubscriptBox[\(g\), \(\[Lambda]\[Mu]\)]\) ="];


(* ::Subsubsection:: *)
(*Koszul Cyclic Permutation*)


(* ::Text:: *)
(*By MTW, Equation 8.24b, page 210, find a linear combination of covariant derivatives of g that isolates a factor of \[CapitalGamma] in a coordinate basis (Subscript[c, a b c]\[Congruent]0). The particular one we choose is *)


(* ::DisplayFormulaNumbered:: *)
(*Grad[Subscript[g, \[Nu] \[Lambda]],\[Mu]]+Grad[Subscript[g, \[Lambda] \[Mu]],\[Nu]]-Grad[Subscript[g, \[Mu] \[Nu]],\[Lambda]]=0*)


(* ::Input:: *)
(**)


Echo[koszul=(term2+term3-term1),"0 ="];


(* ::Subsubsection:: *)
(*Solve for \[CapitalGamma]*)


(* ::Text:: *)
(*Because the Koszul equation is zero, we'll coalesce the terms with \[CapitalGamma] and set them equal to the negative of the terms without \[CapitalGamma]:*)


(* ::Text:: *)
(*Pick the terms with a factor of \[CapitalGamma]:*)


(* ::Input:: *)
(**)


Echo[gammaPart=Select[List@@koszul,!FreeQ[#,\[CapitalGamma]]&]//Total,"\[CapitalGamma] part ="];


(* ::Text:: *)
(*Coalesce (canonicalize) and simplify:*)


(* ::Input:: *)
(**)


Echo[lhsRaw=-gammaPart//CanonicalizeIndices,"\[CapitalGamma] part (canonical) ="];


(* ::Text:: *)
(*Pick out the terms with no \[CapitalGamma] factor:*)


(* ::Input:: *)
(**)


Echo[rhsRaw=Select[List@@koszul,FreeQ[#,\[CapitalGamma]]&]//Total,"g partials ="];


(* ::Input:: *)
(**)


Echo[TensorForm[lhsRaw==rhsRaw],"Isolated Equation :"];


(* ::Subsubsection:: *)
(*Isolate \[CapitalGamma]*)


(* ::Text:: *)
(*The strategy is to contract this equation over a contravariant metric, schematically g^( \[Sigma] \[Lambda]), to generate a Kronecker delta, Subscript[\[Delta]^\[Sigma], \[Rho]], leaving a bare \[CapitalGamma] on the left. It takes a few steps of \[ScriptCapitalU]\[ScriptCapitalD] algebra to do this robustly:*)


(* ::Text:: *)
(*Find the metric inside the lhs:*)


(* ::Input:: *)
(**)


Echo[metricFound=First[Cases[lhsRaw,g[__],Infinity]],"g found :"];


(* ::Text:: *)
(*Find the \[CapitalGamma] inside the lhs:*)


(* ::Input:: *)
(**)


Echo[gammaFound=First[Cases[lhsRaw,\[CapitalGamma][__],Infinity]],"\[CapitalGamma] found :"];


(* ::Text:: *)
(*Extract raw symbols from the indices:*)


(* ::Input:: *)
(**)


Echo[metricIndices=First/@(List@@metricFound),"g indices :"];
Echo[gammaUpIndex=First[First[gammaFound]],"\[CapitalGamma] up index"];


(* ::Text:: *)
(*The g index that matches the \[CapitalGamma] up-index is the dummy; the other is the target index:*)


(* ::Input:: *)
(**)


Echo[targetIndex=If[metricIndices[[1]]===gammaUpIndex,
metricIndices[[2]],metricIndices[[1]]],"target index :"];


(* ::Text:: *)
(*Construct the specific contravariant g needed; include a factor of 1/2 to cancel out the factor of 2 from the \[CapitalGamma] part*)


(* ::Input:: *)
(**)


Echo[invMetricFactor=1/2 g[\[ScriptCapitalU][\[Sigma]],\[ScriptCapitalU][targetIndex]],"\!\(\*FractionBox[\(1\), \(2\)]\)\!\(\*SuperscriptBox[\(g\), \(-1\)]\) ="];


(* ::Text:: *)
(*Contract via a custom rule:*)


(* ::Input:: *)
(**)


Echo[deltaContractionRule=\[Delta][\[ScriptCapitalU][s_],\[ScriptCapitalD][r_]]*\[CapitalGamma][\[ScriptCapitalU][r_],a_,b_]:>\[CapitalGamma][\[ScriptCapitalU][s],a,b],"\[Delta]-\[CapitalGamma] contraction rule: "];


(* ::Item:: *)
(*TODO: promote the contraction rule to the main \[ScriptCapitalU]\[ScriptCapitalD] code base.*)


(* ::Text:: *)
(*Solve the lhs:*)


(* ::Input:: *)
(**)


Echo[lhsSolved=lhsRaw*invMetricFactor//.metricRules//.deltaContractionRule,"LHS solved :"];


(* ::Text:: *)
(*Rewrite the RHS with the contravariant metric:*)


(* ::Input:: *)
(**)


Echo[rhsSolved=Collect[rhsRaw*invMetricFactor,g[\[ScriptCapitalU][_],\[ScriptCapitalU][_]]],"RHS solved :"];


(* ::Subsubsection:: *)
(*Final Equation*)


(* ::Input:: *)
(**)


Echo[(finalEquation=lhsSolved==rhsSolved)//TensorForm,"Fundamental Theorem of Differential Geometry :"]; 


(* ::Section:: *)
(*Conclusion*)


(* ::Text:: *)
(*We began this notebook with a non-tensorial partial derivative of a contravariant tensor. However, by adding a correction and then demanding linearity and tensoriality, we forced a covariant derivative to emerge, with the correction proportional to the non-tensorial connection \[CapitalGamma].*)


(* ::Text:: *)
(*Also, not just accepting textbook derivations of the Christoffel symbols for gravitation, we derive them ab-initio in \[ScriptCapitalU]\[ScriptCapitalD]. By insisting that lengths be preserved (\[Del]g=0) and twisting be forbidden (Subscript[\[CapitalGamma], \[Mu] \[Nu]]=Subscript[\[CapitalGamma], \[Nu] \[Mu]]), \[ScriptCapitalU]\[ScriptCapitalD] collapses the Koszul sum into the standard formula: *)


(* ::DisplayFormula:: *)
(*Subscript[\[CapitalGamma]^\[Sigma], \[Mu] \[Nu]]=1/2 g^(\[Sigma] \[Lambda]) (Subscript[g, \[Lambda] \[Mu],\[Nu]]+Subscript[g, \[Lambda] \[Nu],\[Mu]]-Subscript[g, \[Mu] \[Nu],\[Lambda]])*)


(* ::Text:: *)
(*The form of the Christoffel symbols shows that gravitation, manifested in the connection, is solely determined by the metric and its ordinary partial derivatives. *)


(* ::Text:: *)
(*With a working covariant derivative in \[ScriptCapitalU]\[ScriptCapitalD]'s Relativity Toolkit, we are equipped for future installments on geodesics, geodesic deviation, tidal forces, accelerated observers, Schwarzschild and Kerr metrics, black holes, etc. *)


(* ::Section:: *)
(*References*)


(* ::ItemNumbered:: *)
(*Poisson, Eric. A Relativist's Toolkit: The Mathematics of Black Hole Mechanics. Cambridge University Press, 2004.*)


(* ::ItemNumbered:: *)
(*Beckman, Brian. From the steam age to quantum fields: a unified approach via UD tensorial calculus. Wolfram Community post. *)


(* ::ItemNumbered:: *)
(*Beckman, Brian. General relativity in the UD calculus. Wolfram Community post.*)


(* ::ItemNumbered:: *)
(*Beckman, Brian. Formal Differential Geometry in the UD Calculus: Part 1. Wolfram Community post.*)


(* ::ItemNumbered:: *)
(*[MTW] Misner, Charles W.; Thorne, Kip S.; Wheeler, John Archibald. Gravitation. Princeton University Press, 2017.*)


(* ::ItemNumbered:: *)
(*[L&R] Lovelock, David and Rund, Hanno. Tensors, Differential Forms, and Variational Principles. Dover.*)


(* ::ItemNumbered:: *)
(*John M. Lee, Riemannian Manifolds: An Introduction to Curvature. Springer.*)


(* ::ItemNumbered:: *)
(*Manfredo P. do Carmo, Riemannian Geometry. Springer. *)


(* ::ItemNumbered:: *)
(*Wolfram Function Repository. MetricTensor, RiemannTensor, EinsteinTensor. For component-based calculations.*)


(* ::ItemNumbered:: *)
(*Wolfram Research. Mathematica Documentation. Tensor Calculus.*)
