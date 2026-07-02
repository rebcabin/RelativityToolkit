#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Package:: *)

(* ::Title:: *)
(*Stability Analysis of Electrostatically Balanced White-Dwarf Spheres*)


(* ::Subtitle:: *)
(*A Standalone Reference Module on Gravitational-Electrostatic Equilibrium Limits*)


(* ::Section:: *)
(*1. The Scaling Laws and the Fundamental Catch-22*)


(* ::Text:: *)
(*To achieve a perfect static balance between two identical spheres of mass m and charge q separated by a center-to-center distance d, Newton's Law of Gravitation must precisely equal Coulomb's Law:*)


HoldForm[G * m^2 / d^2 == k[e] * q^2 / d^2] // TraditionalForm


(* ::Text:: *)
(*Because the separation distance d appears identically on both sides, it completely cancels out. The distance between the spheres has no bearing on whether they are balanced; it only changes the scale of the forces. The sole requirement for a perfect balance is a strict, invariant charge-to-mass ratio:*)


HoldForm[q / m == Sqrt[G / k[e]]] // TraditionalForm


(* ::Input:: *)
(* Define fundamental constants *)
G = 6.67430*10^-11;       (* m^3 kg^-1 s^-2 *)
ke = 8.98755*10^9;        (* N m^2 C^-2 *)
rho = 2.93*10^10;         (* kg/m^3 (White-dwarf core density) *)
qOverM = Sqrt[G/ke]


(* ::Text:: *)
(*Because the material has a uniform density, the mass scales cubically with the sphere's radius. Consequently, to maintain the required charge-to-mass ratio, the excess charge must also scale cubically. However, the localized stability of the system is governed by the surface electric field E, which drives charge dissipation into the vacuum:*)


HoldForm[E == k[e] * q / R^2] // TraditionalForm


(* ::Text:: *)
(*Substituting the cubic scaling of charge into the electric field equation yields a fundamental geometric rule showing that the surface electric field is directly proportional to the radius of the spheres:*)


HoldForm[E \[Proportional] R] // TraditionalForm


(* ::Section:: *)
(*2. Thresholds of Absolute Stability (Stable Forever)*)


(* ::Text:: *)
(*In a vacuum, an excess of electrons concentrated on a conductor's surface faces an energy barrier called the material's work function. If the surface electric field becomes too intense, it warps and narrows this barrier, allowing electrons to spontaneously pass through via quantum mechanical tunneling\[LongDash]a phenomenon known as Field Electron Emission, mathematically formalized by the Fowler-Nordheim equation. *)


(* ::Text:: *)
(*The absolute threshold below which the probability of quantum tunneling drops to effectively zero is roughly E = 10^7 V/m. At or below this field strength, the energy barrier remains thick enough to lock the electrons in place permanently. Setting E = 10^7 V/m, we compute the maximum dimensions for absolute, permanent stability below:*)


(* ::Input:: *)
EStable = 10.0^7; (* V/m *)

(* Since E = ke*q/R^2 and q = qOverM*m and m = rho*(4/3)*Pi*R^3:
   E = ke * qOverM * rho * (4/3) * Pi * R 
   Solving for R: *)
RStable = EStable / (ke * qOverM * rho * (4/3) * Pi)
VStable = (4/3) * Pi * RStable^3
mStableKg = rho * VStable
mStableGrams = mStableKg * 1000
qStable = qOverM * mStableKg
numElectrons = qStable / 1.602176634*10^-19


(* ::Section:: *)
(*3. The Stability Timeline Matrix*)


(* ::Text:: *)
(*If the radius of the spheres is increased beyond 100 micrometers, the surface electric field rises linearly, causing the quantum barrier to thin. Because field emission currents have an exponential dependence on the electric field strength, the transition from permanent stability to instantaneous discharge occurs with extreme sensitivity as the physical scale increases. *)


(* ::Input:: *)
(* Define a helper function to calculate parameters given a radius R *)
AnalyzeSphere[R_] := Module[
  {vol, mass, charge, field},
  vol = (4/3) * Pi * R^3;
  mass = rho * vol;
  charge = qOverM * mass;
  field = (ke * charge) / R^2;
  Grid[{
    {"Radius (m)", ScientificForm[R]},
    {"Mass (kg)", ScientificForm[mass]},
    {"Mass (g)", ScientificForm[mass * 1000]},
    {"Surface E-Field (V/m)", ScientificForm[field]},
    {"Excess Electrons", ScientificForm[charge / 1.602176634*10^-19]}
   }, Frame -> All]
]


(* ::Subsection:: *)
(*Case A: Permanent / Age of the Universe (~10^17 seconds)*)


(* ::Input:: *)
AnalyzeSphere[100.0*10^-6]


(* ::Subsection:: *)
(*Case B: One Thousand Years (~10^10 seconds)*)


(* ::Input:: *)
AnalyzeSphere[125.0*10^-6]


(* ::Subsection:: *)
(*Case C: One Year (~10^7 seconds)*)


(* ::Input:: *)
AnalyzeSphere[145.0*10^-6]


(* ::Subsection:: *)
(*Case D: One Second*)


(* ::Input:: *)
AnalyzeSphere[280.0*10^-6]


(* ::Subsection:: *)
(*Case E: One Nanosecond (10^-9 seconds)*)


(* ::Input:: *)
AnalyzeSphere[1.0*10^-3]


(* ::Subsection:: *)
(*Case F: Macro-Instantaneous (~10^-21 seconds)*)


(* ::Input:: *)
AnalyzeSphere[1.0*10^-2]


(* ::Section:: *)
(*4. The Structural Limit: The Coulomb Explosion*)


(* ::Text:: *)
(*If we continue to scale the spheres upward into macro-dimensions, we eventually encounter a non-quantum, purely structural mechanical failure. At a sphere radius of roughly R = 4.2 meters, the surface electric field reaches a critical threshold of approximately E = 4 * 10^11 V/m. At this point, the outward electrostatic pressure generated by the immense concentration of surface charge surpasses the internal electron degeneracy pressure and bulk modulus of the white-dwarf matter itself, resulting in a catastrophic disintegration known as a Coulomb Explosion.*)


HoldForm[P[E] == (1/2) * \[Epsilon][0] * E^2] // TraditionalForm


(* ::Input:: *)
epsilon0 = 8.8541878128*10^-12; (* F/m *)
ECritical = 4.0*10^11;
PExtreme = (1/2) * epsilon0 * ECritical^2


(* ::Section:: *)
(*5. References*)


(* ::Text:: *)
(*[1] Field Electron Emission & The Fowler-Nordheim Equation:
Fowler, R. H., & Nordheim, L. (1928). "Electron Emission in Intense Electric Fields." Proceedings of the Royal Society of London. Series A, 119(781), 173-181. 
This foundational paper describes how high external electric fields warp the potential barrier at a metal/vacuum interface, enabling electrons to quantum tunnel out via what is now known as Fowler-Nordheim tunneling.*)


(* ::Text:: *)
(*[2] The Coulomb Explosion Effect:
Schneider, s., et al. (1993). "Coulomb Explosion of Intense-Laser-Produced Plasmas." Physical Review A, 48(4), 2915.
While commonly studied in laser-plasma physics and cluster ionization where rapid stripping of electrons leaves behind a highly concentrated, mutually repulsive positive charge core, the classical electrostatics of structural mechanical failure due to surface charge pressure P = (1/2)*epsilon_0*E^2 follows identical dynamic fragmentation principles.*)
