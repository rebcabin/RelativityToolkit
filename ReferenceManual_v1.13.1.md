# Relativity Toolkit Reference Manual (v1.13.1)

*Written with the assistance of Gemini AI.*

## Overview
The Relativity Toolkit is a Mathematica package designed for symbolic tensor calculus in general relativity and gauge theory. It employs a specialized index notation system ($\mathcal{U}/\mathcal{D}$) to handle covariant and contravariant indices, coordinate-independent derivatives, and automated contraction.

---

## 1. Core Syntax: Indices and Valance

The toolkit distinguishes between upper (contravariant) and lower (covariant) indices using the following wrappers:
*   `\[ScriptCapitalU][idx]` : Upper index
*   `\[ScriptCapitalD][idx]` : Lower index

### `valence[expr]`
Returns the index structure of a tensor as `{upper_indices, lower_indices}`.

**Examples:**
```mathematica
valence[x[\[ScriptCapitalU][\[Mu]]]] (* Output: {{\[Mu]}, {}} *)
valence[p[\[ScriptCapitalD][\[Nu]]]] (* Output: {{}, {\[Nu]}} *)
valence[T[\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Nu]]]] (* Output: {{\[Mu]}, {\[Nu]}} *)
```

---

## 2. Differentiation

### `CD[tensor, coordinate]`
Computes the Covariant Derivative ($\nabla_\mu$) of a tensor.

**Examples:**
```mathematica
(* Covariant derivative of a vector A^\[Mu] *)
CD[A[\[ScriptCapitalU][\[Mu]]], x[\[ScriptCapitalU][\[Nu]]]]
```

### `CD[field, coordinate, coupling, gaugeField]`
Computes the mixed geometric and gauge covariant derivative.

**Examples:**
```mathematica
(* Gauge covariant derivative for U(1) symmetry *)
CD[\[Psi], x[\[ScriptCapitalU][\[Mu]]], e, A]
(* Returns: CD[\[Psi], x^\[Mu]] - I * e * A_\[Mu] * \[Psi] *)
```

### `ExpandDerivatives[expr]`
Expands symbolic derivatives (like `Partials` and `CD`) into their Leibniz or connection-coefficient forms.

---

## 3. Curvature and Connections

### `ChristoffelsFromMetric[gDD, gUU, \[Sigma], \[Mu], \[Nu]]`
Defines the symbolic Christoffel symbols ($\Gamma^\sigma_{\mu\nu}$) derived from a metric $g$ and its inverse.

### `CalculateRiemannComponent[\[Mu], \[Lambda], \[Nu], \[Rho], RGet, coords]`
Calculates a specific component of the Riemann curvature tensor $R^\mu_{\lambda\nu\rho}$.

### `CalculateRicciComponent[\[Lambda], \[Rho], RGet, coords]`
Calculates a specific component of the Ricci tensor $R_{\lambda\rho}$ by contracting the Riemann tensor.

---

## 4. Compiler and Indexer Utilities

### `MatrixToUDRules[matrix, symbol, type, coords]`
Converts a standard Mathematica matrix into a set of $\mathcal{U}/\mathcal{D}$ substitution rules.

**Examples:**
```mathematica
coords = {t, r, \[Theta], \[Phi]};
SchwAnsatz = DiagonalMatrix[{-A[r], B[r], r^2, r^2 Sin[\[Theta]]^2}];
SchwRules = MatrixToUDRules[SchwAnsatz, gDD, \[ScriptCapitalD], coords];
```

### `MakeIndexer[tensorArray, coords]`
Creates a function that allows you to index a multi-dimensional array using symbolic coordinates instead of integers.

**Examples:**
```mathematica
\[CapitalGamma]Get = MakeIndexer[Schw\[CapitalGamma], coords];
\[CapitalGamma]Get[r, t, t] (* Returns the r-t-t component *)
```

### `TensorForm[expr]`
Displays a complex tensor expression in a readable, canonicalized format.

---

## 5. Task-Driven Workflows

### Task: Deriving the Schwarzschild Metric from an Ansatz
A common workflow involves solving the Einstein Vacuum Equations for a spherically symmetric metric.

1.  **Define the Metric Ansatz:**
    ```mathematica
    coords = {t, r, \[Theta], \[Phi]};
    metric = DiagonalMatrix[{-A[r], B[r], r^2, r^2 Sin[\[Theta]]^2}];
    ```
2.  **Generate Index Rules:**
    ```mathematica
    gDDRules = MatrixToUDRules[metric, gDD, \[ScriptCapitalD], coords];
    gUURules = MatrixToUDRules[Inverse[metric], gUU, \[ScriptCapitalU], coords];
    ```
3.  **Compute Christoffel Symbols:**
    ```mathematica
    (* Contract over all coordinates to evaluate Gamma *)
    \[CapitalGamma]Components = Table[
      ContractAll[ChristoffelsFromMetric[gDD, gUU, s, m, n], coords] /. 
      gDDRules /. gUURules // EvaluateUDPartials,
      {s, coords}, {m, coords}, {n, coords}
    ];
    \[CapitalGamma]Get = MakeIndexer[\[CapitalGamma]Components, coords];
    ```
4.  **Compute Ricci Tensor:**
    ```mathematica
    Riemann = Table[CalculateRiemannComponent[m, l, n, r, \[CapitalGamma]Get, coords], 
                    {m, coords}, {l, coords}, {n, coords}, {r, coords}];
    RGet = MakeIndexer[Riemann, coords];
    Ricci = Table[CalculateRicciComponent[l, r, RGet, coords], {l, coords}, {r, coords}];
    ```
5.  **Solve Einstein Equations:**
    ```mathematica
    (* Set Ricci components to zero and solve for A[r] and B[r] *)
    RicciGet = MakeIndexer[Ricci, coords];
    DSolve[RicciGet[t, t] == 0 && RicciGet[\[Theta], \[Theta]] == 0, {A[r], B[r]}, r]
    ```

### Task: Verifying Gauge Invariance in Electrodynamics
1.  **Define Field Strength:**
    ```mathematica
    F = Partials[A[\[ScriptCapitalD][n]], x[\[ScriptCapitalU][m]]] - 
        Partials[A[\[ScriptCapitalD][m]], x[\[ScriptCapitalU][n]]];
    ```
2.  **Apply Gauge Transformation:**
    ```mathematica
    gaugeRules = {A[\[ScriptCapitalD][idx_]] :> A[\[ScriptCapitalD][idx]] + Partials[\[CapitalLambda], x[\[ScriptCapitalU][idx]]]};
    transformedF = (F /. gaugeRules // ExpandDerivatives) // Expand;
    (* Verification *)
    Simplify[transformedF == F] (* Returns True *)
    ```
