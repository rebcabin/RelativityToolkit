# Relativity Toolkit v1.8.0 Reference Manual

## 1. System Configuration & Constants

### `RelativityToolkitVersion`

A string containing the current version number of the engine.

- **Value:** `"1.8.0"`
    

### `RelativityConnection`

The global symbol currently used to represent the affine connection coefficients in Covariant Derivatives (`CD`).

- **Default:** `\[CapitalGamma]` ($\Gamma$)
    
- **Usage:** Used internally by `CD`. Change via `SetConnection`.
    

### `SetConnection[sym]`

Sets the global `RelativityConnection` to the specified symbol.

- **Arguments:** `sym` (Symbol) - The new symbol to use for the connection (e.g., `A` for gauge fields).
    
- **Effect:** Future evaluations of `CD` will generate corrections using this symbol.
    

### `noValence`

A constant representing the valence of a scalar or non-tensor object.

- **Value:** `{{}, {}}` (Empty Upper, Empty Lower).
    

---

## 2. Types & Syntax Literals

### `\[ScriptCapitalU]` ($\mathcal{U}$)

Wrapper indicating an **Upper** (Contravariant) index.

- **Syntax:** `vec[\[ScriptCapitalU][\mu]]` represents $v^\mu$.
    
- **Input Alias:** `Esc scU Esc`.
    

### `\[ScriptCapitalD]` ($\mathcal{D}$)

Wrapper indicating a **Lower** (Covariant) index.

- **Syntax:** `vec[\[ScriptCapitalD][\mu]]` represents $v_\mu$.
    
- **Input Alias:** `Esc scD Esc`.
    

### `x`

The reserved head for coordinate functions.

- **Syntax:** `x[\[ScriptCapitalU][\mu]]` represents the coordinate $x^\mu$.
    
- **Usage:** Used as the "denominator" in partial derivatives to indicate differentiation with respect to a coordinate.
    

### `g`

The reserved symbol for the **Metric Tensor** within the built-in `metricRules`.

- **Syntax:** `g[\[ScriptCapitalD][\mu], \[ScriptCapitalD][\nu]]` ($g_{\mu\nu}$) or `g[\[ScriptCapitalU][\mu], \[ScriptCapitalU][\nu]]` ($g^{\mu\nu}$).
    
- **Note:** `metricRules` specifically looks for the symbol `g`.
    

### `\[CapitalGamma]` ($\Gamma$)

The default symbol for **Christoffel Symbols** (Connection Coefficients).

- **Syntax:** `\[CapitalGamma][\[ScriptCapitalU][\lambda], \[ScriptCapitalD][\mu], \[ScriptCapitalD][\nu]]` represents $\Gamma^\lambda_{\mu\nu}$.
    

### `\[Delta]` ($\delta$)

The **Kronecker Delta**.

- **Syntax:** `\[Delta][\[ScriptCapitalU][\mu], \[ScriptCapitalD][\nu]]` represents $\delta^\mu_\nu$.
    
- **Origin:** Generated automatically when contracting the metric with its inverse via `metricRules`.
    

---

## 3. Valence & Tensor Logic

### `valence[expr]`

Returns the index structure of a tensor expression.

- **Returns:** `{ {upper_indices...}, {lower_indices...} }`.
    
- **Example:** `valence[T[\[ScriptCapitalU][\mu], \[ScriptCapitalD][\nu]]]` returns `{{\mu}, {\nu}}`.
    

### `upQ[expr]`

Checks if an expression is purely contravariant (all indices upper) or is a valid upper index wrapper.

- **Returns:** `True` or `False`.
    

### `downQ[expr]`

Checks if an expression is purely covariant (all indices lower) or is a valid lower index wrapper.

- **Returns:** `True` or `False`.
    

### `contractValence[{up, down}]`

Helper function that cancels matching indices from the upper and lower lists.

- **Usage:** Used internally by the arithmetic engine to calculate the resulting valence of a product.
    

### `CanonicalizeTerm[term]`

Renames dummy (summation) indices in a single term to a canonical form (`\[FormalI]1`, `\[FormalI]2`...) to allow structural comparison.

### `CanonicalizeIndices[expr]`

Applies `CanonicalizeTerm` to every term in a sum or equation.

- **Usage:** Use this to verify if two tensor expressions are mathematically equivalent despite having different dummy index names.
    

### `TensorForm[expr]`

Formats a tensor expression for display by replacing canonical/formal dummy indices with human-readable Greek letters ($\lambda, \kappa, \rho, \sigma...$).

### `ExtractCoefficient[expr, field]`

Implements the **Quotient Theorem**. Extracts the coefficient tensor of a specified vector/tensor field.

- **Arguments:**
    
    - `expr`: The tensor expression.
        
    - `field`: The symbol of the field to "divide out" (e.g., `A` for $A^\mu$).
        
- **Usage:** If `expr` is $R_{\mu\nu}A^\nu$, `ExtractCoefficient[expr, A]` returns $R_{\mu\nu}$.
    

---

## 4. Algebraic Rules

### `metricRules`

A list of replacement rules for metric operations.

- **Operations:** Raising/Lowering indices, contracting metric with inverse, and simplifying Kronecker deltas.
    
- **Target:** Operates on the symbol `g`.
    

### `torsionRules`

A list of replacement rules enforcing symmetry of the connection (Torsion-Free).

- **Effect:** Sorts lower indices: $\Gamma^\lambda_{\nu\mu} \to \Gamma^\lambda_{\mu\nu}$.
    

### `robustTransformRules`

Rules for handling alpha-conversion (index renaming) to prevent variable collisions during substitution.

---

## 5. Calculus Operations

### `Partials[expr, var]`

Represents the **Partial Derivative** $\partial_\nu T$.

- **Arguments:**
    
    - `expr`: The tensor being differentiated.
        
    - `var`: The coordinate, usually `x[\[ScriptCapitalU][\nu]]`.
        
- **Output:** formats as $T_{,\nu}$ or $\frac{\partial T}{\partial x^\nu}$.
    

### `CD[expr, var]`

Represents the **Covariant Derivative** $\nabla_\nu T$.

- **Arguments:** `expr` and `var` (same as `Partials`).
    
- **Evaluation:** Automatically expands into Partial Derivatives + Connection Coefficient corrections based on the valence of `expr`.
    

### `differentiationRules`

Low-level rules for expanding `Partials` over sums and products (Leibniz rule).

### `ExpandDerivatives[expr]`

Applies `differentiationRules` repeatedly to fully expand all partial derivatives in an expression.

### `EvaluateUDPartials[expr]`

Converts abstract `Partials` into concrete Wolfram Language `D[...]` operations.

- **Usage:** Used in the final compilation step when binding tensors to explicit coordinate grids.
    

---

## 6. The Compiler (Relativity Engine)

### `ChristoffelsFromMetric[gDD, gUU, sigma, mu, nu]`

Computes the symbolic Christoffel Symbol $\Gamma^\sigma_{\mu\nu}$ from a metric and its inverse.

- **Arguments:**
    
    - `gDD`: The covariant metric tensor (e.g., `gDD`).
        
    - `gUU`: The contravariant inverse metric (e.g., `gUU`).
        
    - `sigma, mu, nu`: The explicit indices.
        
- **Returns:** An expression involving partial derivatives of the metric.
    

### `CalculateRiemannComponent[mu, lambda, nu, rho, GammaGet, coords]`

Computes a specific component of the Riemann Tensor $R^\mu_{\lambda\nu\rho}$.

- **Arguments:**
    
    - `indices`: The four specific indices.
        
    - `GammaGet`: A lookup function (created by `MakeIndexer`) to retrieve Christoffel values.
        
    - `coords`: The list of coordinate symbols.
        

### `CalculateRicciComponent[lambda, rho, RGet, coords]`

Computes a specific component of the Ricci Tensor $R_{\lambda\rho}$ by contracting the Riemann Tensor.

- **Arguments:**
    
    - `RGet`: A lookup function (created by `MakeIndexer`) to retrieve Riemann values.
        

### `Contract[expr, coords]`

Performs Einstein Summation on the **first** detected pair of matching upper/lower indices.

- **Arguments:**
    
    - `expr`: The tensor expression.
        
    - `coords`: A list of coordinate symbols to sum over (e.g., `{t, r, \theta, \phi}`).
        
- **Behavior:** Expands the first pair it finds into a sum. Useful for step-by-step debugging.
    

### `ContractAll[expr, coords]`

Recursively performs Einstein Summation on **all** matching upper/lower indices until no dummy pairs remain.

- **Usage:** The standard "Evaluate" function for tensor algebra.
    

### `MatrixToUDRules[matrix, symbol, valence, coords]`

Converts a 2D Wolfram matrix into a list of UD replacement rules.

- **Example:** `MatrixToUDRules[DiagonalMatrix[{...}], g, \[ScriptCapitalD], {t,r}]` generates `{g[\[ScriptCapitalD][t], \[ScriptCapitalD][t]] -> ...}`.

- **Limitation**: Does not generate rules for zero components. 
  
- TODO: Make a version that generates a Dispatch

- TODO: Make a SparseArray version that handles zero components more gracefully.
    

### `MakeIndexer[table, coords]`

Creates a high-performance lookup function from a pre-computed tensor array.

- **Usage:** `GammaGet = MakeIndexer[GammaArray, {t, r}]`.
    
- **Call:** `GammaGet[t, r, r]` returns the value from the array.
