# RelativityToolkit

**A Symbolic Tensor Domain-Specific Language Embedded in Wolfram.**
with emphasis on general relativity

> Current Version: v1.8.0

> License: MIT

## Overview

**RelativityToolkit** (RT) is an embedded domain-specific language (EDSL) for differential geometry built on the Wolfram language. It features tensorial algebra in standard index notation and compilation (lowering) to Wolfram arrays. Current applications, in the NOTEBOOKS folder, emphasize general relativity, building progressively from covariant derivatives, through connections, metrics, Christoffel symbols, and back to metrics again, illustrated by the Schwarzschild metric for black holes. All derivations are *ab-initio*, assuming only an undergraduate's exposure to advanced calculus, differential equations, and classical physics. RT treats derivations both as exercises in computer algebra and numerics and as exercises in software engineering. 

RT is based on the novel $\mathcal{U}\mathcal{D}$ (up-down) calculus, which manipulates tensorial expressions in the standard component notation, with an input form hospitable to the Wolfram language and an output suitable for human inspection. For example: 

* `A[`$\mathcal{U}$`[`$\mu$`]]` displays as $A^\mu$, representing a contravariant vector

* `g[`$\mathcal{D}$`[`$\mu$], $\mathcal{D}$`[`$\nu$`]]A[`$\mathcal{U}$`[`$\mu$`]]==A[`$\mathcal{D}$`[`$\nu$`]]` displays as $g_{\mu\nu}A^\mu=A_\nu$, representing the contraction of $A^\mu$ with the metric via Einstein summation to covariant form

* `CD[A[`$\mathcal{U}$`[`$\mu$`]], x[A[`$\mathcal{U}$`[`$\nu$`]]]]`, displays as $A^\mu_{\;;\nu}$ , representing a covariant derivative

RT compiles representations of the Christoffel symbols, Riemann, and Ricci tensors into concrete component arrays, differential equations, and physical solutions, supporting calculations such as the derivation of the Schwarzschild metric.

## Key Features

- **Canonicalization:** Automatic renaming of dummy summation indices to prevent capture of free indices.

* **Index Coalescing:** Recognizes, for instance, that $A^{\mu}B_\mu+A^{\lambda}B_\lambda=2A^{\mu}B_\mu$.

- **Testing**: Includes a regression suite with $\sim{}50$ unit tests covering algebra, differentiation, and valence.
    
- **Special Compiler Applications:**
    
    - `ChristoffelsFromMetric`: Generates Christoffel symbols.
        
    - `ContractAll`: Performs Einstein summation recursively on tensor expressions.
        
    - `EvaluateUDPartials`: Compiles symbolic gradients into Wolfram `D[...]` operators, supporting equations and solutions symbolically and numerically

## Getting Started

### Prerequisites

- To run notebooks: Wolfram Language (Mathematica) (Tested on 14.3).

* To view notebooks: the free [Wolfram Player](https://www.google.com/url?sa=i&source=web&rct=j&url=https://www.wolfram.com/player/&ved=2ahUKEwjq6trg2qWSAxXTDTQIHeGhE1UQy_kOegQIBRAB&opi=89978449&cd&psig=AOvVaw2stCUJP_p4DojxsrA1smCR&ust=1769396769789000) 

- To read PDF files, there are no prerequisites.

### Installation

Clone the repository:

Bash

```
git clone https://github.com/rebcabin/RelativityToolkit.git
```

### Usage

The best way to get started is to read and evaluate the notebooks in the NOTEBOOKS folder. These have all been submitted to the [Wolfram Community web site](https://community.wolfram.com/web/bcbeckman), where they enjoy *Staff Pick* status. I recommend starting with [Covariant Derivative and Connections](https://github.com/rebcabin/RelativityToolkit/blob/main/NOTEBOOKS/CovariantDervativeAndConnectionsInTheUDCalculus004.nb), then moving up to [Riemann Refactored](https://github.com/rebcabin/RelativityToolkit/blob/main/NOTEBOOKS/RiemannInUDRefactored.nb), [Schwarzschild Orbits](https://github.com/rebcabin/RelativityToolkit/blob/main/NOTEBOOKS/SchwarzschildOrbitInUD_002.nb), and [Schwarzschild from Scratch](https://github.com/rebcabin/RelativityToolkit/blob/main/NOTEBOOKS/SchwarzschildFromScratch001.nb). 

These

All these notebooks have corresponding PDFs in the PDFS folder for those who do not wish to run the Wolfram kernel or Mathematica.

   

## Repository Structure

- `RelativityToolkit.wl`: **The Engine.** Contains all compiler definitions (`Contract`, `CD`, `TensorForm`).
    
- `ReferenceManual.md`: Complete API documentation for v1.8.0.
    
- `RelativityToolkit_RegressionTests.wl`: Unit tests ensuring algebraic correctness.
    
- `schwarzschild_from_scratch.wl`: The flagship demo script.
    
- `NOTEBOOKS/`: Mathematica notebooks used for development and verification.
    
- `PDFS/`: Readable exports of the derivation.
    

## Roadmap

- **v1.8.0 (Current):** Stable prototype, Script mode, Global namespace.
    
- **v2.0.0 (Planned):** Full Wolfram Package (`BeginPackage["RelativityToolkit`"]`), namespace sanitation, sparse array support.
    

## Author

**Brian Beckman** (@rebcabin)

## License

This project is licensed under the MIT License - see the [LICENSE](https://www.google.com/search?q=LICENSE) file for details.
