# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .py
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.19.3
#   kernelspec:
#     display_name: Python 3 (ipykernel)
#     language: python
#     name: python3
# ---

# %% [markdown]
# # Schwarzschild Derivation: Python Phase B
# This notebook replicates the ab-initio derivation of the Schwarzschild metric from the RelativityToolkit, using SymPy.

# %%
import sympy
from sympy import symbols, Function, diff, Matrix, simplify, Rational, sin, dsolve, Eq

# %% [markdown]
# ## 1. Coordinates and Metric Ansatz
# We assume a spherically symmetric, static metric of the form:
# $ds^2 = -A(r)dt^2 + B(r)dr^2 + r^2 d\theta^2 + r^2 \sin^2\theta d\phi^2$

# %%
t, r, theta, phi = symbols('t r theta phi')
coords = [t, r, theta, phi]

A = Function('A')(r)
B = Function('B')(r)

# Covariant Metric g_uv
g_dd = Matrix([
    [-A, 0, 0, 0],
    [0, B, 0, 0],
    [0, 0, r**2, 0],
    [0, 0, 0, r**2 * sin(theta)**2]
])

# Contravariant Metric g^uv
g_uu = g_dd.inv()

# %% [markdown]
# ## 2. Christoffel Symbols
# $\Gamma^\sigma_{\mu\nu} = \frac{1}{2} g^{\sigma\lambda} (\partial_\nu g_{\lambda\mu} + \partial_\mu g_{\lambda\nu} - \partial_\lambda g_{\mu\nu})$

# %%
def get_christoffel(sigma, mu, nu):
    res = 0
    for lam in range(len(coords)):
        term1 = diff(g_dd[lam, mu], coords[nu])
        term2 = diff(g_dd[lam, nu], coords[mu])
        term3 = diff(g_dd[mu, nu], coords[lam])
        res += 0.5 * g_uu[sigma, lam] * (term1 + term2 - term3)
    return simplify(res)

# Pre-calculate non-zero Gammas to speed up Riemann/Ricci
Gammas = {}
for s in range(4):
    for m in range(4):
        for n in range(m, 4): # Symmetric in m, n
            val = get_christoffel(s, m, n)
            if val != 0:
                Gammas[(s, m, n)] = val
                Gammas[(s, n, m)] = val

# %% [markdown]
# ## 3. Riemann Tensor
# $R^\rho_{\sigma\mu\nu} = \partial_\mu \Gamma^\rho_{\nu\sigma} - \partial_\nu \Gamma^\rho_{\mu\sigma} + \Gamma^\rho_{\mu\lambda} \Gamma^\lambda_{\nu\sigma} - \Gamma^\rho_{\nu\lambda} \Gamma^\lambda_{\mu\sigma}$

# %%
def get_riemann(rho, sigma, mu, nu):
    # Partial terms
    term1 = diff(Gammas.get((rho, nu, sigma), 0), coords[mu])
    term2 = diff(Gammas.get((rho, mu, sigma), 0), coords[nu])
    
    # Interaction terms
    term3 = 0
    term4 = 0
    for lam in range(4):
        term3 += Gammas.get((rho, mu, lam), 0) * Gammas.get((lam, nu, sigma), 0)
        term4 += Gammas.get((rho, nu, lam), 0) * Gammas.get((lam, mu, sigma), 0)
        
    return simplify(term1 - term2 + term3 - term4)

# %% [markdown]
# ## 4. Ricci Tensor (Vacuum Field Equations)
# $R_{\sigma\nu} = R^\mu_{\sigma\mu\nu} = 0$

# %%
print("Calculating Ricci Components...")
R_dd = {}
for s in range(4):
    for n in range(s, 4):
        val = 0
        for m in range(4):
            val += get_riemann(m, s, m, n)
        if val != 0:
            R_dd[(s, n)] = simplify(val)
            R_dd[(n, s)] = R_dd[(s, n)]
            print(f"R_{coords[s]}{coords[n]} is non-zero")

# %% [markdown]
# ## 5. Solving for A(r) and B(r)

# %%
# Extract key equations
R_tt = R_dd[(0, 0)]
R_rr = R_dd[(1, 1)]
R_thth = R_dd[(2, 2)]

# Strategy: R_tt/A + R_rr/B = 0  => A'B + AB' = 0 => (AB)' = 0
eq_comb = simplify(R_tt / A + R_rr / B)
print(f"Combined R_tt/A + R_rr/B: {eq_comb}")

# Solving (AB)' = 0 => AB = constant. Boundary condition at infinity A=1, B=1 => AB = 1
# So B = 1/A

# Now solve R_thth = 0 with B = 1/A
final_eq = simplify(R_thth.subs(B, 1/A).subs(diff(B, r), diff(1/A, r)))
print(f"Final Equation for A(r): {final_eq} = 0")

# Solving A + r*A' - 1 = 0
sol = dsolve(Eq(final_eq, 0), A)
print(f"General Solution for A(r): {sol}")

# Matching Newtonian Limit: A(r) = 1 - 2M/r
# %%
