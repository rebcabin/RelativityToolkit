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
# # UD Calculus: Python Proof of Concept (POC)
# This notebook demonstrates the implementation of "Up-Down" (UD) index logic in Python using SymPy.

# %%
import sympy
from sympy import symbols, Function, diff, Matrix, simplify, Rational

# %% [markdown]
# ## 1. Core Logic: Index & Tensor Wrappers

# %%
class Index:
    def __init__(self, name, is_up=True):
        self.name = name
        self.is_up = is_up
        self.symbol = symbols(name)

    def __repr__(self):
        return f"{self.name}{'^' if self.is_up else '_'}"

def U(name): return Index(name, is_up=True)
def D(name): return Index(name, is_up=False)

# %% [markdown]
# ## 2. Problem Setup: 2D Polar Coordinates
# $ds^2 = dr^2 + r^2 d\theta^2$

# %%
r, theta = symbols('r theta')
coords = [r, theta]

# Covariant Metric g_uv
g_dd = Matrix([
    [1, 0],
    [0, r**2]
])

# Contravariant Metric g^uv
g_uu = g_dd.inv()

# %% [markdown]
# ## 3. Christoffel Symbol Calculation (Ab-initio)
# $\Gamma^\sigma_{\mu\nu} = \frac{1}{2} g^{\sigma\lambda} (\partial_\nu g_{\lambda\mu} + \partial_\mu g_{\lambda\nu} - \partial_\lambda g_{\mu\nu})$

# %%
def get_g_dd(i, j):
    return g_dd[i, j]

def get_g_uu(i, j):
    return g_uu[i, j]

def christoffel(sigma, mu, nu):
    res = 0
    for lam in range(len(coords)):
        term1 = diff(get_g_dd(lam, mu), coords[nu])
        term2 = diff(get_g_dd(lam, nu), coords[mu])
        term3 = diff(get_g_dd(mu, nu), coords[lam])
        res += 0.5 * get_g_uu(sigma, lam) * (term1 + term2 - term3)
    return simplify(res)

# %% [markdown]
# ## 4. Evaluation & Verification

# %%
print("Christoffel Symbols for Polar Coordinates:")
for s in range(len(coords)):
    for m in range(len(coords)):
        for n in range(len(coords)):
            val = christoffel(s, m, n)
            if val != 0:
                print(f"Gamma^{coords[s]}_{coords[m]}{coords[n]} = {val}")

# %% [markdown]
# ### Expected Results:
# - $\Gamma^r_{\theta\theta} = -r$
# - $\Gamma^\theta_{r\theta} = \Gamma^\theta_{\theta r} = 1/r$
# %%
