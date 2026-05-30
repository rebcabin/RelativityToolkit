import sympy
from sympy import symbols, Function, diff, Matrix, simplify, Rational, Sum, Indexed, Idx, LeviCivita, Transpose

class RelativityEngine:
    """
    Python implementation of the UD Calculus (Relativity Toolkit) logic using SymPy.
    """
    def __init__(self, coords):
        self.coords = coords
        self.dim = len(coords)
        self.g_dd = None
        self.g_uu = None
        self.connection_symbol = 'Gamma'

    def set_metric(self, matrix):
        """Sets the covariant metric and precomputes the inverse."""
        self.g_dd = Matrix(matrix)
        self.g_uu = self.g_dd.inv()

    def get_christoffel(self, sigma, mu, nu):
        """
        Calculates Christoffel symbol (Levi-Civita connection):
        Gamma^sigma_{mu,nu} = 1/2 * g^sigma,lam * (d_nu g_lam,mu + d_mu g_lam,nu - d_lam g_mu,nu)
        """
        res = 0
        for lam in range(self.dim):
            term1 = diff(self.g_dd[lam, mu], self.coords[nu])
            term2 = diff(self.g_dd[lam, nu], self.coords[mu])
            term3 = diff(self.g_dd[mu, nu], self.coords[lam])
            res += 0.5 * self.g_uu[sigma, lam] * (term1 + term2 - term3)
        return simplify(res)

    def get_riemann(self, mu, lam, nu, rho, gamma_func):
        """
        Calculates Riemann Curvature Tensor component:
        R^mu_{lam,nu,rho} = d_rho Gamma^mu_{lam,nu} - d_nu Gamma^mu_{lam,rho} 
                          + Gamma^kap_{lam,nu} * Gamma^mu_{kap,rho} 
                          - Gamma^kap_{lam,rho} * Gamma^mu_{kap,nu}
        """
        d_gamma1 = diff(gamma_func(mu, lam, nu), self.coords[rho])
        d_gamma2 = diff(gamma_func(mu, lam, rho), self.coords[nu])
        
        interact1 = sum(gamma_func(kap, lam, nu) * gamma_func(mu, kap, rho) for kap in range(self.dim))
        interact2 = sum(gamma_func(kap, lam, rho) * gamma_func(mu, kap, nu) for kap in range(self.dim))
        
        return simplify(d_gamma1 - d_gamma2 + interact1 - interact2)

    def get_ricci(self, lam, rho, riemann_func):
        """
        Calculates Ricci Tensor component via contraction of Riemann:
        R_{lam,rho} = R^mu_{lam,mu,rho}
        """
        return simplify(sum(riemann_func(mu, lam, mu, rho) for mu in range(self.dim)))

    def get_ricci_scalar(self, ricci_matrix):
        """Calculates Ricci Scalar R = g^mu,nu R_mu,nu"""
        return simplify(sum(self.g_uu[i, j] * ricci_matrix[i, j] for i in range(self.dim) for j in range(self.dim)))
