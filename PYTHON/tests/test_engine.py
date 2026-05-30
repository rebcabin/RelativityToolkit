import pytest
import sympy
from sympy import symbols, sin, cos, Function
from relativity_toolkit import RelativityEngine

def test_engine_initialization():
    coords = symbols('t r theta phi')
    engine = RelativityEngine(coords)
    assert engine.dim == 4
    assert engine.coords == coords

def test_metric_inverse():
    r, theta = symbols('r theta')
    coords = [r, theta]
    engine = RelativityEngine(coords)
    
    # Polar metric
    g_dd = [[1, 0], [0, r**2]]
    engine.set_metric(g_dd)
    
    expected_inv = sympy.Matrix([[1, 0], [0, 1/r**2]])
    assert engine.g_uu == expected_inv

def test_polar_christoffels():
    r, theta = symbols('r theta')
    coords = [r, theta]
    engine = RelativityEngine(coords)
    g_dd = [[1, 0], [0, r**2]]
    engine.set_metric(g_dd)
    
    # Expected: Gamma^r_{theta,theta} = -r
    # Indices: r=0, theta=1
    gamma_r_th_th = engine.get_christoffel(0, 1, 1)
    assert sympy.simplify(gamma_r_th_th - (-r)) == 0
    
    # Expected: Gamma^theta_{r,theta} = 1/r
    gamma_th_r_th = engine.get_christoffel(1, 0, 1)
    assert sympy.simplify(gamma_th_r_th - 1/r) == 0

def test_schwarzschild_vacuum_logic():
    t, r, theta, phi = symbols('t r theta phi')
    coords = [t, r, theta, phi]
    A = Function('A')(r)
    B = Function('B')(r)
    
    engine = RelativityEngine(coords)
    g_dd = [
        [-A, 0, 0, 0],
        [0, B, 0, 0],
        [0, 0, r**2, 0],
        [0, 0, 0, r**2 * sin(theta)**2]
    ]
    engine.set_metric(g_dd)
    
    def gamma(s, m, n): return engine.get_christoffel(s, m, n)
    def riemann(mu, lam, nu, rho): return engine.get_riemann(mu, lam, nu, rho, gamma)
    
    # R_tt + R_rr identity check (the trick used in the notebook)
    R_tt = engine.get_ricci(0, 0, riemann)
    R_rr = engine.get_ricci(1, 1, riemann)
    
    identity = sympy.simplify(R_tt/A + R_rr/B)
    
    # The identity should simplify to a first order term involving A'/A and B'/B
    # Specifically: - (A'B + AB') / (r A B^2)
    expected_identity = -(A.diff(r)*B + A*B.diff(r)) / (r * A * B**2)
    assert sympy.simplify(identity - expected_identity) == 0

def test_ricci_scalar_flat_space():
    t, x, y, z = symbols('t x y z')
    coords = [t, x, y, z]
    engine = RelativityEngine(coords)
    
    # Minkowski metric (flat)
    g_dd = sympy.diag(-1, 1, 1, 1)
    engine.set_metric(g_dd)
    
    def gamma(s, m, n): return engine.get_christoffel(s, m, n)
    def riemann(mu, lam, nu, rho): return engine.get_riemann(mu, lam, nu, rho, gamma)
    
    ricci_matrix = sympy.Matrix([[engine.get_ricci(i, j, riemann) for j in range(4)] for i in range(4)])
    R = engine.get_ricci_scalar(ricci_matrix)
    
    assert R == 0
