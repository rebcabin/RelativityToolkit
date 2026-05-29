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
# # Interactive Schwarzschild Orbits
# This notebook implements a numerical lab for exploring orbits around a Schwarzschild black hole, replicating the "Manipulate" functionality from the RelativityToolkit.

# %%
import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import solve_ivp
import ipywidgets as widgets
from IPython.display import display

# %% [markdown]
# ## 1. Physics: The Geodesic Equations
# In the Schwarzschild metric, the orbits of a particle are governed by the effective potential:
# $V_{eff}(r) = \sqrt{(1 - 2M/r)(1 + L^2/r^2)}$
#
# However, to simulate the actual path, we solve the equations of motion for $u = 1/r$ with respect to $\phi$:
# $\frac{d^2u}{d\phi^2} + u = \frac{M}{L^2} + 3Mu^2$

# %%
def geodesic_eq(phi, y, M, L):
    """
    y[0] = u = 1/r
    y[1] = du/dphi
    """
    u, v = y
    dudphi = v
    dvdphi = (M / L**2) - u + 3 * M * u**2
    return [dudphi, dvdphi]

def solve_orbit(M, L, r0, v_r0):
    """
    r0: initial radius
    v_r0: initial radial velocity (dr/dphi)
    """
    u0 = 1.0 / r0
    v0 = - (1.0 / r0**2) * v_r0
    
    # Event Detection: Stop if we hit the event horizon (u = 1/2M) or escape too far
    def event_horizon(phi, y, M, L):
        return y[0] - 1.0/(2.0*M + 1e-9) # Stop slightly before the singularity
    event_horizon.terminal = True
    
    phi_span = (0, 40 * np.pi) 
    phi_eval = np.linspace(0, 40 * np.pi, 3000)
    
    sol = solve_ivp(geodesic_eq, phi_span, [u0, v0], args=(M, L), 
                    t_eval=phi_eval, events=event_horizon, rtol=1e-6)
    
    r = 1.0 / sol.y[0]
    phi = sol.t
    
    # Convert to Cartesian for plotting
    x = r * np.cos(phi)
    y = r * np.sin(phi)
    
    return x, y

# %% [markdown]
# ## 2. Interactive Plotting

# %%
def update_plot(M, L, r0, vr0):
    plt.figure(figsize=(8, 8))
    
    # Solve
    try:
        x, y = solve_orbit(M, L, r0, vr0)
        plt.plot(x, y, label="Orbit Path", color='cyan')
        
        # Draw the Event Horizon
        horizon_theta = np.linspace(0, 2*np.pi, 100)
        plt.plot(2*M*np.cos(horizon_theta), 2*M*np.sin(horizon_theta), 'r--', label="Event Horizon (2M)")
        plt.fill(2*M*np.cos(horizon_theta), 2*M*np.sin(horizon_theta), 'black', alpha=0.3)
        
    except Exception as e:
        plt.text(0, 0, f"Orbit Terminated: {str(e)}", ha='center')

    plt.axhline(0, color='grey', lw=0.5)
    plt.axvline(0, color='grey', lw=0.5)
    plt.title(f"Schwarzschild Orbit: M={M}, L={L}")
    plt.xlabel("x")
    plt.ylabel("y")
    plt.legend()
    plt.axis('equal')
    
    # Set limits based on r0 to keep the view stable
    limit = r0 * 1.5
    plt.xlim(-limit, limit)
    plt.ylim(-limit, limit)
    plt.show()

# Create widgets
m_slider = widgets.FloatSlider(value=1.0, min=0.1, max=5.0, step=0.1, description='Mass (M)')
l_slider = widgets.FloatSlider(value=4.0, min=0.0, max=10.0, step=0.1, description='Ang. Mom (L)')
r0_slider = widgets.FloatSlider(value=15.0, min=5.0, max=50.0, step=1.0, description='Start r')
vr0_slider = widgets.FloatSlider(value=0.0, min=-5.0, max=5.0, step=0.1, description='Start vr')

# Display UI
ui = widgets.VBox([m_slider, l_slider, r0_slider, vr0_slider])
out = widgets.interactive_output(update_plot, {'M': m_slider, 'L': l_slider, 'r0': r0_slider, 'vr0': vr0_slider})

display(ui, out)

# %% [markdown]
# ## 3. How to View this Content
# 1. **VS Code:** Open `Schwarzschild_Orbits_Interactive.py`. If you have the Jupyter extension, it will show an "Open in Notebook Editor" button.
# 2. **Terminal (Jupyter Lab):**
#    - Run `source .venv/bin/activate`
#    - Run `jupyter lab`
#    - Open the `.py` file. Jupytext will automatically present it as a notebook.
# 3. **Static View:** You can read the `.py` file in any text editor; the `# %%` markers denote cell boundaries.
