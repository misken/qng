__author__ = 'mark'

import qng

# M/H2/4
exact = 0.79
whitt = 0.79
simio = 0
rho = 0.9
m = 4
lam = rho * m
mu = 1
ca2 = 1.0
cs2 = 2.25
approx = qng.ggm_prob_wait_whitt(lam, mu, m, ca2, cs2)
whichpi = qng._ggm_prob_wait_whitt_whichpi(rho, m, ca2, cs2)
print("exact={0}, whitt={1}, mywhitt={2:6.3f}, simio={3}, whichpi={4}".format(exact, whitt, approx, simio, whichpi))

# D/H2/4 (cs2=2.0) -
exact = 0.70
whitt = 0.71
simio = 0.74
rho = 0.9
m = 4
lam = rho * m
mu = 1
ca2 = 0.0
cs2 = 2.0
approx = qng.ggm_prob_wait_whitt(lam, mu, m, ca2, cs2)
whichpi = qng._ggm_prob_wait_whitt_whichpi(rho, m, ca2, cs2)
print("exact={0}, whitt={1}, mywhitt={2:6.3f}, simio={3}, whichpi={4}".format(exact, whitt, approx, simio, whichpi))