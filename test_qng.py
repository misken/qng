__author__ = 'misken'

import qng
from nose.tools import assert_equals
from nose.tools import assert_almost_equal

def test_qng():

    # Example 2.1 on pp74 of Gross and Harris, Fundamentals of Queueing Theory, 2ed
    barber_arr = 5
    barber_svc = 6
    barber_c = 1

    assert_almost_equal(qng.mmc_mean_qsize(barber_arr, barber_svc, barber_c), 4.0 + 1/6.0, 7)
    assert_almost_equal(qng.mmc_mean_syssize(barber_arr, barber_svc, barber_c), 5.0, 7)
    assert_almost_equal(qng.mm1_qwait_cdf(1.0,barber_arr, barber_svc), 1.0 - 0.3065662, 7)
    assert_almost_equal(qng.mmc_qwait_cdf(1.0,barber_arr, barber_svc, barber_c), 1.0 - 0.3065662, 7)


    # Example 2.2 on pp91-93 of Gross and Harris, Fundamentals of Queueing Theory, 2ed
    assert_almost_equal(qng.mmc_prob_n(0, 6, 3, 3), 1/9.0, 7)
    assert_almost_equal(qng.mmc_mean_qsize(6, 3, 3), 8/9.0, 7)
    assert_almost_equal(qng.mmc_mean_qwait(6, 3, 3), (8/9.0)/6.0, 7)
    assert_almost_equal(1.0 - qng.erlangc(6.0/3.0, 3), 5/9.0, 7)
    assert_almost_equal(qng.mmc_mean_systime(6, 3, 3), 13/27.0, 7)



def test_cosmetatos():
    # The following test the Cosmetatos approximation for Lq and Wq.
    # Test values are from Table 1 on p601 of:
    # Kimura, Toshikazu. Refining Cosmetatos' approximation for the mean waiting
    # time in the M/D/s queue. Journal of the Operational Research Society (1991): 595-603.
    mu = 1.0
    c = 2
    assert_almost_equal(qng.mdc_mean_qwait_cosmetatos(0.9 * mu * c, mu, c), 2.1445, 4)
    assert_almost_equal(qng.mdc_mean_qwait_cosmetatos(0.7 * mu * c, mu, c), 0.4916, 4)
    assert_almost_equal(qng.mdc_mean_qwait_cosmetatos(0.5 * mu * c, mu, c), 0.1757, 4)
    assert_almost_equal(qng.mdc_mean_qwait_cosmetatos(0.3 * mu * c, mu, c), 0.0557, 4)
    assert_almost_equal(qng.mdc_mean_qwait_cosmetatos(0.1 * mu * c, mu, c), 0.0075, 4)

    c = 4
    assert_almost_equal(qng.mdc_mean_qwait_cosmetatos(0.9 * mu * c, mu, c), 1.0000, 3)
    assert_almost_equal(qng.mdc_mean_qwait_cosmetatos(0.7 * mu * c, mu, c), 0.1890, 4)
    assert_almost_equal(qng.mdc_mean_qwait_cosmetatos(0.5 * mu * c, mu, c), 0.0494, 4)
    assert_almost_equal(qng.mdc_mean_qwait_cosmetatos(0.3 * mu * c, mu, c), 0.0087, 4)
    assert_almost_equal(qng.mdc_mean_qwait_cosmetatos(0.1 * mu * c, mu, c), 0.000245, 6)

    mu = 0.01
    c = 25
    assert_almost_equal(qng.mdc_mean_qwait_cosmetatos(0.9 * mu * c, mu, c), 10.7922, 4)
    assert_almost_equal(qng.mdc_mean_qwait_cosmetatos(0.8 * mu * c, mu, c), 2.3845, 4)
    assert_almost_equal(qng.mdc_mean_qwait_cosmetatos(0.7 * mu * c, mu, c), 0.5258, 4)
    assert_almost_equal(qng.mdc_mean_qwait_cosmetatos(0.6 * mu * c, mu, c), 0.0853, 3)

    mu = 0.01
    c = 50
    assert_almost_equal(qng.mdc_mean_qwait_cosmetatos(0.9 * mu * c, mu, c), 3.9838, 4)
    assert_almost_equal(qng.mdc_mean_qwait_cosmetatos(0.8 * mu * c, mu, c), 0.5275, 4)
    assert_almost_equal(qng.mdc_mean_qwait_cosmetatos(0.7 * mu * c, mu, c), 0.0502, 4)
    assert_almost_equal(qng.mdc_mean_qwait_cosmetatos(0.6 * mu * c, mu, c), 0.0021, 3)

def test_kimura_bjorklund():
    # The following test the Kimura approximation for E[Wq] in M/G/c
    # Test values are from Table 1 on p357 of:
    # Kimura, Toshikazu. "Approximations for multi-server queues: system interpolations."
    # Queueing Systems 17.3-4 (1994): 347-382.

   # Kimura
    mu = 1.0
    c = 5
    cv2 = 4.0
    assert_almost_equal(qng.mgc_mean_qsize_kimura(0.9 * c, mu, c, cv2), 16.26, 1)
    assert_almost_equal(qng.mgc_mean_qsize_kimura(0.7 * c, mu, c, cv2), 1.82, 1)
    assert_almost_equal(qng.mgc_mean_qsize_kimura(0.5 * c, mu, c, cv2), 0.23, 2)
    assert_almost_equal(qng.mgc_mean_qsize_kimura(0.3 * c, mu, c, cv2), 0.01, 2)

    cv2 = 1.5
    assert_almost_equal(qng.mgc_mean_qsize_kimura(0.9 * c, mu, c, cv2), 8.50, 1)
    assert_almost_equal(qng.mgc_mean_qsize_kimura(0.7 * c, mu, c, cv2), 1.06, 1)
    assert_almost_equal(qng.mgc_mean_qsize_kimura(0.5 * c, mu, c, cv2), 0.15, 2)
    assert_almost_equal(qng.mgc_mean_qsize_kimura(0.3 * c, mu, c, cv2), 0.01, 2)

    cv2 = 0.5
    assert_almost_equal(qng.mgc_mean_qsize_kimura(0.9 * c, mu, c, cv2), 5.19, 2)
    assert_almost_equal(qng.mgc_mean_qsize_kimura(0.7 * c, mu, c, cv2), 0.68, 2)
    assert_almost_equal(qng.mgc_mean_qsize_kimura(0.5 * c, mu, c, cv2), 0.10, 1)
    assert_almost_equal(qng.mgc_mean_qsize_kimura(0.3 * c, mu, c, cv2), 0.01, 2)

    # Bjorklund and Eddlin
    mu = 1.0
    c = 5
    cv2 = 4.0
    assert_almost_equal(qng.mgc_mean_qsize_bjorklund(0.9 * c, mu, c, cv2), 16.96, 1)
    assert_almost_equal(qng.mgc_mean_qsize_bjorklund(0.7 * c, mu, c, cv2), 2.10, 1)
    assert_almost_equal(qng.mgc_mean_qsize_bjorklund(0.5 * c, mu, c, cv2), 0.29, 2)
    assert_almost_equal(qng.mgc_mean_qsize_bjorklund(0.3 * c, mu, c, cv2), 0.02, 2)

    cv2 = 1.5
    assert_almost_equal(qng.mgc_mean_qsize_bjorklund(0.9 * c, mu, c, cv2), 8.55, 1)
    assert_almost_equal(qng.mgc_mean_qsize_bjorklund(0.7 * c, mu, c, cv2), 1.09, 1)
    assert_almost_equal(qng.mgc_mean_qsize_bjorklund(0.5 * c, mu, c, cv2), 0.16, 2)
    assert_almost_equal(qng.mgc_mean_qsize_bjorklund(0.3 * c, mu, c, cv2), 0.01, 2)

    cv2 = 0.5
    assert_almost_equal(qng.mgc_mean_qsize_bjorklund(0.9 * c, mu, c, cv2), 5.18, 2)
    assert_almost_equal(qng.mgc_mean_qsize_bjorklund(0.7 * c, mu, c, cv2), 0.68, 2)
    assert_almost_equal(qng.mgc_mean_qsize_bjorklund(0.5 * c, mu, c, cv2), 0.10, 1)
    assert_almost_equal(qng.mgc_mean_qsize_bjorklund(0.3 * c, mu, c, cv2), 0.01, 2)


def test_whitt_prob_wait():

    # See Whitt, Ward. "Approximations for the GI/G/m queue"
    # Production and Operations Management 2, 2 (Spring 1993): 114-161.


    # M/M/4
    exact = 0.79
    whitt = 0.79
    rho = 0.9
    m = 4
    lam = rho * m
    mu = 1
    ca2 = 1.0
    cs2 = 1.0
    approx = qng.ggm_prob_wait_whitt(lam, mu, m, ca2, cs2)
    assert abs(whitt-approx) < 0.009

    # D/M/4
    exact = 0.67
    whitt = 0.65
    simio = 0.664
    rho = 0.9
    m = 4
    lam = rho * m
    mu = 1
    ca2 = 0.0
    cs2 = 1.0
    approx = qng.ggm_prob_wait_whitt(lam, mu, m, ca2, cs2)
    assert abs(whitt-approx) < 0.009

    # H2/M/4 (ca2=2.25)
    exact = 0.85
    whitt = 0.85
    simio = 0.845
    rho = 0.9
    m = 4
    lam = rho * m
    mu = 1
    ca2 = 2.25
    cs2 = 1.0
    approx = qng.ggm_prob_wait_whitt(lam, mu, m, ca2, cs2)
    assert abs(whitt-approx) < 0.009

    # E4/M/4
    exact = 0.71
    whitt = 0.70
    simio = 0
    rho = 0.9
    m = 4
    lam = rho * m
    mu = 1
    ca2 = 0.25
    cs2 = 1.0
    approx = qng.ggm_prob_wait_whitt(lam, mu, m, ca2, cs2)
    assert abs(whitt-approx) < 0.009

    # G/M/4
    exact = 0.75
    whitt = 0.75
    simio = 0 # Unknown G
    rho = 0.9
    m = 4
    lam = rho * m
    mu = 1
    ca2 = 0.56
    cs2 = 1.0
    approx = qng.ggm_prob_wait_whitt(lam, mu, m, ca2, cs2)
    assert abs(whitt-approx) < 0.009

    # M/D/4
    exact = 0.78
    whitt = 0.79
    simio = 0
    rho = 0.9
    m = 4
    lam = rho * m
    mu = 1
    ca2 = 1.0
    cs2 = 0.0
    approx = qng.ggm_prob_wait_whitt(lam, mu, m, ca2, cs2)
    assert abs(whitt-approx) < 0.009

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
    assert abs(whitt-approx) < 0.009

    # M/G/4
    exact = 0.79
    whitt = 0.79
    rho = 0.9
    m = 4
    lam = rho * m
    mu = 1
    ca2 = 1.0
    cs2 = 0.75
    approx = qng.ggm_prob_wait_whitt(lam, mu, m, ca2, cs2)
    assert abs(whitt-approx) < 0.009

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
    assert abs(whitt-approx) < 0.009

    # E2/H2/4
    exact = 0.76
    whitt = 0.76
    simio = 0
    rho = 0.9
    m = 4
    lam = rho * m
    mu = 1
    ca2 = 0.5
    cs2 = 2.0
    approx = qng.ggm_prob_wait_whitt(lam, mu, m, ca2, cs2)
    assert abs(whitt-approx) < 0.009

    # D/H2/4 -
    exact = 0.74
    whitt = 0.75
    simio = 0
    rho = 0.9
    m = 4
    lam = rho * m
    mu = 1
    ca2 = 0.0
    cs2 = 9.0
    approx = qng.ggm_prob_wait_whitt(lam, mu, m, ca2, cs2)
    assert abs(whitt-approx) < 0.009

    # H2/H2/4 - Need to simulate
    exact = 0.92
    whitt = 0.86
    simio = 0
    rho = 0.9
    m = 4
    lam = rho * m
    mu = 1
    ca2 = 9.0
    cs2 = 9.0
    approx = qng.ggm_prob_wait_whitt(lam, mu, m, ca2, cs2)
    assert abs(whitt-approx) < 0.009

    # G/E2/4 - -
    exact = 0.64
    whitt = 0.60
    simio = 0
    rho = 0.9
    m = 4
    lam = rho * m
    mu = 1
    ca2 = 0.1
    cs2 = 0.5
    approx = qng.ggm_prob_wait_whitt(lam, mu, m, ca2, cs2)
    assert abs(whitt-approx) < 0.009

    # D/M/20 - Matching whitt but not exact. Need to simulate this one
    exact = 0.39
    whitt = 0.31
    simio = 0
    rho = 0.9
    m = 20
    lam = rho * m
    mu = 1
    ca2 = 0.0
    cs2 = 1.0
    approx = qng.ggm_prob_wait_whitt(lam, mu, m, ca2, cs2)
    assert abs(whitt-approx) < 0.009

    # G/M/4 -
    exact = 0.39
    whitt = 0.38
    simio = 0
    rho = 0.6
    m = 2
    lam = rho * m
    mu = 1
    ca2 = 0.5
    cs2 = 9.0
    approx = qng.ggm_prob_wait_whitt(lam, mu, m, ca2, cs2)
    assert abs(whitt-approx) < 0.009

    # H2/D/4 - pretty close. Simulation not matching exact.
    exact = 0.86
    whitt = 0.86
    simio = 0.83
    rho = 0.9
    m = 4
    lam = rho * m
    mu = 1
    ca2 = 2.25
    cs2 = 0.0
    approx = qng.ggm_prob_wait_whitt(lam, mu, m, ca2, cs2)
    assert abs(whitt-approx) < 0.009


def test_whitt_qcondwait_cdf():

    # See Whitt, Ward. "Approximations for the GI/G/m queue"
    # Production and Operations Management 2, 2 (Spring 1993): 114-161.


    # M/M/4
    exact = 0.79
    whitt = 0.79
    rho = 0.9
    m = 4
    lam = rho * m
    mu = 1
    ca2 = 1.0
    cs2 = 1.0
    approx = qng.ggm_prob_wait_whitt(lam, mu, m, ca2, cs2)
    assert abs(whitt-approx) < 0.009
