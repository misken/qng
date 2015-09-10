__author__ = 'misken'

import numpy as np
import scipy.stats as stats
import math


def poissoninv(prob, mean):
    """
    Return the cumulative inverse of the Poisson distribution.

    Useful for capacity planning approximations. Uses normal
    approximation to the Poisson distribution for mean > 50.

    Parameters
    ----------
    mean : float
        mean of the Poisson distribution
    prob :
        percentile desired

    Returns
    -------
    int
        minimum value, c,  such that P(X>c) <= prob

    """

    return stats.poisson.ppf(prob, mean)


def erlangb_direct(load, c):
    """
    Return the the probability of loss in M/G/c/c system.

    Parameters
    ----------
    load : float
        average arrival rate * average service time (units are erlangs)
    c : int
        number of servers


    Returns
    -------
    float
        probability arrival finds system full

    """

    p = stats.poisson.pmf(c, load) / stats.poisson.cdf(c, load)

    return p


def erlangb(load, c):
    """
    Return the the probability of loss in M/G/c/c system using recursive approach.

    Much faster than direct computation via
    scipy.stats.poisson.pmf(c, load) / scipy.stats.poisson.cdf(c, load)

    Parameters
    ----------
    load : float
        average arrival rate * average service time (units are erlangs)
    c : int
        number of servers

    Returns
    -------
    float
        probability arrival finds system full

    """

    invb = 1.0
    for j in range(1, c + 1):
        invb = 1.0 + invb * j / load

    b = 1.0 / invb
    return b


def erlangc(load, c):
    """
    Return the the probability of delay in M/M/c/inf system using recursive Erlang B approach.


    Parameters
    ----------
    load : float
        average arrival rate * average service time (units are erlangs)
    c : int
        number of servers

    Returns
    -------
    float
        probability all servers busy

    """

    rho = load / float(c)
    eb = erlangb(load, c)
    ec = 1.0 / (rho + (1 - rho) * (1.0 / eb))

    return ec


def erlangcinv(prob, load):
    """
    Return the number of servers such that probability of delay in M/M/c/inf system is
    less than specified probability


    Parameters
    ----------
    prob : float
        threshold delay probability
    load : float
        average arrival rate * average service time (units are erlangs)

    Returns
    -------
    c : int
        number of servers

    """

    c = math.ceil(load)
    ec = erlangc(load, c)
    if ec <= prob:
        return c
    else:
        while ec > prob:
            c += 1
            ec = erlangc(load, c)

    return c


def mmc_prob_n(n, arr_rate, svc_rate, c):
    """
    Return the the probability of n customers in system in M/M/c/inf queue.

    Uses recursive approach from See Tijms, H.C. (1994), "Stochastic Models: An Algorithmic Approach",
    John Wiley and Sons, Chichester (Section 4.5.1, p287)


    Parameters
    ----------
    n : int
        number of customers for which probability is desired
    arr_rate : float
        average arrival rate to queueing system
    svc_rate : float
        average service rate (each server). 1/svc_rate is mean service time.
    c : int
        number of servers

    Returns
    -------
    float
        probability n customers in system (in service plus in queue)

    """

    rho = arr_rate / (svc_rate * float(c))

    # Step 0: Initialization - p[0] is initialized to one via creation method

    pbar = np.ones(max(n + 1, c))

    # Step 1: compute pbar

    for j in range(1, c):
        pbar[j] = arr_rate * pbar[j - 1] / (j * svc_rate)

    # Step 2: compute normalizing constant and normalize pbar

    gamma = sum(pbar) + rho * pbar[c - 1] / (1 - rho)
    p = pbar / gamma

    # Step 3: compute probs beyond c - 1

    for j in range(c, n + 1):
        p[j] = p[c - 1] * (rho ** (j - c + 1))

    return p[n]


def mmc_mean_qsize(arr_rate, svc_rate, c):
    """
    Return the the mean queue size in M/M/c/inf queue.

    Parameters
    ----------
    arr_rate : float
        average arrival rate to queueing system
    svc_rate : float
        average service rate (each server). 1/svc_rate is mean service time.
    c : int
        number of servers

    Returns
    -------
    float
        mean number of customers in queue

    """

    rho = arr_rate / (svc_rate * float(c))

    mean_qsize = (rho ** 2 / (1 - rho) ** 2) * mmc_prob_n(c - 1, arr_rate, svc_rate, c)

    return mean_qsize


def mmc_mean_qwait(arr_rate, svc_rate, c):
    """
    Return the the mean wait in queue time in M/M/c/inf queue.

    Uses mmc_mean_qsize along with Little's Law.

    Parameters
    ----------
    arr_rate : float
        average arrival rate to queueing system
    svc_rate : float
        average service rate (each server). 1/svc_rate is mean service time.
    c : int
        number of servers

    Returns
    -------
    float
        mean wait time in queue

    """

    return mmc_mean_qsize(arr_rate, svc_rate, c) / arr_rate


def mmc_mean_systime(arr_rate, svc_rate, c):
    """
    Return the mean time in system (wait in queue + service time) in M/M/c/inf queue.

    Uses mmc_mean_qsize along with Little's Law (via mmc_mean_qwait) and relationship between W and Wq..

    Parameters
    ----------
    arr_rate : float
        average arrival rate to queueing system
    svc_rate : float
        average service rate (each server). 1/svc_rate is mean service time.
    c : int
        number of servers

    Returns
    -------
    float
        mean wait time in queue

    """

    return mmc_mean_qwait(arr_rate, svc_rate, c) + 1 / svc_rate


def mmc_prob_wait_normalapprox(arr_rate, svc_rate, c):
    """
    Return the approximate probability of waiting (i.e. erlang C) in M/M/c/inf queue using a normal approximation.

    Uses normal approximation approach by Kolesar and Green, "Insights
    on Service System Design from a Normal Approximation to Erlang's
    Delay Formula", POM, V7, No3, Fall 1998, pp282-293

    Parameters
    ----------
    arr_rate : float
        average arrival rate to queueing system
    svc_rate : float
        average service rate (each server). 1/svc_rate is mean service time.
    c : int
        number of servers

    Returns
    -------
    float
        approximate probability of delay in queue

    """

    load = arr_rate / svc_rate

    prob_wait = 1.0 - stats.norm.cdf(c - load - 0.5) / math.sqrt(load)

    return prob_wait


def mmc_waitq_cdf(arr_rate, svc_rate, c, t):
    """
    Return P(Wq < t) in M/M/c/inf queue.


    Parameters
    ----------
    arr_rate : float
        average arrival rate to queueing system
    svc_rate : float
        average service rate (each server). 1/svc_rate is mean service time.
    c : int
        number of servers
    t : float
        wait time of interest

    Returns
    -------
    float
        probability wait time in queue is < t

    """

    rho = arr_rate / (svc_rate * float(c))

    term1 = rho / (1 - rho)
    term2 = mmc_prob_n(c - 1, arr_rate, svc_rate, c)
    term3 = -c * svc_rate * (1 - rho) * t

    prob_wq_lt_t = 1.0 + term1 * term2 * math.exp(term3)

    return prob_wq_lt_t


def mdc_mean_qwait_cosmetatos(arr_rate, svc_rate, c):
    """
    Return the approximate mean queue wait in M/D/c/inf queue using Cosmetatos approximation.

    See Cosmetatos, George P. "Approximate explicit formulae for the average queueing time in the processes (M/D/r)
    and (D/M/r)." Infor 13.3 (1975): 328-331.

    Parameters
    ----------
    arr_rate : float
        average arrival rate to queueing system
    svc_rate : float
        average service rate (each server). 1/svc_rate is mean service time.
    c : int
        number of servers

    Returns
    -------
    float
        mean number of customers in queue

    """

    rho = arr_rate / (svc_rate * float(c))

    term1 = 0.5
    term2 = (c - 1) * (math.sqrt(4 + 5 * c) - 2) / (16 * c)
    term3 = (1 - rho) / rho
    term4 = mmc_mean_qwait(arr_rate, svc_rate, c)

    mean_qwait = term1 * (1 + term2 * term3) * term4

    return mean_qwait


def mdc_mean_qsize_cosmetatos(arr_rate, svc_rate, c):
    """
    Return the approximate mean queue size in M/D/c/inf queue using Cosmetatos approximation.

    See Cosmetatos, George P. "Approximate explicit formulae for the average queueing time in the processes (M/D/r)
    and (D/M/r)." Infor 13.3 (1975): 328-331.

    Parameters
    ----------
    arr_rate : float
        average arrival rate to queueing system
    svc_rate : float
        average service rate (each server). 1/svc_rate is mean service time.
    c : int
        number of servers

    Returns
    -------
    float
        mean wait time in queue

    """

    mean_qwait = mdc_mean_qwait_cosmetatos(arr_rate, svc_rate, c)
    mean_qsize = mean_qwait * arr_rate

    return mean_qsize


def mgc_mean_qwait_kimura(arr_rate, svc_rate, c, cv2_svc_time):
    """
    Return the approximate mean queue wait in M/G/c/inf queue using Kimura approximation.

    See Kimura, Toshikazu. "Approximations for multi-server queues: system interpolations."
    Queueing Systems 17.3-4 (1994): 347-382.

    It's based on interpolation between an M/D/c and a M/M/c queueing system.

    Parameters
    ----------
    arr_rate : float
        average arrival rate to queueing system
    svc_rate : float
        average service rate (each server). 1/svc_rate is mean service time.
    c : int
        number of servers
    cv2_svc_time : float
        squared coefficient of variation for service time distribution

    Returns
    -------
    float
        mean number of customers in queue

    """

    term1 = 1.0 + cv2_svc_time
    term2 = 2.0 * cv2_svc_time / mmc_mean_qwait(arr_rate, svc_rate, c)
    term3 = (1.0 - cv2_svc_time) / mdc_mean_qwait_cosmetatos(arr_rate, svc_rate, c)

    mean_qwait = term1 / (term2 + term3)

    return mean_qwait


def mgc_mean_qsize_kimura(arr_rate, svc_rate, c, cv2_svc_time):
    """
    Return the approximate mean queue size in M/G/c/inf queue using Kimura approximation.

    See Kimura, Toshikazu. "Approximations for multi-server queues: system interpolations."
    Queueing Systems 17.3-4 (1994): 347-382.

    It's based on interpolation between an M/D/c and a M/M/c queueing system.

    Parameters
    ----------
    arr_rate : float
        average arrival rate to queueing system
    svc_rate : float
        average service rate (each server). 1/svc_rate is mean service time.
    c : int
        number of servers
    cv2_svc_time : float
        squared coefficient of variation for service time distribution

    Returns
    -------
    float
        mean number of customers in queue

    """

    mean_qwait = mgc_mean_qwait_kimura(arr_rate, svc_rate, c, cv2_svc_time)
    mean_qsize = mean_qwait * arr_rate

    return mean_qsize


def mgc_mean_qwait_bjorklund(arr_rate, svc_rate, c, cv2_svc_time):
    """
    Return the approximate mean queue wait in M/G/c/inf queue using Bjorklund and Elldin approximation.

    See Kimura, Toshikazu. "Approximations for multi-server queues: system interpolations."
    Queueing Systems 17.3-4 (1994): 347-382.

    It's based on interpolation between an M/D/c and a M/M/c queueing system.

    Parameters
    ----------
    arr_rate : float
        average arrival rate to queueing system
    svc_rate : float
        average service rate (each server). 1/svc_rate is mean service time.
    c : int
        number of servers
    cv2_svc_time : float
        squared coefficient of variation for service time distribution

    Returns
    -------
    float
        mean number of customers in queue

    """

    term1 = cv2_svc_time * mmc_mean_qwait(arr_rate, svc_rate, c)
    term2 = (1.0 - cv2_svc_time) * mdc_mean_qwait_cosmetatos(arr_rate, svc_rate, c)

    mean_qwait = term1 + term2

    return mean_qwait


def mgc_mean_qsize_bjorklund(arr_rate, svc_rate, c, cv2_svc_time):
    """
    Return the approximate mean queue wait in M/G/c/inf queue using Bjorklund and Elldin approximation.

    See Kimura, Toshikazu. "Approximations for multi-server queues: system interpolations."
    Queueing Systems 17.3-4 (1994): 347-382.

    It's based on interpolation between an M/D/c and a M/M/c queueing system.

    Parameters
    ----------
    arr_rate : float
        average arrival rate to queueing system
    svc_rate : float
        average service rate (each server). 1/svc_rate is mean service time.
    c : int
        number of servers
    cv2_svc_time : float
        squared coefficient of variation for service time distribution

    Returns
    -------
    float
        mean number of customers in queue

    """

    mean_qwait = mgc_mean_qwait_bjorklund(arr_rate, svc_rate, c, cv2_svc_time)
    mean_qsize = mean_qwait * arr_rate

    return mean_qsize


def mgc_qcondwait_pctile_2moment(prob, arr_rate, svc_rate, c, cv2_svc_time):
    """
    Return an approximate conditional queue wait percentile in M/G/c/inf system.

    The approximation is based on a first order approximation using the M/M/c delay percentile.
    See Tijms, H.C. (1994), "Stochastic Models: An Algorithmic Approach", John Wiley and Sons, Chichester
    Chapter 4, p299-300

    The percentile is conditional on Wq>0 (i.e. on event customer waits)

    This 1st order approximation is OK for 0<=CVSquared<=2 and dblProb>1-Prob(Delay)
    Note that for Prob(Delay) we use MMC as approximation for same quantity in MGC.
    Justification in Tijms (p296)


    Parameters
    ----------
    arr_rate : float
        average arrival rate to queueing system
    svc_rate : float
        average service rate (each server). 1/svc_rate is mean service time.
    c : int
        number of servers
    cv2_svc_time : float
        squared coefficient of variation for service time distribution

    Returns
    -------
    float
        percentile of conditional customer wait time

    """

    rho = load / float(c)
    eb = erlangb(load, c)
    ec = 1.0 / (rho + (1 - rho) * (1.0 / eb))

    return ec