__author__ = 'misken'

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
    c:
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

    Much faster than direct computation via scipy.stats.poisson.pmf(c, load) / scipy.stats.poisson.cdf(c, load)

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
    c:
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
    int
        number of servers

    """

    c = math.ceil(load)
    ec = erlangc(load, c)
    if (ec <= prob):
        return c
    else:
        while (ec > prob):
            c += 1
            ec = erlangc(load, c)

    return c


def mmc_waitq_cdf(arr_rate, svc_rate, c, t):
    return 0

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
    c:
        number of servers

    Returns
    -------
    float
        probability n customers in system (in service plus in queue)

    """


