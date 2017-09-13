#=
This exercise is on some matrix operations that arise in certain problems, including when dealing with linear stochastic difference equations

If you aren’t familiar with all the terminology don’t be concerned — you can skim read the background discussion and focus purely on the matrix exercise

With that said, consider the stochastic difference equation

X[t+1] = AX[t] + b + \Sigma W[t+1] (1)

Where:

- X[t], b and X[t+1} are nx1
- A is nxn
- \Sigma is nxk
- W[T] is kx1 and {W[t]} is iid with zero mean and variance covariance matrix equals to the identity matrix.

Let S[t] denote the nxn variance-covariance matrix of X[t].

Using the rules for computing variances in matrix experssions, it can be shown from (1) that {S[t]} obeys:

    S[t+1] = AS[t]A' + \Sigma \Sigma' (2)

It can be shown that, provided all eigenvalues of A lie with the unit circle, the sequence {S[t]} converges to a unique limit S.

This is the unconditional variance or asymptotic variance of the stochastic difference equation.

As an exercise, try writing a single function that solves for the limit S by iterating on (2) given A and \Sigma.

To test your solution, observe that the limit S is a solution to the matrix equation:

S = ASA' + Q where Q := \Sigma \Sigma' (3)

This kind of equation is knonw as a discrete time Lyapunov equation.

The QuantEcon package provides a function called solve_discrete_lyapunov that implements a fast doubling algorithm to solve this equation.

Test your iterative method against solve_discrete_lyapunov using matrices:

A = [[0.8 -0.2] [-0.1, 0.7]]
\Sigma = [[0.5 0.4] [0.4 0.6]]
=#

using Base.Test
using QuantEcon

function my_solve_discrete_lyapunov(
                                    A::Matrix,
                                    Σ::Matrix,
                                    tolerance::Float64 = 1e-7,
                                    max_iter::Int64 = 5000)
    Q = Σ * Σ'
    calc_s(S) = A * S * A' + Q
    S = calc_s(Q)
    error = tolerance + 1
    iter = 0
    while error >= tolerance && iter < max_iter
        new_S = calc_s(S)
        error = norm(S - new_S)
        S = new_S
    end
    return S
end

A = [0.8 -0.2; -0.1 0.7]
Σ = [0.5 0.4; 0.4 0.6]

expected = solve_discrete_lyapunov(A, Σ * Σ')
received = my_solve_discrete_lyapunov(A, Σ)

@test norm(expected - received) < 1e-6
