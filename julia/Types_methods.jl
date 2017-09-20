using Distributions
using Plots

#=
Exercise 1:
Considere the AR(1) processes:

X[t+1] = a X[t] + b + \sigma * \phi[t+1] (1)

- Write a function with the signature `simulate(m::AR1, n::Integer, x0::Real)` that takes as arguments an instance AR1, an integer and a real number and returns an array containing a time series of length n generated according to (1) where:
    - The primitives of the AR1 process are as specified in m.
    - The inital condition X0 is set equal to `x0`.
=#

struct AR1{T <: Real}
    a::T
    b::T
    σ::T
    ϕ::Distribution
end

function simulate(m::AR1, n::Integer, x0::Real)
    X = Array{Float64}(n)
    X[1] = x0
    for t in 1:(n-1)
        X[t+1] = m.a * X[t] + m.b + m.σ * rand(m.ϕ)
    end
    return X
end

m = AR1(0.9, 1.0, 1.0, Beta(5, 5))
X = simulate(m, 100, 0.0)
plot(X, legend=:none)
gui()
