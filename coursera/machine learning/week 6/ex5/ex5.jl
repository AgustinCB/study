using Base.Test
using Distributions
using MAT
using Optim

sigmoid(z) = 1.0 ./ (1.0 .+ exp.(-z))

#=
Part 1, load the data
=#

file = matopen("ex5data1.mat")

X = read(file, "X")[:]
y = read(file, "y")[:]
Xval = read(file, "Xval")[:]
yval = read(file, "yval")[:]
Xtest = read(file, "Xtest")[:]
ytest = read(file, "ytest")[:]
m = size(X, 1)

#=
Part 2, regularized linear regression
=#

J(X, θ, λ) = 1 / (2 * m) * sum((X * θ .- y) .^ 2) + λ / (2 * m) * sum(θ[2:end] .^ 2)

res = J([ones(m) X], ones(2), 1.0)

@test res - 303.993192 < 1e-6

#=
Part 3, implement gradients
=#

jdev(X, θ, λ, j) = (1/m) * sum((X * θ .- y) .* X[:, j]) + (j == 1 ? 0 : (λ/m * θ[j]))

@test jdev([ones(m) X], ones(2), 1, 1) - (-15.303016) < 1e-6
@test jdev([ones(m) X], ones(2), 1, 1) - 598.250744 < 1e-6

#=
Part 4, train linear regression using lambda 0
=#

function train(X::Array{Float64, 1}, λ::Float64)
    costFunc(θ) = J([ones(m) X], θ, 0.0)
    grad(θ, j) = jdev([ones(m) X], θ, 0.0, j)
    function g!(storage, θ)
        for i in 1:length(θ)
            storage[i] = grad(θ, i)
        end
    end
    res = optimize(costFunc, g!, ones(2))
    return Optim.minimizer(res)
end

θ = train(X, 0.0)
