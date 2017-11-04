using Base.Test
using DataFrames
using MAT
using Plots
using PyPlot

pyplot()

file = matopen("ex8data1.mat")
X = read(file, "X")
Xval = read(file, "Xval")
yval = read(file, "yval")[:]

function multivariateGaussian(X::Matrix, μ::Array{Float64, 1}, Σ2::Matrix)
    k = length(μ)
    m = size(X, 1)
    return ((2π)^(-k/2) * det(Σ2)^(-0.5)) .* [exp(-(X[i, :] - μ)'*inv(Σ2)*(X[i, :] - μ)/2) for i in 1:m]
end

#=
Part 1: Estimate Gaussian parameters
=#

function estimateGaussian(X::Matrix)
    m = size(X, 1)
    n = size(X, 2)
    μ = zeros(n)
    σ2 = zeros(n)
    for i in 1:n
        μ[i] = sum(X[:, i]) / m
        σ2[i] = sum((X[:, i] .- μ[i]).^2)/m
    end
    return [μ, σ2]
end

μ, σ2 = estimateGaussian(X)
println("mu: ", μ, " sigma^2: ", σ2)

#=
Part 2: Find a good epsilon given our Gaussian distribution
=#

function selectThreshold(yval::Array{Float64, 1}, pval::Array{Float64, 1})
    bestEpsilon = 0
    bestF1 = 0
    stepsize = (maximum(pval) - minimum(pval)) / 1000
    true_positives(pred::Array{Float64, 1}, yval::Array{Float64, 1}) = length([i for i in pred.+yval if i == 2])
    true_negatives(pred::Array{Float64, 1}, yval::Array{Float64, 1}) = length([i for i in pred.+yval if i == 0])
    false_positives(pred::Array{Float64, 1}, yval::Array{Float64, 1}) = length([i for i in pred.-yval if i == 1])
    false_negatives(pred::Array{Float64, 1}, yval::Array{Float64, 1}) = length([i for i in pred.-yval if i == -1])
    function prec(pred::Array{Float64, 1}, yval::Array{Float64, 1})
        total_positives = true_positives(pred, yval) + false_positives(pred, yval)

        if (total_positives == 0)
            return 0
        else
            return true_positives(pred, yval) / (true_positives(pred, yval) + false_positives(pred, yval))
        end
    end
    function rec(pred::Array{Float64, 1}, yval::Array{Float64, 1})
        tp = true_positives(pred, yval)
        real_positives = tp + false_negatives(pred, yval)
        if tp == real_positives && tp == 0
            return 1
        else
            return tp / real_positives
        end
    end
    function f1(pred::Array{Float64, 1}, yval::Array{Float64, 1})
        r = rec(pred, yval)
        p = prec(pred, yval)

        if r == 0 && p == 0
            return 0
        else
            return 2 * (r * p / (r+p))
        end
    end
    for ϵ in minimum(pval):stepsize:maximum(pval)
        pred = convert.(Float64, pval .< ϵ)
        score = f1(pred, yval)
        if score > bestF1
            bestF1 = score
            bestEpsilon = ϵ
        end
    end
    return [bestEpsilon bestF1]
end

pval = multivariateGaussian(Xval, μ, diagm(σ2))

ϵ, F1 = selectThreshold(yval, pval)

@test ϵ - 8.99e-0 < 1e-7
@test F1 - 0.875 < 1e-7

#=
Part 3: Repeat for a bigger dataset
=#

file = matopen("ex8data2.mat")
X = read(file, "X")
Xval = read(file, "Xval")
yval = read(file, "yval")[:]

μ, σ2 = estimateGaussian(X)

p = multivariateGaussian(X, μ, diagm(σ2))

pval = multivariateGaussian(Xval, μ, diagm(σ2))

ϵ, F1 = selectThreshold(yval, pval)

@test ϵ - 1.38e-18 < 1e-7
@test F1 - 0.615385 < 1e-7
