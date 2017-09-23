using Base.Test
using MAT
using Optim

sigmoid(z) = 1.0 ./ (1.0 .+ exp.(-z))

#=
Part 2a: Vectorized logistic regression
Reuse the code from the previous exercise making sure it works with vectors
=#

J(θ, X, y, m, λ) = (1/m) * sum(-y'*log.(sigmoid(X * θ)) - (1-y)'*log.(1-sigmoid(X*θ))) + λ/(2 * m) * sum(θ[2:end] .^ 2)
jdev(θ, X, y, m, λ, j) = (1/m) * sum((sigmoid(X * θ) - y) .* X[:, j]) + (j == 1 ? 0 : (λ/m * θ[j]))
y = [1; 0; 1; 0; 1]
X = [ones(5) reshape(1:15, 5, 3)./10]
m = size(X, 1)
θ = [-2; -1; 1; 2]

cost = J(θ, X, y, m, 3)
grad = [jdev(θ, X, y, m, 3, 1) jdev(θ, X, y, m, 3, 2) jdev(θ, X, y, m, 3, 3) jdev(θ, X, y, m, 3, 4)]

@test cost - 2.534819 < 1e-6
@test maximum(grad - [0.146561 -0.548558 0.724722 1.398003]) < 1e-6

#=
Part 2b: One vs All
Using the data provided and the cost function implemented, write a one vs all approach.
The function oneVsAll should receive a dataset, the expected output, the number of classes and lambda
and return a group of classifiers, one for every class
=#

function oneVsAll(X::Matrix, y::Array{Float64, 1}, num_classes::Int64, λ::Float64)
    X = [ones(size(X, 1)) X]
    m = size(X, 1)
    n = size(X, 2)

    function getClassifier(class::Int64)
        println("CLASSIFIER ", class)
        out = convert.(UInt8, y .== class)
        costFunc(θ) = J(θ, X, out, m, λ)
        grad(θ, j) = jdev(θ, X, out, m, λ, j)
        function g!(storage, θ)
           for i in 1:length(θ)
               storage[i] = grad(θ, i)
           end
        end
        res = optimize(costFunc, g!, zeros(n, 1), LBFGS())
        return Optim.minimizer(res)[:, 1]
    end

    return [getClassifier(i) for i in 1:num_classes]
end

input_layer_size  = 400
num_labels = 10

file = matopen("ex3data1.mat")

X = read(file, "X")
y = read(file, "y")[:, 1]

classifiers = oneVsAll(X, y, num_labels, 0.1)
println(classifiers)

#=
Part 3: Predict One vs All
Using the previous function, measure the accuracy of the model
=#

function predictOneVsAll(classifiers::Array{Array{Float64, 1}, 1}, X::Matrix)
    X = [ones(size(X, 1)) X]
    compute(x, θ) = sigmoid(x' * θ)
    class(x) = indmax([ compute(x, θ) for θ in classifiers ])
    m = size(X, 1)
    return [ class(X[i, :]) for i in 1:m ]
end

pred = predictOneVsAll(classifiers, X)

println(mean(convert.(Float64, pred .== y)) * 100)
