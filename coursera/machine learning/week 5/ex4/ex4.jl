using Base.Test
using Distributions
using MAT
using Optim

sigmoid(z) = 1.0 ./ (1.0 .+ exp.(-z))

input_layer_size  = 400
hidden_layer_size = 25
num_labels = 10

#=
First, load weights and unroll them and the input and output data
=#

file = matopen("ex4weights.mat")

Θ = [read(file, "Theta1")[:]; read(file,"Theta2")[:]]

file = matopen("ex4data1.mat")

X = read(file, "X")
y = read(file, "y")[:]

#=
Second create the nnCostFunction

It will compute feedforward without regularization
=#

function nn_output(Θ1::Array{Float64, 2}, Θ2::Array{Float64, 2}, input_layer_size::Int64, hidden_layer_size::Int64, num_labels::Int64, X::Array{Float64, 2}, y::Array{Float64, 1}, λ::Float64)
    m = size(X, 1)
    a1 = [ones(m) X]
    z2 = a1 * Θ1'
    a2 = [ones(m) sigmoid(z2)]
    sigmoid(a2 * Θ2')
end

function nnCostFunction(Θ::Array{Float64, 1}, input_layer_size::Int64, hidden_layer_size::Int64, num_labels::Int64, X::Array{Float64, 2}, y::Array{Float64, 1}, λ::Float64)
    m = size(X, 1)
    Θ1 = reshape(Θ[1:(hidden_layer_size * (input_layer_size + 1))], hidden_layer_size, (input_layer_size + 1))    
    Θ2 = reshape(Θ[1+(hidden_layer_size * (input_layer_size + 1)):end], num_labels, (hidden_layer_size + 1))    
    h = nn_output(Θ1, Θ2, input_layer_size, hidden_layer_size, num_labels, X, y, λ)

    output = zeros(m, num_labels)
    for i in 1:m
        output[i, convert(Int64, y[i])] = 1
    end

    (-1/m) * sum(output .* log.(h) .+ (1 .- output) .* log.(1 .- h))
end

λ = 1.0
res = nnCostFunction(Θ, input_layer_size, hidden_layer_size, num_labels, X, y, λ)

@test abs(res - 0.287629) < 1e-6

#=
Third step, add regularization
=#

function nnCostFunction1(Θ::Array{Float64, 1}, input_layer_size::Int64, hidden_layer_size::Int64, num_labels::Int64, X::Array{Float64, 2}, y::Array{Float64, 1}, λ::Float64)
    m = size(X, 1)
    Θ1 = reshape(Θ[1:(hidden_layer_size * (input_layer_size + 1))], hidden_layer_size, (input_layer_size + 1))    
    Θ2 = reshape(Θ[1+(hidden_layer_size * (input_layer_size + 1)):end], num_labels, (hidden_layer_size + 1))    
    h = nn_output(Θ1, Θ2, input_layer_size, hidden_layer_size, num_labels, X, y, λ)

    output = zeros(m, num_labels)
    for i in 1:m
        output[i, convert(Int64, y[i])] = 1
    end

    (-1/m) * sum(output .* log.(h) .+ (1 .- output) .* log.(1 .- h)) + λ/(2*m) * (sum(Θ1[:, 2:end].^2) + sum(Θ2[:, 2:end].^2))
end

res = nnCostFunction1(Θ, input_layer_size, hidden_layer_size, num_labels, X, y, λ)

@test abs(res - 0.383770) < 1e-6

#=
Forth step, create the derivative of the sigmoid function
=#

g(z) = sigmoid(z) .* (1 .- sigmoid(z))

#=
Fifth step, create a function to randomly initialize the weights
=#

randInitializeWeights(x::Int64, y::Int64) = rand(Uniform(-0.1, 0.1), x, y)

Θ1 = randInitializeWeights(hidden_layer_size, input_layer_size + 1)
Θ2 = randInitializeWeights(num_labels, hidden_layer_size + 1)

prev_Θ = copy(Θ)
Θ = [Θ1[:]; Θ2[:]]

#=
Implement backpropagation and check with numerical descent that the derivatives are similar
=#

function computeNumericalGradient(J::Function, Θ::Array{Float64, 1})
    numgrad = zeros(size(Θ))
    perturb = zeros(size(Θ))
    ϵ = 1e-4
    for p = 1:length(Θ)
        perturb[p] = ϵ
        loss1 = J(Θ - perturb)
        loss2 = J(Θ + perturb)
        numgrad[p] = (loss2 - loss1) / (2*ϵ)
        perturb[p] = 0
    end
    numgrad
end

function nnDerivativeFunction(Θ::Array{Float64, 1}, input_layer_size::Int64, hidden_layer_size::Int64, num_labels::Int64, X::Array{Float64, 2}, y::Array{Float64, 1}, λ::Float64)
    m = size(X, 1)
    Θ1 = reshape(Θ[1:(hidden_layer_size * (input_layer_size + 1))], hidden_layer_size, (input_layer_size + 1))    
    Θ2 = reshape(Θ[1+(hidden_layer_size * (input_layer_size + 1)):end], num_labels, (hidden_layer_size + 1))    

    Θ1_grad = zeros(size(Θ1))
    Θ2_grad = zeros(size(Θ2))

    output = zeros(m, num_labels)
    for i in 1:m
        output[i, convert(Int64, y[i])] = 1
    end

    for t in 1:m
        a1 = prepend!(X[t, :],1)
        z2 = Θ1 * a1
        a2 = [1; sigmoid(z2)]
        z3 = Θ2 * a2
        a3 = sigmoid(z3)
        h = a3

        δ3 = a3 - output[t, :]
        δ2 = (Θ2' * δ3) .* g(a2)
        δ2 = δ2[2:end]

        Θ1_grad = Θ1_grad + δ2 * a1'
        Θ2_grad = Θ2_grad + δ3 * a2'
    end

    Δ1 = 1/m * Θ1_grad + λ/m * [zeros(size(Θ1, 1)) Θ1[:,2:end]]
    Δ2 = 1/m * Θ2_grad + λ/m * [zeros(size(Θ2, 1)) Θ2[:,2:end]]
    [Δ1[:]; Δ2[:]]
end

J(Θ) = nnCostFunction1(Θ, input_layer_size, hidden_layer_size, num_labels, X, y, λ)
derivative = nnDerivativeFunction(Θ, input_layer_size, hidden_layer_size, num_labels, X, y, λ)
computed_derivative = computeNumericalGradient(J, Θ)
n = norm(computed_derivative - derivative) / norm(computed_derivative + derivative)

println(n)
@test n < 0.02

#=
Now run using regularization
=#

λ = 3
J(Θ) = nnCostFunction1(Θ, input_layer_size, hidden_layer_size, num_labels, X, y, λ)
derivative = nnDerivativeFunction(prev_Θ, input_layer_size, hidden_layer_size, num_labels, X, y, λ)
computed_derivative = computeNumericalGradient(J, prev_Θ)
n = norm(computed_derivative - derivative) / norm(computed_derivative + derivative)
res = nnCostFunction1(prev_Θ, input_layer_size, hidden_layer_size, num_labels, X, y, λ)

println(n)
@test n < 0.02
