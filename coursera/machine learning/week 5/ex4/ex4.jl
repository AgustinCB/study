using Base.Test
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
J = nnCostFunction(Θ, input_layer_size, hidden_layer_size, num_labels, X, y, λ)

@test abs(J - 0.287629) < 1e-6

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

J = nnCostFunction1(Θ, input_layer_size, hidden_layer_size, num_labels, X, y, λ)

@test abs(J - 0.383770) < 1e-6

#=
Forth step, 
