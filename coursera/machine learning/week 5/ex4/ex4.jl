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

Θ = [read(file, "Theta2")[:]; read(file,"Theta1")[:]]

file = matopen("ex4data1.mat")

X = read(file, "X")
y = read(file, "y")[:]

#=
Second create the nnCostFunction

It will compute feedforward without regularization
=#

function nn_output(nn_params::Array{Float64, 1}, input_layer_size::Int64, hidden_layer_size::Int64, num_labels::Int64, X::Array{Float64, 2}, y::Array{Float64, 1}, lambda::Float64)
    m = size(X, 1)
    Θ1 = reshape(Θ[1:(hidden_layer_size * (input_layer_size + 1))], hidden_layer_size, (input_layer_size + 1))    
    Θ2 = reshape(Θ[1+(hidden_layer_size * (input_layer_size + 1)):end], num_labels, (hidden_layer_size + 1))    
    a1 = [ones(m) X]
    z2 = a1 * Θ1'
    a2 = [ones(m) sigmoid(z2)]
    z3 = a2 * Θ2'
    output = sigmoid(z3)
    [indmax(output[i, :]) for i in 1:sixe(m)]
end

function nnCostFunction(nn_params::Array{Float64, 1}, input_layer_size::Int64, hidden_layer_size::Int64, num_labels::Int64, X::Array{Float64, 2}, y::Array{Float64, 1}, lambda::Float64)
    m = size(X, 1)
    h = nn_output(Θ, input_layer_size, hidden_layer_size, num_labels, X, y, lambda)
    (-1/m) * sum(h)
end

J = nnCostFunction(Θ, input_layer_size, hidden_layer_size, num_labels, X, y, lambda)

@test abs(J - 0.287629) < 1e-6
