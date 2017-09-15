using Base.Test
using DataFrames
using Optim
using Plots
using StatPlots

function plotDecisionBoundary(θ::Array{Float64, 1}, X::Matrix, y::DataArrays.DataArray{Float64, 1}, labels::Array{String, 1}, atext::Array{String, 1})
    if size(X, 2) <= 3
        # Get 2 points to define a line
        plot_x = [minimum(X[:, 2])-2; maximum(X[:, 2])+2]

        # Calculate the decision boundary line
        plot_y = (-1/θ[3]).*(θ[2].*plot_x + θ[1])

        x_annotation = (plot_x[2]-plot_x[1])/2 + plot_x[1]
        max_y = maximum(plot_y)
        min_y = minimum(plot_y)
        annotations = [
            (x_annotation, (max_y - min_y) * 3/4 + min_y, atext[1]),
            (x_annotation, (max_y - min_y) * 1/4 + min_y, atext[2])
        ]
        plot(plot_x, plot_y, annotations = annotations, xlabel=labels[1], ylabel=labels[2])
        gui()
    end
end

#=
Plot Data: 
- Load data in ex2data1.txt
- Plot the revenue in the y axis and the size in the x.
- Set the labels to Exam 1 and Exam 2
=#

data = readdlm("ex2data1.txt", ',')
df = DataFrame(exam1 = data[:, 1], exam2 = data[:, 2], passed = data[:, 3])
scatter(df, :exam1, :exam2, group = :passed, xlabel = "Exam 1", ylabel = "Exam 2", label = ["Not passed" "passed"], markershape=[:xcross :cross])
gui()

#=
Cost function:
- implement the cost function for logistic regression
- Implement the gradient function for logistic regression
- Test it with [-24; 0.2; 0.2] (expects 0.218 approx)
=#
n = size(data)[2]
m = length(df[:exam1])
X = [ones(m) df[1] df[2]]
y = df[n]
θ = zeros(n)
sigmoid(z) = 1.0 ./ (1.0 .+ exp.(-z))
J(θ) = (1/m) * sum(-y'*log.(sigmoid(X * θ)) - (1-y)'*log.(1-sigmoid(X*θ)))
jdev(θ, j) = (1/m) * sum((sigmoid(X * θ) - y) .* X[:, j])

@test J(θ) - 0.693 < 1e-3
@test jdev(θ, 1) - (-0.1) == 0
@test jdev(θ, 2) - (-12.0092) < 1e-4
@test jdev(θ, 3) - (-11.2628) < 1e-4

test_theta = [-24; 0.2; 0.2]

@test J(test_theta) - 0.218 < 1e-3
@test jdev(test_theta, 1) - 0.043 < 1e-3
@test jdev(test_theta, 2) - 2.566 < 1e-3
@test jdev(test_theta, 3) - 2.647 < 1e-3

#=
Optimization
=#
function g!(storage, θ)
    storage[1] = jdev(θ, 1)
    storage[2] = jdev(θ, 2)
    storage[3] = jdev(θ, 3)
end

res = optimize(J, g!, θ, LBFGS())
θ = Optim.minimizer(res)
@test norm(θ - [-25.161; 0.206; 0.201]) < 1e-3
@test Optim.minimum(res) - 0.203 < 1e-3

plotDecisionBoundary(θ, X, y, ["Exam 1" "Exam 2"], ["Admitted" "Not Admitted"])

#=
Prediction
- Predict probability of admission for an student with 45 and 85
- Measure the accuracy of the model
=#

prob = sigmoid([1 45 85] * θ)[1]
p = [i > 0.5 ? 1 : 0 for i in sigmoid(X * θ)]

@test abs(prob - 0.775) < 2e-3
@test mean(convert.(Float64, p .== y)) * 100 == 89

#=
Exercise 2: Regularization

In this case we will use polynomial features created by mapFeature
=#

#=
Plot Data: 
- Load data in ex2data2.txt
- Plot the revenue in the y axis and the size in the x.
- Set the labels to Microchip 1 and Microchip 2
=#

data = readdlm("ex2data1.txt", ',')
df = DataFrame(microchip1 = data[:, 1], microchip2 = data[:, 2], passed = data[:, 3])
scatter(df, :microchip1, :microchip2, group = :passed, xlabel = "Microchip 1", ylabel = "Microchip 2", label = ["y = 0 " " y = 1 "], markershape=[:xcross :cross])
gui()

function mapFeature(X::Matrix, m::Int64)
    out = Matrix(m, 0)
    degree = 6
    for i in 1:6
        for j in 0:j
            out = hcat((X[:,1].^(i-j)).*(X[:,2].^j), out)
        end
    end
    return hcat(ones(m, 1), out)
end

n = size(data)[2]
m = length(df[:exam1])
X = mapFeature([df[1] df[2]], m)
y = df[n]
θ = zeros(n)
λ = 1

```
J(\theta) = 1/2m sum(1, m, (h(x) - y)^2 + \lambda sum(1,n,\theta^2))
```
J(θ) = (1/m) * sum(-y'*log.(sigmoid(X * θ)) - (1-y)'*log.(1-sigmoid(X*θ))) + λ sum(θ .^ 2)
jdev(θ, j) = (1/m) * sum((sigmoid(X * θ) - y) .* X[:, j])
