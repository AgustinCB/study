using Plots

#=
First exercise: Warmup

Create and print an identity matrix
=#

println("5x5 identity matrix")
println(eye(5))

#=
Plot Data: 
- Load data in ex1data1.txt
- Plot the revenue in the y axis and the size in the x.
- Set the labels to population and profit
=#

function plotData(x::Array{Float64, 1}, y::Array{Float64, 1})
    return scatter(x, y, xlabel = "population", label = "profit", markershape = :xcross)
end

data = readdlm("ex1data1.txt", ',')
x = data[:, 1]
y = data[:, 2]
plotData(x, y)
gui()

#=
Comput cost:
- Write a function that receives an X matrix (formed by a column of ones and the x vector set before) with the paremeters, a y vector with the expected value and a \theta vector with the fitting parameters and returns the cost.
- Try it using theta [0; 0] and [-1; 2]
=#
function computeCost(X::Array{Float64, 2}, y::Array{Float64, 1}, θ)
    H = X*θ
    return sum((H - y).^2) / (2 * length(y))
end

m = length(x)
X = [ones(m) x]
θ = zeros(2)

println("Testing with (0, 0), expecting around 32.07")
println(computeCost(X, y, θ))

println("Testing with (-1, 2), expecting around 54.24")
println(computeCost(X, y, [-1; 2]))

#=
Compute gradient descent
- Write a function called gradientDescent that accepts an X matrix, the expected value, an initial theta and the maximum number of iterations and returns the theta after applying gradient descent.
- Execute it against [0 0]
- Plot the predicted value. Bonus point: Compare it with the actual value.
- Predict values for population sizes of 35k and 70k
=#

function gradientDescent(X::Array{Float64, 2}, y::Array{Float64, 1}, θ, α::Float64, max::Int64)
    counter = 0
    m = length(y)
    x = X[:, 2]
    θ1 = θ.-1
    while counter < max && θ != θ1
        θ1 = θ
        H = X*θ
        diff = H - y
        factor = α / m
        θ = [(θ[1] - factor * sum(diff));  (θ[2] - factor * sum(diff.*x))]
        counter += 1
    end
    return θ
end

max = 1500
α = 0.01

println("Theta found expeting (-3.6303, 1.1664)")
θ = gradientDescent(X, y, zeros(2), α, max)
println(θ)

scatter(x, [y X*θ], xlabel="population", label=["profit" "prediction"], markershape = [:cross :circle], color = [:blue :red])
gui()

println("Prediction for population = 35000")
println([1 3.5] * θ * 10000)
println("Prediction for population = 70000")
println([1 7] * θ * 10000)
