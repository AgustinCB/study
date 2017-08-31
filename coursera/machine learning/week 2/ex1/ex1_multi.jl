using Plots

#=
Load Data: 
=#

data = readdlm("ex1data2.txt", ',')
cols = size(data, 2)
m = size(data, 1)
x = data[:, 1:(cols-1)]
y = data[:, cols]

function resize(size::Int64, a::Array{Float64, 1})
    return [i for _=1:size, i=a]
end
columns(M::Matrix) = map(x->getindex(M, :, x), 1:size(M)[2])

#=
Normalize features
=#

function featureNormalize(x::Matrix)
    s = size(x)
    μ = map(i -> mean(x[:,i]), 1:s[2])
    σ = map(i -> std(x[:,i]), 1:s[2])
    new_x = map((i,m,s) -> ((i.-m)./s), x, resize(s[1], μ), resize(s[1], σ))
    return [new_x, μ, σ]
end


const features, μ, σ = featureNormalize(x)
X = [ones(m, 1) features]


#=
Compute gradient descent
- Write a function called gradientDescent that accepts an X matrix, the expected value, an initial theta and the maximum number of iterations and returns the theta after applying gradient descent.
- Execute it against zeros(ndims(data, 1)
- Plot the predicted value. Bonus point: Compare it with the actual value.
- Estimate the price of a 1650 sq-ft, 3 br house
=#

function computeCost(X::Matrix, y::Array{Float64, 1}, θ)
    H = X*θ
    return sum((H - y).^2) / (2 * length(y))
end
function gradientDescent(X::Matrix, y::Array{Float64, 1}, θ::Matrix, α::Float64, max::Int64)
    counter = 0
    m = length(y)
    features = columns(X)
    θ1 = θ.-1
    history = []
    while counter < max && θ != θ1
        θ1 = θ
        H = X*θ
        diff = H - y
        factor = α / m
        θ = map((feature, t) -> (t - factor * sum(feature.*diff)), features, θ)
        history = push!(history, computeCost(X, y, θ))
        counter += 1
    end
    return [θ, history]
end

max = 500000000
α = 0.01

println("Theta found expeting")
const θ, history = gradientDescent(X, y, zeros(cols, 1), α, max)
println(θ)

println("Difference of estimation and known:")
println(computeCost(X, y, θ))

plot(history)
gui()

println("Estimation for a 1650 sq-ft, 3 br house")
println([1.0 1650.0 3.0] * θ)

#=
Normal equation
=#

println("Now, let's use normal equation")
θ1 = pinv(transpose(X) * X) * transpose(X) * y
println("Theta: ", θ1)

println("Estimation for a 1650 sq-ft, 3 br house")
println([1.0 1650.0 3.0] * θ1)
