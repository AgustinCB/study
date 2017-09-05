using Base.Test

#=
Exercise 1: 
- Given two numeric arrays or tuples of equal length, calculate inner product using zip
- using array comprehesion, count the number of even numbers between 0 and 99.
- Using array comprehesion, take pairs = ((2, 5), (4, 2), (9, 8), (12, 10)) and count the number of pairs such that both are even
=#

function ex1part1(v1::Array{Float64, 1}, v2::Array{Float64, 1})
    return sum(map(p->p[1] * p[2], zip(v1,v2)))
end

v1 = randn(4)
v2 = randn(4)
println("Testing exercise 1, part 1")
@test ex1part1(v1, v2) == sum(v1.*v2)

function ex1part2(from::Int64,to::Int64)
    return length([ i for i=from:to if i%2==0 ])
end

println("Testing exercise 1, part 2")
@test ex1part2(0, 99) == 50

function ex1part3(pairs::Array{Tuple{Int64, Int64}, 1})
    return length([p for p=pairs if p[1]%2 == 0 && p[2]%2 == 0 ])
end

println("Testing exercise 1, part 3")
@test ex1part3([(2, 5), (4, 2), (9, 8), (12, 10)]) == 2

#=
Exercise 2
Consider the polynomial

p(x) = a0 + a1 x + a2 x^2 + ... + an x^n = sum(0, n, i -> ai x^i) (1)

Using enumerate in your loop, write a function p such that p(x, coeff) computes the value in (1) given a point x and an array of coefficients coeff.
=#

p(x::Float64, coeff::Array{Float64}) = sum([ x^(i-1) * a for (i, a) in enumerate(coeff) ])

println("Testing exercise 2")
@test p(1.0, [1.0, 1.0]) == 2.0
@test p(2.0, [2.0, 1.0]) == 4.0
@test p(2.0, [2.0, 1.0, 1.0]) == 8.0

#=
Exercise 3
Write a function that takes a string as an argument and returns the number of capital letters in the string
=#

function ex3(s::String)
    return length([ l for l in s if isupper(l) ])
end

println("Testing exercise 3")
@test ex3("asd") == 0
@test ex3("ASD") == 3
@test ex3("AsD") == 2
@test ex3("aSd") == 1
@test ex3("   aSD 1234   ") == 2

#=
Exercise 4
Write a function that takes two sequences as arguments and returns true if every element in the first one is also an element in the second one
=#

function ex4(seq_a::Array, seq_b::Array)
    return all(i -> i in seq_b, seq_a)
end

println("Testing exercise 4")
@test ex4([1, 2, 3], ['a', 'b', 'c']) == false
@test ex4([1, 2, 3], [4, 1, 3, 2, 5]) == true
@test ex4([1, 'a', 3], [4, 1, 3, 2, 5]) == false
@test ex4([1, 'a', 3], [4, 1, 3, 2, 'a']) == true

#=
Exercise 5
The Julia libraries include functions for interpolation and approximation.
Nevertheless, let's write our own function approximation routine as an exercise.
In particular, write a function linapprox that takes as arguments:
- A function f mapping some interval [a, b] into R
- Two scalars a and b providing the limits of the interval
- An integer n determining the number of grid points
- A number x satisfying a <= x <= b
And returns the piecewise linear interpolation of f at x, based on n evenly spaced grid points a = point[1] < point[2] < ... < point[n] = b
=#

function linapprox(f::Function, a::Number, b::Number, n::Int64, x::Number)
    range(min::Number, max::Number, n::Int64) = rand(n) .* (max-min) .+ min
    get_error(xs::Array, ys::Array) = (ys[1:length(ys)-1] - ys[2:end]) ./ (xs[1:length(xs)-1] - xs[2:end])
    xs = vcat([a], sort(range(a, b, n-2)), [b])
    ys = f.(xs)
    errors = get_error(xs, ys)
    error_index = find(i -> i < x, xs)[end]
    error = errors[error_index]
    return (x - xs[error_index]) * error + ys[error_index]
end

println("Testing exercise 5")
ff(x) = if (x < 2) return x * 0.75 elseif (x < 6) return 1.5 elseif (x < 8) return (x - 6) * -0.25 + 1.5 else return (x - 8) * 0.5 + 1 end
@test linapprox(ff, 0, 10, 100, 9) == 1.5
@test linapprox(ff, 0, 10, 100, 7) == 1.25
@test linapprox(ff, 0, 10, 100, 4) == 1.5
@test linapprox(ff, 0, 10, 100, 1) == 0.75

#=
Write a program that reads us_cities.txt and calculate total population accroos these cities
=#

function total_population()
    open("us_cities.txt") do f
        return sum([ parse(Int, split(l)[end]) for l in eachline(f) ])
    end
end

println("Testing exercise 6")
@test total_population() == 23831986
