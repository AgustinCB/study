using Base.Test
using Plots
using Distributions

function part1()
    ts_length = 100
    epsilon_values = randn(ts_length)
    return plot(epsilon_values, color="blue")
end

function part2()
    function plot_histogram(distribution, n)
        epsilon_values = rand(distribution, n)
        histogram(epsilon_values)
    end

    lp = Laplace()
    return plot_histogram(lp, 500)
end

function ex1(n::Int64)
    fac = 1
    for i in 1:n
        fac *= i
    end
    return fac
end

function ex2(n::Int64, p::Float64)
    x = 1:n
    function binomial_rv(n1::Int64)
        function check_prob(a::Float64)
            return a < p
        end
        tries = randn(n1)
        total = map(check_prob, tries)
        passed = filter(x -> x, total)
        return length(passed)
    end
    y = map(binomial_rv, x)
    println(y)
    return plot(x,y)
end

function ex3(n::Int64)
    total = 0
    function is_in_circle(x::Float64, y::Float64)
        return (x^2+y^2) < 1
    end
    for i in 1:n
        if is_in_circle(rand(Uniform(-1, 1)), rand(Uniform(-1, 1)))
            total += 1
        end
    end
    p = total / n
    return p * 4
end

function ex4()
    row = 0
    for i in 1:10
        passed = rand() > 0.5
        if passed
            row += 1
            if row == 3
                println("Pay!")
                return
            end
        else
            row = 0
        end
    end
    println("No pay!")
end

function get_serie(α::Float64)
    T = 200
    y = [0.0]
    for i in 1:T
        y = push!(y, α * y[i] + randn())
    end
    return y
end

function ex5()
    return plot(get_serie(0.9), color="blue")
end

function ex6()
    return plot([get_serie(0.0), get_serie(0.8), get_serie(0.98)], label=reshape(["alpha=0", "alpha=0.8", "alpha=0.98"], 1, 3))
end

p = part1()
gui(p)
p = part2()
gui(p)

println("Testing exercise one")
@test factorial(1) == ex1(1)
@test factorial(10) == ex1(10)
@test factorial(20) == ex1(20)

p = ex2(10, 0.5)
gui(p)

println(ex3(50))
println(ex3(100))
println(ex3(200))
println(ex3(500))
println(ex3(1000))
println(ex3(2000))

ex4()
ex4()
ex4()
ex4()
ex4()
ex4()
ex4()
ex4()
ex4()
ex4()

p = ex5()
gui(p)

p = ex6()
gui(p)
