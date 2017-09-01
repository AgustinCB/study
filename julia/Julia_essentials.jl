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
@test ex1part1(v1, v2) == sum(v1.*v2)

function ex1part2(from::Int64,to::Int64)
    return length([ i for i=from:to if i%2==0 ])
end

@test ex1part2(0, 99) == 50

function ex1part3(pairs::Array{Tuple{Int64, Int64}, 1})
    return length([p for p=pairs if p[1]%2 == 0 && p[2]%2 == 0 ])
end

@test ex1part3([(2, 5), (4, 2), (9, 8), (12, 10)]) == 2

