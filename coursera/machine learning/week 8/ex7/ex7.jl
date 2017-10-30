using Base.Test
using DataFrames
using MAT
using Plots
using PyPlot

pyplot()

file = matopen("ex7data2.mat")
X = read(file, "X")

#=
Part 1: Find closest centroids
=#

function findClosestCentroids(X::Matrix, centroids::Matrix)
    k = size(centroids, 1)
    n = size(X, 1)
    vdistance(x::Array{Float64, 1}, centroid) = norm(x - centroid) ^ 2
    closestDistance(x::Array{Float64, 1}) = indmin([vdistance(x, centroids[i, :]) for i in 1:k])
    return [closestDistance(X[i, :]) for i in 1:n]
end

K = 3
initial_centroids = [3 3; 6 2; 8 5]

idx = findClosestCentroids(X, initial_centroids)

@test idx[1:3] == [1, 3, 2]

#=
Part 2: After implementing the closest centroids function, you should now complete the computeCentroids function.
=#

function computeCentroids(X::Matrix, idx::Array{Int64, 1}, K::Int64)
    centroids = zeros(K, size(X, 2))

    for k in 1:K
        cluster = [X[i, :] for i in 1:size(X, 1) if idx[i] == k]
        centroids[k, :] = sum(cluster) ./ length(cluster)
    end

    return centroids
end

centroids = computeCentroids(X, idx, K)

@test norm(centroids - [2.428301 3.157924; 5.813503 2.633656; 7.119387 3.616684]) < 1e-6

#=
Part 3: Run the k-means algorithm
=#

function runkMeans(X::Matrix, centroids::Matrix, max_iters::Int64)
    K = size(centroids, 1)
    for i in 1:max_iters
        idx = findClosestCentroids(X, centroids)
        prev_centroids = copy(centroids)
        centroids = computeCentroids(X, idx, K)
        if centroids == prev_centroids
            break
        end
    end
    idx = findClosestCentroids(X, centroids)
    return [centroids, idx]
end

max_iters = 10

initial_centroids = [3 3; 6 2; 8 5]

res = runkMeans(X, initial_centroids, max_iters)

df = DataFrame(input1 = X[:, 1], input2 = X[:, 2], output = res[2])
Plots.scatter(df[:input1],df[:input2],g=df[:output],m=[:xcross :cross :hexagon])
png("file1.png")

#=
Part 4: Use K-means to compress an image. To do this, you will first run K-Means on the colors of the pixels in the image and then you will map each pixel onto its closest centroid. 
=#

function kMeansInitCentroids(X::Matrix, K::Int64)
    centroids = zeros(K, size(X, 2))
    m = size(X, 1)

    for k in 1:K
        index = rand(1:m)
        centroids[k, :] = X[index, :]
    end

    return centroids
end

A = imread("bird_small.png")

img_size = size(A)

X = reshape(A, img_size[1] * img_size[2], 3)
X = convert.(Float64, X)

K = 16
max_iters = 10

initial_centroids = kMeansInitCentroids(X, K)

res = runkMeans(X, initial_centroids, max_iters)
idx = res[2]
centroids = res[1]

#=
Part 5: Image compression. Use the clusters of K-Means to compress an image. To do this, we first find the closest clusters foreach example. Essentially, now we have represented the image X as in terms of the indices in idx.
=#

# We can now recover the image from the indices (idx) by mapping each pixel
# (specified by its index in idx) to the centroid value
X_recovered = centroids[idx,:]

# Reshape the recovered image into proper dimensions
X_recovered = reshape(X_recovered, img_size[1], img_size[2], 3)

PyPlot.imshow(A)
PyPlot.savefig("expected.png")

PyPlot.imshow(X_recovered)
PyPlot.savefig("obtained.png")
