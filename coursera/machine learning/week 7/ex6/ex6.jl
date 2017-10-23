using Base.Test
using MAT
using Optim
using Plots
using PyPlot
using ScikitLearn
using ScikitLearn.GridSearch: GridSearchCV
using StatPlots

@sk_import svm: SVC

pyplot()

file = matopen("ex6data1.mat")
X = read(file, "X")
y = read(file, "y")[:]

df = DataFrame(input1 = X[:, 1], input2 = X[:, 2], output = y)
scatter!(df, :input1, :input2, group = :output, xlabel = "Input 1", ylabel = "Input 2", markershape=[:xcross :cross])
png("file.png")

model = SVC(C=1, kernel="linear", tol=1e-3)
fit!(model, X, y)
println("Accuracy of dataset 1: ", sum(predict(model, X) .== y) / length(y))

gaussianKernel(x1, x2, σ) = exp(-sum((x1-x2).^2)/(2*σ^2))

@test abs(gaussianKernel([1 2 1], [0 4 -1], 2) - 0.324652) < 1e-6

file = matopen("ex6data2.mat")
X = read(file, "X")
y = read(file, "y")[:]

df = DataFrame(input1 = X[:, 1], input2 = X[:, 2], output = y)
scatter!(df, :input1, :input2, group = :output, xlabel = "Input 1", ylabel = "Input 2", markershape=[:xcross :cross])
png("file1.png")

C = 1
σ = 0.1
function customKernel(X1::Matrix, X2::Matrix, σ)
    gram_matrix = zeros(size(X1, 1), size(X2, 1))
    for i in 1:size(X1, 1)
        for j in 1:size(X2, 1)
            x1 = X1[i, :]
            x2 = X2[j, :]
            gram_matrix[i,j] = gaussianKernel(x1, x2, σ)
        end
    end
    return gram_matrix
end
model = SVC(C=C, kernel=customKernel, tol=1e-3, gamma=σ)
fit!(model, X, y)
println("Accuracy of dataset 2: ", sum(predict(model, X) .== y) / length(y))

file = matopen("ex6data3.mat")
X = read(file, "X")
y = read(file, "y")[:]

df = DataFrame(input1 = X[:, 1], input2 = X[:, 2], output = y)
scatter!(df, :input1, :input2, group = :output, xlabel = "Input 1", ylabel = "Input 2", markershape=[:xcross :cross])
png("file2.png")

gridsearch = GridSearchCV(SVC(), Dict(:kernel => [customKernel], :C => 0.1:0.1:2.0, :gamma => 0.1:0.1:2.0))
fit!(gridsearch, X, y)
println("Best hyper-parameters: $(gridsearch.best_params_)")

model = SVC(C=gridsearch.best_params_[:C], kernel=customKernel, tol=1e-3, gamma=gridsearch.best_params_[:gamma])
fit!(model, X, y)
println("Accuracy of dataset 3: ", sum(predict(model, X) .== y) / length(y))
