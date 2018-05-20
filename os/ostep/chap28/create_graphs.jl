using DataFrames
using CSV
using Plots

pyplot()

function create_graph(df::DataFrame)
  plot(
       df[:iterations],
       df[:ratio],
       title="Wasted CPU time per number of loop iterations",
       xlabel="Iterations",
       ylabel="Wasted CPU time")
  png("wasted")
end

function main()
  df = CSV.read("ratios.txt", types=[Int,Int,Int], delim=' ', nullable=false)
  df[:ratio] = 1 .- (df[:optimal] ./ df[:default])
  create_graph(df)
end

main()
