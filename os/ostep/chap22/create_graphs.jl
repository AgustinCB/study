using DataFrames
using CSV
using Plots

pyplot()

function create_graph(file_name::String, df::DataFrame)
  graph_df = df[df[:method] .== file_name, :]
  plot(
       graph_df[:size],
       graph_df[:hitrate],
       title="$file_name performance",
       xlabel="Cache Size",
       ylabel="Hitrate")
  png(file_name)
end

function main()
  df = CSV.read("data.csv", types=[String,Int,Float64], nullable=false)
  create_graph("FIFO", df)
  create_graph("LRU", df)
  create_graph("MRU", df)
end

main()
