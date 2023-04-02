import pstats

stats = pstats.Stats('profiling_results.prof')
# Sort the results by cumulative time spent in each function
stats.sort_stats(pstats.SortKey.CUMULATIVE)
# Print the top 10 most time-consuming functions
stats.print_stats(10)
