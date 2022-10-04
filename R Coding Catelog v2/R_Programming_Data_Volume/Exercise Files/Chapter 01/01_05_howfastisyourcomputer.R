# Description: how fast is your computer?

install.packages("benchmarkme")
library(benchmarkme)

# this creates your benchmark and rank
bm_results <- benchmark_std() #generates a benchmark to compare
upload_results(bm_results)
create_bundle(bm_results, filename = "HighVolBenchMarkResults.rds")

# benchmarks for various machines
# https://jumpingrivers.shinyapps.io/benchmarkme/ 


