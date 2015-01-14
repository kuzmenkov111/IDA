# http://berndbischl.github.io/mlr/tutorial/html/parallelization/index.html

# parallelMap supports all major parallelization backends: 
# - local multicore execution using parallel
# - socket and MPI clusters using snow
# - makeshift SSH-cluster using BatchJobs and high performance computing clusters 
#      (managed by a scheduler like SLURM, Torque/PBS, SGE or LSF) also using BatchJobs.

library(mlr)
library(parallelMap)

parallelStartSocket(2)

rdesc = makeResampleDesc("CV", iters=3)
r =resample("classif.lda", iris.task, rdesc)

parallelStop()

# On Linux or Mac OS X, you may want to use parallelStartMulticore instead
# - different parallelization levels for fine grained control over the parallelization

# level of parallelization can be set on 'parallelStart' function eg level="mlr.resample"
# available levels
parallelShowRegisteredLevels()
# mlr : benchmark,resample,selectFeatures,tuneParams

# see parallelMap for further details: https://github.com/berndbischl/parallelMap