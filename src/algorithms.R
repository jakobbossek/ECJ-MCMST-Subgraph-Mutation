evolAnalyse = function(instance,
  mu,
  mut = mcMST::mutSubgraphMST,
  selMating = NULL, selSurvival = ecr::selNondom,
  ref.point = NULL,
  max.iter = 100L,
  no.survival.selection = FALSE,
  mut.args = list()) {

  print(instance)
  # get number of nodes
  n = instance$getV()
  n.objectives = instance$getW()

  # convert to C++ graph structure
  force(instance)

  fitness.fun = function(ind, instance) {
    ind$getSumOfEdgeWeights()
  }

  control = ecr::initECRControl(fitness.fun, n.objectives = n.objectives)

  # now generate an initial population, i.e.,
  # a list of random spanning trees
  population = lapply(1:mu, function(i) {
    instance$getRandomMST()
  })
  fitness = ecr::evaluateFitness(control, population, instance = instance)

  storage = data.frame()
  iter = 0L
  n.sel.off.total = 0L

  while (iter < max.iter) {
    offspring = lapply(population, function(ind) {
      do.call(mut, c(list(ind, instance = instance), mut.args))
    })
    BBmisc::messagef("[EMOA] Generated %i offspring individuals.", length(offspring))
    fitness.o = ecr::evaluateFitness(control, offspring, instance = instance)

    fitness.union = cbind(fitness, fitness.o)
    population.union = c(population, offspring)
    idx.sel = ecr::selNondom(fitness.union, n.select = mu)

    # now count how many of those individuals survived
    idx.sel.off = idx.sel[idx.sel > mu]
    n.sel.off = length(idx.sel.off)

    n.sel.off.total = n.sel.off.total + n.sel.off

    BBmisc::messagef("[EMOA] %i of %i offspring survived.", n.sel.off, mu)
    # now store all information in data frame
    if (no.survival.selection) {
      idx.sel.off = (mu + 1):(2 * mu)
    }
    if (length(idx.sel.off) > 0) {
      idx.sel.off = idx.sel.off - mu
      for (i in idx.sel.off) {
        storage = rbind(storage,
          data.frame(
            p1 = fitness[1L, i],
            p2 = fitness[2L, i],
            c1 = fitness.o[1L, i],
            c2 = fitness.o[2L, i],
            dominates = dominates(fitness.o[, i], fitness[, i]),
            iter = iter
          )
        )
      }
    }

    if (no.survival.selection) {
      fitness = fitness.o
      population = offspring
    } else {
      fitness = fitness.union[, idx.sel, drop = FALSE]
      population = population.union[idx.sel]
    }
    iter = iter + 1L
  }

  BBmisc::messagef("[EMOA] In total %i out of %i mutations (%.3f) successful", n.sel.off.total, mu * max.iter, n.sel.off.total / (mu * max.iter))

  return(list(
    pareto.front = ecr::toParetoDf(fitness, filter.dups = TRUE),
    pareto.set = lapply(population, function(el) el$toEdgeList()),
    storage = storage
  ))
}
