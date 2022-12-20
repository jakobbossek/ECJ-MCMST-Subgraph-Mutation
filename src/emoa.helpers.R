# Mutator for edge list representation.
# We select a random edge and traverse the tree until > sigma elements are reached.
# Then we replace the subtree with the optimal subtree regarding objective 1 or 2
# with equal probability.
mutMixedMST = makeMutator(
  mutator = function(ind, sigma = floor(ind$getV() / 2), instance = NULL) {
    if (runif(1L) < 0.8)
      return(mcMST::mutSubgraphMST(ind, sigma = sigma, scalarize = TRUE, instance = instance))
    else
      return(mcMST::mutKEdgeExchange(ind, k = 1L, instance = instance))
  },
  supported = "custom"
)
