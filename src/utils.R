stringToEdgeList = function(s) {
  nodelist = as.integer(strsplit(s, split = "-", fixed = TRUE)[[1L]])
  m = length(nodelist) / 2
  matrix(nodelist, ncol = m, byrow = FALSE)
}

edgeListToString = function(el) {
  m = ncol(el)
  BBmisc::collapse(as.character(el), sep = "-")
}

getFrontsAfterAsDataframe = function(log, gr, at = c(0.1, 0.5, 1.0)) {
  pops = getPopulations(log, trim = TRUE)
  n.gens = length(pops)
  gens.selected = floor(at * n.gens)
  pops = pops[gens.selected]

  # now get all those non-dominated points
  approx = lapply(pops, function(pop) {
    idx.nondom = which.nondominated(pop$fitness)
    list(
      front = ecr::toParetoDf(pop$fitness[, idx.nondom, drop = FALSE]),
      set   = pop$population[idx.nondom]
    )
  })

  fronts = lapply(approx, function(tmp) tmp$front)
  n.sols = sapply(fronts, nrow)
  sets   = lapply(approx, function(tmp) tmp$set)

  fronts = do.call(rbind, fronts)
  fronts$generation = rep(floor(100 * at), n.sols)

  converter = new(mcMST:::RepresentationConverter)

  sets = do.call(c, sets)
  sets = lapply(sets, function(sol) {
    if (is.atomic(sol)) {
      sol = converter$prueferCodeToGraph(gr, sol)
    }
    sol$toEdgeList()
  })

  time.passed = getStatistics(log, trim = TRUE)$time.passed[gens.selected]
  time.passed = data.frame(generation = floor(100 * at), time.passed = time.passed)

  return(list(front = fronts, set = sets, time.passed = time.passed))
}


writeFront = function(front, path) {
  dn = dirname(path)
  if (!dir.exists(dn))
    dir.create(dn)
  write.table(front, file = path, sep = " ", quote = FALSE, row.names = FALSE, col.names = TRUE)
}

writeSet = function(set, path) {
  assertList(set)
  dn = dirname(path)
  if (!dir.exists(dn))
    dir.create(dn)
  set = do.call(rbind, set)
  write.table(set, file = path, sep = " ", quote = FALSE, row.names = FALSE, col.names = FALSE)
}

readFront = function(path) {
  front = read.table(path, header = TRUE, stringsAsFactors = FALSE, sep = " ")
  return(front)
}

readSet = function(path) {
  set = read.table(path, header = FALSE, stringsAsFactors = FALSE, sep = " ")
  n = nrow(set)
  set2 = list()
  for (i in seq(1L, n - 1L, by = 2L)) {
    set2 = c(set2, list(as.matrix(set[i:(i + 1L), , drop = FALSE])))
  }
  return(set2)
}

writeResult = function(res, res.detailed = NULL, time.passed, path) {
  assertList(res)
  assertString(path)
  #writeFront(res$pareto.front, file.path(path, "pf.csv"))
  #writeSet(res$pareto.set, file.path(path, "ps.csv"))

  # write final fronts with encoded sets
  front = res$pareto.front
  front$edgelist = sapply(res$pareto.set, edgeListToString)
  write.table(front, file = file.path(path, "pfps_detailed.csv"), sep = " ", quote = FALSE, row.names = FALSE, col.names = TRUE)
  write.table(data.frame(time.passed = time.passed), file = file.path(path, "time_detailed.csv"), sep = " ", quote = FALSE, row.names = FALSE, col.names = TRUE)

  if (!is.null(res.detailed)) {
    # now write populations
    #write.table(res.detailed$front, file = file.path(path, "pf_detailed.csv"), sep = " ", quote = FALSE, row.names = FALSE, col.names = TRUE)

    front = res.detailed$front
    front$edgelist = sapply(res.detailed$set, edgeListToString)
    write.table(front, file = file.path(path, "pfps_detailed.csv"), sep = " ", quote = FALSE, row.names = FALSE, col.names = TRUE)

    write.table(res.detailed$time.passed, file = file.path(path, "time_detailed.csv"), sep = " ", quote = FALSE, row.names = FALSE, col.names = TRUE)
  }
}

readResult2 = function(path) {
  tmp = readr::read_delim(sprintf("%s/pfps_detailed.csv", path), delim = " ")
  list(
    pf = tmp[, 1:2],
    ps = lapply(tmp$edgelist, function(el) stringToEdgeList(el))
  )
}

readResult = function(path) {
  list(
    pf = readFront(file.path(path, "pf.csv")),
    ps = readSet(file.path(path, "ps.csv"))
  )
}

#FIXME: move to grapherator
getGraphMetaFromString = function(s, generators = FALSE) {
  graph.part = gsub("graph_", "", s)
  graph.part = strsplit(graph.part, "---", fixed = TRUE)[[1L]]

  node.types = edge.types = weight.types = NULL
  if (generators) {
    node.types = strsplit(graph.part[2L], "-", fixed = TRUE)[[1L]]
    edge.types = strsplit(graph.part[3L], "-", fixed = TRUE)[[1L]]
    weight.types = strsplit(graph.part[4L], "-", fixed = TRUE)[[1L]]
  }

  main.part = graph.part[1L]
  main.parts = strsplit(main.part, "-", fixed = TRUE)[[1L]]
  n.nodes = as.integer(gsub("N", "", main.parts[1L]))
  n.edges = as.integer(gsub("E", "", main.parts[2L]))
  n.clusters = as.integer(gsub("C", "", main.parts[3L]))
  n.weights = as.integer(gsub("W", "", main.parts[4L]))

  return(list(n.nodes = n.nodes, n.edges = n.edges, n.clusters = n.clusters, n.weights = n.weights,
    node.types = node.types, edge.types = edge.types, weight.types = weight.types))
}

buildResultFilePath = function(root, format.string, ...) {
  checkmate::assertDirectoryExists(root)
  checkmate::assertString(format.string)
  path = sprintf(format.string, ...)
  path = file.path(root, path)
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    BBmisc::catf("[buildResultFilePath] Created output path '%s'.", path)
  }
}

dropExtension = function(fn, levels = 1L) {
  for (i in 1:levels)
    fn = gsub("\\.[[:alpha:]]*$", "", fn)
  return(fn)
}
