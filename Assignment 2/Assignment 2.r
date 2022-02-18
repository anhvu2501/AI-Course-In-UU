# This file contains the function ourFunction that we created to be passed to 
runWheresCroc
# This was done by Kawthar El Ouardi, Victoria Sedig and Vu Ho Tran Anh
#Calculate the probability of a node given its neighbors' probabilities
sumProbNeighbours = function(node, neighbours, probs, numberOfNb) {
  sum = 0
  for (n in neighbours) {
    sum = sum + probs[[n]] * 1 / numberOfNb
  }
  return (sum)
}
#Emission Matrix
getEmissionMatrix = function(readings, probs) {
  densitySal = dnorm(readings[1],
                     mean = probs$salinity[, 1],
                     sd = probs$salinity[, 2])
  densityPho = dnorm(readings[2],
                     mean = probs$phosphate[, 1],
                     sd = probs$phosphate[, 2])
  densityNit = dnorm(readings[3],
                     mean = probs$nitrogen[, 1],
                     sd = probs$nitrogen[, 2])
  emissionMatrix = list()
  for (i in 1:40) {
    emissionMatrix <-
      append(emissionMatrix, densitySal[i] * densityPho[i] * densityNit[i])
  }
  return(emissionMatrix)
}
#Initialize the memory on the first round or a new game
initializeMem = function (moveInfo,transitionMatrix) {
  moveInfo$mem$croc = 0
  moveInfo$mem$status = 2
  moveInfo$mem$matrix = list()
  moveInfo$mem$matrix <-
    append(moveInfo$mem$matrix, list(transitionMatrix))
  return (moveInfo$mem)
}
#' ourFunction
#'
#' Function to pass as an argument to runWheresCroc function
#' Uses the forward algorithm to find where the crocodile is
ourFunction = function(moveInfo,
                       readings,
                       positions,
                       edges,
                       probs) {
  options = getOptions(positions[3], edges)
  results = getEmissionMatrix(readings, probs)
  transitionMatrix = list()
  if (moveInfo$mem$status == 0 ||
      moveInfo$mem$status == 1) { #first game or first round of new game
    for (i in 1:40) {
      element = results[[i]] * 1/40
      transitionMatrix <- append(transitionMatrix, list(element))
    }
    transitionMatrix[positions[1]] = 0
    transitionMatrix[positions[2]] = 0
    CrocIndex = which.max(transitionMatrix)
    moveInfo$mem = initializeMem(moveInfo,transitionMatrix)
  } else {
    CrocIndex = -1
    if (!is.na(positions[1]) && positions[1] < 0) {
      for (i in 0:40) {
        transitionMatrix[i] = 0
      }
      transitionMatrix[-positions[1]] = 1
      moveInfo$mem$croc = -positions[1]
    } else if (!is.na(positions[2]) && positions[2] < 0) {
      for (i in 0:40) {
        transitionMatrix[i] = 0
      }
      transitionMatrix[-positions[2]] = 1
      moveInfo$mem$croc = -positions[2]
    } else {
      matrixBefore = moveInfo$mem$matrix[[length(moveInfo$mem$matrix)]]
      for (i in 1:40) {
        i_neighbours = getOptions(i, edges)
        sumProb = sumProbNeighbours(i, i_neighbours, matrixBefore, 
length(i_neighbours))
        element = results[[i]] * sumProb
        transitionMatrix <- append(transitionMatrix, list(element))
      }
      transitionMatrix[positions[1]] = 0
      transitionMatrix[positions[2]] = 0
      moveInfo$mem$matrix <-
        append(moveInfo$mem$matrix, list(transitionMatrix))
    }
    CrocIndex = which.max(transitionMatrix)
    if (moveInfo$mem$croc > 0) {
      if (CrocIndex %in% getOptions(moveInfo$mem$croc,edges)) {
        moveInfo$mem$croc = 0
      } else {
        CrocIndex = moveInfo$mem$croc
        moveInfo$mem$croc = 0
      }
    }
  }
  if (CrocIndex %in% options) {
    moveInfo$moves = c(CrocIndex,0)
  } else {
    path = searchNode(positions[3],edges,CrocIndex)
    if (length(path) >= 2) {
      moveInfo$moves = c(path[1], path[2])
    }
    if (length(path) == 1) {
      moveInfo$moves = c(path[1], 0)
    }
    if (length(path) == 0) {
      moveInfo$moves = c(0, 0)
    }
  }
  return(moveInfo)
}
#Uses the bfs algorithm to find the shortest path to the goal
searchNode = function(node,edges,crocIndex) {
  visited = list(node)
  queue = list(node)
  parents = list()
  for (i in 0:40) {
    parents[i] = 0
  }
  parents[node] = -1
  while (length(queue) != 0) {
    currNode = queue[1]
    queue = queue[-1]
    neighbors = getOptions(currNode, edges)
    neighbors = neighbors[!(neighbors %in% visited)]
    for (n in neighbors) {
      if (!(n %in% visited)) {
        queue = append(queue, n)
        parents[n] = currNode
        visited = append(visited, list(n))
      }
    }
  }
  currNode = crocIndex
  path = list()
  while (currNode != -1) {
    if (parents[currNode] != -1) {
      path = append(list(currNode), path)
    }
    currNode = parents[[currNode]][1]
  }
  return (path)
}
getOptions=function(point,edges) {
  c(edges[which(edges[,1]==point),2],edges[which(edges[,2]==point),1],point)
}