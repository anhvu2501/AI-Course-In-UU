library("DeliveryMan")

myFunction <- function(trafficMatrix, carInfo, packageMatrix) {
  next_package = nextPickUp(carInfo$x, carInfo$y, packageMatrix, carInfo$load)
  if (carInfo$x == next_package$x && carInfo$y == next_package$y) {
    carInfo$nextMove <- 5
    #stay still
  } else {
    path <-
      aStar(trafficMatrix,
            carInfo$x,
            carInfo$y,
            next_package$x,
            next_package$y)
    next_move = path[[2]]
    if (next_move$y > carInfo$y) {
      carInfo$nextMove <- 8
    } else if (next_move$x > carInfo$x) {
      carInfo$nextMove <- 6
    } else if (next_move$y < carInfo$y) {
      carInfo$nextMove <- 2
    } else if (next_move$x < carInfo$x) {
      carInfo$nextMove <- 4
    }
    
  }
  return(carInfo)
}

manhattanDistance <- function(from_x, from_y, to_x, to_y) {
  return(abs(from_x - to_x) + abs(from_y - to_y))
}

findClosestPackage <- function(x, y, packageMatrix) {
  closest = 1
  closest_distance = -1
  for (p in 1:nrow(packageMatrix)) {
    package_status = packageMatrix[p, 5]
    if (package_status == 0) {
      distance =
        manhattanDistance(x, y, packageMatrix[p, 1], packageMatrix[p, 2])
      if (closest_distance == -1 || distance < closest_distance) {
        closest = p
        closest_distance = distance
      }
    }
  }
  return(closest)
}

#check if package has been picked up or not
nextPickUp <- function(x, y, packageMatrix, load) {
  nextPickUp = list(x = -1, y = -1)
  if (load == 0) {
    nextPackage = findClosestPackage (x, y, packageMatrix)
    nextPickUp$x = packageMatrix[nextPackage, 1]
    nextPickUp$y = packageMatrix[nextPackage, 2]
  } else {
    nextPickUp$x = packageMatrix[load, 3]
    nextPickUp$y = packageMatrix[load, 4]
  }
  return(nextPickUp)
}



aStar = function (trafficMatrix, from_x, from_y, to_x, to_y) {
  frontier = list()
  visited = list()
  initial_manhattan_distance = manhattanDistance(from_x, from_y, to_x, to_y)
  initial_node = list(
    x = from_x,
    y = from_y,
    cost = 0,
    md = initial_manhattan_distance,
    path = list ()
  )
  
  frontier[[1]] <- initial_node
  
  #find best road horizontally and vertically
  best_road_list = list ()
  best_road_list[1] = trafficMatrix$vroads[which.min(trafficMatrix$vroads)]
  best_road_list[2] = trafficMatrix$hroads[which.min(trafficMatrix$hroads)]
  best_road = best_road_list[[which.min(best_road_list)]]
  
  running = TRUE
  while (running) {
    picking = TRUE
    expand_node = NULL
    while (picking) {
      costs = sapply(frontier, function(item)
        item$cost + item$md)
      best_index = which.min(costs)
      expand_node = frontier[[best_index]]
      frontier = frontier[-best_index]
      
      picking = FALSE
      for (node in visited) {
        if (node$x == expand_node$x && node$y == expand_node$y) {
          picking = TRUE
          break
        }
      }
      #
    }
    visited[[length(visited) + 1]] <- expand_node
    
    
    if (expand_node$x == to_x && expand_node$y == to_y) {
      running = FALSE
      final_path = expand_node$path
      final_path[[length(final_path) + 1]] <-
        list(x = expand_node$x, y = expand_node$y)
      return (final_path)
    } else {
      # Expand up
      if (expand_node$y < 10) {
        frontier[[length(frontier) + 1]] <-
          create_new_node(0, 1, expand_node, trafficMatrix, best_road, to_x, to_y)
      }
      #Expand down
      if (expand_node$y > 1) {
        frontier[[length(frontier) + 1]] <-
          create_new_node(0,-1, expand_node, trafficMatrix, best_road, to_x, to_y)
      }
      #Expand left
      if (expand_node$x > 1) {
        frontier[[length(frontier) + 1]] <-
          create_new_node(-1, 0, expand_node, trafficMatrix, best_road, to_x, to_y)
      }
      #Expand right
      if (expand_node$x < 10) {
        frontier[[length(frontier) + 1]] <-
          create_new_node(1, 0, expand_node, trafficMatrix, best_road, to_x, to_y)
      }
    }
  }
}

#select new node to add to frontier
create_new_node = function(x, y, expand_node, trafficMatrix, best_road, to_x, to_y) {
  new_x = expand_node$x + x
  new_y = expand_node$y + y
  new_path = expand_node$path
  new_path[[length(new_path) + 1]] <-
    list(x = expand_node$x, y = expand_node$y)
  new_manhattan_dist = manhattanDistance(new_x, new_y, to_x, to_y) * best_road
  #calculate new cost
  if (y == -1) {
    new_cost = expand_node$cost + trafficMatrix$vroads[expand_node$x, expand_node$y - 1]
  } else if (y == 1) {
    new_cost = expand_node$cost + trafficMatrix$vroads[expand_node$x, expand_node$y]
  } else if (x == -1) {
    new_cost = expand_node$cost + trafficMatrix$hroads[expand_node$x - 1, expand_node$y]
  } else {
    new_cost = expand_node$cost + trafficMatrix$hroads[expand_node$x, expand_node$y]
  }
  new_node = list(x = new_x, y = new_y, cost = new_cost, md = new_manhattan_dist,path = new_path)
  return (new_node)
}