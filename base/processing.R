process_tsp <- function(problem_data, problem_name, user_alpha, user_gamma, e_greedy, user_episodes) {
  start_time <- Sys.time()
  arq <- tolower(problem_name)
  data <- as.data.frame(sapply(problem_data, as.numeric))
  N <- ((lengths(data))[1]) # Number of nodes
  
  atsp = c('ftv33', 'p43', 'ftv47','ftv44', 'ft53','ftv64','ft70','ry48p') # ATSP instances
  tsp = c('swiss42','eil51','eil76','eil101','berlin52','st70','pr76','rat99','kroa100','bier127','ch130','ch150','a280','lin318','d1655') # TSP instances
  
  # The way the data is arranged. Note: The "d" stands for data.
  dMatrix <- c('ftv33', 'p43','ftv44', 'ftv47', 'ft53','ftv64','ft70','ry48p','swiss42')  
  dEuc2d <- c('eil51','eil76','eil101','berlin52','st70','pr76','rat99','kroa100','bier127','ch130','ch150','a280','lin318','d1655')
  
  if ((arq %in% tsp) && (arq %in% dEuc2d)) {
    type <- 'tsp'
    data_type <- 'euc2d'
  } else if ((arq %in% tsp) && (arq %in% dMatrix)) {
    type <- 'tsp'
    data_type <- 'matrix'
  } else if (arq %in% atsp) {
    type <- 'atsp'
    data_type <- 'matrix'
  } else{
    type <- 'unavailable'
    data_type <- 'unavailable'
  }
  
  if ((data_type != 'unavailable') && (type != 'unavailable')) {
    if (data_type == 'euc2d') {
      # Loading vectors with X and Y positions
      x <- rep(0, N)
      y <- rep(0, N)
      
      for (i in 1:N) {
        x[i] <- data[i, 2]
        y[i] <- data[i, 3]
      }
      
      d = matrix(0, N, N, T) # Distance matrix
      
      # Loading the matrix with distances and costs
      for (i in 1:N) {
        for (j in 1:N) {
          if (i != j) {
            Dx <- ((x[j] - x[i]) ** 2)
            Dy <- ((y[j] - y[i]) ** 2)
            d[i, j] <- (as.integer(sqrt(Dx + Dy)))
          } else{
            d[i, j] <- 9999
          }
        }
      }
      
    } else{
      d = data # Distance matrix
    }
    
    # Generating the learning, distance, and reinforcement matrices
    r = matrix(0, N, N, T) # Reinforcement matrix
    r <- (-d) # Load the rewards matrix
    
    ######################################################################################################################
    
    # Setting the Parameters
    e <- e_greedy # Greedy Policy
    epochs <- 1
    num_episodes <- user_episodes
    final_alpha <- user_alpha
    final_gamma <- user_gamma
    
    q_learning <- function(alpha = 0.75, gamma = 0.15, e = 0.01, num_episodes = 1000, N, q) {
      count <- 1 # External counter
      initial_state <- sample(1:N, 1) # Set the initial state for the first execution
      state <- initial_state # Update the state to the initial state
      found_solution <- rep(0, N + 1)
      solutions <- matrix(0, nrow = num_episodes, ncol = N + 1) # Matrix to store the sequences of each episode
      
      while (count <= num_episodes) {
        # Runs the defined number of episodes
        cat("\nProgress: ", count,"/", num_episodes, sep = "")
        
        chosen_states <- 1:N # Generates a vector with all available cities
        chosen_states[initial_state] <- 0 # Remove the initial state from the vector of available cities
        
        for (i in 1:N) {
          # Runs the number of nodes in the file
          
          available_states <- which(chosen_states != 0) # Returns the positions where the values are not 0, indicating the available cities
          
          row <- rep(0, length(available_states))
          for (k in 1:length(available_states)) {
            row[k] <- q[state, available_states[k]] # Receives the values ​​from matrix q where the states are available
          }
          
          if (i != N) {
            # If it's not the last state before returning to the initial one, there are still available actions
            
            e_random <- runif(1, 0, 1) # Randomly selects a value between 0 and 1
            
            if (e_random > e) {
              # Best action
              
              maximum <- max(row)# Selects the highest value from the row (best action for the current state)
              
              for (k in 1:length(available_states)) {
                aux <- available_states[k]
                if (maximum == q[state, aux]) {
                  action <- aux # Selects the position of the highest value in the row as the chosen action
                }
              }
            } else{
              # Random action
              
              if (length(available_states) == 1) {
                action <- available_states # If there's only one available state, that will be the chosen action
              } else {
                action <- sample(available_states, size = 1, replace = F)  # Randomly selects a value from the available ones
              }
            }
            
          } else {
            # If it's the last state before returning to the initial one, there are no more available actions, therefore the next action is the initial state
            action <- found_solution[1] # There are no other possible actions
          }
          
          future_state <- action # Update to the future state
          
          found_solution[i] <- state # Adds the current state to the vector of found solutions
          
          chosen_states[action] <- 0 # Removes the chosen action from the vector of available cities
          
          aux <- which(q[future_state, ] != 0) #Receives the positions that do not equal 0
          aux2 <- rep(0, length(aux))
          
          if (length(aux) != 0) {
            # Checks if "aux" is empty
            
            for (z in 1:length(aux)) {
              # Load "aux2" with the absolute values
              
              aux2[z] <- abs(q[future_state, aux[z]])
            }
            
            maxFut <- (min(aux2)) * (-1) #  Gets the smallest value from the row and multiplies it by -1
          } else{
            # If "aux" is zero, then the row only contains zeros
            maxFut <- max(q[future_state, ])
          }
          
          q[state, action] <- q[state, action]  + alpha * ((r[state, action]) + (gamma * maxFut) - q[state, action]) # Update the q matrix
          
          state <- future_state # Update the current state to the future state/chosen action
          
        }
        found_solution[length(found_solution)] <- found_solution[1] # Inserts the initial city as the final one as well
        solutions[count,] <- found_solution # Saves the found solution in the matrix containing the solutions found for each episode
        found_solution <- rep(0, N + 1)
        initial_state <- sample(1:N, size = 1, replace = F) # Chooses a random initial state
        state <- initial_state # Update the state to the initial state
        count <- count + 1 # Updates the external counter
      }
      
      return(solutions)
    }
    
    final_cost_function <- function(solutions, num_episodes) {
      final_cost <- rep(0, num_episodes)
      for (i in 1:((dim(solutions))[1])) {
        # Traverse the rows of the solutions matrix
        
        sequence <- solutions[i,] # Load the sequence vector with the solution from the current row to calculate its total distance
        dist <- rep(0, length(sequence) - 1)
        
        for (k in 1:(length(dist))) {
          dist[k] <- as.integer(d[sequence[k], sequence[k + 1]]) # Load the distance vector
        }
        
        final_cost[i] <- sum(dist) # Adds to the final cost vector the final cost of each episode
      }
      
      return(final_cost)
    }
    
    solutions_P2 <- matrix(0, nrow = epochs, ncol = N + 1)
    final_cost_M <- matrix(0, nrow = epochs, ncol = num_episodes)
    
    for (ep in 1:epochs) {
      # Runs the defined number of epochs
      q = matrix(0, N, N, T) # Learning matrix
      
      solutions <- q_learning(final_alpha, final_gamma, e, num_episodes, N, q)
      
      final_cost_M[ep,] <- final_cost_function(solutions, num_episodes)
      
      minimum <- min(final_cost_M[ep, ])
      position <- match(minimum, final_cost_M[ep, ])
      solutions_P2[ep,] <- solutions[position,]
    }
    
    final_cost <- final_cost_function(solutions_P2, epochs)
    
    minimum <- min(final_cost)
    position <- match(minimum, final_cost)
    sequence <- solutions_P2[position,]
    
    # Displays the number of episodes, the smallest distance found, and the largest
    
    xRoute <- NULL
    yRoute <- NULL
    if ((type == 'tsp') && (data_type == 'euc2d')) {
      # Loading vectors X and Y with cities in the sequence that forms the optimal path found
      xOrder <- rep(0, length(sequence))
      yOrder <- rep(0, length(sequence))
      for (i in 1:length(sequence)) {
        xOrder[i] <- x[sequence[i]]
        yOrder[i] <- y[sequence[i]]
      }
      
      # Plotting graph in order
      xRoute <- xOrder
      yRoute <- yOrder
    }
    
    end_time <- Sys.time()
    
    time <- difftime(end_time, start_time, units = "secs")
    units <- "secs"
    if(time > 60){
      time <- difftime(end_time, start_time, units = "mins")
      units <- "mins"
    } else if(time > 3600){
      time <- difftime(end_time, start_time, units = "hours")
      units <- "hours"
    }
    
    results <- list(
      dist_graph = final_cost_M[position, ],
      min_distance = min(final_cost_M[position, ]),
      avg_distance = mean(final_cost_M[position, ]),
      ep_dist_min = match(min(final_cost_M[position, ]), final_cost_M),
      time = time,
      units = units,
      xRoute = xRoute,
      yRoute = yRoute
    )
    
    return(results)
  }
}

process_sop <- function(problem_data, user_alpha, user_gamma, e_greedy, user_episodes) {
  start_time <- Sys.time()
  D_SOP <- as.data.frame(sapply(problem_data, as.numeric))
  N_SOP <- length(D_SOP)        #  Number of nodes in the instance
  R_SOP <- -D_SOP               #  Reward function
  
  e <- e_greedy # Greedy Policy
  
  # SOP
  alpha <- user_alpha
  gamma <- user_gamma
  epochs <- 1
  smallest_distances <- rep(0, epochs)
  
  # Action Selection - RLSOP - Function
  SOPAA <- function(Q, SS, AA, R, e, greedy, bag, D) {
    bag2 <- bag
    bag2[AA] <- 0 #  bag2 stores the indices of cities already visited and also cities that cannot be visited at the moment
    rest2 <- which(bag2 != 0) #  The variable rest2 stores the cities that can still be selected
    
    ok <- 0 # Loop control
    while (ok == 0) {
      #  It will only exit the loop when it verifies that AA can be visited or until it finds a location that has no constraints
      ok <- 1 # ok = 1 is the exit control
      t <- length(rest2) # Size of rest2
      if (t > 0) {
        for (i in 1:t) {
          # Checks for the remaining set of cities if there are still precedence constraints for AA
          j <- rest2[i]
          
          if (D[AA, j] == -1) {
            # If there is a precedence constraint, then ok = 0
            ok <- 0
          }
        }
      }
      if (ok == 0) {
        # There is a precedence constraint for the selected action
        t <- length(rest2)
        if (greedy < e) {
          # Selects a new action randomly
          index <- round(runif(1, 1, t))
          AA <- rest2[index]
        } else{
          # Selects the best-estimated action in state SS
          
          rest2Q <- Q[SS, rest2] #  restQ receives values only from the Q matrix of the cities available in rest2
          max2Q <- which(rest2Q == max(rest2Q)) # Check the index of the highest value in the Q-Table - available cities
          AA <- rest2[max2Q[1]]   # The new action is the highest value from the Q-Table - Available cities
        }
        bag2[AA] <- 0 # Remove action AA from the list
        rest2 <- which(bag2 != 0) # Updates rest2 only with the available cities
      }
    }
    
    return(AA) # Returns the action selected by the SOPAA
  }# End of Function - SOPAA
  
  sarsa_SOP <- function(alpha = 0.75, gamma = 0.15, q, num_episodes = 1000) {
    count <- 1 # Episode counter
    So <- 1  # Initial State
    Ao <- sample(1:N_SOP, 1) # Initial Action
    S <- So
    A <- Ao
    distance <- rep(0, num_episodes) #inicializando o vetor de distâncias
    
    while (count <= (num_episodes)) {
      cat("\nProgress: ", count,"/", num_episodes, sep = "")
      
      distance[count] <- 0 # Initial distance is zero
      bag <- 1:N_SOP           # Available cities
      bag[So] <- 0 # The initial city is accessed and becomes unavailable (receives 0)
      for (i in 1:N_SOP) {
        # Controls the visit to all cities
        bag[A] <- 0 # The destination city is accessed and becomes unavailable (receives 0)
        rest <- which(bag != 0) # Returns the index of the cities not visited
        if (length(rest) == 0) {
          # Checks if there are still cities to visit
          rest <- So # If there are none left: then return to the starting city
        }
        
        #--------------------------SARSA
        
        SS <- A     #  The new state is related to the selected action
        restQ <- q[SS, rest] # restQ receives values only from the Q matrix of the available cities
        largest_Q <- which(restQ == max(restQ)) # Check the index of the highest value in the Q-Table - available cities
        AA <- rest[largest_Q[1]]   # The new action is the highest value from the Q-Table - Available cities
        
        # Greedy Policy %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        
        rest_size <- length(rest)  # Size of rest: how many cities are available
        greedy <- runif(1, 0, 1)      # Generates a random number: [0, 1]
        if (greedy < e) {
          index <- round(runif(1, 1, rest_size))  # Generates a random index according to the size of the rest
          AA <- rest[index]                    # New random action - only among the available ones (rest)
        }
        
        #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        
        # Action Selection - RLSOP
        AA <- SOPAA(q, SS, AA, R_SOP, e, greedy, bag, D_SOP)
        
        # SARSA Update
        q[S, A] <- q[S, A] + alpha * (R_SOP[S, A] + gamma * q[SS, AA] - q[S, A])
        
        #--------------
        S <- SS  # State Update
        A <- AA  # Action Update
        
        if (D_SOP[S, A] == -1) {
          # Update the distance only if it's not a precedence constraint
          distance[count] = distance[count]
        } else{
          distance[count] = distance[count] + D_SOP[S, A]
        }
      }
      count <- count + 1
      # Episode counter update
    }
    return(distance)
  }
  
  num_episodes <- user_episodes # Number of episodes
  SOP_distances <- matrix(0, nrow = epochs, ncol = num_episodes)
  
  for (ep in 1:epochs) {
    # Runs the defined number of epochs
    q_sop <- matrix(0, N_SOP, N_SOP, T) # Learning matrix
    SOP_distances[ep,] <- sarsa_SOP(alpha, gamma, q_sop, num_episodes)
    smallest_distances[ep] <- min(SOP_distances[ep,])
  }
  position <- which.min(smallest_distances)
  distances <- SOP_distances[position, ]
  
  end_time <- Sys.time()
  
  time <- difftime(end_time, start_time, units = "secs")
  units <- "secs"
  if(time > 60){
    time <- difftime(end_time, start_time, units = "mins")
    units <- "mins"
  } else if(time > 3600){
    time <- difftime(end_time, start_time, units = "hours")
    units <- "hours"
  }
  
  results <- list(
    episodes = num_episodes,
    dist_graph = distances,
    alpha = alpha,
    gamma = gamma,
    e_greedy = e,
    time = time,
    units = units,
    min_distance = min(distances),
    avg_distance = mean(distances),
    ep_dist_min = match(min(distances), distances)
  )
  
  return(results)
}

process_tsp_automl <- function(problem_data, problem_name) {
  start_time <- Sys.time()
  arq <- tolower(problem_name)
  data <- as.data.frame(sapply(problem_data, as.numeric))
  N <- ((lengths(data))[1]) # Number of nodes
  
  atsp = c('ftv33', 'p43', 'ftv47','ftv44', 'ft53','ftv64','ft70','ry48p') # ATSP instances
  tsp = c('swiss42','eil51','eil76','eil101','berlin52','st70','pr76','rat99','kroa100','bier127','ch130','ch150','a280','lin318','d1655') # TSP instances
  
  # The way the data is arranged. Note: The "d" stands for data.
  dMatrix <- c('ftv33', 'p43','ftv44', 'ftv47', 'ft53','ftv64','ft70','ry48p','swiss42')  
  dEuc2d <- c('eil51','eil76','eil101','berlin52','st70','pr76','rat99','kroa100','bier127','ch130','ch150','a280','lin318','d1655')
  
  if ((arq %in% tsp) && (arq %in% dEuc2d)) {
    type <- 'tsp'
    data_type <- 'euc2d'
  } else if ((arq %in% tsp) && (arq %in% dMatrix)) {
    type <- 'tsp'
    data_type <- 'matrix'
  } else if (arq %in% atsp) {
    type <- 'atsp'
    data_type <- 'matrix'
  } else{
    type <- 'unavailable'
    data_type <- 'unavailable'
  }
  
  if ((data_type != 'unavailable') && (type != 'unavailable')) {
    #cat('Arquivo:', arq, '| Tipo:', type, '| Dados:', data_type, '\n')
    
    if (data_type == 'euc2d') {
      # Loading vectors with X and Y positions
      x <- rep(0, N)
      y <- rep(0, N)
      
      for (i in 1:N) {
        x[i] <- data[i, 2]
        y[i] <- data[i, 3]
      }
      
      d = matrix(0, N, N, T)# Distance matrix
      
      # Loading the matrix with distances and costs
      for (i in 1:N) {
        for (j in 1:N) {
          if (i != j) {
            Dx <- ((x[j] - x[i]) ** 2)
            Dy <- ((y[j] - y[i]) ** 2)
            d[i, j] <- (as.integer(sqrt(Dx + Dy)))
          } else{
            d[i, j] <- 9999
          }
        }
      }
      
    } else{
      d = data # Distance matrix
    }
    
    # Generating the learning, distance, and reinforcement matrices
    r = matrix(0, N, N, T) # Reinforcement matrix
    r <- (-d) # Load the rewards matrix
    
    ######################################################################################################################
    
    # Setting the Parameters
    alphas <- c(0.01, 0.15, 0.30, 0.45, 0.60, 0.75, 0.90, 0.99) # Learning rates
    gammas <- c(0.01, 0.15, 0.30, 0.45, 0.60, 0.75, 0.90, 0.99) # Discount factors
    e <- 0.01 # Greedy Policy
    epochs <- 5
    num_episodes <- 1000 # Number of episodes
    
    final_data <- matrix(0, nrow = ((length(alphas)) * (length(gammas)) * (epochs)), ncol = 3)
    
    q_learning <- function(alpha = 0.75, gamma = 0.15, e = 0.01, num_episodes = 1000, N, q) {
      count <- 1 # External counter
      initial_state <- sample(1:N, 1) # Set the initial state for the first execution
      state <- initial_state # Update the state to the initial state
      found_solution <- rep(0, N + 1)
      solutions <- matrix(0, nrow = num_episodes, ncol = N + 1) # Matrix to store the sequences of each episode
      
      while (count <= num_episodes) {
        # Runs the defined number of episodes
        
        chosen_states <- 1:N # Generates a vector with all available cities
        chosen_states[initial_state] <- 0 # Remove the initial state from the vector of available cities
        
        for (i in 1:N) {
          # Runs the number of nodes in the file
          
          available_states <- which(chosen_states != 0) # Returns the positions where the values are not 0, indicating the available cities
          
          row <- rep(0, length(available_states))
          for (k in 1:length(available_states)) {
            row[k] <- q[state, available_states[k]] # Receives the values ​​from matrix q where the states are available
          }
          
          if (i != N) {
            # If it's not the last state before returning to the initial one, there are still available actions
            
            e_random <- runif(1, 0, 1) # Randomly selects a value between 0 and 1
            
            if (e_random > e) {
              # Best action
              
              maximum <- max(row)# Selects the highest value from the row (best action for the current state)
              
              for (k in 1:length(available_states)) {
                aux <- available_states[k]
                if (maximum == q[state, aux]) {
                  action <- aux # Selects the position of the highest value in the row as the chosen action
                }
              }
            } else{
              # Random action
              
              if (length(available_states) == 1) {
                action <- available_states # If there's only one available state, that will be the chosen action
              } else {
                action <- sample(available_states, size = 1, replace = F)  # Randomly selects a value from the available ones
              }
            }
            
          } else {
            # If it's the last state before returning to the initial one, there are no more available actions, therefore the next action is the initial state
            action <- found_solution[1] # There are no other possible actions
          }
          
          future_state <- action # Update to the future state
          
          found_solution[i] <- state # Adds the current state to the vector of found solutions
          
          chosen_states[action] <- 0 # Removes the chosen action from the vector of available cities
          
          aux <- which(q[future_state, ] != 0) #Receives the positions that do not equal 0
          aux2 <- rep(0, length(aux))
          
          if (length(aux) != 0) {
            # Checks if "aux" is empty
            
            for (z in 1:length(aux)) {
              # Load "aux2" with the absolute values
              
              aux2[z] <- abs(q[future_state, aux[z]])
            }
            
            maxFut <- (min(aux2)) * (-1) #  Gets the smallest value from the row and multiplies it by -1
          } else{
            # If "aux" is zero, then the row only contains zeros
            maxFut <- max(q[future_state, ])
          }
          
          q[state, action] <- q[state, action]  + alpha * ((r[state, action]) + (gamma * maxFut) - q[state, action]) # Update the q matrix
          
          state <- future_state # Update the current state to the future state/chosen action
          
        }
        found_solution[length(found_solution)] <- found_solution[1] # Inserts the initial city as the final one as well
        solutions[count,] <- found_solution # Saves the found solution in the matrix containing the solutions found for each episode
        found_solution <- rep(0, N + 1)
        initial_state <- sample(1:N, size = 1, replace = F) # Chooses a random initial state
        state <- initial_state # Update the state to the initial state
        count <- count + 1 # Updates the external counter
      }
      
      return(solutions)
    }
    
    final_cost_function <- function(solutions, num_episodes) {
      final_cost <- rep(0, num_episodes)
      for (i in 1:((dim(solutions))[1])) {
        # Traverse the rows of the solutions matrix
        
        sequence <- solutions[i,] # Load the sequence vector with the solution from the current row to calculate its total distance
        dist <- rep(0, length(sequence) - 1)
        
        for (k in 1:(length(dist))) {
          dist[k] <- as.integer(d[sequence[k], sequence[k + 1]]) # Load the distance vector
        }
        
        final_cost[i] <- sum(dist) # Adds to the final cost vector the final cost of each episode
      }
      
      return(final_cost)
    }
    
    total_count <- 1
    for (alpha in alphas) {
      # Traverses the alpha vector
      for (gamma in gammas) {
        # Traverses the gamma vector
        for (ep in 1:epochs) {
          # Runs the defined number of epochs
          
          q = matrix(0, N, N, T)# Learning matrix
          
          solutions <- q_learning(alpha, gamma, e, num_episodes, N, q)
          
          final_cost <- final_cost_function(solutions, num_episodes)
          
          cat("\nAutoML - Progress: ", total_count,"/", length(alphas)*length(gammas)*epochs, sep = "")
          
          final_data[total_count, 1] <- alpha
          final_data[total_count, 2] <- gamma
          final_data[total_count, 3] <- min(final_cost)
          total_count <- total_count + 1
        }
      }
    }
    
    final_file <- data.frame(final_data)
    colnames(final_file) <- c('Alpha', 'Gamma', 'dist')
    
    model = lm(dist ~ Alpha + Gamma + Alpha * Gamma + I(Alpha ^ 2) + I(Gamma ^ 2), data = final_file)
    
    model_summary <- summary(model)
    
    anova_data <- anova(model)
    
    pks <- ks.test(resid(model), 'pnorm', mean(resid(model)), sd(resid(model)))
    
    #----------------------------------- Analysis of the Optimal Point (Canonical Analysis) --------------------
    
    anova_count <- 0 # Check for significance
    
    for (i in anova_data$`Pr(>F)`) {
      if ((!is.na(i)) && (i < 0.05)) {
        anova_count <- anova_count + 1
      }
    }
    
    fit <- rsm(dist ~ SO(Alpha, Gamma), data = final_file)
    
    aux <- canonical(fit)
    
    final_alpha <- aux$xs["Alpha"]
    final_gamma <- aux$xs["Gamma"]
    
    normal <- TRUE
    if ((pks$p.value < 0.05) ||
        (final_alpha < 0.01) ||
        (final_alpha > 1) ||
        (final_gamma < 0) || (final_gamma > 1) || (anova_count < 2)) {
      #cat("\nNon-normal residuals\n")
      smallest_dist <- min(final_file[, 3])
      min_position <- which.min(final_file[, 3])
      final_alpha <- final_file[min_position, 1]
      final_gamma <- final_file[min_position, 2]
      normal <- FALSE
    }
    
    # Part 2
    num_episodes <- 10000 # Number of episodes
    
    solutions_P2 <- matrix(0, nrow = epochs, ncol = N + 1)
    final_cost_M <- matrix(0, nrow = epochs, ncol = num_episodes)
    
    for (ep in 1:epochs) {
      # Runs the defined number of epochs
      q = matrix(0, N, N, T)# Learning matrix
      
      solutions <- q_learning(final_alpha, final_gamma, e, num_episodes, N, q)
      
      final_cost_M[ep,] <- final_cost_function(solutions, num_episodes)
      
      minimum <- min(final_cost_M[ep, ])
      position <- match(minimum, final_cost_M[ep, ])
      solutions_P2[ep,] <- solutions[position,]
      cat("\nFinal step - Progress: ", ep,"/", epochs, sep = "")
    }
    
    final_cost <- final_cost_function(solutions_P2, epochs)
    
    minimum <- min(final_cost)
    position <- match(minimum, final_cost)
    sequence <- solutions_P2[position,]
    
    xRoute <- NULL
    yRoute <- NULL
    
    if ((type == 'tsp') && (data_type == 'euc2d')) {
      # Loading vectors X and Y with cities in the sequence that forms the optimal path found
      xOrder <- rep(0, length(sequence))
      yOrder <- rep(0, length(sequence))
      for (i in 1:length(sequence)) {
        xOrder[i] <- x[sequence[i]]
        yOrder[i] <- y[sequence[i]]
      }
      
      # Plotting graph in order
      xRoute <- xOrder
      yRoute <- yOrder
    }
    
    end_time <- Sys.time()
    
    time <- difftime(end_time, start_time, units = "secs")
    units <- "secs"
    if(time > 60){
      time <- difftime(end_time, start_time, units = "mins")
      units <- "mins"
    } else if(time > 3600){
      time <- difftime(end_time, start_time, units = "hours")
      units <- "hours"
    }
    
    results <- list(
      episodes = num_episodes,
      dist_graph = final_cost_M[position, ],
      alpha = final_alpha,
      gamma = final_gamma,
      e_greedy = e,
      min_distance = min(final_cost_M[position, ]),
      avg_distance = mean(final_cost_M[position, ]),
      ep_dist_min = match(min(final_cost_M[position, ]), final_cost_M[position, ]),
      epoch_min_dist = position,
      epochs = epochs,
      xRoute = xRoute,
      yRoute = yRoute,
      model = model,
      time = time,
      units = units,
      df = final_file,
      normal = normal,
      model_summary = model_summary,
      anova_data = anova_data,
      pks = pks
    )
    
    return(results)
  }
}

process_sop_automl <- function(problem_data) {
  start_time <- Sys.time()
  D_SOP <- as.data.frame(sapply(problem_data, as.numeric))
  N_SOP <- length(D_SOP)        #  Number of nodes in the instance
  R_SOP <- (-D_SOP)               #  Reward function
  
  # Setting the Parameters
  alphas <- c(0.01, 0.15, 0.30, 0.45, 0.60, 0.75, 0.90, 0.99) # Learning rates
  gammas <- c(0.01, 0.15, 0.30, 0.45, 0.60, 0.75, 0.90, 0.99) # Discount factors
  e <- 0.01 # Greedy Policy
  epochs <- 5
  num_episodes <- 1000 # Number of episodes

  final_data <- matrix(0, nrow = ((length(alphas)) * (length(gammas)) * (epochs)), ncol = 3)
  
  SOPAA <- function(Q, SS, AA, R, e, greedy, bag, D) {
    bag2 <- bag
    bag2[AA] <- 0 #  bag2 stores the indices of cities already visited and also cities that cannot be visited at the moment
    rest2 <- which(bag2 != 0) #  The variable rest2 stores the cities that can still be selected
    
    ok <- 0 # Loop control
    while (ok == 0) {
      #  It will only exit the loop when it verifies that AA can be visited or until it finds a location that has no constraints
      ok <- 1 # ok = 1 is the exit control
      t <- length(rest2) # Size of rest2
      if (t > 0) {
        for (i in 1:t) {
          # Checks for the remaining set of cities if there are still precedence constraints for AA
          j <- rest2[i]
          
          if (D[AA, j] == -1) {
            # If there is a precedence constraint, then ok = 0
            ok <- 0
          }
        }
      }
      if (ok == 0) {
        # There is a precedence constraint for the selected action
        t <- length(rest2)
        if (greedy < e) {
          # Selects a new action randomly
          index <- round(runif(1, 1, t))
          AA <- rest2[index]
        } else{
          # Selects the best-estimated action in state SS
          
          rest2Q <- Q[SS, rest2] #  restQ receives values only from the Q matrix of the cities available in rest2
          max2Q <- which(rest2Q == max(rest2Q)) # Check the index of the highest value in the Q-Table - available cities
          AA <- rest2[max2Q[1]]   # The new action is the highest value from the Q-Table - Available cities
        }
        bag2[AA] <- 0 # Remove action AA from the list
        rest2 <- which(bag2 != 0) # Updates rest2 only with the available cities
      }
    }
    
    return(AA) # Returns the action selected by the SOPAA
  }# End of Function - SOPAA
  
  sarsa_SOP <- function(alpha = 0.75, gamma = 0.15, q, num_episodes = 1000) {
    count <- 1 # Episode counter
    So <- 1  # Initial State
    Ao <- sample(1:N_SOP, 1) # Initial Action
    S <- So
    A <- Ao
    distance<-rep(0, num_episodes)
    
    while (count <= (num_episodes)) {
      distance[count] <- 0 # Initial distance is zero
      bag <- 1:N_SOP           # Available cities
      bag[So] <- 0 # The initial city is accessed and becomes unavailable (receives 0)
      for (i in 1:N_SOP) {
        # Controls the visit to all cities
        bag[A] <- 0 # The destination city is accessed and becomes unavailable (receives 0)
        rest <- which(bag != 0) # Returns the index of the cities not visited
        if (length(rest) == 0) {
          # Checks if there are still cities to visit
          rest <- So # If there are none left: then return to the starting city
        }
        
        #--------------------------SARSA
        
        SS <- A     #  The new state is related to the selected action
        restQ <- q[SS, rest] # restQ receives values only from the Q matrix of the available cities
        largest_Q <- which(restQ == max(restQ)) # Check the index of the highest value in the Q-Table - available cities
        AA <- rest[largest_Q[1]]   # The new action is the highest value from the Q-Table - Available cities
        
        # Greedy Policy %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        
        rest_size <- length(rest)  # Size of rest: how many cities are available
        greedy <- runif(1, 0, 1)      # Generates a random number: [0, 1]
        if (greedy < e) {
          index <- round(runif(1, 1, rest_size))  # Generates a random index according to the size of the rest
          AA <- rest[index]                    # New random action - only among the available ones (rest)
        }
        
        #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        
        # Action Selection - RLSOP
        AA <- SOPAA(q, SS, AA, R_SOP, e, greedy, bag, D_SOP)
        
        # SARSA Update
        q[S, A] <- q[S, A] + alpha * (R_SOP[S, A] + gamma * q[SS, AA] - q[S, A])
        
        #--------------
        S <- SS  # State Update
        A <- AA  # Action Update
        
        if (D_SOP[S, A] == -1) {
          # Update the distance only if it's not a precedence constraint
          distance[count] = distance[count]
        } else{
          distance[count] = distance[count] + D_SOP[S, A]
        }
      }
      count <- count + 1
      # Episode counter update
      
    }
    return(distance)
  }
  
  ######################################################################################################################
  
  final_cost_function <- function(solutions, num_episodes) {
    final_cost <- rep(0, num_episodes)
    for (i in 1:((dim(solutions))[1])) {
      # Traverse the rows of the solutions matrix
      
      sequence <- solutions[i,] # Load the sequence vector with the solution from the current row to calculate its total distance
      dist <- rep(0, length(sequence) - 1)
      
      for (k in 1:(length(dist))) {
        dist[k] <- as.integer(D_SOP[sequence[k], sequence[k + 1]]) # Load the distance vector
      }
      
      final_cost[i] <- sum(dist) # Adds to the final cost vector the final cost of each episode
    }
    
    return(final_cost)
  }
  
  total_count <- 1
  for (alpha in alphas) {
    # Traverses the alpha vector
    for (gamma in gammas) {
      # Traverses the gamma vector
      for (ep in 1:epochs) {
        # Runs the defined number of epochs
        
        q = matrix(0, N_SOP, N_SOP, T)# Learning matrix
        
        final_cost <- sarsa_SOP(alpha, gamma, q, num_episodes)
        
        cat("\nAutoML - Progress: ", total_count,"/", length(alphas)*length(gammas)*epochs, sep = "")
        
        final_data[total_count, 1] <- alpha
        final_data[total_count, 2] <- gamma
        final_data[total_count, 3] <- min(final_cost)
        total_count <- total_count + 1
      }
    }
  }
  
  final_file <- data.frame(final_data)
  colnames(final_file) <- c('Alpha', 'Gamma', 'dist')
  
  model = lm(dist ~ Alpha + Gamma + Alpha * Gamma + I(Alpha ^ 2) + I(Gamma ^ 2), data = final_file)
  model_summary <- summary(model)
  
  anova_data <- anova(model)
  
  pks <- ks.test(resid(model), 'pnorm', mean(resid(model)), sd(resid(model)))
  
  #----------------------------------- Analysis of the Optimal Point (Canonical Analysis) --------------------
  
  anova_count <- 0 # Check for significance
  
  for (i in anova_data$`Pr(>F)`) {
    if ((!is.na(i)) && (i < 0.05)) {
      anova_count <- anova_count + 1
    }
  }
  
  fit <- rsm(dist ~ SO(Alpha, Gamma), data = final_file)
  
  aux <- canonical(fit)
  
  final_alpha <- aux$xs["Alpha"]
  final_gamma <- aux$xs["Gamma"]
  
  normal <- TRUE
  if ((pks$p.value < 0.05) ||
      (final_alpha < 0.01) ||
      (final_alpha > 1) ||
      (final_gamma < 0) || (final_gamma > 1) || (anova_count < 2)) {
    #cat("\nNon-normal residuals\n")
    smallest_dist <- min(final_file[, 3])
    min_position <- which.min(final_file[, 3])
    final_alpha <- final_file[min_position, 1]
    final_gamma <- final_file[min_position, 2]
    normal <- FALSE
  } 
  
  # Part 2
  num_episodes <- 10000 # Number of episodes

  solutions_P2 <-  rep(0, epochs) 
  final_cost_M <- matrix(0, nrow = epochs, ncol = num_episodes)
  
  for (ep in 1:epochs) {
    # Runs the defined number of epochs
    q = matrix(0, N_SOP, N_SOP, T)# Learning matrix
    
    final_cost_M[ep,] <- sarsa_SOP(alpha, gamma, q, num_episodes)
    
    minimum <- min(final_cost_M[ep, ])
    position <- match(minimum, final_cost_M[ep, ])
    solutions_P2[ep] <- minimum
    cat("\nFinal step - Progress: ", ep,"/", epochs, sep = "")
  }
  
  minimum <- min(solutions_P2)
  position <- match(minimum, solutions_P2)
  
  end_time <- Sys.time()
  
  time <- difftime(end_time, start_time, units = "secs")
  units <- "secs"
  if(time > 60){
    time <- difftime(end_time, start_time, units = "mins")
    units <- "mins"
  } else if(time > 3600){
    time <- difftime(end_time, start_time, units = "hours")
    units <- "hours"
  }
  
  results <- list(
    episodes = num_episodes,
    dist_graph = final_cost_M[position, ],
    alpha = final_alpha,
    gamma = final_gamma,
    e_greedy = e,
    min_distance = min(final_cost_M[position, ]),
    avg_distance = mean(final_cost_M[position, ]),
    ep_dist_min = match(min(final_cost_M[position, ]), final_cost_M[position, ]),
    epoch_min_dist = position,
    epochs = epochs,
    model = model,
    time = time,
    units = units,
    df = final_file,
    normal = normal,
    model_summary = model_summary,
    anova_data = anova_data,
    pks = pks
  )
  
  return(results)
  
}

process_tsp_atsp_user <- function(data, user_alpha, user_gamma, e_greedy, user_episodes, type, data_type) {
  start_time <- Sys.time()
  N <- ((lengths(data))[1]) # Number of nodes 
  
  if (data_type == 'euc2d') {
    # Loading vectors with X and Y positions
    x <- rep(0, N)
    y <- rep(0, N)
    
    for (i in 1:N) {
      x[i] <- if (ncol(data) == 2) data[i, 1] else data[i, 2]
      y[i] <- if (ncol(data) == 2) data[i, 2] else data[i, 3]
    }
    
    d = matrix(0, N, N, T)# Distance matrix
    
    # Loading the matrix with distances and costs
    for (i in 1:N) {
      for (j in 1:N) {
        if (i != j) {
          Dx <- ((x[j] - x[i]) ** 2)
          Dy <- ((y[j] - y[i]) ** 2)
          d[i, j] <- (as.integer(sqrt(Dx + Dy)))
        } else{
          d[i, j] <- 9999
        }
      }
    }
    
  } else{
    d = data # Distance matrix
  }
  
  # Generating the learning, distance, and reinforcement matrices
  r = matrix(0, N, N, T) # Reinforcement matrix
  r <- (-d) # Load the rewards matrix
  
  ######################################################################################################################
  
  # Setting the Parameters
  e <- e_greedy # Greedy Policy
  epochs <- 1
  num_episodes <- user_episodes
  final_alpha <- user_alpha
  final_gamma <- user_gamma
  
  q_learning <- function(alpha = 0.75, gamma = 0.15, e = 0.01, num_episodes = 1000, N, q) {
    count <- 1 # External counter
    initial_state <- sample(1:N, 1) # Set the initial state for the first execution
    state <- initial_state # Update the state to the initial state
    found_solution <- rep(0, N + 1)
    solutions <- matrix(0, nrow = num_episodes, ncol = N + 1) # Matrix to store the sequences of each episode
    
    while (count <= num_episodes) {
      # Runs the defined number of episodes
      cat("\nProgress: ", count,"/", num_episodes, sep = "")
      
      chosen_states <- 1:N # Generates a vector with all available cities
      chosen_states[initial_state] <- 0 # Remove the initial state from the vector of available cities
      
      for (i in 1:N) {
        # Runs the number of nodes in the file
        
        available_states <- which(chosen_states != 0) # Returns the positions where the values are not 0, indicating the available cities
        
        row <- rep(0, length(available_states))
        for (k in 1:length(available_states)) {
          row[k] <- q[state, available_states[k]] # Receives the values ​​from matrix q where the states are available
        }
        
        if (i != N) {
          # If it's not the last state before returning to the initial one, there are still available actions
          
          e_random <- runif(1, 0, 1) # Randomly selects a value between 0 and 1
          
          if (e_random > e) {
            # Best action
            
            maximum <- max(row)# Selects the highest value from the row (best action for the current state)
            
            for (k in 1:length(available_states)) {
              aux <- available_states[k]
              if (maximum == q[state, aux]) {
                action <- aux # Selects the position of the highest value in the row as the chosen action
              }
            }
          } else{
            # Random action
            
            if (length(available_states) == 1) {
              action <- available_states # If there's only one available state, that will be the chosen action
            } else {
              action <- sample(available_states, size = 1, replace = F)  # Randomly selects a value from the available ones
            }
          }
          
        } else {
          # If it's the last state before returning to the initial one, there are no more available actions, therefore the next action is the initial state
          action <- found_solution[1] # There are no other possible actions
        }
        
        future_state <- action # Update to the future state
        
        found_solution[i] <- state # Adds the current state to the vector of found solutions
        
        chosen_states[action] <- 0 # Removes the chosen action from the vector of available cities
        
        aux <- which(q[future_state, ] != 0) #Receives the positions that do not equal 0
        aux2 <- rep(0, length(aux))
        
        if (length(aux) != 0) {
          # Checks if "aux" is empty
          
          for (z in 1:length(aux)) {
            # Load "aux2" with the absolute values
            
            aux2[z] <- abs(q[future_state, aux[z]])
          }
          
          maxFut <- (min(aux2)) * (-1) #  Gets the smallest value from the row and multiplies it by -1
        } else{
          # If "aux" is zero, then the row only contains zeros
          maxFut <- max(q[future_state, ])
        }
        
        q[state, action] <- q[state, action]  + alpha * ((r[state, action]) + (gamma * maxFut) - q[state, action]) # Update the q matrix
        
        state <- future_state # Update the current state to the future state/chosen action
        
      }
      found_solution[length(found_solution)] <- found_solution[1] # Inserts the initial city as the final one as well
      solutions[count,] <- found_solution # Saves the found solution in the matrix containing the solutions found for each episode
      found_solution <- rep(0, N + 1)
      initial_state <- sample(1:N, size = 1, replace = F) # Chooses a random initial state
      state <- initial_state # Update the state to the initial state
      count <- count + 1 # Updates the external counter
    }
    
    return(solutions)
  }
  
  final_cost_function <- function(solutions, num_episodes) {
    final_cost <- rep(0, num_episodes)
    for (i in 1:((dim(solutions))[1])) {
      # Traverse the rows of the solutions matrix
      
      sequence <- solutions[i,] # Load the sequence vector with the solution from the current row to calculate its total distance
      dist <- rep(0, length(sequence) - 1)
      
      for (k in 1:(length(dist))) {
        dist[k] <- as.integer(d[sequence[k], sequence[k + 1]]) # Load the distance vector
      }
      
      final_cost[i] <- sum(dist) # Adds to the final cost vector the final cost of each episode
    }
    
    return(final_cost)
  }
  
  solutions_P2 <- matrix(0, nrow = epochs, ncol = N + 1)
  final_cost_M <- matrix(0, nrow = epochs, ncol = num_episodes)
  
  for (ep in 1:epochs) {
    # Runs the defined number of epochs
    q = matrix(0, N, N, T) # Learning matrix
    
    solutions <- q_learning(final_alpha, final_gamma, e, num_episodes, N, q)
    
    final_cost_M[ep,] <- final_cost_function(solutions, num_episodes)
    
    minimum <- min(final_cost_M[ep, ])
    position <- match(minimum, final_cost_M[ep, ])
    solutions_P2[ep,] <- solutions[position,]
  }
  
  final_cost <- final_cost_function(solutions_P2, epochs)
  
  minimum <- min(final_cost)
  position <- match(minimum, final_cost)
  sequence <- solutions_P2[position,]
  
  # Displays the number of episodes, the smallest distance found, and the largest
  
  xRoute <- NULL
  yRoute <- NULL
  if ((type == 'tsp') && (data_type == 'euc2d')) {
    # Loading vectors X and Y with cities in the sequence that forms the optimal path found
    xOrder <- rep(0, length(sequence))
    yOrder <- rep(0, length(sequence))
    for (i in 1:length(sequence)) {
      xOrder[i] <- x[sequence[i]]
      yOrder[i] <- y[sequence[i]]
    }
    
    # Plotting graph in order
    xRoute <- xOrder
    yRoute <- yOrder
  }
  
  end_time <- Sys.time()
  
  time <- difftime(end_time, start_time, units = "secs")
  units <- "secs"
  if(time > 60){
    time <- difftime(end_time, start_time, units = "mins")
    units <- "mins"
  } else if(time > 3600){
    time <- difftime(end_time, start_time, units = "hours")
    units <- "hours"
  }
  
  results <- list(
    dist_graph = final_cost_M[position, ],
    min_distance = min(final_cost_M[position, ]),
    avg_distance = mean(final_cost_M[position, ]),
    ep_dist_min = match(min(final_cost_M[position, ]), final_cost_M),
    time = time,
    units = units,
    xRoute = xRoute,
    yRoute = yRoute
  )
  
  return(results)
}

process_tsp_atsp_user_automl <- function(data, type, data_type) {
  start_time <- Sys.time()
  N <- ((lengths(data))[1]) # Number of nodes
    
  if (data_type == 'euc2d') {
    # Loading vectors with X and Y positions
    x <- rep(0, N)
    y <- rep(0, N)
    
    for (i in 1:N) {
      x[i] <- if (ncol(data) == 2) data[i, 1] else data[i, 2]
      y[i] <- if (ncol(data) == 2) data[i, 2] else data[i, 3]
    }
    
    d = matrix(0, N, N, T)# Distance matrix
    
    # Loading the matrix with distances and costs
    for (i in 1:N) {
      for (j in 1:N) {
        if (i != j) {
          Dx <- ((x[j] - x[i]) ** 2)
          Dy <- ((y[j] - y[i]) ** 2)
          d[i, j] <- (as.integer(sqrt(Dx + Dy)))
        } else{
          d[i, j] <- 9999
        }
      }
    }
    
  } else{
    d = data # Distance matrix
  }
  
  # Generating the learning, distance, and reinforcement matrices
  r = matrix(0, N, N, T) # Reinforcement matrix
  r <- (-d) # Load the rewards matrix
  
  ######################################################################################################################
  
  # Setting the Parameters
  alphas <- c(0.01, 0.15, 0.30, 0.45, 0.60, 0.75, 0.90, 0.99) # Learning rates
  gammas <- c(0.01, 0.15, 0.30, 0.45, 0.60, 0.75, 0.90, 0.99) # Discount factors
  e <- 0.01 # Greedy Policy
  epochs <- 5
  num_episodes <- 1000 # Number of episodes

  final_data <- matrix(0, nrow = ((length(alphas)) * (length(gammas)) * (epochs)), ncol = 3)
  
  q_learning <- function(alpha = 0.75, gamma = 0.15, e = 0.01, num_episodes = 1000, N, q) {
    count <- 1 # External counter
    initial_state <- sample(1:N, 1) # Set the initial state for the first execution
    state <- initial_state # Update the state to the initial state
    found_solution <- rep(0, N + 1)
    solutions <- matrix(0, nrow = num_episodes, ncol = N + 1) # Matrix to store the sequences of each episode
    
    while (count <= num_episodes) {
      # Runs the defined number of episodes
      
      chosen_states <- 1:N # Generates a vector with all available cities
      chosen_states[initial_state] <- 0 # Remove the initial state from the vector of available cities
      
      for (i in 1:N) {
        # Runs the number of nodes in the file
        
        available_states <- which(chosen_states != 0) # Returns the positions where the values are not 0, indicating the available cities
        
        row <- rep(0, length(available_states))
        for (k in 1:length(available_states)) {
          row[k] <- q[state, available_states[k]] # Receives the values ​​from matrix q where the states are available
        }
        
        if (i != N) {
          # If it's not the last state before returning to the initial one, there are still available actions
          
          e_random <- runif(1, 0, 1) # Randomly selects a value between 0 and 1
          
          if (e_random > e) {
            # Best action
            
            maximum <- max(row)# Selects the highest value from the row (best action for the current state)
            
            for (k in 1:length(available_states)) {
              aux <- available_states[k]
              if (maximum == q[state, aux]) {
                action <- aux # Selects the position of the highest value in the row as the chosen action
              }
            }
          } else{
            # Random action
            
            if (length(available_states) == 1) {
              action <- available_states # If there's only one available state, that will be the chosen action
            } else {
              action <- sample(available_states, size = 1, replace = F)  # Randomly selects a value from the available ones
            }
          }
          
        } else {
          # If it's the last state before returning to the initial one, there are no more available actions, therefore the next action is the initial state
          action <- found_solution[1] # There are no other possible actions
        }
        
        future_state <- action # Update to the future state
        
        found_solution[i] <- state # Adds the current state to the vector of found solutions
        
        chosen_states[action] <- 0 # Removes the chosen action from the vector of available cities
        
        aux <- which(q[future_state, ] != 0) #Receives the positions that do not equal 0
        aux2 <- rep(0, length(aux))
        
        if (length(aux) != 0) {
          # Checks if "aux" is empty
          
          for (z in 1:length(aux)) {
            # Load "aux2" with the absolute values
            
            aux2[z] <- abs(q[future_state, aux[z]])
          }
          
          maxFut <- (min(aux2)) * (-1) #  Gets the smallest value from the row and multiplies it by -1
        } else{
          # If "aux" is zero, then the row only contains zeros
          maxFut <- max(q[future_state, ])
        }
        
        q[state, action] <- q[state, action]  + alpha * ((r[state, action]) + (gamma * maxFut) - q[state, action]) # Update the q matrix
        
        state <- future_state # Update the current state to the future state/chosen action
        
      }
      found_solution[length(found_solution)] <- found_solution[1] # Inserts the initial city as the final one as well
      solutions[count,] <- found_solution # Saves the found solution in the matrix containing the solutions found for each episode
      found_solution <- rep(0, N + 1)
      initial_state <- sample(1:N, size = 1, replace = F) # Chooses a random initial state
      state <- initial_state # Update the state to the initial state
      count <- count + 1 # Updates the external counter
    }
    
    return(solutions)
  }
  
  final_cost_function <- function(solutions, num_episodes) {
    final_cost <- rep(0, num_episodes)
    for (i in 1:((dim(solutions))[1])) {
      # Traverse the rows of the solutions matrix
      
      sequence <- solutions[i,] # Load the sequence vector with the solution from the current row to calculate its total distance
      dist <- rep(0, length(sequence) - 1)
      
      for (k in 1:(length(dist))) {
        dist[k] <- as.integer(d[sequence[k], sequence[k + 1]]) # Load the distance vector
      }
      
      final_cost[i] <- sum(dist) # Adds to the final cost vector the final cost of each episode
    }
    
    return(final_cost)
  }
  
  total_count <- 1
  for (alpha in alphas) {
    # Traverses the alpha vector
    for (gamma in gammas) {
      # Traverses the gamma vector
      for (ep in 1:epochs) {
        # Runs the defined number of epochs
        
        q = matrix(0, N, N, T)# Learning matrix
        
        solutions <- q_learning(alpha, gamma, e, num_episodes, N, q)
        
        final_cost <- final_cost_function(solutions, num_episodes)
        
        cat("\nAutoML - Progress: ", total_count,"/", length(alphas)*length(gammas)*epochs, sep = "")
        
        final_data[total_count, 1] <- alpha
        final_data[total_count, 2] <- gamma
        final_data[total_count, 3] <- min(final_cost)
        total_count <- total_count + 1
      }
    }
  }
  
  final_file <- data.frame(final_data)
  colnames(final_file) <- c('Alpha', 'Gamma', 'dist')
  
  model = lm(dist ~ Alpha + Gamma + Alpha * Gamma + I(Alpha ^ 2) + I(Gamma ^ 2), data = final_file)
  
  model_summary <- summary(model)
  
  anova_data <- anova(model)
  
  pks <- ks.test(resid(model), 'pnorm', mean(resid(model)), sd(resid(model)))
  
  #----------------------------------- Analysis of the Optimal Point (Canonical Analysis) --------------------
  
  anova_count <- 0 # Check for significance
  
  for (i in anova_data$`Pr(>F)`) {
    if ((!is.na(i)) && (i < 0.05)) {
      anova_count <- anova_count + 1
    }
  }
  
  fit <- rsm(dist ~ SO(Alpha, Gamma), data = final_file)
  
  aux <- canonical(fit)
  
  final_alpha <- aux$xs["Alpha"]
  final_gamma <- aux$xs["Gamma"]
  
  normal <- TRUE
  if ((pks$p.value < 0.05) ||
      (final_alpha < 0.01) ||
      (final_alpha > 1) ||
      (final_gamma < 0) || (final_gamma > 1) || (anova_count < 2)) {
    #cat("\nNon-normal residuals\n")
    smallest_dist <- min(final_file[, 3])
    min_position <- which.min(final_file[, 3])
    final_alpha <- final_file[min_position, 1]
    final_gamma <- final_file[min_position, 2]
    normal <- FALSE
  }
  
  # Part 2
  num_episodes <- 10000 # Number of episodes

  solutions_P2 <- matrix(0, nrow = epochs, ncol = N + 1)
  final_cost_M <- matrix(0, nrow = epochs, ncol = num_episodes)
  
  for (ep in 1:epochs) {
    # Runs the defined number of epochs
    q = matrix(0, N, N, T)# Learning matrix
    
    solutions <- q_learning(final_alpha, final_gamma, e, num_episodes, N, q)
    
    final_cost_M[ep,] <- final_cost_function(solutions, num_episodes)
    
    minimum <- min(final_cost_M[ep, ])
    position <- match(minimum, final_cost_M[ep, ])
    solutions_P2[ep,] <- solutions[position,]
    cat("\nFinal step - Progress: ", ep,"/", epochs, sep = "")
  }
  
  final_cost <- final_cost_function(solutions_P2, epochs)
  
  minimum <- min(final_cost)
  position <- match(minimum, final_cost)
  sequence <- solutions_P2[position,]
  
  xRoute <- NULL
  yRoute <- NULL
  
  if ((type == 'tsp') && (data_type == 'euc2d')) {
    # Loading vectors X and Y with cities in the sequence that forms the optimal path found
    xOrder <- rep(0, length(sequence))
    yOrder <- rep(0, length(sequence))
    for (i in 1:length(sequence)) {
      xOrder[i] <- x[sequence[i]]
      yOrder[i] <- y[sequence[i]]
    }
    
    # Plotting graph in order
    xRoute <- xOrder
    yRoute <- yOrder
  }
  
  end_time <- Sys.time()
  
  time <- difftime(end_time, start_time, units = "secs")
  units <- "secs"
  if(time > 60){
    time <- difftime(end_time, start_time, units = "mins")
    units <- "mins"
  } else if(time > 3600){
    time <- difftime(end_time, start_time, units = "hours")
    units <- "hours"
  }
  
  results <- list(
    episodes = num_episodes,
    dist_graph = final_cost_M[position, ],
    alpha = final_alpha,
    gamma = final_gamma,
    e_greedy = e,
    min_distance = min(final_cost_M[position, ]),
    avg_distance = mean(final_cost_M[position, ]),
    ep_dist_min = match(min(final_cost_M[position, ]), final_cost_M[position, ]),
    epoch_min_dist = position,
    epochs = epochs,
    xRoute = xRoute,
    yRoute = yRoute,
    model = model,
    time = time,
    units = units,
    df = final_file,
    normal = normal,
    model_summary = model_summary,
    anova_data = anova_data,
    pks = pks
  )
  
  return(results)
}