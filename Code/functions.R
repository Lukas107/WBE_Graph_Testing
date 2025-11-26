library(readxl)
library(data.tree)
library(dplyr)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(ggplot2)
library(patchwork)
library(rlang)
library(xtable)

# Tree Stuff ####

# Recursively add children to node
add_children <- function(parent_node, parent_label) {
  child_labels <- names(which(adj_matrix[parent_label, ] == 1))
  for (child_label in child_labels) {
    if (!child_label %in% names(rpop_map)) {
      stop("Error: Child string is not part of Grundlagendaten")
    }
    child_rpop <- rpop_map[[child_label]]
    child <- parent_node$AddChild(child_label, rpop = child_rpop,
                                  contamination = FALSE)
    add_children(child, child_label)
  }
}


# Compute cum_pop
compute_cum_pop <- function(root) {
  # compute absolute cumulative mass bottom-up
  root$Do(function(n) {
    n$cum_abs <- n$rpop +
      sum(vapply(n$children, function(ch) ch$cum_abs, numeric(1)))},
    traversal = "post-order")
  
  # normalize within this subtree so the subtree root has cum_pop = 1
  total <- root$cum_abs
  if (is.na(total) || total <= 0) stop("Subtree has zero total rpop.")
  root$Do(function(n) n$cum_pop <- n$cum_abs / total)
}


# set_up_tree function
set_up_tree <- function(contaminated_node="KLA_1") {
  # Root should be first name in both label vectors
  root_name <- col_labels[1]
  
  # Root gets rpop = 0 because it's not in Grundlagendaten
  root <- Node$new(root_name, rpop=0, contamination=FALSE)
  
  add_children(root, root_name)
  
  # Rename according to mapping
  name_map <- setNames(rename_df$new, rename_df$old)
  root$Do(function(node) {
    if (node$name %in% names(name_map)) {
      node$name <- name_map[[node$name]]
    }
  })
  
  # Calculate cumulative populations per subtree
  compute_cum_pop(root)
  

  picked_name = contaminated_node
  picked_node <- FindNode(root, picked_name)
  
  # Flag the picked node and all its ancestors as contaminated
  current_node <- picked_node
  while (!is.null(current_node)) {
    current_node$contamination = TRUE
    current_node <- current_node$parent
  }
  return(root)
}



# Strategies ####

# Always choose nodes with highest cumulative population
max_cum_pop <- function(root, number_testers) {
  nodes <- Traverse(root)
  
  # The root node is always assumed to be contaminated, no strategy necessary
  if (root$totalCount <= 1) stop("Passed too few nodes to nary_split")
  
  # If only the root exists, nothing to test
  if (length(nodes) <= 1) {
    return(list(root))
  }
  
  # 1) Exclude current root from candidates
  candidate_nodes <- nodes[-1]
  
  # 2) Compute scores (cumulative population)
  cumulative_scores <- vapply(candidate_nodes, function(node) node$cum_pop,
                              numeric(1))
  
  # 3) Order candidates by descending cumulative population
  ranked_indices <- order(cumulative_scores, decreasing = TRUE)
  
  # 4) Select the number of testers we can actually use
  num_nodes_to_select <- min(number_testers, length(candidate_nodes))
  
  # 5) Pick the top nodes
  selected_nodes <- candidate_nodes[ranked_indices[seq_len(num_nodes_to_select)]]
  
  return(selected_nodes)
}


# Choose nodes with highest cum_pop, but always skip a level
skipping_cum_pop <- function(root, number_testers) {
  nodes <- Traverse(root)
  
  # The root node is always assumed to be contaminated, no strategy necessary
  if (root$totalCount <= 1) stop("Passed too few nodes to nary_split")
  
  candidate_nodes <- nodes[-1]
  candidate_names <- vapply(candidate_nodes, function(x) x$name, character(1))
  
  # Rank by cum_pop
  cum_pop_scores  <- vapply(candidate_nodes, function(n) n$cum_pop, numeric(1))
  ranked_indices  <- order(cum_pop_scores, decreasing = TRUE)
  
  # Required number of nodes that should be returned
  max_to_pick <- min(number_testers, length(candidate_nodes))
  
  # The result will be saved here
  selected_nodes <- vector("list", max_to_pick)
  
  # children of already-selected nodes
  skipped_names <- character(0)
  
  for (i in 1:max_to_pick) {
    # first try to find the highest-ranked NOT skipped
    not_skipped_mask <- !(candidate_names[ranked_indices] %in% skipped_names)
    
    if (any(not_skipped_mask)) {
      picked_node_index <- ranked_indices[not_skipped_mask][1]
    }
    # if none left, allow skipped nodes
    else {
      picked_node_index <- ranked_indices[1]
    }
    
    picked_node <- candidate_nodes[[picked_node_index]]
    selected_nodes[[i]] <- picked_node
    
    # mark the selected node's children as skipped
    if (length(picked_node$children) > 0) {
      child_names <- vapply(picked_node$children, function(ch) ch$name, character(1))
      skipped_names <- c(skipped_names, child_names)
    }
    
    # remove this index, so it is not picked again
    ranked_indices <- ranked_indices[ranked_indices != picked_node_index]
  }
  return(selected_nodes)
}


# Always choose nodes with highest population
max_rpop <- function(root, number_testers) {
  nodes <- Traverse(root)
  
  # The root node is always assumed to be contaminated, no strategy necessary
  if (root$totalCount <= 1) stop("Passed too few nodes to nary_split")
  
  # 1) Exclude current root from candidates
  candidate_nodes <- nodes[-1]
  
  # 2) Compute scores (raw population)
  raw_population_scores <- vapply(
    candidate_nodes,
    function(node) node$rpop,
    numeric(1)
  )
  
  # 3) Order candidates by descending raw population
  ranked_indices <- order(raw_population_scores, decreasing = TRUE)
  
  # 4) Select the number of testers we can actually use
  num_nodes_to_select <- min(number_testers, length(candidate_nodes))
  
  # 5) Pick the top nodes
  selected_nodes <- candidate_nodes[ranked_indices[seq_len(num_nodes_to_select)]]
  
  return(selected_nodes)
}


# Helper function for nary_split
mark_excluded <- function(node) {
  node$excluded <- TRUE
  if (length(node$children) > 0) {
    lapply(node$children, mark_excluded)
  }
}

# Try to split probability mass in 1/(number_testers+1)
nary_split <- function(root, number_testers) {
  
  # The root node is always assumed to be contaminated, no strategy necessary
  if (root$totalCount <= 1) stop("Passed too few nodes to nary_split")
  
  nodes <- Traverse(root)
  
  # Initialize working fields
  root$Set(nary_cum_pop = root$Get("cum_pop"), excluded = FALSE)
  
  # Calculate number of tests that can be performed this step
  num_nodes_to_select <- min(number_testers, length(nodes))
  
  # These will later be used to store results
  selected_names <- character(num_nodes_to_select)
  chosen_nodes   <- vector("list", num_nodes_to_select)
  
  for (i in 1:number_testers) {
    # Get all nodes that are fit for nary splitting
    candidates <- Filter(function(n) {
      !identical(n, root) && !n$excluded &&!(n$name %in% selected_names)
      }, nodes)
    
    # Main part: Try to split into n parts
    if (length(candidates) >= 1) {
      
      # Try to give each partition the same probability mass
      target <- 1 / (number_testers+1)
      
      # Choose non-excluded node closest to target
      scores <- vapply(candidates, function(node) {-abs(node$nary_cum_pop - target)},
                       numeric(1))
      chosen_node <- candidates[[which.max(scores)]]
      chosen_nodes[[i]] <- chosen_node
      selected_names[i] <- chosen_node$name
      
      # Subtract its mass from all ancestors
      mass <- chosen_node$nary_cum_pop
      parent <- chosen_node$parent
      while (!is.null(parent)) {
        parent$nary_cum_pop <- parent$nary_cum_pop - mass
        parent <- parent$parent
      }
      
      # Exclude subtree, so we don't pick it again
      mark_excluded(chosen_node)
    }
    # Fallback: If all remaining nodes have been excluded, choose highest rpop
    # nodes from excluded
    else {
      # Get excluded nodes that have not already been picked
      excluded <- Filter(function(n) !identical(n, root) && n$excluded &&
                           !(n$name %in% selected_names), nodes)
      
      # If there are no excluded nodes left, we are finished
      if (length(excluded) == 0) break
      
      # Pick excluded node with highest rpop 
      r_pops <- vapply(excluded, function(n) n$rpop, FUN.VALUE=numeric(1))
      chosen_node <- excluded[[which.max(r_pops)]]
      chosen_nodes[[i]] <- chosen_node
      selected_names[i] <- chosen_node$name
    }
  }
  
  return(chosen_nodes)
}



# Code for testing strategies ####

# Calculate needed testing iterations, for a tree with a specified contamination
needed_testing_iterations <- function(root, strategy, number_testers = 1,
                                      verbose = FALSE) {
  test_count  <- 0L
  current_node <- root
  
  while (current_node$height > 1) {
    
    tested_nodes <- strategy(current_node, number_testers = number_testers)
    
    if (verbose) {
      cat("Tested nodes:\n")
      print(tested_nodes)
    }
    
    test_count <- test_count + 1
    
    # Nodes that are actually contaminated
    contaminated_nodes <- Filter(function(n) isTRUE(n$contamination), tested_nodes)
    
    if (length(contaminated_nodes) > 0) {
      if (verbose) print("Hit")
      
      # Pick the deepest contaminated node (unique if there is only one contamination)
      contaminated_levels <- vapply(contaminated_nodes, function(x) x$level, numeric(1))
      deepest_idx   <- which.max(contaminated_levels)
      deepest_node  <- contaminated_nodes[[deepest_idx]]
      
      # Continue from the deepest contaminated node
      current_node <- deepest_node
      
      # Renormalise cum_pop within this subtree
      compute_cum_pop(current_node)
      
    }
    else {
      if (verbose) print("Miss")
      
      # Remove all tested subtrees from the current tree
      lapply(tested_nodes, function(n) {
        parent <- n$parent
        if (!is.null(parent)) {
          parent$RemoveChild(n$name)
        }
      })
      
      # Renormalise cum_pop on the remaining tree
      compute_cum_pop(current_node)
    }
  }
  
  if (verbose) {
    cat("Test Count was equal to:\n")
    print(test_count)
  }
  
  return(test_count)
}

# Analytically derive distribution for a strat with one sample taker
cdf_length = 19
get_strat_pmf = function(strategy,  number_testers=1, verbose=TRUE) {
  probs = rep(0, cdf_length)
  if(verbose) {
    print("Testers:")
    print(number_testers)
    }
  
  for (node_name in col_labels) {
    root = set_up_tree(contaminated_node=node_name)
    
    node_rpop = FindNode(root, node_name)$rpop
    # Only consider nodes which could actually be contaminated
    if(node_rpop == 0) next
    
    test_count = needed_testing_iterations(root, strategy,
        number_testers=number_testers)
    probs[test_count] = probs[test_count] + node_rpop
  }
  return(probs)
}


# Needed total tests, for a tree with a specified contamination
needed_total_tests <- function(root, strategy, number_testers = 1,
                               verbose = FALSE) {
  total_tests  <- 0L
  current_node <- root
  
  while (current_node$height > 1) {
    tested_nodes <- strategy(current_node, number_testers = number_testers)
    
    # Add number of nodes tested in this iteration
    total_tests <- total_tests + length(tested_nodes)
    
    if (verbose) {
      cat("Tested nodes:\n")
      cat(vapply(tested_nodes, function(n) n$name, character(1)), sep = "\n")
      cat("Total tests so far:", total_tests, "\n")
    }
    
    # Check which tested nodes are contaminated
    contaminated_nodes <- Filter(function(n) isTRUE(n$contamination),
                                 tested_nodes)
    
    if (length(contaminated_nodes) > 0L) {
      if (verbose) cat("Hit\n")
      
      # Pick deepest contaminated node
      contaminated_levels <- vapply(contaminated_nodes, function(x) x$level, numeric(1))
      deepest_idx   <- which.max(contaminated_levels)
      deepest_node  <- contaminated_nodes[[deepest_idx]]
      
      # Continue from that node
      current_node <- deepest_node
      compute_cum_pop(current_node)
      
    } else {
      if (verbose) cat("Miss\n")
      
      # Remove all tested nodes
      lapply(tested_nodes, function(n) {
        parent <- n$parent
        if (!is.null(parent)) parent$RemoveChild(n$name)
      })
      compute_cum_pop(current_node)
    }
  }
  
  if (verbose) cat("Total tests used:", total_tests, "\n")
  return(total_tests)
}


# Calculate average tests needed by a particular strategy
average_total_tests <- function(strategy, number_testers = 1, verbose = TRUE) {
  weighted_sum <- 0
  weight_total <- 0
  
  # Add up needed tests over all nodes
  for (node_name in col_labels) {
    root <- set_up_tree(contaminated_node = node_name)
    node <- FindNode(root, node_name)
    node_rpop <- if (!is.null(node)) node$rpop else 0
    
    # Only consider valid contamination nodes
    if (is.na(node_rpop) || node_rpop == 0) next
    
    total_tests <- needed_total_tests(root, strategy,
                                      number_testers=number_testers)
    # Add needed tests to result
    weighted_sum  <- weighted_sum + total_tests * node_rpop
  }
  return(weighted_sum)
}



# Strat result summarization ####

cdf_quantile = function(quantile, cdf){
  return(findInterval(quantile, cdf) + 1)
}

mean_tests = function(pmf) {
  cdf_length = length(pmf)
  return(sum(1:cdf_length * pmf))
}

print_strat_summaries = function() {
  # Binary splitting
  mean_binary = sum(nary_split_pmfs[1] * 1:19)
  print("Binary strat:")
  print(sprintf("Min: %d | First Quartile: %d | Median: %d | Mean: %f | Third Quartile: %d | Max: %d",
                cdf_quantile(0, nary_split_cdfs[1]), cdf_quantile(0.25, nary_split_cdfs[1]),
                cdf_quantile(0.5, nary_split_cdfs[1]), mean_tests(nary_split_cdfs[[1]]),
                cdf_quantile(0.75, nary_split_cdfs[1]),
                cdf_quantile(0.99999, nary_split_cdfs[1])))
  
  # cum_pop 
  mean_cum_pop = sum(cum_pop_strat_pmf * 1:19)
  print("cum_pop strat:")
  print(sprintf("Min: %d | First Quartile: %d | Median: %d | Mean: %f | Third Quartile: %d | Max: %d",
                cdf_quantile(0, cum_pop_strat_cdf), cdf_quantile(0.25, cum_pop_strat_cdf),
                cdf_quantile(0.5, cum_pop_strat_cdf), mean_cum_pop,
                cdf_quantile(0.75, cum_pop_strat_cdf),
                cdf_quantile(0.99999, cum_pop_strat_cdf)))
  
  mean_rpop = sum(rpop_strat_pmf * 1:19)
  print("Rpop strat:")
  print(sprintf("Min: %d | First Quartile: %d | Median: %i | Mean: %f | Third Quartile: %d | Max: %d",
                cdf_quantile(0, rpop_strat_cdf), cdf_quantile(0.25, rpop_strat_cdf),
                cdf_quantile(0.5, rpop_strat_cdf), mean_rpop,
                cdf_quantile(0.75, rpop_strat_cdf),
                cdf_quantile(0.99999, rpop_strat_cdf)))
}