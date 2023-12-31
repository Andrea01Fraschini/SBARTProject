#' Get terminal nodes
#' 
#' Given a tree, returns the indexes for terminal nodes. 
#'  
#' @param tree A decision tree.
#' 
#' @return a vector of indexes of the terminal nodes. 
#' @export 
get_terminal_nodes <- function(tree) {
    terminal_nodes <- which(tree$terminal)
    return(terminal_nodes)
}

#' Get second generation internal nodes.
#' 
#' Given a tree, it returns the indexes for the second generation internal nodes.
#' (By second generation internal nodes we mean the internal nodes that have exactly 2 leaf children).  
#'  
#' @param tree A decision tree.
#' 
#' @return a vector of indexes of the second generation internal nodes. 
#' @export 
get_gen2_internal_nodes <- function(tree) {
    terminal_nodes <- get_terminal_nodes(tree)
    gen2_internal_nodes <- which(table(tree$parent[terminal_nodes]) == 2)
    gen2_internal_nodes <- as.integer(names(gen2_internal_nodes)) # necessary due to the fact that table gives names to vector elements 
    return(gen2_internal_nodes) 
}

#' Get node depth
#' 
#' Given a tree and the index of one of its nodes, returns the depth for that node in the tree. 
#'  
#' @param tree A decision tree.
#' @param node_index Index of a node in the tree.
#' 
#' @return the depth of the node in the tree. 
#' @export 
get_node_depth <- function(tree, node_index) {
    node_position <- tree$position[node_index]
    depth <- floor(log2(node_position))
    return(depth)
}

#' Get node index by position
#' 
#' Given a tree and the position of one of its nodes, returns the index where the data of that node can be found. 
#'  
#' @param tree A decision tree.
#' @param node_position Position of a node in the tree.
#' 
#' @return the index of the node corresponding to the given position. If the node position is not in the tree, returns an empty array. 
#' @export 
get_node_index_by_pos <- function(tree, node_position) {
    return(which(tree$position == node_position))
}