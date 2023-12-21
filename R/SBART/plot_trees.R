source("R/common/tree_utilities.R")

# ---- Example of use: ----
# trees_list <- list()
# example_dt <- list(
#     position = c(1, 2, 3, 6, 7),
#     terminal = c(FALSE, TRUE, FALSE, TRUE, TRUE),
#     split = c(3, NA, 4, NA, NA),
#     value = c(6, NA, 9, NA, NA),
#     mu = c(NA, 34.1, NA, 1.2, 0.86),
#     (other fields) = (...)
# )
#
# trees_list[[1]] <- exmaple_dt
# plot_decision_trees(trees_list)
# --------------------------

#' Plot decision trees
#'
#' This function creates a single plot containing all decision trees.
#'
#' @param trees A list of decision trees. If no tree_format_adapter is specified, the trees must be a list with (at least) the following fileds:
#' - position: position of the nodes,
#' - terminal: TRUE if the node is a leaf node, FALSE otherwise,
#' - split: indexes of the split variables,
#' - value: indexes of the values of the splitting constants (related to the splitting variables),
#' - mu: if the node is a leaf, this represents the prediction made by the node.
#' @param ncol An integer indicating the number of columns in the plot.
#' @param nrow An integer indicating the number of rows in the plot.
#' @param tree_format_adapter A function that takes a single tree as an agument and formats it in a specific way so that it can be plotted.
#'
#' @export
plot_decision_trees <- function(trees, ncol = 10L, nrow = ceiling(length(trees) / ncol), tree_format_adapter = default_dt_adapter) {
    tree_structures <- list()
    for (i in 1:length(trees)) {
        tree_structures[[i]] <- tree_format_adapter(trees[[i]])
    }
    combineWidgets(list = lapply(tree_structures, function(tree) plot(tree)), ncol = ncol, nrow = nrow)
}

#' Default DT adapter
#'
#' Adapts the input decision tree structure in a data.tree tree.
#'
#' @param dt A decision tree. The tree must be a list with (at least) the following fileds:
#' - position: position of the nodes,
#' - terminal: TRUE if the node is a leaf node, FALSE otherwise,
#' - split: indexes of the split variables,
#' - value: indexes of the values of the splitting constants (related to the splitting variables),
#' - mu: if the node is a leaf, this represents the prediction made by the node.
#'
#' @return a decision tree in a data.tree "object".
default_dt_adapter <- function(dt) {
    head_ptr <- Node$new("HEAD")
    add_children_recursive(dt, node_index = 1, head_ptr)
    return(head_ptr$children[[1]])
}

#' Format node data
#'
#' Given a node, extracts only the relevant information to create a node to plot.
#'
#' @param dt A decision tree.
#' @param node_index Index of the node to consider.
#'
#' @return a formatted version of the given node cotaining only the data of interest.
format_node_data <- function(dt, node_index) {
    node <- list()
    if (dt$terminal[node_index]) {
        node$text <- toString(dt$mu[node_index])
        node$terminal <- TRUE
    } else {
        split_var <- dt$split[node_index]
        split_const <- dt$value[node_index]
        node$text <- paste(paste0("x", split_var), "<", paste0("c", split_var, ",", split_const), sep = " ")
        node$terminal <- FALSE
    }

    return(node)
}

#' Add children (recursive)
#'
#' Given a subtree, the position of its root, and the parent, recursively builds the tree in a new format.
#'
#' @param dt A decision subtree.
#' @param node_index Index of the root node of the subtree.
#' @param parent Parent node of the root node of the subtree.
#'
#' @return a formatted version of the subtree ready to be plotted.
add_children_recursive <- function(dt, node_index, parent) {
    current_node_data <- format_node_data(dt, node_index)
    current_node <- parent$AddChild(current_node_data$text)

    if (current_node_data$terminal == FALSE) {
        current_position <- dt$position[node_index]
        left_child_position <- current_position * 2
        right_child_position <- current_position * 2 + 1
        left_child_index <- get_node_index_by_pos(dt, left_child_position)
        right_child_index <- get_node_index_by_pos(dt, right_child_position)
        if (length(left_child_index) > 0) add_children_recursive(dt, left_child_index, current_node)
        if (length(right_child_index) > 0) add_children_recursive(dt, right_child_index, current_node)
    }
}
