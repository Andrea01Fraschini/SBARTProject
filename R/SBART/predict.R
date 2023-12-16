mean_predict <- function(
    dt, # list of decision trees
    x_list, # list of all covariates 
    x_mult, 
    x_cut, # partition of the predictor space 
    n_available # number of availavle observatons
) {
    n <- n_available
    p <- length(x_list) # number of covariates

    terminal_nodes <- which(dt$terminal)
    terminal.len <- length(terminal_nodes)

    if (terminal.len <= 1) {
        return(list(T = rep(dt$mu, n)))
    }

    split.hist <- list()
    value.hist <- list()
    side.hist <- list()

    # traverse tree
    for (i in 1:terminal.len) {
        parent.node <- dt$parent[terminal_nodes[i]]
        current.node <- dt$position[terminal_nodes[i]]
        split.hist[[i]] <- dt$split[dt$position == parent.node]
        value.hist[[i]] <- dt$value[dt$position == parent.node]
        side.hist[[i]] <- (current.node) %% 2
        
        while (parent.node != 1) {
            current.node <- dt$position[dt$position == parent.node]
            parent.node <- dt$parent[dt$position == parent.node]
            split.hist[[i]] <- c(split.hist[[i]], dt$split[dt$position == parent.node])
            value.hist[[i]] <- c(value.hist[[i]], dt$value[dt$position == parent.node])
            side.hist[[i]] <- c(side.hist[[i]], (current.node) %% 2)
        }

        split.hist[[i]] <- rev(split.hist[[i]])
        value.hist[[i]] <- rev(value.hist[[i]])
        side.hist[[i]] <- rev(side.hist[[i]])
    }

    # TODO: fix, there should be no NaNs in 
    # sel_ind at the end of each iteration. 
    sel_ind.list <- list()
    T <- rep(0, n)

    x_list.temp <- x_mult

    for (i in 1:terminal.len) {
        count <- 0
        
        while (count < length(split.hist[[i]])) {

            count <- count + 1
            if (side.hist[[i]][count] == 0) {
                sub.ind <- which(x_list.temp[[split.hist[[i]][count]]] < x_cut[[split.hist[[i]][count]]][value.hist[[i]][count]])
                x_list.temp <- lapply(x_list.temp, function(x) x[sub.ind])
            } else {
                sub.ind <- which(x_list.temp[[split.hist[[i]][count]]] >= x_cut[[split.hist[[i]][count]]][value.hist[[i]][count]])
                x_list.temp <- lapply(x_list.temp, function(x) x[sub.ind])
            }
        }

        sel_ind.list[[i]] <- x_list.temp[[(p + 1)]]

        x_list.temp <- lapply(x_mult, function(x) x[-sel_ind.list[[i]]])
        T[sel_ind.list[[i]]] <- dt$mu[terminal_nodes[i]]   
    }

    if(length(T)!=225){
        print(length(T))
    }

    return(list(T = T))
}