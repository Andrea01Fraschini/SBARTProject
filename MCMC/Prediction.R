Mean.predict <- function(
    dt, # list of decision trees
    x.list, # list of all covariates 
    x.mult, 
    xcut, # partition of the predictor space 
    n.available # number of availavle observatons
) {
    n <- n.available
    p <- length(x.list) # number of covariates

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
    x.list.temp <- x.mult

    print(x.list.temp)

    for (i in 1:terminal.len) {
        count <- 0
        
        while (count < length(split.hist[[i]])) {
            browser() # TODO: fix

            count <- count + 1
            if (side.hist[[i]][count] == 0) {
                sub.ind <- which(x.list.temp[[split.hist[[i]][count]]] < xcut[[split.hist[[i]][count]]][value.hist[[i]][count]])
                x.list.temp <- lapply(x.list.temp, function(x) x[sub.ind])
            } else {
                sub.ind <- which(x.list.temp[[split.hist[[i]][count]]] >= xcut[[split.hist[[i]][count]]][value.hist[[i]][count]])
                x.list.temp <- lapply(x.list.temp, function(x) x[sub.ind])
            }
        }

        sel_ind.list[[i]] <- x.list.temp[[(p + 1)]]
        
        #  print(x.list.temp)
        print(sel_ind.list)

        x.list.temp <- lapply(x.mult, function(x) x[-sel_ind.list[[i]]])
        T[sel_ind.list[[i]]] <- dt$mu[terminal_nodes[i]]
    }

    return(list(T = T))
}