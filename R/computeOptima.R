#' Use matrix bigraph to find matrix optimum row and column subsets.
#' 
#' This function applies \code{\link{computeBicliques}} the bigraph constructed
#' from the row and column indices of a given matrix, wherein a row and column
#' are tied if their intersection is a non-missing cell.
#' @param mat A matrix.
#' @export

computeOptima <-
    function(mat) {
        
        # If infinite values are to be considered missing, remove them
        if(inf.na) mat[is.infinite(mat)] <- NA
        
        # Identify rows and columns with missing entries
        nar <- apply(mat, 1, function(vec) any(is.na(vec)))
        nac <- apply(mat, 2, function(vec) any(is.na(vec)))
        # Remove unnecessary rows and columns
        smat <- mat[which(nar), which(nac), drop = FALSE]
        if(all(dim(smat) == 0)) return(list(list(which(!nar), which(!nac))))
        
        # Construct bigraph
        smat[!is.na(smat)] <- 1
        smat[is.na(smat)] <- 0
        graph <- graph.incidence(smat)
        
        # Compute bicliques!
        cb <- computeBicliques(graph)
        
        # Re-index bicliques to row and column indices
        for(i in 1:length(cb)) {
            if(length(cb[[i]]) == 2) {
                cb[[i]][[2]] <- cb[[i]][[2]] + nrow(smat)
            }
        }
        
        # Include trivial solutions (all rows, no columns & vice-versa)
        # ???
        
        # Remove empties
        empty <- sapply(cb, function(lst) sum(sapply(lst, length)) == 0)
        cb <- cb[-which(empty)]
        
        # Clean up results
        cb <- lapply(cb, function(opt) {
            # Restore original row and column indices as well as names
            list(which(nar)[opt[[1]]], which(nac)[opt[[2]]])
        })
        
        cb
    }
