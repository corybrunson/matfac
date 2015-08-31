#' Seek optimum row and column subsets
#' 
#' This function applies the heuristic \code{\link{ladderOptima}} search to both
#' a given matrix and its transpose, and synthesizes the results into optimal
#' row and colmun subsets.
#' @param mat A matrix.
#' @param pareto Logical; whether to restrict the results to Pareto-optimal 
#'   subsets.
#' @export

# Seek out optima in both directions
findOptima <-
    function(mat, pareto = FALSE, to.remove = FALSE, inf.na = TRUE) {
        
        # If infinite values are to be considered missing, remove them
        mat[is.infinite(mat)] <- NA
        
        # Identify rows and columns with missing entries
        nar <- apply(mat, 1, function(vec) any(is.na(vec)))
        nac <- apply(mat, 2, function(vec) any(is.na(vec)))
        # Remove unnecessary rows and columns
        smat <- mat[which(nar), which(nac), drop = FALSE]
        if(all(dim(smat) == 0)) return(list(list(which(!nar), which(!nac))))
        
        # Ladder algorithm optima
        ladder.opt <- ladderOptima(smat, pareto = pareto, to.remove = to.remove)
        
        # Chute algorithm optima
        chute.opt <- lapply(ladderOptima(t(smat),
                                         pareto = pareto,
                                         to.remove = to.remove),
                            rev)
        
        # All optima
        all.opt <- unique(c(ladder.opt, chute.opt))
        if(pareto & length(all.opt) > 1) {
            dat <- data.frame(
                nr = sapply(all.opt, function(opt) length(opt[[1]])),
                nc = sapply(all.opt, function(opt) length(opt[[2]]))
            )
            all.opt <- all.opt[paretoDat(dat)]
        }
        
        # Clean up results
        all.opt <- lapply(all.opt, function(opt) {
            # Restore original row and column indices as well as names
            opt <- list(which(nar)[opt[[1]]], which(nac)[opt[[2]]])
            # If returning indices to keep, combine with those removed above
            if(!to.remove) opt <- list(
                sort(union(opt[[1]], which(!nar))),
                sort(union(opt[[2]], which(!nac)))
            )
            opt
        })
        
        all.opt
    }
