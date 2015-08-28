#' Seek optimum row and column subsets
#' 
#' This function applies the heuristic ladderOptima search to both a given
#' matrix and its transpose, and synthesizes the results into optimal row and
#' colmun subsets.
#' @param mat A matrix.
#' @param pareto Logical; whether to restrict the results to Pareto-optimal 
#'   subsets.
#' @export

# Seek out optima in both directions
findOptima <-
    function(mat, pareto = FALSE) {
        
        # Remove unnecessary rows and columns
        whr <- which(apply(mat, 1, function(vec) any(is.na(vec))))
        whc <- which(apply(mat, 2, function(vec) any(is.na(vec))))
        smat <- mat[whr, whc]
        
        # Ladder algorithm optima
        ladder.opt <- ladderOptima(smat, pareto = pareto)
        
        # Chute algorithm optima
        chute.opt <- lapply(ladderOptima(t(smat), pareto = pareto), rev)
        
        # All optima
        all.opt <- unique(c(ladder.opt, chute.opt))
        if(pareto & length(all.opt) > 1) {
            dat <- data.frame(
                nr = sapply(all.opt, function(opt) length(opt[[1]])),
                nc = sapply(all.opt, function(opt) length(opt[[2]]))
            )
            all.opt <- all.opt[paretoDat(dat)]
        }
        
        # Restores original row and column indexes as well as names
        lapply(all.opt, function(opt) list(whr[opt[[1]]], whc[opt[[2]]]))
    }
