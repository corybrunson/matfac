#' Seek optimum row subsets
#' 
#' This function conducts a heuristic search for row subsets that minimize the 
#' number of omitted rows that remove all missing values from a given matrix.
#' @param mat A matrix.
#' @param pareto Logical; whether to restrict the results to Pareto-optimal 
#'   subsets.
#' @param to.remove Logical; whether to return subsets of rows and columns to
#'   keep (\code{FALSE}) or to remove (\code{TRUE}).
#' @export

# Optimum seeker starting with all rows and no empty rows/columns
ladderOptima <-
    function(mat, pareto = FALSE, to.remove = FALSE) {
        
    # Start with a single fresh optimal solution: all rows
    fresh <- list(list(1:nrow(mat), c()))
    
    # Assume that the opposite optimum solution is already spent
    spent <- list(list(c(), 1:ncol(mat)))
    
    # While fresh optima remain...
    while(length(fresh) > 0) {
        
        # Update variables
        rem <- fresh[[1]]
        fresh <- setdiff(fresh, list(rem)) # not yet sprouted
        spent <- c(spent, list(rem)) # yet sprouted
        newopt <- list() # sprout
        
        # For every row...
        for(r in rem[[1]]) {
            rem2 <- rem
            # Remove the row
            rem2[[1]] <- setdiff(rem2[[1]], r)
            # Add any columns with NAs in the row
            rem2[[2]] <- sort(union(rem2[[2]], which(is.na(mat[r, ]))))
            # Remove any rows with NAs only in omitted columns
            rem2[[1]] <- setdiff(rem2[[1]], which(apply(mat, 1, function(vec) {
                all(which(is.na(vec)) %in% rem2[[2]])
            })))
            # Save new optimum
            newopt <- c(newopt, list(rem2))
        }
        newopt <- setdiff(unique(newopt), c(fresh, spent))
        fresh <- c(fresh, newopt)
        
        # Restrict to pareto optimal fresh subsets
        if(pareto & length(fresh) > 1) {
            dat <- data.frame(
                nr = sapply(fresh, function(opt) -length(opt[[1]])),
                nc = sapply(fresh, function(opt) -length(opt[[2]]))
            )
            fresh <- fresh[paretoDat(dat)]
        }
    }
    
    if(!to.remove) {
        
        # Convert the collection to one of row and column subsets to include
        spent <- lapply(spent, function(rem) {
            list(setdiff(1:nrow(mat), rem[[1]]), setdiff(1:ncol(mat), rem[[2]]))
        })
    }
    spent
}
