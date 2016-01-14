#' Identify bicliques.
#' 
#' This function identifies the maximal bicliques in a give bipartite graph. It 
#' was obtained from \url{http://stackoverflow.com/a/31048125/4556798} on 1 
#' September 2015.
#' @param graph A bipartite graph.
#' @param k Integer; the minimum number of nodes of type \code{FALSE}. Defaults
#'   to 2.
#' @param l Integer; the minimum number of nodes of type \code{TRUE}. Defaults
#'   to 2.

computeBicliques <- function(graph, k = 2, l = 2) {
    
    stopifnot(require(igraph))
    
    vMode1 <- c()
    if (!is.null(V(graph)$type)) {
        
        vMode1 <- which(!V(graph)$type)
        vMode1 <- intersect(vMode1, which(degree(graph) >= l))
    }
    
    nb <- get.adjlist(graph)
    
    bicliques <- list()
    
    if (length(vMode1) >= k) {
        
        comb <- combn(vMode1, k)
        i <- 1
        sapply(1:ncol(comb), function(c) {
            
            commonNeighbours <- c()
            isFirst <- TRUE
            
            sapply(comb[,c], function(n) {
                
                if (isFirst) {
                    
                    isFirst <<- FALSE
                    commonNeighbours <<- nb[[n]]
                } else {
                    
                    commonNeighbours <<- intersect(commonNeighbours, nb[[n]])
                }
            })
            
            if (length(commonNeighbours) >= l) {
                
                bicliques[[i]] <<- list(m1=comb[,c], m2=commonNeighbours)
            }
            
            i <<- i + 1
        })
    }
    bicliques
}
