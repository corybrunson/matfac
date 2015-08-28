#' Identify the Pareto maximum rows of a data frame (preserving ties)
#' 
#' @param dat A data frame.
#' @param logic Logical; whether to return a logical vector of length the number
#'   of entries in \code{dat} or a vector of entry indices of \code{dat}.
#' @export

paretoDat <-
    function(dat, logic = FALSE) {
    uni <- unique(dat)
    par <- apply(dat, 1, function(row) {
        geqs <- uni >= sapply(row, rep, times = nrow(uni))
        length(which(apply(geqs, 1, all))) == 1
    })
    if(logic) par else which(par)
}
