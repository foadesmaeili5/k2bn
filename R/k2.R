
.f3 <- function(x,val) {
  ###########
  if(!is.factor(x[[1]])) {
    x <- lapply(x, function(y){
      factor(y,levels = 0:(length(unique(y))-1))
    })
    x <- as.data.frame(x)
    x <- x
  }

  #####
  r <- length(unique(x[[val]]))
  y <- aggregate(formula(paste(val,"~.")),data =x ,table)
  alpha <- y[[length(y)]]
  N <- rowSums(alpha)
  alpha2 <- lgamma(alpha+1)
  palpha <- apply(alpha2, 1, sum)
  res <- sum(lgamma(r)-lgamma(N+r) + palpha)
  return(res)
}
# -------------------------------------------------------------------------

#' k2 algorithm for structure learning
#' @details
#' K2 Algorithm is a structure learning algorithm for bayesian network, which is usefull
#' when the orders of the nodes are availeable, 
#' 
#' @note
#' the algorithm uses the logarithm of the score function.
#' 
#' @param variables A character vector specifying the variable names in order.
#' @param x data.frame containing the variables.
#' @param u The maximum number of parents for each variable.
#' @param show Logical, indicating whether to display intermediate steps (default is FALSE).
#'
#' @return A Bayesian network structure represented as a graph.
#'
#' @examples
#' require(bnlearn)
#' require(Rgraphviz)
#'
#' x1 <- c(1,1,0,1,0,0,1,0,1,0)
#' x2 <- c(0,1,0,1,0,1,1,0,1,0)
#' x3 <- c(0,1,1,1,0,1,1,0,1,0)
#' df <- data.frame(x1, x2, x3)
#'
#' arti_k2 <- k2(c("x1", "x2", "x3"), df, 3, TRUE)
#' graphviz.plot(arti_k2, shape = "ellipse")
#'
#' @references
#' Cooper, G. F., & Herskovits, E. (1992). A Bayesian method for the induction of probabilistic networks
#' from data. Machine learning, 9(4), 309-347.
#'
#' @seealso
#' \code{\link{graphviz.plot}}, \code{\link{empty.graph}}, \code{\link{arcs}}
#'
#' @importFrom bnlearn k2
#' @importFrom Rgraphviz graphviz.plot empty.graph arcs
#'
#' @keywords bayesian network structure learning k2 algorithm
#'
#' @export
k2 <- function(variables,x,u,show = FALSE) {
  parent <- list()
  length(parent) <- length(variables)
  names(parent) <- variables
  for(j in seq_along(variables)) {
    canbeParent <- variables[j:1]
    pold <- -Inf
    ppnew <- NA
    while(length(canbeParent)>0 && length(parent[[j]]) <u) {
      ppold <- pold
      pnew <- c()
      for(i in canbeParent) {
        if(i==variables[j]) {
          pnew[i] <- .f3(x[variables[j]],variables[j])
        } else {
          pnew[i] <- .f3(select(x,parent[[j]],i,variables[j]),variables[j])
        }
      }
      if(show) {
        print(noquote(paste0("var: ",variables[j])))
        print(noquote(paste0("parent: ",paste(parent[[j]],collapse = " "))))
        print(noquote(paste0("pold: ",pold)))
        print(pnew)
        print("------------------------------")
      }
      ppnew <- pnew[which.max(pnew)]
      if(ppnew>pold) {
        parent[[j]] <- c(parent[[j]],names(pnew)[which.max(pnew)])
        pold <- ppnew
        canbeParent <- canbeParent[-which(canbeParent %in% parent[[j]])]
      }

      if(any(parent[[j]] %in% variables[j])) {
        parent[[j]] <- parent[[j]][-which(parent[[j]] %in% variables[j])]
      }

      # canbeParent <- canbeParent[-which(canbeParent %in% variables[j])]
      if(ppold==pold) {
        canbeParent <- c()
      }
      #Sys.sleep(2)
    }
  }


  mx <- mapply(cbind, parent,names(parent))
  l <- sapply(mx,ncol)
  res <- do.call(rbind,mx[l>1])
  bnfile <- empty.graph(names(mx))
  arcs(bnfile) <- res

  return(bnfile)
}
